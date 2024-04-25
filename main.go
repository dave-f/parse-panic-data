package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Tile struct {
	Empty      bool
	Index      int
	FlippedX   bool
	Hookable   bool
	Climbable  bool
	Collidable bool
}

type Screen struct {
	Tileset      int
	LayoutIndex  int
	StringIndex  int
	EffectFlags  int
	NumberAliens int
	ExitNorth    int
	ExitSouth    int
	ExitEast     int
	ExitWest     int
	SanityLoss   bool
	HasItem      bool
}

type Layout struct {
	Tiles [12][8]Tile
}

type OutputFile struct {
	Version int
	Strings []string
	Screens []Screen
	Layouts []Layout
}

var Screens []Screen
var StringTable []string
var ScreenHeaders [][8]byte
var ScreenData [][12][8]byte
var ScreenTiles [][12][8]Tile
var ScreenTable []int

var StringRegexp = regexp.MustCompile(`^\.s[0-9]+:`)
var ScreenRegexp = regexp.MustCompile(`^\.screen[0-9]+Data`)
var LayoutRegexp = regexp.MustCompile(`^EQUW screen([0-9]+)Data.*`)

func buildStringTable() error {
	f, err := os.Open("../panic/memory.asm")

	if err != nil {
		return err
	}

	defer f.Close()
	scanner := bufio.NewScanner(f)

	for scanner.Scan() {
		if StringRegexp.MatchString(scanner.Text()) {
			if !scanner.Scan() {
				return scanner.Err()
			}
			idxOne := strings.Index(scanner.Text(), `"`)
			idxTwo := strings.LastIndex(scanner.Text(), `"`)
			if idxOne != -1 && idxTwo != -1 {
				StringTable = append(StringTable, scanner.Text()[idxOne+1:idxTwo])
			}
		}
	}

	return nil
}

func buildLevelTable() error {

	const (
		Undefined = iota
		Searching
		FalseBlock
		LevelBlock
	)

	f, err := os.Open("../panic/leveldata.asm")

	if err != nil {
		return err
	}

	defer f.Close()

	scanner := bufio.NewScanner(f)
	screens := 0
	bytesExpected := 0
	state := Searching
	var thisScreen bytes.Buffer

loop:
	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		switch state {
		case Searching:
			if l == ".screenTable:" {
				break loop
			}
			if l == "IF FALSE" {
				state = FalseBlock
				continue
			}
			if strings.HasPrefix(l, "EQUB") {
				b, err := parseBytesFromString(l[4:])
				if err != nil {
					return err
				}
				if len(b) > 8 {
					return errors.New("unexpected byte count")
				}
				thisScreen.Write(b)
				bytesExpected = 8 - len(b)
				state = LevelBlock
				continue
			}
		case FalseBlock:
			if l == "ENDIF" {
				state = Searching
			}
			continue
		case LevelBlock:
			if strings.HasPrefix(l, "EQUB") {
				b, err := parseBytesFromString(l[4:])
				if err != nil {
					return err
				}
				if len(b) > bytesExpected {
					return errors.New("unexpected byte count")
				}
				thisScreen.Write(b)
				bytesExpected = bytesExpected - len(b)
				if bytesExpected == 0 {
					screens++
					ScreenHeaders = append(ScreenHeaders, [8]byte((thisScreen.Bytes())))
					thisScreen.Reset()
					state = Searching
				}
				continue
			}
		}
	}

	if scanner.Err() != nil {
		return scanner.Err()
	}

	offs := 0

	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(l, "EQUW") {
			idx := 0
			if !strings.HasPrefix(l, "EQUW congratulationsScreen") {
				if strings.HasPrefix(l,"EQUW 0") {
					offs++
				} else {
					m := LayoutRegexp.FindStringSubmatch(l)
					if m == nil {
						return errors.New("unexpected data in screenTable")
					} else {
						idx, _ = strconv.Atoi(m[1])
						idx = idx - offs
					}
				}
				ScreenTable = append(ScreenTable, idx)
			}
		} else {
			break
		}
	}

	return scanner.Err()
}

// Ignore items and aliens for now, maybe if the header flags suggest these we can decode them
func buildScreenData(org bool) error {

	const (
		Undefined = iota
		Searching
		DecodingRow
		FalseBlock
	)

	f, err := os.Open("../panic/leveldata.asm")

	if err != nil {
		return err
	}

	defer f.Close()

	scanner := bufio.NewScanner(f)
	state := Searching
	var unpackedScreen [][8]byte
	rowCount := 0 // pack in at 12
	screen40 := false

	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		switch state {
		case Searching:
			if l == "IF FALSE" {
				state = FalseBlock
				continue
			}
			if ScreenRegexp.MatchString(scanner.Text()) {
				if strings.HasPrefix(scanner.Text(), ".screen40Data") {
					screen40 = true
				} else {
					screen40 = false
				}
				//fmt.Println("Decoding", scanner.Text())
				state = DecodingRow
			}
		case FalseBlock:
			if l == "ENDIF" {
				state = Searching
			}
			continue
		case DecodingRow:
			if strings.HasPrefix(l, "EQUB") {
				b, err := parseBytesFromString(l[4:])
				if err != nil {
					return err
				}
				if len(b) == 1 {
					// Special case for screen 40
					if screen40 && b[0] == 0x68 {
						continue
					} else if b[0] != 0 { // If there is only one byte this row, it must be 0 to indicate no data
						return errors.New("unexpected byte")
					} else {
						unpackedScreen = append(unpackedScreen, [8]byte{0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff})
					}
				} else {
					// Do the decode
					// But remember the RLE packing
					// 0xf0 is .. repeat for rest of row
					// 0xfn is .. repeat for n tiles
					var unpackedRow [8]byte
					controlByte := b[0]
					rest := bytes.NewBuffer(b[1:])
					runLengthCount := 0
					var runLengthByte byte
					idx := 0
					for i := 0; i < 8; i++ {
						if controlByte&0x80 == 0x80 { // Is there a tile here?
							if runLengthCount > 0 {
								unpackedRow[idx] = runLengthByte
								idx++
								runLengthCount--
							} else {
								tb, err := rest.ReadByte()
								if err != nil {

									// On screen 40, we check for an EOF.  This is because the data ends 1 byte early on the line,
									// followed by a label before the final byte so the code can patch the byte easily.
									if screen40 {
										if errors.Is(io.EOF, err) {
											//fmt.Println("Patching special byte")
											tb = 8 | 0x20 | 0x40
										} else {
											return err
										}
									} else {
										return err
									}
								}
								if tb&0xf0 == 0xf0 {
									runLengthCount = int(tb & 0xf)
									if runLengthCount == 0 { // 0xf0 means rest of row
										runLengthCount = 255
									} else {
										runLengthCount--
										if runLengthCount == 0 {
											return errors.New("unexpected run length count")
										}
									}
									runLengthByte, err = rest.ReadByte() // get the byte to repeat
									if err != nil {
										return err
									}
									unpackedRow[idx] = runLengthByte
									idx++
								} else {
									unpackedRow[idx] = tb
									idx++
								}
							}
						} else {
							unpackedRow[idx] = 0xff
							idx++
						}
						controlByte = controlByte << 1
					}
					if len(unpackedRow) != 8 {
						return errors.New("unexpected row length")
					}
					unpackedScreen = append(unpackedScreen, unpackedRow)
				}
				rowCount++
				if rowCount == 12 {
					if len(unpackedScreen) != 12 {
						return errors.New("unexpcted screensize")
					}
					ScreenData = append(ScreenData, [12][8]byte(unpackedScreen))
					rowCount = 0
					state = Searching
					unpackedScreen = nil
				}
			}
		}
	}

	return scanner.Err()
}

func buildScreenTiles() error {
	for _, v := range ScreenData {
		var newScreenTile [12][8]Tile
		for i := 0; i < 12; i++ {
			for j := 0; j < 8; j++ {
				thisByte := v[i][j]
				newTile := Tile{
					Empty:      thisByte == 0xff,
					Index:      int(thisByte & 0xf),
					FlippedX:   thisByte&0x20 == 0x20,
					Hookable:   thisByte&0x10 == 0x10,
					Climbable:  thisByte&0x80 == 0x80,
					Collidable: thisByte&0x40 == 0x40,
				}
				newScreenTile[i][j] = newTile
			}
		}
		ScreenTiles = append(ScreenTiles, newScreenTile)
	}

	return nil
}

func showScreen(s int) {

	f, err := os.Open("output.json")

	if err != nil {
		panic(err)
	}

	defer f.Close()

	b, err := io.ReadAll(f)

	if err != nil {
		panic(err)
	}

	var Test OutputFile
	err = json.Unmarshal(b, &Test)

	if err != nil {
		panic(err)
	}

	showScreen := func(i int) {
		for _, i := range Test.Layouts[i].Tiles {
			for _, j := range i {
				tileFlags := []byte{0x2e, 0x2e, 0x2e, 0x2e}
				if j.Collidable {
					tileFlags[0] = 'C' // Collide
				}
				if j.Climbable {
					tileFlags[1] = 'L' // Ladder
				}
				if j.Hookable {
					tileFlags[2] = 'H' // Hookable
				}
				if j.FlippedX {
					tileFlags[3] = 'X' // Flipped in X
				}
				if j.Empty {
					fmt.Printf("...... ")
				} else {
					fmt.Printf("%02d%s ", j.Index, string(tileFlags))
				}
			}
			fmt.Println()
		}
	}

	if s < len(Screens) {
		showScreen(Screens[s].LayoutIndex)
		fmt.Printf("Screen   : %d/%d\n", s, len(Screens)-1)
		fmt.Printf("String   : %d(%s)\n", Screens[s].StringIndex, StringTable[Screens[s].StringIndex])
		fmt.Println("Layout   :", Screens[s].LayoutIndex)
		fmt.Println("Tileset  :", Screens[s].Tileset)
		fmt.Println("Has Item :", Screens[s].HasItem)
		fmt.Println("San Loss :", Screens[s].SanityLoss)
	}
}

func main() {

	var scr = flag.Int("n", -1, "Display screen n (255=display all)")
	var org = flag.Bool("org", true, "Use original Mountain Panic format")

	flag.Parse()

	err := buildStringTable()

	if err != nil {
		panic(err)
	}

	err = buildLevelTable()

	if err != nil {
		panic(err)
	}

	err = buildScreenData(*org)

	if err != nil {
		panic(err)
	}

	err = buildScreenTiles()

	if err != nil {
		panic(err)
	}

	// Build proper level structure ready to marshal to XML/JSON
	for _, v := range ScreenHeaders {
		if v[1] != 45 { // 45 is the congratulations page which we do not have data for
			newScreen := Screen{
				Tileset:      int(v[0] >> 5),
				LayoutIndex:  int(ScreenTable[v[1]] - 1),
				StringIndex:  int(v[0] & 0x1f),
				EffectFlags:  int(v[2] & 0xf0),
				HasItem:      v[7]&0x1 == 0x1,
				SanityLoss:   v[7]&0x2 == 0x2,
				NumberAliens: 0,
				ExitNorth:    int(v[3]),
				ExitSouth:    int(v[4]),
				ExitEast:     int(v[5]),
				ExitWest:     int(v[6]),
			}
			Screens = append(Screens, newScreen)
		}
	}

	if *scr != -1 {
		showScreen(*scr)
		return
	}

	var Test OutputFile

	if *org {
		Test.Version = 1
	} else {
		Test.Version = 2
	}
	Test.Strings = StringTable
	Test.Screens = Screens

	for _, v := range ScreenTiles {
		var nsl Layout
		nsl.Tiles = v
		Test.Layouts = append(Test.Layouts, nsl)
	}

	b, err := json.MarshalIndent(Test, "", "  ")

	if err != nil {
		panic(err)
	}

	err = os.WriteFile("output.json", b, 0666)

	if err != nil {
		fmt.Println("Error writing data:", err)
	} else {
		fmt.Printf("Written %d screens\n", len(Screens))
	}
}
