package main

import (
	"bufio"
	"bytes"
	"errors"

	//"errors"
	//"flag"
	"fmt"
	"regexp"

	//"io"
	//"io/ioutil"
	"os"
	"strings"
)

type Tile struct {
	Index     int
	FlippedX  bool
	FlippedY  bool
	Hookable  bool
	Climbable bool
}

type Screen struct {
	Tileset       int
	StringIndex   int
	ScreenIndex   int
	Flipped       bool
	EffectFlags   int
	ItemCollected bool
	NumberAliens  int
	ExitNorth     int
	ExitSouth     int
	ExitEast      int
	ExitWest      int
	Data          [8][12]Tile
}

var StringTable []string
var ScreenHeaders [][8]byte
var ScreenData [][]byte

var StringRegexp = regexp.MustCompile(`^\.s[0-9]+:`)
var ScreenRegexp = regexp.MustCompile(`^\.screen[0-9]+Data`)

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
					ScreenHeaders = append(ScreenHeaders, [8]byte(thisScreen.Bytes()))
					thisScreen.Reset()
					state = Searching
				}
				continue
			}
		}
	}

	return scanner.Err()
}

// Ignore items and aliens for now, maybe if the header flags suggest these we can decode them
func buildScreenData() error {

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
					fmt.Println("Ignoring screen 40 for now")
					continue
				} else {
					fmt.Println("Decoding", scanner.Text())
				}
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
					// If there is only one byte this row, it must be 0 to indicate no data
					if b[0] != 0 {
						return errors.New("unexpected byte")
					}
					unpackedScreen = append(unpackedScreen, [8]byte{})
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
									return err
								}
								if tb&0xf0 == 0xf0 {
									runLengthCount = int(tb & 0xf)
									if runLengthCount == 0 { // 0xf0 means rest of row
										runLengthCount = 255
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
							idx++
						}
						controlByte = controlByte << 1
					}
					// unpack
					// assert lenth of unpacked is the same?
					//}
					unpackedScreen = append(unpackedScreen, unpackedRow)
				}
				rowCount++
				if rowCount == 12 {
					rowCount = 0
					state = Searching
				}
			}
		}
	}

	return scanner.Err()
}

func main() {

	err := buildStringTable()

	if err != nil {
		panic(err)
	}

	fmt.Printf("Found %d strings\n", len(StringTable))

	err = buildLevelTable()

	if err != nil {
		panic(err)
	}

	fmt.Printf("Found %d screen headers\n", len(ScreenHeaders))

	for i, v := range ScreenHeaders {
		fmt.Printf("%d) %s\n", i, StringTable[v[0]&0x1f])
	}

	err = buildScreenData()

	if err != nil {
		panic(err)
	}

	// TODO Build proper level structure ready to marshal to XML/JSON
}
