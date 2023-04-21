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

	// another byte yet but not sure of its use
}

var StringTable []string
var StringRegexp = regexp.MustCompile(`^\.s[0-9]+:`)
var ScreenHeaders [][8]byte

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

const (
	Undefined = iota
	FalseBlock
	LevelBlock
)

func buildLevelTable() error {

	f, err := os.Open("../panic/leveldata.asm")

	if err != nil {
		return err
	}

	defer f.Close()

	scanner := bufio.NewScanner(f)
	screens := 0
	bytesExpected := 0
	state := Undefined
	var thisScreen bytes.Buffer

loop:
	for scanner.Scan() {
		l := strings.TrimSpace(scanner.Text())
		switch state {
		case Undefined:
			if l == ".screenTable:" {
				//fmt.Println("End of screen table")
				break loop
			}
			if l == "IF FALSE" {
				//fmt.Println("If false block")
				state = FalseBlock
				continue
			}
			if strings.HasPrefix(l, "EQUB") {
				fmt.Println("Start level")
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
				//fmt.Println("EndIf")
				state = Undefined
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
					fmt.Println("End level")
					screens++
					ScreenHeaders = append(ScreenHeaders, [8]byte(thisScreen.Bytes()))
					thisScreen.Reset()
					state = Undefined
				}
				continue
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
}
