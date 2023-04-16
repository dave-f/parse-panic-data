package main

import (
	"bufio"
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
var ScreenHeaders [][5]byte

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
	numBlocks := 0
	state := Undefined

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
				//fmt.Println("Start level")
				_, err := parseBytesFromString(l[4:])
				if err != nil {
					return err
				}
				//fmt.Printf("%d",b)
				numBlocks = 4
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
				_, _ = parseBytesFromString(l[4:])
				numBlocks--
				if numBlocks == 0 {
					fmt.Println("End level")
					screens++
					//fmt.Printf("%d\n",0)
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
