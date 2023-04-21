package main

import (
	"bufio"
	"errors"
	"strconv"
	"strings"
)

const (
	OpNone = iota
	OpAnd
	OpOr
	OpXor
)

var ErrEmptyString = errors.New("empty string")

func init() {

}

func actualParseByte(s string) (b byte, err error) {
	base := 10

	if strings.HasPrefix(s, "&") || strings.HasPrefix(s, "$") {
		base = 16
		s = s[1:]
	} else if strings.HasPrefix(s, "%") {
		base = 2
		s = s[1:]
	}

	pb, err := strconv.ParseUint(s, base, 8)
	b = byte(pb)

	return
}

func parseByte(s string) (b byte, err error) {

	if len(s) == 0 {
		err = ErrEmptyString
		return
	}

	// Effects
	s = strings.ReplaceAll(s, "_effectNone", "&00")
	s = strings.ReplaceAll(s, "_effectSnow", "&10")
	s = strings.ReplaceAll(s, "_effectPaletteChange2", "&40")
	s = strings.ReplaceAll(s, "_effectPaletteChange", "&20")
	s = strings.ReplaceAll(s, "_effectGems", "&80")
	s = strings.ReplaceAll(s, "_effectDark", "&08")

	// Screen flag
	s = strings.ReplaceAll(s, "SCREEN_FLAGS_ITEM_PRESENT", "&04")

	// Tile flags
	s = strings.ReplaceAll(s, "_bitClimbable", "&80")
	s = strings.ReplaceAll(s, "_bitCollidable", "&40")
	s = strings.ReplaceAll(s, "_bitFlipped", "&20")
	s = strings.ReplaceAll(s, "_bitHookable", "&10")

	scanner := bufio.NewScanner(strings.NewReader(s))
	scanner.Split(bufio.ScanWords)

	currentOp := OpNone
	var current byte

scan:
	for scanner.Scan() {
		if scanner.Text() == "OR" {
			currentOp = OpOr
			continue
		}
		switch currentOp {
		case OpOr:
			current, err = actualParseByte(scanner.Text())
			if err != nil {
				break scan
			}
			b |= current
			currentOp = OpNone
		case OpNone:
			current, err = actualParseByte(scanner.Text())
			if err != nil {
				break scan
			}
			b = current
		}
	}

	return
}

// Parse byte(s) from an EQUB string. This may be 1 byte or several.
func parseBytesFromString(s string) (r []byte, err error) {

	// First, trim any comments off
	c := strings.Index(s, ";")

	if c != -1 {
		s = s[:c]
	}

	s = strings.TrimSpace(s)

	parts := strings.Split(s, ",")

	for _, v := range parts {
		var b byte
		b, err = parseByte(strings.TrimSpace(v))

		if err != nil {
			return
		}

		r = append(r, b)
	}

	return
}
