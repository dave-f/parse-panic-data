package main

import (
	"fmt"
	"strconv"
	"strings"
)

func init() {

}

func parseByte(s string) (b byte, err error) {

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

// Parse byte(s) from an EQUB string. This may be 1 byte or several.
func parseBytesFromString(s string) (r []byte, err error) {

	// First, trim any comments off
	c := strings.Index(s, ";")

	if c != -1 {
		s = s[:c]
	}

	s = strings.TrimSpace(s)

	fmt.Println("Input", s)

	parts := strings.Split(s, ",")

	for _, v := range parts {
		var b byte
		b, err = parseByte(strings.TrimSpace(v))

		if err != nil {
			return
		}

		r = append(r, b)
	}

	fmt.Printf("Output %v\n", r)

	return
}
