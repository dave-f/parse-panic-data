package main

import (
	"bufio"
	"errors"
	"strconv"
	"strings"
	"regexp"
)

const (
	OpNone = iota
	OpAnd
	OpOr
	OpXor
)

var ErrEmptyString = errors.New("empty string")
var AsmConstants map[string]string

func init() {

	AsmConstants = make(map[string]string)

	AsmConstants["_bitNull"] = "&00"
    AsmConstants["_bitClimbable"] = "&80"
    AsmConstants["_bitCollidable"] = "&40"
    AsmConstants["_bitFlipped"]	= "&20"
    AsmConstants["_bitHookable"] = "&10"
	AsmConstants["_bitColour"] = "&80"
	AsmConstants["_effectNone"] = "&00"
    AsmConstants["_effectSnow"]	= "&10"
    AsmConstants["_effectPaletteChange"] = "&20"
    AsmConstants["_effectPaletteChange2"] = "&40"
    AsmConstants["_effectGems"] = "&80"
    AsmConstants["_effectDark"]	= "&08"
	AsmConstants["SCREEN_FLAGS_ITEM_PRESENT"] = "&04"
}

func parseByte(s string) (b byte, err error) {

	combine := false // or operation or not

	// We use a scanner as these can be quite involved, with X OR Y OR Z type stuff
	scanner := bufio.NewScanner(strings.NewReader(s))
	scanner.Split(bufio.ScanWords) 

	// Func
	f := func(cb byte, w string, combine *bool, base int) (b byte,err error) {
		nb, err := strconv.ParseUint(w,base,8)
		if err != nil {
			return
		}
		if *combine {
			b = cb | byte(nb)
			*combine = false
		} else {
			b = byte(nb)
		}
		return
	}

	for scanner.Scan() {
		w := scanner.Text()

		if realVal, ok := AsmConstants[w]; ok {
			w = realVal
		}

		if strings.EqualFold(w,"or") {
			combine = true
		} else if strings.HasPrefix(w,"&") || strings.HasPrefix(w,"$") {
			w = w[1:]
			b, err = f(b,w,&combine,16)
			if err != nil {
				return
			}
		} else if strings.HasPrefix(w,"%") {
			w = w[1:]
			b, err = f(b,w,&combine,2)
			if err != nil {
				return
			}
		} else if matched, _ := regexp.MatchString("[0-9]+",w); matched {
			b, err = f(b,w,&combine,10)
			if err != nil {
				return
			}
		} else {
			err = errors.New("unrecognised input")
			return
		}
	}

	err = scanner.Err()
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

	if s == "" {
		return nil, errors.New("no data")
	}

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
