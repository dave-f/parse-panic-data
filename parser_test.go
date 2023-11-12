package main

import "testing"

func SlicesAreEqual(a []byte, b []byte) bool {
	if len(a) != len(b) {
		return false
	}
	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
}

func TestEmptyString(t *testing.T) {
	got, _ := parseBytesFromString("")

	if got != nil {
		t.Errorf("Expected nil got %v", got)
	}
}

func TestSingleBytes(t *testing.T) {
	type TestCase struct {
		Input string
		ExpectedResult byte
	}

	var tests []TestCase

	tests = append(tests,TestCase{Input:"&23 OR $4",ExpectedResult:39})
	tests = append(tests,TestCase{Input:"&f",ExpectedResult:15})
	tests = append(tests,TestCase{Input:"&ff",ExpectedResult:255})
	tests = append(tests,TestCase{Input:"1 or 2 or 4",ExpectedResult:7})
	tests = append(tests,TestCase{Input:"$2 or &4",ExpectedResult:6})
	tests = append(tests,TestCase{Input:"23",ExpectedResult:23})
	tests = append(tests,TestCase{Input:"$2 or &4",ExpectedResult:6})
	tests = append(tests,TestCase{Input:"%1",ExpectedResult:1})
	tests = append(tests,TestCase{Input:"%1 or &2 or $4 or 8",ExpectedResult:15})
	tests = append(tests,TestCase{Input:"SCREEN_FLAGS_ITEM_PRESENT",ExpectedResult:4})

	for _,v := range tests {
		b, err := parseByte(v.Input)
		if err != nil {
			t.Error(err)
		}
		if b != v.ExpectedResult {
			t.Errorf("Expected %v got %v",v.ExpectedResult, b)
		}
	}
}

func TestMultipleBytes(t *testing.T) {

	type TestCase struct {
		Input string
		ExpectedResult []byte
	}

	var tests []TestCase

	tests = append(tests,TestCase{Input:"1,2,3,4",ExpectedResult:[]byte{1,2,3,4}})
	tests = append(tests,TestCase{Input:"1,&ff,$7f,%11,4",ExpectedResult:[]byte{1,255,127,3,4}})
	tests = append(tests,TestCase{Input:"_effectNone,_effectSnow,_effectPaletteChange,_effectPaletteChange2,_effectGems,_effectDark",ExpectedResult:[]byte{0,0x10,0x20,0x40,0x80,0x08}})

	for _,v := range tests {
		b, err := parseBytesFromString(v.Input)
		if err != nil {
			t.Error(err)
		}
		if !SlicesAreEqual(b,v.ExpectedResult) {
			t.Errorf("Expected %v got %v",v.ExpectedResult, b)
		}
	}
}
