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

func TestOneByte(t *testing.T) {
	got, _ := parseBytesFromString("255")
	want := []byte{255}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestOneHexByteWithAmpersand(t *testing.T) {
	got, _ := parseBytesFromString("&ff")
	want := []byte{255}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestOneHexByteWithDollar(t *testing.T) {
	got, _ := parseBytesFromString("$ff")
	want := []byte{255}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestOneBinaryByte(t *testing.T) {
	got, _ := parseBytesFromString("%11")
	want := []byte{3}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestMultipleBytes(t *testing.T) {
	got, _ := parseBytesFromString("1,2,3")
	want := []byte{1, 2, 3}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestMultipleBytesWithWhitespace(t *testing.T) {
	got, _ := parseBytesFromString(" 1, 2 ,3, 4")
	want := []byte{1, 2, 3, 4}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestMultipleBytesWithDifferentBases(t *testing.T) {
	got, _ := parseBytesFromString("1,&ff,$7f,%11,4")
	want := []byte{1, 255, 127, 3, 4}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestEffectStrings(t *testing.T) {
	got, _ := parseBytesFromString("_effectNone,_effectSnow,_effectPaletteChange,_effectPaletteChange2,_effectGems,_effectDark")
	want := []byte{0x00, 0x10, 0x20, 0x40, 0x80, 0x08}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestScreenFlagStrings(t *testing.T) {
	got, _ := parseBytesFromString("SCREEN_FLAGS_ITEM_PRESENT")
	want := []byte{0x04}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func TestBytesWithOrStatements(t *testing.T) {
	got, _ := parseBytesFromString("&01 OR &02 OR &03")
	want := []byte{3}

	if !SlicesAreEqual(got, want) {
		t.Errorf("Expected %v got %v", want, got)
	}
}

func NewTests(t *testing.T) {
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
