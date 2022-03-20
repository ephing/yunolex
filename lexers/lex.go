package lex

type Automata struct {
	startState int
	finStates  []int
	action     string
	deltaT     map[int]map[rune]int
}

type Token struct {
	symbol string
	lexeme []rune
	pos    [2]int
}

func death(mach []int) bool {
	for _, v := range mach {
		if v != -1 {
			return false
		}
	}
	return true
}

func getIndex(spec []Automata, mach []int) int {
	for i := range spec {
		for _, f := range spec[i].finStates {
			if f == mach[i] {
				return i
			}
		}
	}
	return -1
}

func moveAhead(c rune, spec []Automata, mach []int) []int {
	out := make([]int, len(mach), cap(mach))
	for i, v := range mach {
		if v != -1 {
			if t, valid := spec[i].deltaT[v][c]; valid {
				out[i] = t
			} else {
				out[i] = -1
			}
		} else {
			out[i] = -1
		}
	}
	return out
}

func lex(spec []Automata, input []rune) (tkstream []Token, valid bool) {
	lineNum, colNum := 1, 1
	mach := make([]int, len(spec), len(spec))
	token := make([]rune, 0, cap(input))
	var bestFit *Token = nil

	for len(input) != 0 {
		if mv := moveAhead(input[0], spec, mach); len(input) == 0 || death(mv) {
			if bestFit == nil {
				if len(token) == 0 {
					return tkstream, true
				} else {
					return tkstream, false
				}
			} else {
				if len((*bestFit).symbol) >= 5 && (*bestFit).symbol[:5] == "(ERR)" {
					return tkstream, false
				} else if (*bestFit).symbol != "(SKIP)" {
					tkstream = append(tkstream, *bestFit)
				}
				input = append(token[len((*bestFit).lexeme):], input...)
				token = make([]rune, 0, cap(input))
				mach = make([]int, len(spec), len(spec))
				bestFit = nil
			}
		} else {
			mach = mv
			token = append(token, input[0])
			if i := getIndex(spec, mach); i != -1 {
				bestFit = &Token{spec[i].action, token, [2]int{lineNum, colNum}}
			}
			if input[0] == '\n' {
				lineNum += 1
				colNum = 1
			} else {
				colNum += 1
			}
			input = input[1:]
		}
	}
	if bestFit == nil {
		return tkstream, false
	}
	tkstream = append(tkstream, *bestFit)
	tkstream = append(tkstream, Token{"\x18", []rune{'\x18'}, [2]int{0, 0}})
	return tkstream, true
}
