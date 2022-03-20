package lexer

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
		out[i] = -1
		if v == -1 {
			continue
		}
		if t, valid := spec[i].deltaT[v][c]; valid {
			out[i] = t
		}
	}
	return out
}

func Lex(spec []Automata, input []rune) (tkstream []Token, valid bool) {
	lineNum, colNum := 1, 1
	mach := make([]int, len(spec))
	token := make([]rune, 0, cap(input))
	var bestFit *Token = nil
	for len(input) != 0 {
		mv := moveAhead(input[0], spec, mach)
		if len(input) == 0 || death(mv) {
			if bestFit == nil {
				if len(token) == 0 {
					return tkstream, true
				}
				return tkstream, false
			}
			if len((*bestFit).symbol) >= 5 && (*bestFit).symbol[:5] == "(ERR)" {
				return tkstream, false
			}
			if (*bestFit).symbol != "(SKIP)" {
				tkstream = append(tkstream, *bestFit)
			}
			// Append the found token
			input = append(token[len((*bestFit).lexeme):], input...)
			// Clear the context
			token = make([]rune, 0, cap(input))
			mach = make([]int, len(spec))
			// Mark the best fit
			bestFit = nil
			// Continue to the next input
			continue
		}
		// If input exists, start reading
		mach = mv
		// Save the read input token
		token = append(token, input[0])
		if i := getIndex(spec, mach); i != -1 {
			bestFit = &Token{spec[i].action, token, [2]int{lineNum, colNum}}
		}
		colNum += 1
		// Check if it's a newline, adjust the line/column numbers
		if input[0] == '\n' {
			lineNum += 1
			colNum = 1
		}
		// Trim the input by removing the first element we read
		input = input[1:]
	}
	// Failed at best fit, bail
	if bestFit == nil {
		return tkstream, false
	}
	// Save the full token stream
	tkstream = append(tkstream, *bestFit)
	// Mark the terminator to end the stream token
	tkstream = append(tkstream, Token{"\x18", []rune{'\x18'}, [2]int{0, 0}})
	return tkstream, true
}
