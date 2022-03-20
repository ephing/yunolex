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

// Lex creates a stream of tokens using a lexical specification and a list of runes
func Lex(spec []Automata, input []rune) (tkstream []Token, valid bool) {
	lineNum, colNum := 1, 1
	mach := make([]int, len(spec))
	token := make([]rune, 0, cap(input))
	var bestFit *Token = nil
	for len(input) != 0 {
		// Move all automata ahead 1 state using input[0]
		mv := moveAhead(input[0], spec, mach)
		// If entire input has been read, or every automata is in an invalid state
		if len(input) == 0 || death(mv) {
			// If no token has been recognized
			if bestFit == nil {
				if len(token) == 0 {
					return tkstream, true
				}
				return tkstream, false
			}
			// If the token is listed as an error in the specification
			if len((*bestFit).symbol) >= 5 && (*bestFit).symbol[:5] == "(ERR)" {
				return tkstream, false
			}
			if (*bestFit).symbol != "(SKIP)" {
				tkstream = append(tkstream, *bestFit)
			}
			// Un-read characters that were not a part of the bestFit token
			input = append(token[len((*bestFit).lexeme):], input...)
			// Clear the context
			token = make([]rune, 0, cap(input))
			mach = make([]int, len(spec))
			bestFit = nil
			// Continue to the next input
			continue
		}
		// Update machine states to reflect the consumption of the input rune
		mach = mv
		// Save the read input token, for if we need to un-read it later
		token = append(token, input[0])
		// Find the bestFit token if it currently exists
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
	// The final set of runes did not match a token, bail
	if bestFit == nil {
		return tkstream, false
	}
	// Add last token to the token stream
	tkstream = append(tkstream, *bestFit)
	// Mark the terminator to end the stream token
	tkstream = append(tkstream, Token{"\x18", []rune{'\x18'}, [2]int{0, 0}})
	return tkstream, true
}
