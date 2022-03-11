import parser.regexlexer as lexer
import parser.RegexTree as RT

STARTNONTERM = "\31"
start = ""
table = {}

class Parser:
    def __init__(self,tks: str):
        self.stack = [start]
        self.semstack = []
        self.lookahead = 0
        self.tkstream = lexer.lex.start(tks)

    def parse(self):
        while len(self.stack) != 0:
            next = table[self.stack[0]][self.tkstream[self.lookahead]['symbol']]
            #accept
            if next == "accept":
                if len(self.semstack)>0: return self.semstack[0]
                else: 
                    print("CRINGE! Parse error: empty sem stack")
                    exit(1)
            #failure
            elif next == None:
                print("Parse error: cringe token " + str(self.tkstream[self.lookahead]))
                exit(1)
            #shift/goto
            elif isinstance(next,str):
                self.stack = [next] + self.stack
                self.lookahead += 1
            #reduce
            else:
                self.attemptSem()
                self.stack = [table[self.stack[next[1]]][next[0]]] + self.stack[next[1]:]
        print("Parse Error: unexpected EOF")
        exit(1)
    
    def attemptSem(self) -> None:
        if self.stack[0] == "union ::= re ALTERNATE simplere ● ":
            self.semstack = [RT.Split(self.semstack[1], self.semstack[0])] + self.semstack[2:]
        elif self.stack[0] == "concat ::= simplere basicre ● ":
            self.semstack = [RT.Concat(self.semstack[1], self.semstack[0])] + self.semstack[2:]
        elif self.stack[0] == "star ::= elemre KLEENE ● ":
            self.semstack = [RT.MeanKleene(self.semstack[0])] + self.semstack[1:]
        elif self.stack[0] == "plus ::= elemre PLUS ● ":
            self.semstack = [RT.Concat(self.semstack[0], RT.MeanKleene(self.semstack[0]))] + self.semstack[1:]
        elif self.stack[0] == "question ::= elemre QUESTION ● ":
            self.semstack = [RT.Split(self.semstack[0],None)] + self.semstack[1:]
        elif self.stack[0] == "char ::= LETTER ● ":
            self.semstack = [RT.Prim(self.tkstream[self.lookahead-1]["lexeme"], False)] + self.semstack
        elif self.stack[0] == "char ::= NEWLINE ● ":
            self.semstack = [RT.Prim("\n", False)] + self.semstack
        elif self.stack[0] == "char ::= SQUOTE ● ":
            self.semstack = [RT.Prim("\'", False)] + self.semstack
        elif self.stack[0] == "char ::= DQUOTE ● ":
            self.semstack = [RT.Prim("\"", False)] + self.semstack
        elif self.stack[0] == "char ::= SPACE ● ":
            self.semstack = [RT.Prim(" ", False)] + self.semstack
        elif self.stack[0] == "char ::= BSLASH ● ":
            self.semstack = [RT.Prim("\\", False)] + self.semstack
        elif self.stack[0] == "char ::= TAB ● ":
            self.semstack = [RT.Prim("\t", False)] + self.semstack
        elif self.stack[0] == "any ::= WILDCARD ● ":
            self.semstack = [RT.Prim(".", True)] + self.semstack
        elif self.stack[0] == "elemre ::= RBRAC ● ":
            self.semstack = [RT.Prim("]", False)] + self.semstack
        elif self.stack[0] == "elemre ::= CARET ● ":
            self.semstack = [RT.Prim("^", False)] + self.semstack
        elif self.stack[0] == "setitem ::= char ● ":
            self.semstack = [[self.semstack[0].regex]] + self.semstack[1:]
        elif self.stack[0] == "setitem ::= KLEENE ● ":
            self.semstack = [["*"]] + self.semstack
        elif self.stack[0] == "setitem ::= LPAREN ● ":
            self.semstack = [["("]] + self.semstack
        elif self.stack[0] == "setitem ::= RPAREN ● ":
            self.semstack = [[")"]] + self.semstack
        elif self.stack[0] == "setitem ::= PLUS ● ":
            self.semstack = [["+"]] + self.semstack
        elif self.stack[0] == "setitem ::= QUESTION ● ":
            self.semstack = [["?"]] + self.semstack
        elif self.stack[0] == "setitem ::= LBRAC ● ":
            self.semstack = [["["]] + self.semstack
        elif self.stack[0] == "setitem ::= ALTERNATE ● ":
            self.semstack = [["|"]] + self.semstack
        elif self.stack[0] == "setitem ::= WILDCARD ● ":
            self.semstack = [["."]] + self.semstack
        elif self.stack[0] == "negsetitem ::= CARET ● ":
            self.semstack = [["^"]] + self.semstack
        elif self.stack[0] == "setitems ::= setitem negsetitems ● ":
            self.semstack = [self.semstack[1] + self.semstack[0]] + self.semstack[2:]
        elif self.stack[0] == "negsetitems ::= negsetitem negsetitems ● ":
            self.semstack = [self.semstack[1] + self.semstack[0]] + self.semstack[2:]
        elif self.stack[0] == "range ::= char HYPHEN char ● ":
            char1 = self.semstack[1].regex
            char2 = self.semstack[0].regex
            if ord(char1) > ord(char2):
                raise Exception("Bad range: " + char1 + "-" + char2)
            self.semstack += list(map(lambda x : chr(x), range(ord(char1), ord(char2)+1))) + self.semstack[2:]
        elif self.stack[0] == "posset ::= LBRAC setitems RBRAC ● ":
            items = self.semstack[0]
            tree = RT.Prim(items[0], False)
            for i in items[1:]:
                tree = RT.Split(RT.Prim(i, False), tree)
            self.semstack = [tree] + self.semstack[1:]
        elif self.stack[0] == "negset ::= LBRAC CARET negsetitems RBRAC ● ":
            items = list(set(list(map(lambda x: chr(x), range(32, 127))) + ['\n', '\t']) - set(self.semstack[0]))
            if items == []:
                self.semstack = self.semstack[1:]
            tree = RT.Prim(items[0], False)
            for i in items[1:]:
                tree = RT.Split(RT.Prim(i, False), tree)
            self.semstack = [tree] + self.semstack[1:]