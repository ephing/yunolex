
def buildPython(outname: str, pname: str, rules):
    with open(outname,"w") as outfile:
        if pname != None:
            outfile.write("#!/usr/bin/env python3\nimport " + pname + ".lex as lex\n\nlex.rules = [\n")
        else:
            outfile.write("#!/usr/bin/env python3\nimport lex\n\nlex.rules = [\n")
        for x in range(len(rules)):
            outfile.write("\t" + str(rules[x]) + ",\n")
        if pname != None:
            outfile.write("]\n")
        else:
            outfile.write("]\n\n")
            outfile.write("lex.start()\n")

def buildHaskell(outname: str, rules):
    with open(outname, "w") as outfile:
        outfile.write("{-# LANGUAGE GADTs, FlexibleContexts #-}\nimport Lexer\nimport Data.Map\n\n")
        outfile.write("initAutomata :: [Lexer.Automata]\n")
        outfile.write("initAutomata = [\n")
        for x in rules:
            outfile.write("  (" + str(x['dfa']['startState']) + ", " + str(x['dfa']['finStates']) + ", ")
            if x['action'] == '(SKIP)': outfile.write("Skip")
            elif x['action'][:5] == '(ERR)': outfile.write("Err" + x['action'][5:])
            else: outfile.write("Name \"" + x['action'] + "\"")
            outfile.write(', fromList [\n')
            for y in range(len(x['dfa']['deltaT'])):
                if list(filter(lambda b: b != [], x['dfa']['deltaT'][y])) == []: continue
                outfile.write("    (" + str(y) + ", fromList [")
                for z in range(len(x['dfa']['deltaT'][y])):
                    if z != 0: outfile.write(',')
                    if x['dfa']['deltaT'][y][z] != []:
                        let = x['dfa']['sigma'][z]
                        if let == '\t': let = '\\t'
                        elif let == '\\': let = '\\\\'
                        elif let == '\n': let = '\\n'
                        elif let == '\'': let = '\\\''
                        outfile.write('\n      (\'' + let + "\', " + str(x['dfa']['deltaT'][y][z][0]) + ")")
                outfile.write("\n    ])")
            if x != rules[-1]: outfile.write("\n    ]),\n")
            else: outfile.write("\n  ])]")


def buildGo():
    pass
