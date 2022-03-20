
# This file contains functions that print out the lexical specification in various languages

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

def buildHaskell(outname: str, pname: str, rules):
    with open(outname, "w") as outfile:
        outfile.write("{-# LANGUAGE GADTs, FlexibleContexts #-}\n")
        if pname != None:
            outfile.write("module Lex_" + pname + " where\n")
        outfile.write("import Lexer\nimport Data.Map\n\n")
        outfile.write("initAutomata :: [Lexer.Automata]\n")
        outfile.write("initAutomata = [\n")
        for x in rules:
            outfile.write("  (" + str(x['dfa']['startState']) + ", " + str(x['dfa']['finStates']) + ", ")
            if x['action'] == '(SKIP)': outfile.write("Skip")
            elif x['action'][:5] == '(ERR)': outfile.write("Err \"" + x['action'][7:-1].replace('\"', '\\\"') + "\"")
            else: outfile.write("Name \"" + x['action'].replace('\"', '\\\"') + "\"")
            outfile.write(', fromList [')
            for y in range(len(x['dfa']['deltaT'])):
                if list(filter(lambda b: b != None, x['dfa']['deltaT'][y])) == []: continue
                outfile.write("\n    (" + str(y) + ", fromList [")
                for z in range(len(x['dfa']['deltaT'][y])):
                    if z != 0: outfile.write(',')
                    if x['dfa']['deltaT'][y][z] != []:
                        let = x['dfa']['sigma'][z]
                        if let == '\t': let = '\\t'
                        elif let == '\\': let = '\\\\'
                        elif let == '\n': let = '\\n'
                        elif let == '\'': let = '\\\''
                        outfile.write('\n      (\'' + let + "\', " + str(x['dfa']['deltaT'][y][z]) + ")")
                outfile.write("\n    ])")
            if x != rules[-1]: outfile.write("\n    ]),\n")
            else: outfile.write("\n  ])]")

def buildGo(outname: str, pname: str, rules):
    with open(outname, "w") as outfile:
        outfile.write("package " + pname + "\n\n")
        outfile.write("var Spec = []Automata{\n")
        for r in rules:
            outfile.write("\t{\n\t\t" + str(r['dfa']['startState']))
            outfile.write(", []int{" + str(r['dfa']['finStates'])[1:-1] + "}, \"" + r['action'].replace("\"","\\\"") + "\", map[int]map[rune]int{\n")
            for t in range(len(r['dfa']['deltaT'])):
                if len(list(filter(lambda x: x != None, r['dfa']['deltaT'][t]))) != 0:
                    outfile.write("\t\t\t" + str(t) + ": {")
                    for m in range(len(r['dfa']['deltaT'][t])):
                        if r['dfa']['deltaT'][t][m] == None: continue
                        c = r['dfa']['sigma'][m]
                        if c == '\'': c = "\\\'"
                        elif c == '\\': c = "\\\\"
                        elif c == '\t': c = "\\t"
                        elif c == '\n': c = "\\n"
                        outfile.write("\'" + c + "\': " + str(r['dfa']['deltaT'][t][m]) + ",")
                    outfile.write("},\n\t\t},\n")
            outfile.write("\t},\n")
        outfile.write("}\n")
