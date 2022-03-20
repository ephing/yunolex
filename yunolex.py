#!/usr/bin/env python3
import parser.regexparser as RP
import Automata as AM
import DFAPrinter as DP
import argparse
import sys, os, shutil
from build import *

# stored info from spec file
def buildRule(regex: str, action: str, dotdir: str):
    out = {}
    out["regex"] = regex
    out["action"] = action
    tree = RP.p.Parser(regex).parse()
    d = AM.buildAutomata(tree)
    if dotdir != None:
        DP.makeDotFile(d.sigma, d.deltaT, d.finStates, d.startState, regex, dotdir)
    out["dfa"] = AM.toDict(AM.toDFA3(AM.epsremove(d)))

    return out

# Entrypoint
def main():

    parser = argparse.ArgumentParser(description="Yunolex, the greatest lexer of them all")
    parser.add_argument('file', metavar="SPEC", help="format: python3 yunolex.py SPEC")
    parser.add_argument('-i', metavar="PACKAGE", type=str,
                        help="build a lexer package that integrates with other python code")
    parser.add_argument('-p', metavar="DIR", type=str,
                        help="create dot files in directory  representing automata used in lexing")
    parser.add_argument('-o', metavar="FILE", type=str,
                        help="name output file as FILE")
    parser.add_argument('-l', metavar='LANG', type=str,
                        help="Language output by yunolex (currently supports python, haskell, and go")
    args = parser.parse_args()

    if len(sys.argv) < 2:
        parser.print_help()
        exit(1)

    file = args.file
    pname = args.i
    dir = args.p
    outname = "lexer" if args.o is None else args.o
    i = pname is not None
    lang = 'python' if args.l is None else args.l

    if lang == None:
        lang = 'python'
    elif lang.lower() not in ['python', 'haskell', 'go']:
        parser.print_help()
        exit(1)
    else:
        lang = lang.lower()
    
    if i:
        try:
            os.mkdir(pname)
        except OSError:
            pass
        outname = pname + "/" + outname
        if lang == 'python':
            with open(pname + "/__init__.py", "w") as f:
                f.write("\n")
            shutil.copyfile("lexers/lex.py", pname + "/lex.py")
        elif lang == 'haskell':
            shutil.copyfile("lexers/lex.hs", pname + "/lex.hs")
        elif lang == 'go':
            with open(pname + "/lex.go", "w") as outfile:
                outfile.write("package " + pname + "\n")
                with open("lexers/lex.go", "r") as lexfile:
                    outfile.write(''.join(lexfile.readlines()[1:]))

    rules = []
    with open(file,"r") as file:
        count = 0
        line = file.readline().rstrip()
        while line:
            # (ERR)
            if line[-1] == '\"':
                action = "(ERR) " + line[line[:len(line)-1].rfind('\"'):]
                regex = line[:line[:len(line)-1].rfind('\"') - 7]
                rules += [buildRule(regex,action,dir)]
            # (SKIP)
            elif line[-1] == ')':
                regex = line[:line.rfind('(') - 1]
                action = line[line.rfind('('):]
                rules += [buildRule(regex,action,dir)]
            else:
                ind = line.rfind(' ')
                rules += [buildRule(line[:ind],line[ind + 1:],dir)]
            count += 1
            line = file.readline().rstrip()
    
    if lang == 'python':
        buildPython(outname, pname, rules)
    elif lang == 'haskell':
        buildHaskell(outname, pname, rules)
    elif lang == 'go':
        buildGo(outname, pname, rules)

if __name__ == "__main__":
    main()
