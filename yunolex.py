#!/usr/bin/env python3
import parser.regexparser as RP
import Automata as AM
import DFAPrinter as DP
import argparse
import sys, os, shutil

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

    parser = argparse.ArgumentParser(description="Yunolex, the greatest parser of them all")
    parser.add_argument('file', metavar="SPEC", help="format: python3 yunolex.py SPEC")
    parser.add_argument('-i', metavar="PACKAGE", type=str,
                        help="build a lexer package that integrates with other python code")
    parser.add_argument('-p', metavar="DIR", type=str,
                        help="create dot files in directory  representing automata used in lexing")
    parser.add_argument('-o', metavar="FILE", type=str,
                        help="name output file as FILE>")
    args = parser.parse_args()

    if len(sys.argv) < 2:
        parser.print_help()
        exit(1)

    file = args.file
    pname = args.i
    dir = args.p
    outname = "lexer" if args.o is None else args.o
    i = pname is not None
    
    if i:
        try:
            os.mkdir(pname)
        except OSError:
            pass
        outname = pname + "/" + outname
        with open(pname + "/__init__.py", "w") as f:
            f.write("\n")
        shutil.copyfile("parser/lex.py", pname + "/lex.py")

    with open(file,"r") as file:
        with open(outname,"w") as outfile:
            if i:
                outfile.write("#!/usr/bin/env python3\nimport lexer.lex \n\nlexer.lex.rules = {\n")
            else:
                outfile.write("#!/usr/bin/env python3\nimport lex\n\nlex.rules = {\n")
        count = 0
        line = file.readline().rstrip()
        while line:
            # (ERR)
            if line[-1] == '\"':
                action = "(ERR) " + line[line[:len(line)-1].rfind('\"'):]
                regex = line[:line[:len(line)-1].rfind('\"') - 7]
                with open(outname, "a") as outfile:
                    outfile.write("    " + str(count) + ": " + str(buildRule(regex,action,dir)) + ",\n")
            # (SKIP)
            elif line[-1] == ')':
                regex = line[:line.rfind('(') - 1]
                action = line[line.rfind('('):]
                with open(outname, "a") as outfile:
                    outfile.write("    " + str(count) + ": " + str(buildRule(regex,action,dir)) + ",\n")
            else:
                ind = line[:line.rfind(' ')].rfind(' ')
                with open(outname, "a") as outfile:
                    outfile.write("    " + str(count) + ": " + str(buildRule(line[:ind],line[ind + 1:],dir)) + ",\n")
            count += 1
            line = file.readline().rstrip()
        with open(outname, "a") as outfile:
            if i:
                outfile.write("}\n")
            else:
                outfile.write("}\n\n")
                outfile.write("lex.start()\n")

if __name__ == "__main__":
    main()
