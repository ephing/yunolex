#!/usr/bin/env python3
import parser.regexparser as RP
import Automata as AM
import DFAPrinter as DP
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

def printUsage():
    print("Format: python3 main.py <.spec>")
    print("-i <package>: Build a lexer package that integrates with other python code")
    print("-p <dir>: Create dot files in directory <dir> representing automata used in lexing")
    print("-o <file>: Name output file as <file>")

if len(sys.argv) < 2:
    printUsage()
    exit(1)
else:
    i = False
    p = False
    outname = "lexer"
    pname = None
    dir = None
    skip = False
    file = None
    for flag in range(1, len(sys.argv)):
        if skip: 
            skip = False
            continue
        if sys.argv[flag] == "-i":
            i = True
            pname = sys.argv[flag + 1]
            skip = True
        elif sys.argv[flag] == "-p":
            p = True
            dir = sys.argv[flag + 1]
            skip = True
        elif sys.argv[flag] == "-o":
            outname = sys.argv[flag + 1]
            skip = True
        elif file == None:
            file = sys.argv[flag]
        else:
            printUsage()
            exit(1)

    if file == None:
        printUsage()
        exit(1)
    # read and parse spec file

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
