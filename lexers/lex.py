
rules = []
EOF = "\30"

def getNextStates(machStates, c):
    out = []
    for state in range(len(machStates)):
        if c in rules[state]['dfa']['sigma'] and machStates[state] != None:
            cindex = rules[state]['dfa']['sigma'].index(c)
            out.append(rules[state]['dfa']['deltaT'][machStates[state]][cindex])
        else:
            out.append(None)
    return out

def death(states) -> bool:
    return len(list(filter(lambda x: x != None, states))) == 0     

def getIndex(states) -> int:
    for i in range(len(states)):
        if states[i] != None:
            return i
    return -1

def start(input: str):
    lineNum = 1
    colNum = 1
    machStates = list(map(lambda x: x['dfa']['startState'], rules))
    token = ""
    bestFit = None
    tkstream = []

    while input != "":
        if input == "" or death(nextStates := getNextStates(machStates, input[0])):
            if bestFit == None:
                if token == "":
                    return []
                else:
                    raise Exception("Lex Error: Unexpected EOF in token \'" + token + "\'")
            else:
                if len(bestFit["symbol"]) >= 5 and bestFit["symbol"][:5] == "(ERR)":
                    print("Lex Error:" + bestFit["symbol"][5:])
                    return tkstream, False
                elif bestFit["symbol"] != "(SKIP)":
                    tkstream.append(bestFit)
                input = token[len(bestFit['lexeme']):] + input
                token = ""
                machStates = list(map(lambda x: x['dfa']['startState'], rules))
                bestFit = None
        else:
            machStates = nextStates
            token = token + input[0]
            if (i := getIndex(machStates)) != -1:
                bestFit = {"symbol":rules[i]["action"], "lexeme":token, "pos":[lineNum, colNum]}
            if input[0] == '\n': 
                lineNum += 1
                colNum = 1
            else:
                colNum += 1
            input = input[1:]
    else:
        if bestFit == None:
            raise Exception("Lex Error: Unexpected EOF in token \'" + token + "\'")
        tkstream.append(bestFit)

    
    tkstream.append({"symbol":EOF,"lexeme":None,"pos":[lineNum,colNum]})
    return tkstream