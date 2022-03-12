# this code is the majority of the tokenizer. Only code that gets generated is DFA stuff

rules = {}
EOF = "\30"
# keeps track of current and previous state, and whether a machine is accepted
class AMTracker:
    def __init__(self):
        self.reset()

    # delete search history
    def reset(self) -> None:
        self.states = [[rules[x]['dfa']['startState']] for x in rules]

    # transition machines based on input character
    def delta(self,c) -> None:
        for x in rules:
            try:
                cindex = rules[x]['dfa']['sigma'].index(c)
                self.states[x] = [rules[x]['dfa']['deltaT'][self.states[x][0]][cindex][0]] + self.states[x]
            except:
                self.states[x] = [None] + self.states[x]

    # returns non-dead machine index, or None if they're all dead
    def death(self):
        for st in range(len(self.states)):
            if self.states[st][0] != None:
                return st
        return None

    # checks all previous states and returns which machine was the winner as well as
    # how many characters we actually consume with this token
    def revert(self):
        for x in range(len(self.states[0])):
            for r in rules:
                try:
                    if self.states[r][x] in rules[r]['dfa']['finStates']:
                        return (r,len(self.states[0]) - x - 1)
                except:
                    pass            
        raise Exception("No matching tokens!")

sindex = 0
def peek(f:str) -> str:
    global sindex
    if sindex >= len(f): return ""
    return f[sindex]

def start(input: str):
    global sindex
    sindex = 0
    tkstream = []
    amt = AMTracker()
    lineNum = 1
    colNum = 1
    tsLine = 1
    tsCol = 1
    token = ""
    # while not EOF
    c = peek(input)
    while len(c) != 0:
        # move all machines
        amt.delta(c)
        # if they all dead
        if amt.death() == None:
            (rev, tk) = amt.revert()
            act = rules[rev]['action']
            if act[0:5] == "(ERR)":
                print("LEX ERROR: " + act[5:] + " [" + str(tsLine) + ", " + str(tsCol) + "]\n")
            else:
                if act[0] != '(':
                    if act[-2] == 'u': 
                        tkstream.append({
                            "symbol":act[:act.rfind(' ')],
                            "lexeme":token[:tk],"pos":[tsLine,tsCol]})
                    else: 
                        tkstream.append({
                            "symbol":act[:act.rfind(' ')],
                            "lexeme":None,"pos":[tsLine,tsCol]})
            amt.reset()
            # reset file reader to read from tk, in case revert() told us to consume fewer characters
            # then we already did
            sindex = sindex + tk - len(token)
            tsLine = lineNum
            tsCol = colNum - len(token) + tk
            token = ""
        else:
            sindex += 1
            token += c
            colNum += 1
            if c == "\n":
                lineNum += 1
                colNum = 1
            # handle no \n before EOF
            if len(peek(input)) == 0:
                (rev, tk) = amt.revert()
                act = rules[rev]['action']
                if act[0:5] == "(ERR)":
                    print("LEX ERROR: " + act[5:] + " [" + str(tsLine) + ", " + str(tsCol) + "]\n")
                else:
                    if act[0] != '(':
                        if act[-2] == 'u': 
                            tkstream.append({
                                "symbol":act[:act.rfind(' ')],
                                "lexeme":token[:tk],"pos":[tsLine,tsCol]})
                        else: 
                            tkstream.append({
                                "symbol":act[:act.rfind(' ')],
                                "lexeme":None,"pos":[tsLine,tsCol]})
        c = peek(input)
    tkstream.append({"symbol":EOF,"lexeme":None,"pos":[lineNum,colNum]})
    return tkstream