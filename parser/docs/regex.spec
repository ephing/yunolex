[(] LPAREN false
[)] RPAREN false
[[] LBRAC false
] RBRAC false
[^^*+?|.()\\] LETTER true
[*] KLEENE false
[?] QUESTION false
[+] PLUS false
^ CARET false
[|] ALTERNATE false
- HYPHEN false
\\n NEWLINE false
\\_ SPACE false
\\\\ BSLASH false
\\' SQUOTE false
\\" DQUOTE false
\\t TAB false
[.] WILDCARD false
\_|\t|\n (SKIP)
. (ERR) "Bad input"