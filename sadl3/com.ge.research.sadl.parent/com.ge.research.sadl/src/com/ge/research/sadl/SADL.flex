WS=[\ \n\r\t]+
ANY_OTHER=.

ID="^"? [:jletter:] {ID_PART}* ("." {ID_PART}+)*
ID_PART= [:jletterdigit:] | "-" | "%" | "~"

ML_COMMENT="/*" ~"*/"
COMMENT_ERROR_PATTERN="/*" [^*]* ("*"+ [^/*] [^*]*)* "*"?
SL_COMMENT="/""/"[^\r\n]*(\r?\n)?

NUMBER = {DIGIT}+ ("." {DIGIT}+)?
DIGIT = [0-9]
			 
STRING=("'"([^\\\']|{ESCAPE_SEQUENCE})*"'"?)|(\"([^\\\"]|{ESCAPE_SEQUENCE})*\"?)
ESCAPE_SEQUENCE=\\{ANY_OTHER}

%%

<YYINITIAL> {COMMENT_ERROR_PATTERN} { return 0; /* antlr <invalid> */ }