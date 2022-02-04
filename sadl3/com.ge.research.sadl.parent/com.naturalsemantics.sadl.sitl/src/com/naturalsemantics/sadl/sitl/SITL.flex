/************************************************************************
 * 
 * Project: SADL
 * Copyright 2007-2022 - General Electric Company, All Rights Reserved
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
/*
 * SADL Extension for SADL Import Template Language (SITL)
 * Copyright 2022 - Natural Semantics, LLC, All Rights Reserved
 */

WS=[\ \n\r\t]+
ANY_OTHER=.

QNAME_TERMINAL= {ID} ":" {ID}

ID="^"? [:jletter:] {ID_PART}* ("." {ID_PART}+)*
ID_PART= [:jletterdigit:] | "-" | "%" | "~"


ML_COMMENT="/*" ~"*/"
COMMENT_ERROR_PATTERN="/*" [^*]* ("*"+ [^/*] [^*]*)* "*"?
SL_COMMENT="/""/"[^\r\n]*(\r?\n)?

NUMBER = {DIGIT}+ ("." {DIGIT}+)? (("e"|"E") ("+"|"-")? {DIGIT}+)?
		|("." {DIGIT}+) (("e"|"E") ("+"|"-")? {DIGIT}+)?
DIGIT = [0-9]

STRING=("'"([^\\\']|{ESCAPE_SEQUENCE})*"'"?)|(\"([^\\\"]|{ESCAPE_SEQUENCE})*\"?)
ESCAPE_SEQUENCE=\\{ANY_OTHER}

%%

<YYINITIAL> {COMMENT_ERROR_PATTERN} { return 0; /* antlr <invalid> */ }