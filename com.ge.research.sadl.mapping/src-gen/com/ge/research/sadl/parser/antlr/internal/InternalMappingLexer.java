package com.ge.research.sadl.parser.antlr.internal;

// Hack: Use our own Lexer superclass by means of import. 
// Currently there is no other way to specify the superclass for the lexer.
import org.eclipse.xtext.parser.antlr.Lexer;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class InternalMappingLexer extends Lexer {
    public static final int RULE_ID=5;
    public static final int T__26=26;
    public static final int T__25=25;
    public static final int T__24=24;
    public static final int T__23=23;
    public static final int T__22=22;
    public static final int RULE_ANY_OTHER=12;
    public static final int T__21=21;
    public static final int T__20=20;
    public static final int RULE_UNSIGNED_NUMBER=7;
    public static final int EOF=-1;
    public static final int RULE_SL_COMMENT=11;
    public static final int RULE_ML_COMMENT=10;
    public static final int T__19=19;
    public static final int RULE_STRING=4;
    public static final int T__16=16;
    public static final int T__15=15;
    public static final int T__18=18;
    public static final int T__17=17;
    public static final int RULE_EOS=6;
    public static final int T__14=14;
    public static final int T__13=13;
    public static final int RULE_WS=9;
    public static final int RULE_DIGITS=8;

    // delegates
    // delegators

    public InternalMappingLexer() {;} 
    public InternalMappingLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public InternalMappingLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g"; }

    // $ANTLR start "T__13"
    public final void mT__13() throws RecognitionException {
        try {
            int _type = T__13;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:11:7: ( 'uri' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:11:9: 'uri'
            {
            match("uri"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__13"

    // $ANTLR start "T__14"
    public final void mT__14() throws RecognitionException {
        try {
            int _type = T__14;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:12:7: ( 'alias' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:12:9: 'alias'
            {
            match("alias"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__14"

    // $ANTLR start "T__15"
    public final void mT__15() throws RecognitionException {
        try {
            int _type = T__15;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:13:7: ( 'import' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:13:9: 'import'
            {
            match("import"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__15"

    // $ANTLR start "T__16"
    public final void mT__16() throws RecognitionException {
        try {
            int _type = T__16;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:14:7: ( 'as' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:14:9: 'as'
            {
            match("as"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__16"

    // $ANTLR start "T__17"
    public final void mT__17() throws RecognitionException {
        try {
            int _type = T__17;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:15:7: ( '{' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:15:9: '{'
            {
            match('{'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__17"

    // $ANTLR start "T__18"
    public final void mT__18() throws RecognitionException {
        try {
            int _type = T__18;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:16:7: ( '}' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:16:9: '}'
            {
            match('}'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__18"

    // $ANTLR start "T__19"
    public final void mT__19() throws RecognitionException {
        try {
            int _type = T__19;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:17:7: ( 'has' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:17:9: 'has'
            {
            match("has"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__19"

    // $ANTLR start "T__20"
    public final void mT__20() throws RecognitionException {
        try {
            int _type = T__20;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:18:7: ( 'true' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:18:9: 'true'
            {
            match("true"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__20"

    // $ANTLR start "T__21"
    public final void mT__21() throws RecognitionException {
        try {
            int _type = T__21;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:19:7: ( 'false' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:19:9: 'false'
            {
            match("false"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__21"

    // $ANTLR start "T__22"
    public final void mT__22() throws RecognitionException {
        try {
            int _type = T__22;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:20:7: ( '-' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:20:9: '-'
            {
            match('-'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__22"

    // $ANTLR start "T__23"
    public final void mT__23() throws RecognitionException {
        try {
            int _type = T__23;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:21:7: ( '_' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:21:9: '_'
            {
            match('_'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__23"

    // $ANTLR start "T__24"
    public final void mT__24() throws RecognitionException {
        try {
            int _type = T__24;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:22:7: ( '()' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:22:9: '()'
            {
            match("()"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__24"

    // $ANTLR start "T__25"
    public final void mT__25() throws RecognitionException {
        try {
            int _type = T__25;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:23:7: ( '<' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:23:9: '<'
            {
            match('<'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__25"

    // $ANTLR start "T__26"
    public final void mT__26() throws RecognitionException {
        try {
            int _type = T__26;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:24:7: ( '>' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:24:9: '>'
            {
            match('>'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__26"

    // $ANTLR start "RULE_DIGITS"
    public final void mRULE_DIGITS() throws RecognitionException {
        try {
            int _type = RULE_DIGITS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:771:13: ( ( '0' .. '9' )+ )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:771:15: ( '0' .. '9' )+
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:771:15: ( '0' .. '9' )+
            int cnt1=0;
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>='0' && LA1_0<='9')) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:771:16: '0' .. '9'
            	    {
            	    matchRange('0','9'); 

            	    }
            	    break;

            	default :
            	    if ( cnt1 >= 1 ) break loop1;
                        EarlyExitException eee =
                            new EarlyExitException(1, input);
                        throw eee;
                }
                cnt1++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_DIGITS"

    // $ANTLR start "RULE_UNSIGNED_NUMBER"
    public final void mRULE_UNSIGNED_NUMBER() throws RecognitionException {
        try {
            int _type = RULE_UNSIGNED_NUMBER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:22: ( ( RULE_DIGITS | RULE_DIGITS '.' ( '0' .. '9' )* ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | '.' RULE_DIGITS ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | RULE_DIGITS ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:24: ( RULE_DIGITS | RULE_DIGITS '.' ( '0' .. '9' )* ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | '.' RULE_DIGITS ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | RULE_DIGITS ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:24: ( RULE_DIGITS | RULE_DIGITS '.' ( '0' .. '9' )* ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | '.' RULE_DIGITS ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | RULE_DIGITS ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )
            int alt8=4;
            alt8 = dfa8.predict(input);
            switch (alt8) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:25: RULE_DIGITS
                    {
                    mRULE_DIGITS(); 

                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:37: RULE_DIGITS '.' ( '0' .. '9' )* ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )?
                    {
                    mRULE_DIGITS(); 
                    match('.'); 
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:53: ( '0' .. '9' )*
                    loop2:
                    do {
                        int alt2=2;
                        int LA2_0 = input.LA(1);

                        if ( ((LA2_0>='0' && LA2_0<='9')) ) {
                            alt2=1;
                        }


                        switch (alt2) {
                    	case 1 :
                    	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:54: '0' .. '9'
                    	    {
                    	    matchRange('0','9'); 

                    	    }
                    	    break;

                    	default :
                    	    break loop2;
                        }
                    } while (true);

                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:65: ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )?
                    int alt4=2;
                    int LA4_0 = input.LA(1);

                    if ( (LA4_0=='E'||LA4_0=='e') ) {
                        alt4=1;
                    }
                    switch (alt4) {
                        case 1 :
                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:66: ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS
                            {
                            if ( input.LA(1)=='E'||input.LA(1)=='e' ) {
                                input.consume();

                            }
                            else {
                                MismatchedSetException mse = new MismatchedSetException(null,input);
                                recover(mse);
                                throw mse;}

                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:76: ( '+' | '-' )?
                            int alt3=2;
                            int LA3_0 = input.LA(1);

                            if ( (LA3_0=='+'||LA3_0=='-') ) {
                                alt3=1;
                            }
                            switch (alt3) {
                                case 1 :
                                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:
                                    {
                                    if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                                        input.consume();

                                    }
                                    else {
                                        MismatchedSetException mse = new MismatchedSetException(null,input);
                                        recover(mse);
                                        throw mse;}


                                    }
                                    break;

                            }

                            mRULE_DIGITS(); 

                            }
                            break;

                    }


                    }
                    break;
                case 3 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:101: '.' RULE_DIGITS ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )?
                    {
                    match('.'); 
                    mRULE_DIGITS(); 
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:117: ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )?
                    int alt6=2;
                    int LA6_0 = input.LA(1);

                    if ( (LA6_0=='E'||LA6_0=='e') ) {
                        alt6=1;
                    }
                    switch (alt6) {
                        case 1 :
                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:118: ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS
                            {
                            if ( input.LA(1)=='E'||input.LA(1)=='e' ) {
                                input.consume();

                            }
                            else {
                                MismatchedSetException mse = new MismatchedSetException(null,input);
                                recover(mse);
                                throw mse;}

                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:128: ( '+' | '-' )?
                            int alt5=2;
                            int LA5_0 = input.LA(1);

                            if ( (LA5_0=='+'||LA5_0=='-') ) {
                                alt5=1;
                            }
                            switch (alt5) {
                                case 1 :
                                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:
                                    {
                                    if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                                        input.consume();

                                    }
                                    else {
                                        MismatchedSetException mse = new MismatchedSetException(null,input);
                                        recover(mse);
                                        throw mse;}


                                    }
                                    break;

                            }

                            mRULE_DIGITS(); 

                            }
                            break;

                    }


                    }
                    break;
                case 4 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:153: RULE_DIGITS ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS
                    {
                    mRULE_DIGITS(); 
                    if ( input.LA(1)=='E'||input.LA(1)=='e' ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:773:175: ( '+' | '-' )?
                    int alt7=2;
                    int LA7_0 = input.LA(1);

                    if ( (LA7_0=='+'||LA7_0=='-') ) {
                        alt7=1;
                    }
                    switch (alt7) {
                        case 1 :
                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:
                            {
                            if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                                input.consume();

                            }
                            else {
                                MismatchedSetException mse = new MismatchedSetException(null,input);
                                recover(mse);
                                throw mse;}


                            }
                            break;

                    }

                    mRULE_DIGITS(); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_UNSIGNED_NUMBER"

    // $ANTLR start "RULE_EOS"
    public final void mRULE_EOS() throws RecognitionException {
        try {
            int _type = RULE_EOS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:10: ( '.' ( ' ' | '\\t' | '\\r' | '\\n' | EOF ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:12: '.' ( ' ' | '\\t' | '\\r' | '\\n' | EOF )
            {
            match('.'); 
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:16: ( ' ' | '\\t' | '\\r' | '\\n' | EOF )
            int alt9=5;
            switch ( input.LA(1) ) {
            case ' ':
                {
                alt9=1;
                }
                break;
            case '\t':
                {
                alt9=2;
                }
                break;
            case '\r':
                {
                alt9=3;
                }
                break;
            case '\n':
                {
                alt9=4;
                }
                break;
            default:
                alt9=5;}

            switch (alt9) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:17: ' '
                    {
                    match(' '); 

                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:21: '\\t'
                    {
                    match('\t'); 

                    }
                    break;
                case 3 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:26: '\\r'
                    {
                    match('\r'); 

                    }
                    break;
                case 4 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:31: '\\n'
                    {
                    match('\n'); 

                    }
                    break;
                case 5 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:775:36: EOF
                    {
                    match(EOF); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_EOS"

    // $ANTLR start "RULE_WS"
    public final void mRULE_WS() throws RecognitionException {
        try {
            int _type = RULE_WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:777:9: ( ( '\\u00A0' | ' ' | '\\t' | '\\r' | '\\n' )+ )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:777:11: ( '\\u00A0' | ' ' | '\\t' | '\\r' | '\\n' )+
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:777:11: ( '\\u00A0' | ' ' | '\\t' | '\\r' | '\\n' )+
            int cnt10=0;
            loop10:
            do {
                int alt10=2;
                int LA10_0 = input.LA(1);

                if ( ((LA10_0>='\t' && LA10_0<='\n')||LA10_0=='\r'||LA10_0==' '||LA10_0=='\u00A0') ) {
                    alt10=1;
                }


                switch (alt10) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:
            	    {
            	    if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||input.LA(1)=='\r'||input.LA(1)==' '||input.LA(1)=='\u00A0' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt10 >= 1 ) break loop10;
                        EarlyExitException eee =
                            new EarlyExitException(10, input);
                        throw eee;
                }
                cnt10++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_WS"

    // $ANTLR start "RULE_ID"
    public final void mRULE_ID() throws RecognitionException {
        try {
            int _type = RULE_ID;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:779:9: ( ( '^' )? ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '-' | '%' | '~' )* )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:779:11: ( '^' )? ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '-' | '%' | '~' )*
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:779:11: ( '^' )?
            int alt11=2;
            int LA11_0 = input.LA(1);

            if ( (LA11_0=='^') ) {
                alt11=1;
            }
            switch (alt11) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:779:11: '^'
                    {
                    match('^'); 

                    }
                    break;

            }

            if ( (input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:779:40: ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '-' | '%' | '~' )*
            loop12:
            do {
                int alt12=2;
                int LA12_0 = input.LA(1);

                if ( (LA12_0=='%'||LA12_0=='-'||(LA12_0>='0' && LA12_0<='9')||(LA12_0>='A' && LA12_0<='Z')||LA12_0=='_'||(LA12_0>='a' && LA12_0<='z')||LA12_0=='~') ) {
                    alt12=1;
                }


                switch (alt12) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:
            	    {
            	    if ( input.LA(1)=='%'||input.LA(1)=='-'||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z')||input.LA(1)=='~' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop12;
                }
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_ID"

    // $ANTLR start "RULE_STRING"
    public final void mRULE_STRING() throws RecognitionException {
        try {
            int _type = RULE_STRING;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:13: ( ( '\"' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\"' ) ) )* '\"' | '\\'' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\\'' ) ) )* '\\'' ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:15: ( '\"' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\"' ) ) )* '\"' | '\\'' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\\'' ) ) )* '\\'' )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:15: ( '\"' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\"' ) ) )* '\"' | '\\'' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\\'' ) ) )* '\\'' )
            int alt15=2;
            int LA15_0 = input.LA(1);

            if ( (LA15_0=='\"') ) {
                alt15=1;
            }
            else if ( (LA15_0=='\'') ) {
                alt15=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 15, 0, input);

                throw nvae;
            }
            switch (alt15) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:16: '\"' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\"' ) ) )* '\"'
                    {
                    match('\"'); 
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:20: ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\"' ) ) )*
                    loop13:
                    do {
                        int alt13=3;
                        int LA13_0 = input.LA(1);

                        if ( (LA13_0=='\\') ) {
                            alt13=1;
                        }
                        else if ( ((LA13_0>='\u0000' && LA13_0<='!')||(LA13_0>='#' && LA13_0<='[')||(LA13_0>=']' && LA13_0<='\uFFFF')) ) {
                            alt13=2;
                        }


                        switch (alt13) {
                    	case 1 :
                    	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:21: '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' )
                    	    {
                    	    match('\\'); 
                    	    if ( input.LA(1)=='\"'||input.LA(1)=='\''||input.LA(1)=='\\'||input.LA(1)=='b'||input.LA(1)=='f'||input.LA(1)=='n'||input.LA(1)=='r'||(input.LA(1)>='t' && input.LA(1)<='u') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;
                    	case 2 :
                    	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:66: ~ ( ( '\\\\' | '\"' ) )
                    	    {
                    	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='\uFFFF') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop13;
                        }
                    } while (true);

                    match('\"'); 

                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:86: '\\'' ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\\'' ) ) )* '\\''
                    {
                    match('\''); 
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:91: ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' ) | ~ ( ( '\\\\' | '\\'' ) ) )*
                    loop14:
                    do {
                        int alt14=3;
                        int LA14_0 = input.LA(1);

                        if ( (LA14_0=='\\') ) {
                            alt14=1;
                        }
                        else if ( ((LA14_0>='\u0000' && LA14_0<='&')||(LA14_0>='(' && LA14_0<='[')||(LA14_0>=']' && LA14_0<='\uFFFF')) ) {
                            alt14=2;
                        }


                        switch (alt14) {
                    	case 1 :
                    	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:92: '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | 'u' | '\"' | '\\'' | '\\\\' )
                    	    {
                    	    match('\\'); 
                    	    if ( input.LA(1)=='\"'||input.LA(1)=='\''||input.LA(1)=='\\'||input.LA(1)=='b'||input.LA(1)=='f'||input.LA(1)=='n'||input.LA(1)=='r'||(input.LA(1)>='t' && input.LA(1)<='u') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;
                    	case 2 :
                    	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:781:137: ~ ( ( '\\\\' | '\\'' ) )
                    	    {
                    	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='&')||(input.LA(1)>='(' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='\uFFFF') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop14;
                        }
                    } while (true);

                    match('\''); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_STRING"

    // $ANTLR start "RULE_ML_COMMENT"
    public final void mRULE_ML_COMMENT() throws RecognitionException {
        try {
            int _type = RULE_ML_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:783:17: ( '/*' ( options {greedy=false; } : . )* '*/' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:783:19: '/*' ( options {greedy=false; } : . )* '*/'
            {
            match("/*"); 

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:783:24: ( options {greedy=false; } : . )*
            loop16:
            do {
                int alt16=2;
                int LA16_0 = input.LA(1);

                if ( (LA16_0=='*') ) {
                    int LA16_1 = input.LA(2);

                    if ( (LA16_1=='/') ) {
                        alt16=2;
                    }
                    else if ( ((LA16_1>='\u0000' && LA16_1<='.')||(LA16_1>='0' && LA16_1<='\uFFFF')) ) {
                        alt16=1;
                    }


                }
                else if ( ((LA16_0>='\u0000' && LA16_0<=')')||(LA16_0>='+' && LA16_0<='\uFFFF')) ) {
                    alt16=1;
                }


                switch (alt16) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:783:52: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop16;
                }
            } while (true);

            match("*/"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_ML_COMMENT"

    // $ANTLR start "RULE_SL_COMMENT"
    public final void mRULE_SL_COMMENT() throws RecognitionException {
        try {
            int _type = RULE_SL_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:17: ( '//' (~ ( ( '\\n' | '\\r' ) ) )* ( ( '\\r' )? '\\n' )? )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:19: '//' (~ ( ( '\\n' | '\\r' ) ) )* ( ( '\\r' )? '\\n' )?
            {
            match("//"); 

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:24: (~ ( ( '\\n' | '\\r' ) ) )*
            loop17:
            do {
                int alt17=2;
                int LA17_0 = input.LA(1);

                if ( ((LA17_0>='\u0000' && LA17_0<='\t')||(LA17_0>='\u000B' && LA17_0<='\f')||(LA17_0>='\u000E' && LA17_0<='\uFFFF')) ) {
                    alt17=1;
                }


                switch (alt17) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:24: ~ ( ( '\\n' | '\\r' ) )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\t')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop17;
                }
            } while (true);

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:40: ( ( '\\r' )? '\\n' )?
            int alt19=2;
            int LA19_0 = input.LA(1);

            if ( (LA19_0=='\n'||LA19_0=='\r') ) {
                alt19=1;
            }
            switch (alt19) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:41: ( '\\r' )? '\\n'
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:41: ( '\\r' )?
                    int alt18=2;
                    int LA18_0 = input.LA(1);

                    if ( (LA18_0=='\r') ) {
                        alt18=1;
                    }
                    switch (alt18) {
                        case 1 :
                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:785:41: '\\r'
                            {
                            match('\r'); 

                            }
                            break;

                    }

                    match('\n'); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_SL_COMMENT"

    // $ANTLR start "RULE_ANY_OTHER"
    public final void mRULE_ANY_OTHER() throws RecognitionException {
        try {
            int _type = RULE_ANY_OTHER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:787:16: ( . )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:787:18: .
            {
            matchAny(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RULE_ANY_OTHER"

    public void mTokens() throws RecognitionException {
        // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:8: ( T__13 | T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | T__20 | T__21 | T__22 | T__23 | T__24 | T__25 | T__26 | RULE_DIGITS | RULE_UNSIGNED_NUMBER | RULE_EOS | RULE_WS | RULE_ID | RULE_STRING | RULE_ML_COMMENT | RULE_SL_COMMENT | RULE_ANY_OTHER )
        int alt20=23;
        alt20 = dfa20.predict(input);
        switch (alt20) {
            case 1 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:10: T__13
                {
                mT__13(); 

                }
                break;
            case 2 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:16: T__14
                {
                mT__14(); 

                }
                break;
            case 3 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:22: T__15
                {
                mT__15(); 

                }
                break;
            case 4 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:28: T__16
                {
                mT__16(); 

                }
                break;
            case 5 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:34: T__17
                {
                mT__17(); 

                }
                break;
            case 6 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:40: T__18
                {
                mT__18(); 

                }
                break;
            case 7 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:46: T__19
                {
                mT__19(); 

                }
                break;
            case 8 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:52: T__20
                {
                mT__20(); 

                }
                break;
            case 9 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:58: T__21
                {
                mT__21(); 

                }
                break;
            case 10 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:64: T__22
                {
                mT__22(); 

                }
                break;
            case 11 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:70: T__23
                {
                mT__23(); 

                }
                break;
            case 12 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:76: T__24
                {
                mT__24(); 

                }
                break;
            case 13 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:82: T__25
                {
                mT__25(); 

                }
                break;
            case 14 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:88: T__26
                {
                mT__26(); 

                }
                break;
            case 15 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:94: RULE_DIGITS
                {
                mRULE_DIGITS(); 

                }
                break;
            case 16 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:106: RULE_UNSIGNED_NUMBER
                {
                mRULE_UNSIGNED_NUMBER(); 

                }
                break;
            case 17 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:127: RULE_EOS
                {
                mRULE_EOS(); 

                }
                break;
            case 18 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:136: RULE_WS
                {
                mRULE_WS(); 

                }
                break;
            case 19 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:144: RULE_ID
                {
                mRULE_ID(); 

                }
                break;
            case 20 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:152: RULE_STRING
                {
                mRULE_STRING(); 

                }
                break;
            case 21 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:164: RULE_ML_COMMENT
                {
                mRULE_ML_COMMENT(); 

                }
                break;
            case 22 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:180: RULE_SL_COMMENT
                {
                mRULE_SL_COMMENT(); 

                }
                break;
            case 23 :
                // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:1:196: RULE_ANY_OTHER
                {
                mRULE_ANY_OTHER(); 

                }
                break;

        }

    }


    protected DFA8 dfa8 = new DFA8(this);
    protected DFA20 dfa20 = new DFA20(this);
    static final String DFA8_eotS =
        "\1\uffff\1\3\4\uffff";
    static final String DFA8_eofS =
        "\6\uffff";
    static final String DFA8_minS =
        "\2\56\4\uffff";
    static final String DFA8_maxS =
        "\1\71\1\145\4\uffff";
    static final String DFA8_acceptS =
        "\2\uffff\1\3\1\1\1\4\1\2";
    static final String DFA8_specialS =
        "\6\uffff}>";
    static final String[] DFA8_transitionS = {
            "\1\2\1\uffff\12\1",
            "\1\5\1\uffff\12\1\13\uffff\1\4\37\uffff\1\4",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA8_eot = DFA.unpackEncodedString(DFA8_eotS);
    static final short[] DFA8_eof = DFA.unpackEncodedString(DFA8_eofS);
    static final char[] DFA8_min = DFA.unpackEncodedStringToUnsignedChars(DFA8_minS);
    static final char[] DFA8_max = DFA.unpackEncodedStringToUnsignedChars(DFA8_maxS);
    static final short[] DFA8_accept = DFA.unpackEncodedString(DFA8_acceptS);
    static final short[] DFA8_special = DFA.unpackEncodedString(DFA8_specialS);
    static final short[][] DFA8_transition;

    static {
        int numStates = DFA8_transitionS.length;
        DFA8_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA8_transition[i] = DFA.unpackEncodedString(DFA8_transitionS[i]);
        }
    }

    class DFA8 extends DFA {

        public DFA8(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 8;
            this.eot = DFA8_eot;
            this.eof = DFA8_eof;
            this.min = DFA8_min;
            this.max = DFA8_max;
            this.accept = DFA8_accept;
            this.special = DFA8_special;
            this.transition = DFA8_transition;
        }
        public String getDescription() {
            return "773:24: ( RULE_DIGITS | RULE_DIGITS '.' ( '0' .. '9' )* ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | '.' RULE_DIGITS ( ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )? | RULE_DIGITS ( 'e' | 'E' ) ( '+' | '-' )? RULE_DIGITS )";
        }
    }
    static final String DFA20_eotS =
        "\1\uffff\3\30\2\uffff\3\30\1\uffff\1\42\1\26\2\uffff\1\46\1\51"+
        "\1\uffff\1\26\1\uffff\3\26\1\uffff\1\30\1\uffff\1\30\1\60\1\30\2"+
        "\uffff\3\30\7\uffff\1\46\5\uffff\1\65\1\30\1\uffff\1\30\1\70\2\30"+
        "\1\uffff\2\30\1\uffff\1\75\1\30\1\77\1\30\1\uffff\1\101\1\uffff"+
        "\1\102\2\uffff";
    static final String DFA20_eofS =
        "\103\uffff";
    static final String DFA20_minS =
        "\1\0\1\162\1\154\1\155\2\uffff\1\141\1\162\1\141\1\uffff\1\45\1"+
        "\51\2\uffff\1\56\1\60\1\uffff\1\101\1\uffff\2\0\1\52\1\uffff\1\151"+
        "\1\uffff\1\151\1\45\1\160\2\uffff\1\163\1\165\1\154\7\uffff\1\56"+
        "\5\uffff\1\45\1\141\1\uffff\1\157\1\45\1\145\1\163\1\uffff\1\163"+
        "\1\162\1\uffff\1\45\1\145\1\45\1\164\1\uffff\1\45\1\uffff\1\45\2"+
        "\uffff";
    static final String DFA20_maxS =
        "\1\uffff\1\162\1\163\1\155\2\uffff\1\141\1\162\1\141\1\uffff\1"+
        "\176\1\51\2\uffff\1\145\1\71\1\uffff\1\172\1\uffff\2\uffff\1\57"+
        "\1\uffff\1\151\1\uffff\1\151\1\176\1\160\2\uffff\1\163\1\165\1\154"+
        "\7\uffff\1\145\5\uffff\1\176\1\141\1\uffff\1\157\1\176\1\145\1\163"+
        "\1\uffff\1\163\1\162\1\uffff\1\176\1\145\1\176\1\164\1\uffff\1\176"+
        "\1\uffff\1\176\2\uffff";
    static final String DFA20_acceptS =
        "\4\uffff\1\5\1\6\3\uffff\1\12\2\uffff\1\15\1\16\2\uffff\1\22\1"+
        "\uffff\1\23\3\uffff\1\27\1\uffff\1\23\3\uffff\1\5\1\6\3\uffff\1"+
        "\12\1\13\1\14\1\15\1\16\1\17\1\20\1\uffff\1\21\1\22\1\24\1\25\1"+
        "\26\2\uffff\1\4\4\uffff\1\1\2\uffff\1\7\4\uffff\1\10\1\uffff\1\2"+
        "\1\uffff\1\11\1\3";
    static final String DFA20_specialS =
        "\1\0\22\uffff\1\1\1\2\56\uffff}>";
    static final String[] DFA20_transitionS = {
            "\11\26\2\20\2\26\1\20\22\26\1\20\1\26\1\23\4\26\1\24\1\13\4"+
            "\26\1\11\1\17\1\25\12\16\2\26\1\14\1\26\1\15\2\26\32\22\3\26"+
            "\1\21\1\12\1\26\1\2\4\22\1\10\1\22\1\6\1\3\12\22\1\7\1\1\5\22"+
            "\1\4\1\26\1\5\42\26\1\20\uff5f\26",
            "\1\27",
            "\1\31\6\uffff\1\32",
            "\1\33",
            "",
            "",
            "\1\36",
            "\1\37",
            "\1\40",
            "",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "\1\43",
            "",
            "",
            "\1\47\1\uffff\12\50\13\uffff\1\47\37\uffff\1\47",
            "\12\47",
            "",
            "\32\30\4\uffff\1\30\1\uffff\32\30",
            "",
            "\0\53",
            "\0\53",
            "\1\54\4\uffff\1\55",
            "",
            "\1\56",
            "",
            "\1\57",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "\1\61",
            "",
            "",
            "\1\62",
            "\1\63",
            "\1\64",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\47\1\uffff\12\50\13\uffff\1\47\37\uffff\1\47",
            "",
            "",
            "",
            "",
            "",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "\1\66",
            "",
            "\1\67",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "\1\71",
            "\1\72",
            "",
            "\1\73",
            "\1\74",
            "",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "\1\76",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "\1\100",
            "",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "",
            "\1\30\7\uffff\1\30\2\uffff\12\30\7\uffff\32\30\4\uffff\1\30"+
            "\1\uffff\32\30\3\uffff\1\30",
            "",
            ""
    };

    static final short[] DFA20_eot = DFA.unpackEncodedString(DFA20_eotS);
    static final short[] DFA20_eof = DFA.unpackEncodedString(DFA20_eofS);
    static final char[] DFA20_min = DFA.unpackEncodedStringToUnsignedChars(DFA20_minS);
    static final char[] DFA20_max = DFA.unpackEncodedStringToUnsignedChars(DFA20_maxS);
    static final short[] DFA20_accept = DFA.unpackEncodedString(DFA20_acceptS);
    static final short[] DFA20_special = DFA.unpackEncodedString(DFA20_specialS);
    static final short[][] DFA20_transition;

    static {
        int numStates = DFA20_transitionS.length;
        DFA20_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA20_transition[i] = DFA.unpackEncodedString(DFA20_transitionS[i]);
        }
    }

    class DFA20 extends DFA {

        public DFA20(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 20;
            this.eot = DFA20_eot;
            this.eof = DFA20_eof;
            this.min = DFA20_min;
            this.max = DFA20_max;
            this.accept = DFA20_accept;
            this.special = DFA20_special;
            this.transition = DFA20_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( T__13 | T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | T__20 | T__21 | T__22 | T__23 | T__24 | T__25 | T__26 | RULE_DIGITS | RULE_UNSIGNED_NUMBER | RULE_EOS | RULE_WS | RULE_ID | RULE_STRING | RULE_ML_COMMENT | RULE_SL_COMMENT | RULE_ANY_OTHER );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA20_0 = input.LA(1);

                        s = -1;
                        if ( (LA20_0=='u') ) {s = 1;}

                        else if ( (LA20_0=='a') ) {s = 2;}

                        else if ( (LA20_0=='i') ) {s = 3;}

                        else if ( (LA20_0=='{') ) {s = 4;}

                        else if ( (LA20_0=='}') ) {s = 5;}

                        else if ( (LA20_0=='h') ) {s = 6;}

                        else if ( (LA20_0=='t') ) {s = 7;}

                        else if ( (LA20_0=='f') ) {s = 8;}

                        else if ( (LA20_0=='-') ) {s = 9;}

                        else if ( (LA20_0=='_') ) {s = 10;}

                        else if ( (LA20_0=='(') ) {s = 11;}

                        else if ( (LA20_0=='<') ) {s = 12;}

                        else if ( (LA20_0=='>') ) {s = 13;}

                        else if ( ((LA20_0>='0' && LA20_0<='9')) ) {s = 14;}

                        else if ( (LA20_0=='.') ) {s = 15;}

                        else if ( ((LA20_0>='\t' && LA20_0<='\n')||LA20_0=='\r'||LA20_0==' '||LA20_0=='\u00A0') ) {s = 16;}

                        else if ( (LA20_0=='^') ) {s = 17;}

                        else if ( ((LA20_0>='A' && LA20_0<='Z')||(LA20_0>='b' && LA20_0<='e')||LA20_0=='g'||(LA20_0>='j' && LA20_0<='s')||(LA20_0>='v' && LA20_0<='z')) ) {s = 18;}

                        else if ( (LA20_0=='\"') ) {s = 19;}

                        else if ( (LA20_0=='\'') ) {s = 20;}

                        else if ( (LA20_0=='/') ) {s = 21;}

                        else if ( ((LA20_0>='\u0000' && LA20_0<='\b')||(LA20_0>='\u000B' && LA20_0<='\f')||(LA20_0>='\u000E' && LA20_0<='\u001F')||LA20_0=='!'||(LA20_0>='#' && LA20_0<='&')||(LA20_0>=')' && LA20_0<=',')||(LA20_0>=':' && LA20_0<=';')||LA20_0=='='||(LA20_0>='?' && LA20_0<='@')||(LA20_0>='[' && LA20_0<=']')||LA20_0=='`'||LA20_0=='|'||(LA20_0>='~' && LA20_0<='\u009F')||(LA20_0>='\u00A1' && LA20_0<='\uFFFF')) ) {s = 22;}

                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA20_19 = input.LA(1);

                        s = -1;
                        if ( ((LA20_19>='\u0000' && LA20_19<='\uFFFF')) ) {s = 43;}

                        else s = 22;

                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA20_20 = input.LA(1);

                        s = -1;
                        if ( ((LA20_20>='\u0000' && LA20_20<='\uFFFF')) ) {s = 43;}

                        else s = 22;

                        if ( s>=0 ) return s;
                        break;
            }
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 20, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

}