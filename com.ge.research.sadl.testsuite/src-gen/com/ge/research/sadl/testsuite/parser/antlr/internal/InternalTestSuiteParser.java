package com.ge.research.sadl.testsuite.parser.antlr.internal; 

import org.eclipse.xtext.*;
import org.eclipse.xtext.parser.*;
import org.eclipse.xtext.parser.impl.*;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.parser.antlr.AbstractInternalAntlrParser;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;
import org.eclipse.xtext.parser.antlr.XtextTokenStream.HiddenTokens;
import org.eclipse.xtext.parser.antlr.AntlrDatatypeRuleToken;
import com.ge.research.sadl.testsuite.services.TestSuiteGrammarAccess;



import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class InternalTestSuiteParser extends AbstractInternalAntlrParser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_STRING", "RULE_ID", "RULE_INT", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_WS", "RULE_ANY_OTHER", "'Test:'", "'.'"
    };
    public static final int RULE_ID=5;
    public static final int RULE_STRING=4;
    public static final int T__12=12;
    public static final int T__11=11;
    public static final int RULE_ANY_OTHER=10;
    public static final int RULE_INT=6;
    public static final int RULE_WS=9;
    public static final int RULE_SL_COMMENT=8;
    public static final int EOF=-1;
    public static final int RULE_ML_COMMENT=7;

    // delegates
    // delegators


        public InternalTestSuiteParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public InternalTestSuiteParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return InternalTestSuiteParser.tokenNames; }
    public String getGrammarFileName() { return "../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g"; }



     	private TestSuiteGrammarAccess grammarAccess;
     	
        public InternalTestSuiteParser(TokenStream input, TestSuiteGrammarAccess grammarAccess) {
            this(input);
            this.grammarAccess = grammarAccess;
            registerRules(grammarAccess.getGrammar());
        }
        
        @Override
        protected String getFirstRuleName() {
        	return "Model";	
       	}
       	
       	@Override
       	protected TestSuiteGrammarAccess getGrammarAccess() {
       		return grammarAccess;
       	}



    // $ANTLR start "entryRuleModel"
    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:67:1: entryRuleModel returns [EObject current=null] : iv_ruleModel= ruleModel EOF ;
    public final EObject entryRuleModel() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleModel = null;


        try {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:68:2: (iv_ruleModel= ruleModel EOF )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:69:2: iv_ruleModel= ruleModel EOF
            {
             newCompositeNode(grammarAccess.getModelRule()); 
            pushFollow(FOLLOW_ruleModel_in_entryRuleModel75);
            iv_ruleModel=ruleModel();

            state._fsp--;

             current =iv_ruleModel; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleModel85); 

            }

        }
         
            catch (RecognitionException re) { 
                recover(input,re); 
                appendSkippedTokens();
            } 
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleModel"


    // $ANTLR start "ruleModel"
    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:76:1: ruleModel returns [EObject current=null] : ( (lv_tests_0_0= ruleTest ) )* ;
    public final EObject ruleModel() throws RecognitionException {
        EObject current = null;

        EObject lv_tests_0_0 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:79:28: ( ( (lv_tests_0_0= ruleTest ) )* )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:80:1: ( (lv_tests_0_0= ruleTest ) )*
            {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:80:1: ( (lv_tests_0_0= ruleTest ) )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==11) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:81:1: (lv_tests_0_0= ruleTest )
            	    {
            	    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:81:1: (lv_tests_0_0= ruleTest )
            	    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:82:3: lv_tests_0_0= ruleTest
            	    {
            	     
            	    	        newCompositeNode(grammarAccess.getModelAccess().getTestsTestParserRuleCall_0()); 
            	    	    
            	    pushFollow(FOLLOW_ruleTest_in_ruleModel130);
            	    lv_tests_0_0=ruleTest();

            	    state._fsp--;


            	    	        if (current==null) {
            	    	            current = createModelElementForParent(grammarAccess.getModelRule());
            	    	        }
            	           		add(
            	           			current, 
            	           			"tests",
            	            		lv_tests_0_0, 
            	            		"Test");
            	    	        afterParserOrEnumRuleCall();
            	    	    

            	    }


            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);


            }

             leaveRule(); 
        }
         
            catch (RecognitionException re) { 
                recover(input,re); 
                appendSkippedTokens();
            } 
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleModel"


    // $ANTLR start "entryRuleTest"
    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:106:1: entryRuleTest returns [EObject current=null] : iv_ruleTest= ruleTest EOF ;
    public final EObject entryRuleTest() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleTest = null;


        try {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:107:2: (iv_ruleTest= ruleTest EOF )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:108:2: iv_ruleTest= ruleTest EOF
            {
             newCompositeNode(grammarAccess.getTestRule()); 
            pushFollow(FOLLOW_ruleTest_in_entryRuleTest166);
            iv_ruleTest=ruleTest();

            state._fsp--;

             current =iv_ruleTest; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleTest176); 

            }

        }
         
            catch (RecognitionException re) { 
                recover(input,re); 
                appendSkippedTokens();
            } 
        finally {
        }
        return current;
    }
    // $ANTLR end "entryRuleTest"


    // $ANTLR start "ruleTest"
    // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:115:1: ruleTest returns [EObject current=null] : (otherlv_0= 'Test:' ( (lv_name_1_0= RULE_STRING ) ) otherlv_2= '.' ) ;
    public final EObject ruleTest() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token lv_name_1_0=null;
        Token otherlv_2=null;

         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:118:28: ( (otherlv_0= 'Test:' ( (lv_name_1_0= RULE_STRING ) ) otherlv_2= '.' ) )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:119:1: (otherlv_0= 'Test:' ( (lv_name_1_0= RULE_STRING ) ) otherlv_2= '.' )
            {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:119:1: (otherlv_0= 'Test:' ( (lv_name_1_0= RULE_STRING ) ) otherlv_2= '.' )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:119:3: otherlv_0= 'Test:' ( (lv_name_1_0= RULE_STRING ) ) otherlv_2= '.'
            {
            otherlv_0=(Token)match(input,11,FOLLOW_11_in_ruleTest213); 

                	newLeafNode(otherlv_0, grammarAccess.getTestAccess().getTestKeyword_0());
                
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:123:1: ( (lv_name_1_0= RULE_STRING ) )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:124:1: (lv_name_1_0= RULE_STRING )
            {
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:124:1: (lv_name_1_0= RULE_STRING )
            // ../com.ge.research.sadl.testsuite/src-gen/com/ge/research/sadl/testsuite/parser/antlr/internal/InternalTestSuite.g:125:3: lv_name_1_0= RULE_STRING
            {
            lv_name_1_0=(Token)match(input,RULE_STRING,FOLLOW_RULE_STRING_in_ruleTest230); 

            			newLeafNode(lv_name_1_0, grammarAccess.getTestAccess().getNameSTRINGTerminalRuleCall_1_0()); 
            		

            	        if (current==null) {
            	            current = createModelElement(grammarAccess.getTestRule());
            	        }
                   		setWithLastConsumed(
                   			current, 
                   			"name",
                    		lv_name_1_0, 
                    		"STRING");
            	    

            }


            }

            otherlv_2=(Token)match(input,12,FOLLOW_12_in_ruleTest247); 

                	newLeafNode(otherlv_2, grammarAccess.getTestAccess().getFullStopKeyword_2());
                

            }


            }

             leaveRule(); 
        }
         
            catch (RecognitionException re) { 
                recover(input,re); 
                appendSkippedTokens();
            } 
        finally {
        }
        return current;
    }
    // $ANTLR end "ruleTest"

    // Delegated rules


 

    public static final BitSet FOLLOW_ruleModel_in_entryRuleModel75 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleModel85 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleTest_in_ruleModel130 = new BitSet(new long[]{0x0000000000000802L});
    public static final BitSet FOLLOW_ruleTest_in_entryRuleTest166 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleTest176 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_11_in_ruleTest213 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_RULE_STRING_in_ruleTest230 = new BitSet(new long[]{0x0000000000001000L});
    public static final BitSet FOLLOW_12_in_ruleTest247 = new BitSet(new long[]{0x0000000000000002L});

}