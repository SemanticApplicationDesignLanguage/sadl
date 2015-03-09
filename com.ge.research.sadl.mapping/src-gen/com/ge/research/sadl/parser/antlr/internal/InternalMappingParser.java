package com.ge.research.sadl.parser.antlr.internal; 

import org.eclipse.xtext.*;
import org.eclipse.xtext.parser.*;
import org.eclipse.xtext.parser.impl.*;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.parser.antlr.AbstractInternalAntlrParser;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;
import org.eclipse.xtext.parser.antlr.XtextTokenStream.HiddenTokens;
import org.eclipse.xtext.parser.antlr.AntlrDatatypeRuleToken;
import com.ge.research.sadl.services.MappingGrammarAccess;



import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class InternalMappingParser extends AbstractInternalAntlrParser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_STRING", "RULE_ID", "RULE_EOS", "RULE_UNSIGNED_NUMBER", "RULE_DIGITS", "RULE_WS", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_ANY_OTHER", "'uri'", "'alias'", "'import'", "'as'", "'{'", "'}'", "'has'", "'true'", "'false'", "'-'", "'_'", "'()'", "'<'", "'>'"
    };
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
    public static final int RULE_SL_COMMENT=11;
    public static final int EOF=-1;
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


        public InternalMappingParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public InternalMappingParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return InternalMappingParser.tokenNames; }
    public String getGrammarFileName() { return "../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g"; }



     	private MappingGrammarAccess grammarAccess;
     	
        public InternalMappingParser(TokenStream input, MappingGrammarAccess grammarAccess) {
            this(input);
            this.grammarAccess = grammarAccess;
            registerRules(grammarAccess.getGrammar());
        }
        
        @Override
        protected String getFirstRuleName() {
        	return "Model";	
       	}
       	
       	@Override
       	protected MappingGrammarAccess getGrammarAccess() {
       		return grammarAccess;
       	}



    // $ANTLR start "entryRuleModel"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:67:1: entryRuleModel returns [EObject current=null] : iv_ruleModel= ruleModel EOF ;
    public final EObject entryRuleModel() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleModel = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:68:2: (iv_ruleModel= ruleModel EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:69:2: iv_ruleModel= ruleModel EOF
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
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:76:1: ruleModel returns [EObject current=null] : ( ( (lv_uri_0_0= ruleNewModelNS ) ) ( (lv_imports_1_0= ruleImport ) )* ( ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) ) )* ) ;
    public final EObject ruleModel() throws RecognitionException {
        EObject current = null;

        EObject lv_uri_0_0 = null;

        EObject lv_imports_1_0 = null;

        EObject lv_triples_2_1 = null;

        EObject lv_triples_2_2 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:79:28: ( ( ( (lv_uri_0_0= ruleNewModelNS ) ) ( (lv_imports_1_0= ruleImport ) )* ( ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) ) )* ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:80:1: ( ( (lv_uri_0_0= ruleNewModelNS ) ) ( (lv_imports_1_0= ruleImport ) )* ( ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) ) )* )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:80:1: ( ( (lv_uri_0_0= ruleNewModelNS ) ) ( (lv_imports_1_0= ruleImport ) )* ( ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) ) )* )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:80:2: ( (lv_uri_0_0= ruleNewModelNS ) ) ( (lv_imports_1_0= ruleImport ) )* ( ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) ) )*
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:80:2: ( (lv_uri_0_0= ruleNewModelNS ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:81:1: (lv_uri_0_0= ruleNewModelNS )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:81:1: (lv_uri_0_0= ruleNewModelNS )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:82:3: lv_uri_0_0= ruleNewModelNS
            {
             
            	        newCompositeNode(grammarAccess.getModelAccess().getUriNewModelNSParserRuleCall_0_0()); 
            	    
            pushFollow(FOLLOW_ruleNewModelNS_in_ruleModel131);
            lv_uri_0_0=ruleNewModelNS();

            state._fsp--;


            	        if (current==null) {
            	            current = createModelElementForParent(grammarAccess.getModelRule());
            	        }
                   		set(
                   			current, 
                   			"uri",
                    		lv_uri_0_0, 
                    		"NewModelNS");
            	        afterParserOrEnumRuleCall();
            	    

            }


            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:98:2: ( (lv_imports_1_0= ruleImport ) )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==15) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:99:1: (lv_imports_1_0= ruleImport )
            	    {
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:99:1: (lv_imports_1_0= ruleImport )
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:100:3: lv_imports_1_0= ruleImport
            	    {
            	     
            	    	        newCompositeNode(grammarAccess.getModelAccess().getImportsImportParserRuleCall_1_0()); 
            	    	    
            	    pushFollow(FOLLOW_ruleImport_in_ruleModel152);
            	    lv_imports_1_0=ruleImport();

            	    state._fsp--;


            	    	        if (current==null) {
            	    	            current = createModelElementForParent(grammarAccess.getModelRule());
            	    	        }
            	           		add(
            	           			current, 
            	           			"imports",
            	            		lv_imports_1_0, 
            	            		"Import");
            	    	        afterParserOrEnumRuleCall();
            	    	    

            	    }


            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:116:3: ( ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) ) )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==RULE_ID||LA3_0==17||LA3_0==25) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:117:1: ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) )
            	    {
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:117:1: ( (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup ) )
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:118:1: (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup )
            	    {
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:118:1: (lv_triples_2_1= ruleTriple | lv_triples_2_2= ruleGroup )
            	    int alt2=2;
            	    int LA2_0 = input.LA(1);

            	    if ( (LA2_0==RULE_ID||LA2_0==25) ) {
            	        alt2=1;
            	    }
            	    else if ( (LA2_0==17) ) {
            	        alt2=2;
            	    }
            	    else {
            	        NoViableAltException nvae =
            	            new NoViableAltException("", 2, 0, input);

            	        throw nvae;
            	    }
            	    switch (alt2) {
            	        case 1 :
            	            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:119:3: lv_triples_2_1= ruleTriple
            	            {
            	             
            	            	        newCompositeNode(grammarAccess.getModelAccess().getTriplesTripleParserRuleCall_2_0_0()); 
            	            	    
            	            pushFollow(FOLLOW_ruleTriple_in_ruleModel176);
            	            lv_triples_2_1=ruleTriple();

            	            state._fsp--;


            	            	        if (current==null) {
            	            	            current = createModelElementForParent(grammarAccess.getModelRule());
            	            	        }
            	                   		add(
            	                   			current, 
            	                   			"triples",
            	                    		lv_triples_2_1, 
            	                    		"Triple");
            	            	        afterParserOrEnumRuleCall();
            	            	    

            	            }
            	            break;
            	        case 2 :
            	            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:134:8: lv_triples_2_2= ruleGroup
            	            {
            	             
            	            	        newCompositeNode(grammarAccess.getModelAccess().getTriplesGroupParserRuleCall_2_0_1()); 
            	            	    
            	            pushFollow(FOLLOW_ruleGroup_in_ruleModel195);
            	            lv_triples_2_2=ruleGroup();

            	            state._fsp--;


            	            	        if (current==null) {
            	            	            current = createModelElementForParent(grammarAccess.getModelRule());
            	            	        }
            	                   		add(
            	                   			current, 
            	                   			"triples",
            	                    		lv_triples_2_2, 
            	                    		"Group");
            	            	        afterParserOrEnumRuleCall();
            	            	    

            	            }
            	            break;

            	    }


            	    }


            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);


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
    // $ANTLR end "ruleModel"


    // $ANTLR start "entryRuleNewModelNS"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:160:1: entryRuleNewModelNS returns [EObject current=null] : iv_ruleNewModelNS= ruleNewModelNS EOF ;
    public final EObject entryRuleNewModelNS() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleNewModelNS = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:161:2: (iv_ruleNewModelNS= ruleNewModelNS EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:162:2: iv_ruleNewModelNS= ruleNewModelNS EOF
            {
             newCompositeNode(grammarAccess.getNewModelNSRule()); 
            pushFollow(FOLLOW_ruleNewModelNS_in_entryRuleNewModelNS235);
            iv_ruleNewModelNS=ruleNewModelNS();

            state._fsp--;

             current =iv_ruleNewModelNS; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleNewModelNS245); 

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
    // $ANTLR end "entryRuleNewModelNS"


    // $ANTLR start "ruleNewModelNS"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:169:1: ruleNewModelNS returns [EObject current=null] : (otherlv_0= 'uri' ( (lv_baseUri_1_0= RULE_STRING ) ) (otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS ) ;
    public final EObject ruleNewModelNS() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token lv_baseUri_1_0=null;
        Token otherlv_2=null;
        Token lv_prefix_3_0=null;
        Token this_EOS_4=null;

         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:172:28: ( (otherlv_0= 'uri' ( (lv_baseUri_1_0= RULE_STRING ) ) (otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:173:1: (otherlv_0= 'uri' ( (lv_baseUri_1_0= RULE_STRING ) ) (otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:173:1: (otherlv_0= 'uri' ( (lv_baseUri_1_0= RULE_STRING ) ) (otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:173:3: otherlv_0= 'uri' ( (lv_baseUri_1_0= RULE_STRING ) ) (otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS
            {
            otherlv_0=(Token)match(input,13,FOLLOW_13_in_ruleNewModelNS282); 

                	newLeafNode(otherlv_0, grammarAccess.getNewModelNSAccess().getUriKeyword_0());
                
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:177:1: ( (lv_baseUri_1_0= RULE_STRING ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:178:1: (lv_baseUri_1_0= RULE_STRING )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:178:1: (lv_baseUri_1_0= RULE_STRING )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:179:3: lv_baseUri_1_0= RULE_STRING
            {
            lv_baseUri_1_0=(Token)match(input,RULE_STRING,FOLLOW_RULE_STRING_in_ruleNewModelNS299); 

            			newLeafNode(lv_baseUri_1_0, grammarAccess.getNewModelNSAccess().getBaseUriSTRINGTerminalRuleCall_1_0()); 
            		

            	        if (current==null) {
            	            current = createModelElement(grammarAccess.getNewModelNSRule());
            	        }
                   		setWithLastConsumed(
                   			current, 
                   			"baseUri",
                    		lv_baseUri_1_0, 
                    		"STRING");
            	    

            }


            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:195:2: (otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) ) )?
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( (LA4_0==14) ) {
                alt4=1;
            }
            switch (alt4) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:195:4: otherlv_2= 'alias' ( (lv_prefix_3_0= RULE_ID ) )
                    {
                    otherlv_2=(Token)match(input,14,FOLLOW_14_in_ruleNewModelNS317); 

                        	newLeafNode(otherlv_2, grammarAccess.getNewModelNSAccess().getAliasKeyword_2_0());
                        
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:199:1: ( (lv_prefix_3_0= RULE_ID ) )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:200:1: (lv_prefix_3_0= RULE_ID )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:200:1: (lv_prefix_3_0= RULE_ID )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:201:3: lv_prefix_3_0= RULE_ID
                    {
                    lv_prefix_3_0=(Token)match(input,RULE_ID,FOLLOW_RULE_ID_in_ruleNewModelNS334); 

                    			newLeafNode(lv_prefix_3_0, grammarAccess.getNewModelNSAccess().getPrefixIDTerminalRuleCall_2_1_0()); 
                    		

                    	        if (current==null) {
                    	            current = createModelElement(grammarAccess.getNewModelNSRule());
                    	        }
                           		setWithLastConsumed(
                           			current, 
                           			"prefix",
                            		lv_prefix_3_0, 
                            		"ID");
                    	    

                    }


                    }


                    }
                    break;

            }

            this_EOS_4=(Token)match(input,RULE_EOS,FOLLOW_RULE_EOS_in_ruleNewModelNS352); 
             
                newLeafNode(this_EOS_4, grammarAccess.getNewModelNSAccess().getEOSTerminalRuleCall_3()); 
                

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
    // $ANTLR end "ruleNewModelNS"


    // $ANTLR start "entryRuleImport"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:229:1: entryRuleImport returns [EObject current=null] : iv_ruleImport= ruleImport EOF ;
    public final EObject entryRuleImport() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleImport = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:230:2: (iv_ruleImport= ruleImport EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:231:2: iv_ruleImport= ruleImport EOF
            {
             newCompositeNode(grammarAccess.getImportRule()); 
            pushFollow(FOLLOW_ruleImport_in_entryRuleImport387);
            iv_ruleImport=ruleImport();

            state._fsp--;

             current =iv_ruleImport; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleImport397); 

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
    // $ANTLR end "entryRuleImport"


    // $ANTLR start "ruleImport"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:238:1: ruleImport returns [EObject current=null] : (otherlv_0= 'import' ( (lv_importURI_1_0= RULE_STRING ) ) (otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS ) ;
    public final EObject ruleImport() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token lv_importURI_1_0=null;
        Token otherlv_2=null;
        Token lv_alias_3_0=null;
        Token this_EOS_4=null;

         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:241:28: ( (otherlv_0= 'import' ( (lv_importURI_1_0= RULE_STRING ) ) (otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:242:1: (otherlv_0= 'import' ( (lv_importURI_1_0= RULE_STRING ) ) (otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:242:1: (otherlv_0= 'import' ( (lv_importURI_1_0= RULE_STRING ) ) (otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:242:3: otherlv_0= 'import' ( (lv_importURI_1_0= RULE_STRING ) ) (otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) ) )? this_EOS_4= RULE_EOS
            {
            otherlv_0=(Token)match(input,15,FOLLOW_15_in_ruleImport434); 

                	newLeafNode(otherlv_0, grammarAccess.getImportAccess().getImportKeyword_0());
                
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:246:1: ( (lv_importURI_1_0= RULE_STRING ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:247:1: (lv_importURI_1_0= RULE_STRING )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:247:1: (lv_importURI_1_0= RULE_STRING )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:248:3: lv_importURI_1_0= RULE_STRING
            {
            lv_importURI_1_0=(Token)match(input,RULE_STRING,FOLLOW_RULE_STRING_in_ruleImport451); 

            			newLeafNode(lv_importURI_1_0, grammarAccess.getImportAccess().getImportURISTRINGTerminalRuleCall_1_0()); 
            		

            	        if (current==null) {
            	            current = createModelElement(grammarAccess.getImportRule());
            	        }
                   		setWithLastConsumed(
                   			current, 
                   			"importURI",
                    		lv_importURI_1_0, 
                    		"STRING");
            	    

            }


            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:264:2: (otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) ) )?
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==16) ) {
                alt5=1;
            }
            switch (alt5) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:264:4: otherlv_2= 'as' ( (lv_alias_3_0= RULE_ID ) )
                    {
                    otherlv_2=(Token)match(input,16,FOLLOW_16_in_ruleImport469); 

                        	newLeafNode(otherlv_2, grammarAccess.getImportAccess().getAsKeyword_2_0());
                        
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:268:1: ( (lv_alias_3_0= RULE_ID ) )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:269:1: (lv_alias_3_0= RULE_ID )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:269:1: (lv_alias_3_0= RULE_ID )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:270:3: lv_alias_3_0= RULE_ID
                    {
                    lv_alias_3_0=(Token)match(input,RULE_ID,FOLLOW_RULE_ID_in_ruleImport486); 

                    			newLeafNode(lv_alias_3_0, grammarAccess.getImportAccess().getAliasIDTerminalRuleCall_2_1_0()); 
                    		

                    	        if (current==null) {
                    	            current = createModelElement(grammarAccess.getImportRule());
                    	        }
                           		setWithLastConsumed(
                           			current, 
                           			"alias",
                            		lv_alias_3_0, 
                            		"ID");
                    	    

                    }


                    }


                    }
                    break;

            }

            this_EOS_4=(Token)match(input,RULE_EOS,FOLLOW_RULE_EOS_in_ruleImport504); 
             
                newLeafNode(this_EOS_4, grammarAccess.getImportAccess().getEOSTerminalRuleCall_3()); 
                

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
    // $ANTLR end "ruleImport"


    // $ANTLR start "entryRuleGroup"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:298:1: entryRuleGroup returns [EObject current=null] : iv_ruleGroup= ruleGroup EOF ;
    public final EObject entryRuleGroup() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleGroup = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:299:2: (iv_ruleGroup= ruleGroup EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:300:2: iv_ruleGroup= ruleGroup EOF
            {
             newCompositeNode(grammarAccess.getGroupRule()); 
            pushFollow(FOLLOW_ruleGroup_in_entryRuleGroup539);
            iv_ruleGroup=ruleGroup();

            state._fsp--;

             current =iv_ruleGroup; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleGroup549); 

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
    // $ANTLR end "entryRuleGroup"


    // $ANTLR start "ruleGroup"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:307:1: ruleGroup returns [EObject current=null] : (otherlv_0= '{' ( ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) ) )+ otherlv_2= '}' this_EOS_3= RULE_EOS ) ;
    public final EObject ruleGroup() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token otherlv_2=null;
        Token this_EOS_3=null;
        EObject lv_groupLines_1_1 = null;

        EObject lv_groupLines_1_2 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:310:28: ( (otherlv_0= '{' ( ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) ) )+ otherlv_2= '}' this_EOS_3= RULE_EOS ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:311:1: (otherlv_0= '{' ( ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) ) )+ otherlv_2= '}' this_EOS_3= RULE_EOS )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:311:1: (otherlv_0= '{' ( ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) ) )+ otherlv_2= '}' this_EOS_3= RULE_EOS )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:311:3: otherlv_0= '{' ( ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) ) )+ otherlv_2= '}' this_EOS_3= RULE_EOS
            {
            otherlv_0=(Token)match(input,17,FOLLOW_17_in_ruleGroup586); 

                	newLeafNode(otherlv_0, grammarAccess.getGroupAccess().getLeftCurlyBracketKeyword_0());
                
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:315:1: ( ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) ) )+
            int cnt7=0;
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( (LA7_0==RULE_ID||LA7_0==17||LA7_0==25) ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:316:1: ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) )
            	    {
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:316:1: ( (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup ) )
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:317:1: (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup )
            	    {
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:317:1: (lv_groupLines_1_1= ruleTriple | lv_groupLines_1_2= ruleGroup )
            	    int alt6=2;
            	    int LA6_0 = input.LA(1);

            	    if ( (LA6_0==RULE_ID||LA6_0==25) ) {
            	        alt6=1;
            	    }
            	    else if ( (LA6_0==17) ) {
            	        alt6=2;
            	    }
            	    else {
            	        NoViableAltException nvae =
            	            new NoViableAltException("", 6, 0, input);

            	        throw nvae;
            	    }
            	    switch (alt6) {
            	        case 1 :
            	            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:318:3: lv_groupLines_1_1= ruleTriple
            	            {
            	             
            	            	        newCompositeNode(grammarAccess.getGroupAccess().getGroupLinesTripleParserRuleCall_1_0_0()); 
            	            	    
            	            pushFollow(FOLLOW_ruleTriple_in_ruleGroup609);
            	            lv_groupLines_1_1=ruleTriple();

            	            state._fsp--;


            	            	        if (current==null) {
            	            	            current = createModelElementForParent(grammarAccess.getGroupRule());
            	            	        }
            	                   		add(
            	                   			current, 
            	                   			"groupLines",
            	                    		lv_groupLines_1_1, 
            	                    		"Triple");
            	            	        afterParserOrEnumRuleCall();
            	            	    

            	            }
            	            break;
            	        case 2 :
            	            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:333:8: lv_groupLines_1_2= ruleGroup
            	            {
            	             
            	            	        newCompositeNode(grammarAccess.getGroupAccess().getGroupLinesGroupParserRuleCall_1_0_1()); 
            	            	    
            	            pushFollow(FOLLOW_ruleGroup_in_ruleGroup628);
            	            lv_groupLines_1_2=ruleGroup();

            	            state._fsp--;


            	            	        if (current==null) {
            	            	            current = createModelElementForParent(grammarAccess.getGroupRule());
            	            	        }
            	                   		add(
            	                   			current, 
            	                   			"groupLines",
            	                    		lv_groupLines_1_2, 
            	                    		"Group");
            	            	        afterParserOrEnumRuleCall();
            	            	    

            	            }
            	            break;

            	    }


            	    }


            	    }
            	    break;

            	default :
            	    if ( cnt7 >= 1 ) break loop7;
                        EarlyExitException eee =
                            new EarlyExitException(7, input);
                        throw eee;
                }
                cnt7++;
            } while (true);

            otherlv_2=(Token)match(input,18,FOLLOW_18_in_ruleGroup644); 

                	newLeafNode(otherlv_2, grammarAccess.getGroupAccess().getRightCurlyBracketKeyword_2());
                
            this_EOS_3=(Token)match(input,RULE_EOS,FOLLOW_RULE_EOS_in_ruleGroup655); 
             
                newLeafNode(this_EOS_3, grammarAccess.getGroupAccess().getEOSTerminalRuleCall_3()); 
                

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
    // $ANTLR end "ruleGroup"


    // $ANTLR start "entryRuleTriple"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:367:1: entryRuleTriple returns [EObject current=null] : iv_ruleTriple= ruleTriple EOF ;
    public final EObject entryRuleTriple() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleTriple = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:368:2: (iv_ruleTriple= ruleTriple EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:369:2: iv_ruleTriple= ruleTriple EOF
            {
             newCompositeNode(grammarAccess.getTripleRule()); 
            pushFollow(FOLLOW_ruleTriple_in_entryRuleTriple690);
            iv_ruleTriple=ruleTriple();

            state._fsp--;

             current =iv_ruleTriple; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleTriple700); 

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
    // $ANTLR end "entryRuleTriple"


    // $ANTLR start "ruleTriple"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:376:1: ruleTriple returns [EObject current=null] : ( ( ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) ) ) (otherlv_1= 'has' )? ( (otherlv_2= RULE_ID ) ) ( ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) ) ) ) ;
    public final EObject ruleTriple() throws RecognitionException {
        EObject current = null;

        Token otherlv_0=null;
        Token otherlv_1=null;
        Token otherlv_2=null;
        Token otherlv_3=null;
        EObject lv_subj_0_1 = null;

        EObject lv_objval_3_1 = null;

        EObject lv_objval_3_5 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:379:28: ( ( ( ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) ) ) (otherlv_1= 'has' )? ( (otherlv_2= RULE_ID ) ) ( ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) ) ) ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:380:1: ( ( ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) ) ) (otherlv_1= 'has' )? ( (otherlv_2= RULE_ID ) ) ( ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) ) ) )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:380:1: ( ( ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) ) ) (otherlv_1= 'has' )? ( (otherlv_2= RULE_ID ) ) ( ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) ) ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:380:2: ( ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) ) ) (otherlv_1= 'has' )? ( (otherlv_2= RULE_ID ) ) ( ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) ) )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:380:2: ( ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:381:1: ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:381:1: ( (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:382:1: (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:382:1: (lv_subj_0_1= ruleRef | otherlv_0= RULE_ID )
            int alt8=2;
            int LA8_0 = input.LA(1);

            if ( (LA8_0==25) ) {
                alt8=1;
            }
            else if ( (LA8_0==RULE_ID) ) {
                alt8=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 8, 0, input);

                throw nvae;
            }
            switch (alt8) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:383:3: lv_subj_0_1= ruleRef
                    {
                     
                    	        newCompositeNode(grammarAccess.getTripleAccess().getSubjRefParserRuleCall_0_0_0()); 
                    	    
                    pushFollow(FOLLOW_ruleRef_in_ruleTriple748);
                    lv_subj_0_1=ruleRef();

                    state._fsp--;


                    	        if (current==null) {
                    	            current = createModelElementForParent(grammarAccess.getTripleRule());
                    	        }
                           		set(
                           			current, 
                           			"subj",
                            		lv_subj_0_1, 
                            		"Ref");
                    	        afterParserOrEnumRuleCall();
                    	    

                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:398:8: otherlv_0= RULE_ID
                    {

                    			if (current==null) {
                    	            current = createModelElement(grammarAccess.getTripleRule());
                    	        }
                            
                    otherlv_0=(Token)match(input,RULE_ID,FOLLOW_RULE_ID_in_ruleTriple766); 

                    		newLeafNode(otherlv_0, grammarAccess.getTripleAccess().getSubjResourceNameCrossReference_0_0_1()); 
                    	

                    }
                    break;

            }


            }


            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:411:2: (otherlv_1= 'has' )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0==19) ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:411:4: otherlv_1= 'has'
                    {
                    otherlv_1=(Token)match(input,19,FOLLOW_19_in_ruleTriple782); 

                        	newLeafNode(otherlv_1, grammarAccess.getTripleAccess().getHasKeyword_1());
                        

                    }
                    break;

            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:415:3: ( (otherlv_2= RULE_ID ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:416:1: (otherlv_2= RULE_ID )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:416:1: (otherlv_2= RULE_ID )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:417:3: otherlv_2= RULE_ID
            {

            			if (current==null) {
            	            current = createModelElement(grammarAccess.getTripleRule());
            	        }
                    
            otherlv_2=(Token)match(input,RULE_ID,FOLLOW_RULE_ID_in_ruleTriple804); 

            		newLeafNode(otherlv_2, grammarAccess.getTripleAccess().getPredResourceNameCrossReference_2_0()); 
            	

            }


            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:428:2: ( ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:429:1: ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:429:1: ( (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:430:1: (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:430:1: (lv_objval_3_1= ruleRef | otherlv_3= RULE_ID | lv_objval_3_5= ruleLiteralValue )
            int alt10=3;
            switch ( input.LA(1) ) {
            case 25:
                {
                alt10=1;
                }
                break;
            case RULE_ID:
                {
                alt10=2;
                }
                break;
            case RULE_STRING:
            case RULE_UNSIGNED_NUMBER:
            case 20:
            case 21:
            case 22:
                {
                alt10=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 10, 0, input);

                throw nvae;
            }

            switch (alt10) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:431:3: lv_objval_3_1= ruleRef
                    {
                     
                    	        newCompositeNode(grammarAccess.getTripleAccess().getObjvalRefParserRuleCall_3_0_0()); 
                    	    
                    pushFollow(FOLLOW_ruleRef_in_ruleTriple827);
                    lv_objval_3_1=ruleRef();

                    state._fsp--;


                    	        if (current==null) {
                    	            current = createModelElementForParent(grammarAccess.getTripleRule());
                    	        }
                           		set(
                           			current, 
                           			"objval",
                            		lv_objval_3_1, 
                            		"Ref");
                    	        afterParserOrEnumRuleCall();
                    	    

                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:446:8: otherlv_3= RULE_ID
                    {

                    			if (current==null) {
                    	            current = createModelElement(grammarAccess.getTripleRule());
                    	        }
                            
                    otherlv_3=(Token)match(input,RULE_ID,FOLLOW_RULE_ID_in_ruleTriple845); 

                    		newLeafNode(otherlv_3, grammarAccess.getTripleAccess().getObjvalResourceNameCrossReference_3_0_1()); 
                    	

                    }
                    break;
                case 3 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:456:8: lv_objval_3_5= ruleLiteralValue
                    {
                     
                    	        newCompositeNode(grammarAccess.getTripleAccess().getObjvalLiteralValueParserRuleCall_3_0_2()); 
                    	    
                    pushFollow(FOLLOW_ruleLiteralValue_in_ruleTriple864);
                    lv_objval_3_5=ruleLiteralValue();

                    state._fsp--;


                    	        if (current==null) {
                    	            current = createModelElementForParent(grammarAccess.getTripleRule());
                    	        }
                           		set(
                           			current, 
                           			"objval",
                            		lv_objval_3_5, 
                            		"LiteralValue");
                    	        afterParserOrEnumRuleCall();
                    	    

                    }
                    break;

            }


            }


            }


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
    // $ANTLR end "ruleTriple"


    // $ANTLR start "entryRuleLiteralValue"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:482:1: entryRuleLiteralValue returns [EObject current=null] : iv_ruleLiteralValue= ruleLiteralValue EOF ;
    public final EObject entryRuleLiteralValue() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleLiteralValue = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:483:2: (iv_ruleLiteralValue= ruleLiteralValue EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:484:2: iv_ruleLiteralValue= ruleLiteralValue EOF
            {
             newCompositeNode(grammarAccess.getLiteralValueRule()); 
            pushFollow(FOLLOW_ruleLiteralValue_in_entryRuleLiteralValue903);
            iv_ruleLiteralValue=ruleLiteralValue();

            state._fsp--;

             current =iv_ruleLiteralValue; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleLiteralValue913); 

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
    // $ANTLR end "entryRuleLiteralValue"


    // $ANTLR start "ruleLiteralValue"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:491:1: ruleLiteralValue returns [EObject current=null] : ( ( (lv_literalNumber_0_0= ruleNUMBER ) ) | ( (lv_literalString_1_0= RULE_STRING ) ) | ( ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) ) ) ) ;
    public final EObject ruleLiteralValue() throws RecognitionException {
        EObject current = null;

        Token lv_literalString_1_0=null;
        Token lv_literalBoolean_2_1=null;
        Token lv_literalBoolean_2_2=null;
        AntlrDatatypeRuleToken lv_literalNumber_0_0 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:494:28: ( ( ( (lv_literalNumber_0_0= ruleNUMBER ) ) | ( (lv_literalString_1_0= RULE_STRING ) ) | ( ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) ) ) ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:495:1: ( ( (lv_literalNumber_0_0= ruleNUMBER ) ) | ( (lv_literalString_1_0= RULE_STRING ) ) | ( ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) ) ) )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:495:1: ( ( (lv_literalNumber_0_0= ruleNUMBER ) ) | ( (lv_literalString_1_0= RULE_STRING ) ) | ( ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) ) ) )
            int alt12=3;
            switch ( input.LA(1) ) {
            case RULE_UNSIGNED_NUMBER:
            case 22:
                {
                alt12=1;
                }
                break;
            case RULE_STRING:
                {
                alt12=2;
                }
                break;
            case 20:
            case 21:
                {
                alt12=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 12, 0, input);

                throw nvae;
            }

            switch (alt12) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:495:2: ( (lv_literalNumber_0_0= ruleNUMBER ) )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:495:2: ( (lv_literalNumber_0_0= ruleNUMBER ) )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:496:1: (lv_literalNumber_0_0= ruleNUMBER )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:496:1: (lv_literalNumber_0_0= ruleNUMBER )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:497:3: lv_literalNumber_0_0= ruleNUMBER
                    {
                     
                    	        newCompositeNode(grammarAccess.getLiteralValueAccess().getLiteralNumberNUMBERParserRuleCall_0_0()); 
                    	    
                    pushFollow(FOLLOW_ruleNUMBER_in_ruleLiteralValue959);
                    lv_literalNumber_0_0=ruleNUMBER();

                    state._fsp--;


                    	        if (current==null) {
                    	            current = createModelElementForParent(grammarAccess.getLiteralValueRule());
                    	        }
                           		set(
                           			current, 
                           			"literalNumber",
                            		lv_literalNumber_0_0, 
                            		"NUMBER");
                    	        afterParserOrEnumRuleCall();
                    	    

                    }


                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:514:6: ( (lv_literalString_1_0= RULE_STRING ) )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:514:6: ( (lv_literalString_1_0= RULE_STRING ) )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:515:1: (lv_literalString_1_0= RULE_STRING )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:515:1: (lv_literalString_1_0= RULE_STRING )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:516:3: lv_literalString_1_0= RULE_STRING
                    {
                    lv_literalString_1_0=(Token)match(input,RULE_STRING,FOLLOW_RULE_STRING_in_ruleLiteralValue982); 

                    			newLeafNode(lv_literalString_1_0, grammarAccess.getLiteralValueAccess().getLiteralStringSTRINGTerminalRuleCall_1_0()); 
                    		

                    	        if (current==null) {
                    	            current = createModelElement(grammarAccess.getLiteralValueRule());
                    	        }
                           		setWithLastConsumed(
                           			current, 
                           			"literalString",
                            		lv_literalString_1_0, 
                            		"STRING");
                    	    

                    }


                    }


                    }
                    break;
                case 3 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:533:6: ( ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) ) )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:533:6: ( ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) ) )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:534:1: ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:534:1: ( (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' ) )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:535:1: (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:535:1: (lv_literalBoolean_2_1= 'true' | lv_literalBoolean_2_2= 'false' )
                    int alt11=2;
                    int LA11_0 = input.LA(1);

                    if ( (LA11_0==20) ) {
                        alt11=1;
                    }
                    else if ( (LA11_0==21) ) {
                        alt11=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 11, 0, input);

                        throw nvae;
                    }
                    switch (alt11) {
                        case 1 :
                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:536:3: lv_literalBoolean_2_1= 'true'
                            {
                            lv_literalBoolean_2_1=(Token)match(input,20,FOLLOW_20_in_ruleLiteralValue1013); 

                                    newLeafNode(lv_literalBoolean_2_1, grammarAccess.getLiteralValueAccess().getLiteralBooleanTrueKeyword_2_0_0());
                                

                            	        if (current==null) {
                            	            current = createModelElement(grammarAccess.getLiteralValueRule());
                            	        }
                                   		setWithLastConsumed(current, "literalBoolean", lv_literalBoolean_2_1, null);
                            	    

                            }
                            break;
                        case 2 :
                            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:548:8: lv_literalBoolean_2_2= 'false'
                            {
                            lv_literalBoolean_2_2=(Token)match(input,21,FOLLOW_21_in_ruleLiteralValue1042); 

                                    newLeafNode(lv_literalBoolean_2_2, grammarAccess.getLiteralValueAccess().getLiteralBooleanFalseKeyword_2_0_1());
                                

                            	        if (current==null) {
                            	            current = createModelElement(grammarAccess.getLiteralValueRule());
                            	        }
                                   		setWithLastConsumed(current, "literalBoolean", lv_literalBoolean_2_2, null);
                            	    

                            }
                            break;

                    }


                    }


                    }


                    }
                    break;

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
    // $ANTLR end "ruleLiteralValue"


    // $ANTLR start "entryRuleNUMBER"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:571:1: entryRuleNUMBER returns [String current=null] : iv_ruleNUMBER= ruleNUMBER EOF ;
    public final String entryRuleNUMBER() throws RecognitionException {
        String current = null;

        AntlrDatatypeRuleToken iv_ruleNUMBER = null;


         
        		HiddenTokens myHiddenTokenState = ((XtextTokenStream)input).setHiddenTokens();
        	
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:575:2: (iv_ruleNUMBER= ruleNUMBER EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:576:2: iv_ruleNUMBER= ruleNUMBER EOF
            {
             newCompositeNode(grammarAccess.getNUMBERRule()); 
            pushFollow(FOLLOW_ruleNUMBER_in_entryRuleNUMBER1101);
            iv_ruleNUMBER=ruleNUMBER();

            state._fsp--;

             current =iv_ruleNUMBER.getText(); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleNUMBER1112); 

            }

        }
         
            catch (RecognitionException re) { 
                recover(input,re); 
                appendSkippedTokens();
            } 
        finally {

            	myHiddenTokenState.restore();

        }
        return current;
    }
    // $ANTLR end "entryRuleNUMBER"


    // $ANTLR start "ruleNUMBER"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:586:1: ruleNUMBER returns [AntlrDatatypeRuleToken current=new AntlrDatatypeRuleToken()] : ( (kw= '-' )? this_UNSIGNED_NUMBER_1= RULE_UNSIGNED_NUMBER ) ;
    public final AntlrDatatypeRuleToken ruleNUMBER() throws RecognitionException {
        AntlrDatatypeRuleToken current = new AntlrDatatypeRuleToken();

        Token kw=null;
        Token this_UNSIGNED_NUMBER_1=null;

         enterRule(); 
        		HiddenTokens myHiddenTokenState = ((XtextTokenStream)input).setHiddenTokens();
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:590:28: ( ( (kw= '-' )? this_UNSIGNED_NUMBER_1= RULE_UNSIGNED_NUMBER ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:591:1: ( (kw= '-' )? this_UNSIGNED_NUMBER_1= RULE_UNSIGNED_NUMBER )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:591:1: ( (kw= '-' )? this_UNSIGNED_NUMBER_1= RULE_UNSIGNED_NUMBER )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:591:2: (kw= '-' )? this_UNSIGNED_NUMBER_1= RULE_UNSIGNED_NUMBER
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:591:2: (kw= '-' )?
            int alt13=2;
            int LA13_0 = input.LA(1);

            if ( (LA13_0==22) ) {
                alt13=1;
            }
            switch (alt13) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:592:2: kw= '-'
                    {
                    kw=(Token)match(input,22,FOLLOW_22_in_ruleNUMBER1155); 

                            current.merge(kw);
                            newLeafNode(kw, grammarAccess.getNUMBERAccess().getHyphenMinusKeyword_0()); 
                        

                    }
                    break;

            }

            this_UNSIGNED_NUMBER_1=(Token)match(input,RULE_UNSIGNED_NUMBER,FOLLOW_RULE_UNSIGNED_NUMBER_in_ruleNUMBER1172); 

            		current.merge(this_UNSIGNED_NUMBER_1);
                
             
                newLeafNode(this_UNSIGNED_NUMBER_1, grammarAccess.getNUMBERAccess().getUNSIGNED_NUMBERTerminalRuleCall_1()); 
                

            }


            }

             leaveRule(); 
        }
         
            catch (RecognitionException re) { 
                recover(input,re); 
                appendSkippedTokens();
            } 
        finally {

            	myHiddenTokenState.restore();

        }
        return current;
    }
    // $ANTLR end "ruleNUMBER"


    // $ANTLR start "entryRuleRef"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:615:1: entryRuleRef returns [EObject current=null] : iv_ruleRef= ruleRef EOF ;
    public final EObject entryRuleRef() throws RecognitionException {
        EObject current = null;

        EObject iv_ruleRef = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:616:2: (iv_ruleRef= ruleRef EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:617:2: iv_ruleRef= ruleRef EOF
            {
             newCompositeNode(grammarAccess.getRefRule()); 
            pushFollow(FOLLOW_ruleRef_in_entryRuleRef1221);
            iv_ruleRef=ruleRef();

            state._fsp--;

             current =iv_ruleRef; 
            match(input,EOF,FOLLOW_EOF_in_entryRuleRef1231); 

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
    // $ANTLR end "entryRuleRef"


    // $ANTLR start "ruleRef"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:624:1: ruleRef returns [EObject current=null] : ( ( (lv_ref_0_0= ruleColumnName ) ) ( (lv_addlcols_1_0= '_' ) )? ( ruleColumnName )* ( (lv_row_3_0= '()' ) )? ) ;
    public final EObject ruleRef() throws RecognitionException {
        EObject current = null;

        Token lv_addlcols_1_0=null;
        Token lv_row_3_0=null;
        AntlrDatatypeRuleToken lv_ref_0_0 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:627:28: ( ( ( (lv_ref_0_0= ruleColumnName ) ) ( (lv_addlcols_1_0= '_' ) )? ( ruleColumnName )* ( (lv_row_3_0= '()' ) )? ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:628:1: ( ( (lv_ref_0_0= ruleColumnName ) ) ( (lv_addlcols_1_0= '_' ) )? ( ruleColumnName )* ( (lv_row_3_0= '()' ) )? )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:628:1: ( ( (lv_ref_0_0= ruleColumnName ) ) ( (lv_addlcols_1_0= '_' ) )? ( ruleColumnName )* ( (lv_row_3_0= '()' ) )? )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:628:2: ( (lv_ref_0_0= ruleColumnName ) ) ( (lv_addlcols_1_0= '_' ) )? ( ruleColumnName )* ( (lv_row_3_0= '()' ) )?
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:628:2: ( (lv_ref_0_0= ruleColumnName ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:629:1: (lv_ref_0_0= ruleColumnName )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:629:1: (lv_ref_0_0= ruleColumnName )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:630:3: lv_ref_0_0= ruleColumnName
            {
             
            	        newCompositeNode(grammarAccess.getRefAccess().getRefColumnNameParserRuleCall_0_0()); 
            	    
            pushFollow(FOLLOW_ruleColumnName_in_ruleRef1277);
            lv_ref_0_0=ruleColumnName();

            state._fsp--;


            	        if (current==null) {
            	            current = createModelElementForParent(grammarAccess.getRefRule());
            	        }
                   		set(
                   			current, 
                   			"ref",
                    		lv_ref_0_0, 
                    		"ColumnName");
            	        afterParserOrEnumRuleCall();
            	    

            }


            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:646:2: ( (lv_addlcols_1_0= '_' ) )?
            int alt14=2;
            int LA14_0 = input.LA(1);

            if ( (LA14_0==23) ) {
                alt14=1;
            }
            switch (alt14) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:647:1: (lv_addlcols_1_0= '_' )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:647:1: (lv_addlcols_1_0= '_' )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:648:3: lv_addlcols_1_0= '_'
                    {
                    lv_addlcols_1_0=(Token)match(input,23,FOLLOW_23_in_ruleRef1295); 

                            newLeafNode(lv_addlcols_1_0, grammarAccess.getRefAccess().getAddlcols_Keyword_1_0());
                        

                    	        if (current==null) {
                    	            current = createModelElement(grammarAccess.getRefRule());
                    	        }
                           		addWithLastConsumed(current, "addlcols", lv_addlcols_1_0, "_");
                    	    

                    }


                    }
                    break;

            }

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:661:3: ( ruleColumnName )*
            loop15:
            do {
                int alt15=2;
                int LA15_0 = input.LA(1);

                if ( (LA15_0==25) ) {
                    int LA15_2 = input.LA(2);

                    if ( (LA15_2==RULE_ID) ) {
                        int LA15_3 = input.LA(3);

                        if ( (LA15_3==26) ) {
                            alt15=1;
                        }


                    }
                    else if ( (LA15_2==RULE_DIGITS) ) {
                        int LA15_4 = input.LA(3);

                        if ( (LA15_4==26) ) {
                            alt15=1;
                        }


                    }


                }


                switch (alt15) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:662:5: ruleColumnName
            	    {
            	     
            	            newCompositeNode(grammarAccess.getRefAccess().getColumnNameParserRuleCall_2()); 
            	        
            	    pushFollow(FOLLOW_ruleColumnName_in_ruleRef1326);
            	    ruleColumnName();

            	    state._fsp--;

            	     
            	            afterParserOrEnumRuleCall();
            	        

            	    }
            	    break;

            	default :
            	    break loop15;
                }
            } while (true);

            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:669:3: ( (lv_row_3_0= '()' ) )?
            int alt16=2;
            int LA16_0 = input.LA(1);

            if ( (LA16_0==24) ) {
                alt16=1;
            }
            switch (alt16) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:670:1: (lv_row_3_0= '()' )
                    {
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:670:1: (lv_row_3_0= '()' )
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:671:3: lv_row_3_0= '()'
                    {
                    lv_row_3_0=(Token)match(input,24,FOLLOW_24_in_ruleRef1345); 

                            newLeafNode(lv_row_3_0, grammarAccess.getRefAccess().getRowLeftParenthesisRightParenthesisKeyword_3_0());
                        

                    	        if (current==null) {
                    	            current = createModelElement(grammarAccess.getRefRule());
                    	        }
                           		setWithLastConsumed(current, "row", lv_row_3_0, "()");
                    	    

                    }


                    }
                    break;

            }


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
    // $ANTLR end "ruleRef"


    // $ANTLR start "entryRuleColumnName"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:692:1: entryRuleColumnName returns [String current=null] : iv_ruleColumnName= ruleColumnName EOF ;
    public final String entryRuleColumnName() throws RecognitionException {
        String current = null;

        AntlrDatatypeRuleToken iv_ruleColumnName = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:693:2: (iv_ruleColumnName= ruleColumnName EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:694:2: iv_ruleColumnName= ruleColumnName EOF
            {
             newCompositeNode(grammarAccess.getColumnNameRule()); 
            pushFollow(FOLLOW_ruleColumnName_in_entryRuleColumnName1396);
            iv_ruleColumnName=ruleColumnName();

            state._fsp--;

             current =iv_ruleColumnName.getText(); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleColumnName1407); 

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
    // $ANTLR end "entryRuleColumnName"


    // $ANTLR start "ruleColumnName"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:701:1: ruleColumnName returns [AntlrDatatypeRuleToken current=new AntlrDatatypeRuleToken()] : (kw= '<' this_ColumnID_1= ruleColumnID kw= '>' ) ;
    public final AntlrDatatypeRuleToken ruleColumnName() throws RecognitionException {
        AntlrDatatypeRuleToken current = new AntlrDatatypeRuleToken();

        Token kw=null;
        AntlrDatatypeRuleToken this_ColumnID_1 = null;


         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:704:28: ( (kw= '<' this_ColumnID_1= ruleColumnID kw= '>' ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:705:1: (kw= '<' this_ColumnID_1= ruleColumnID kw= '>' )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:705:1: (kw= '<' this_ColumnID_1= ruleColumnID kw= '>' )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:706:2: kw= '<' this_ColumnID_1= ruleColumnID kw= '>'
            {
            kw=(Token)match(input,25,FOLLOW_25_in_ruleColumnName1445); 

                    current.merge(kw);
                    newLeafNode(kw, grammarAccess.getColumnNameAccess().getLessThanSignKeyword_0()); 
                
             
                    newCompositeNode(grammarAccess.getColumnNameAccess().getColumnIDParserRuleCall_1()); 
                
            pushFollow(FOLLOW_ruleColumnID_in_ruleColumnName1467);
            this_ColumnID_1=ruleColumnID();

            state._fsp--;


            		current.merge(this_ColumnID_1);
                
             
                    afterParserOrEnumRuleCall();
                
            kw=(Token)match(input,26,FOLLOW_26_in_ruleColumnName1485); 

                    current.merge(kw);
                    newLeafNode(kw, grammarAccess.getColumnNameAccess().getGreaterThanSignKeyword_2()); 
                

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
    // $ANTLR end "ruleColumnName"


    // $ANTLR start "entryRuleColumnID"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:736:1: entryRuleColumnID returns [String current=null] : iv_ruleColumnID= ruleColumnID EOF ;
    public final String entryRuleColumnID() throws RecognitionException {
        String current = null;

        AntlrDatatypeRuleToken iv_ruleColumnID = null;


        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:737:2: (iv_ruleColumnID= ruleColumnID EOF )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:738:2: iv_ruleColumnID= ruleColumnID EOF
            {
             newCompositeNode(grammarAccess.getColumnIDRule()); 
            pushFollow(FOLLOW_ruleColumnID_in_entryRuleColumnID1526);
            iv_ruleColumnID=ruleColumnID();

            state._fsp--;

             current =iv_ruleColumnID.getText(); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleColumnID1537); 

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
    // $ANTLR end "entryRuleColumnID"


    // $ANTLR start "ruleColumnID"
    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:745:1: ruleColumnID returns [AntlrDatatypeRuleToken current=new AntlrDatatypeRuleToken()] : (this_ID_0= RULE_ID | this_DIGITS_1= RULE_DIGITS ) ;
    public final AntlrDatatypeRuleToken ruleColumnID() throws RecognitionException {
        AntlrDatatypeRuleToken current = new AntlrDatatypeRuleToken();

        Token this_ID_0=null;
        Token this_DIGITS_1=null;

         enterRule(); 
            
        try {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:748:28: ( (this_ID_0= RULE_ID | this_DIGITS_1= RULE_DIGITS ) )
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:749:1: (this_ID_0= RULE_ID | this_DIGITS_1= RULE_DIGITS )
            {
            // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:749:1: (this_ID_0= RULE_ID | this_DIGITS_1= RULE_DIGITS )
            int alt17=2;
            int LA17_0 = input.LA(1);

            if ( (LA17_0==RULE_ID) ) {
                alt17=1;
            }
            else if ( (LA17_0==RULE_DIGITS) ) {
                alt17=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 17, 0, input);

                throw nvae;
            }
            switch (alt17) {
                case 1 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:749:6: this_ID_0= RULE_ID
                    {
                    this_ID_0=(Token)match(input,RULE_ID,FOLLOW_RULE_ID_in_ruleColumnID1577); 

                    		current.merge(this_ID_0);
                        
                     
                        newLeafNode(this_ID_0, grammarAccess.getColumnIDAccess().getIDTerminalRuleCall_0()); 
                        

                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping/src-gen/com/ge/research/sadl/parser/antlr/internal/InternalMapping.g:757:10: this_DIGITS_1= RULE_DIGITS
                    {
                    this_DIGITS_1=(Token)match(input,RULE_DIGITS,FOLLOW_RULE_DIGITS_in_ruleColumnID1603); 

                    		current.merge(this_DIGITS_1);
                        
                     
                        newLeafNode(this_DIGITS_1, grammarAccess.getColumnIDAccess().getDIGITSTerminalRuleCall_1()); 
                        

                    }
                    break;

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
    // $ANTLR end "ruleColumnID"

    // Delegated rules


 

    public static final BitSet FOLLOW_ruleModel_in_entryRuleModel75 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleModel85 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNewModelNS_in_ruleModel131 = new BitSet(new long[]{0x0000000002028022L});
    public static final BitSet FOLLOW_ruleImport_in_ruleModel152 = new BitSet(new long[]{0x0000000002028022L});
    public static final BitSet FOLLOW_ruleTriple_in_ruleModel176 = new BitSet(new long[]{0x0000000002020022L});
    public static final BitSet FOLLOW_ruleGroup_in_ruleModel195 = new BitSet(new long[]{0x0000000002020022L});
    public static final BitSet FOLLOW_ruleNewModelNS_in_entryRuleNewModelNS235 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleNewModelNS245 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_13_in_ruleNewModelNS282 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_RULE_STRING_in_ruleNewModelNS299 = new BitSet(new long[]{0x0000000000004040L});
    public static final BitSet FOLLOW_14_in_ruleNewModelNS317 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_RULE_ID_in_ruleNewModelNS334 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_RULE_EOS_in_ruleNewModelNS352 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleImport_in_entryRuleImport387 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleImport397 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_15_in_ruleImport434 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_RULE_STRING_in_ruleImport451 = new BitSet(new long[]{0x0000000000010040L});
    public static final BitSet FOLLOW_16_in_ruleImport469 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_RULE_ID_in_ruleImport486 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_RULE_EOS_in_ruleImport504 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleGroup_in_entryRuleGroup539 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleGroup549 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_17_in_ruleGroup586 = new BitSet(new long[]{0x0000000002020020L});
    public static final BitSet FOLLOW_ruleTriple_in_ruleGroup609 = new BitSet(new long[]{0x0000000002060020L});
    public static final BitSet FOLLOW_ruleGroup_in_ruleGroup628 = new BitSet(new long[]{0x0000000002060020L});
    public static final BitSet FOLLOW_18_in_ruleGroup644 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_RULE_EOS_in_ruleGroup655 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleTriple_in_entryRuleTriple690 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleTriple700 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleRef_in_ruleTriple748 = new BitSet(new long[]{0x0000000000080020L});
    public static final BitSet FOLLOW_RULE_ID_in_ruleTriple766 = new BitSet(new long[]{0x0000000000080020L});
    public static final BitSet FOLLOW_19_in_ruleTriple782 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_RULE_ID_in_ruleTriple804 = new BitSet(new long[]{0x00000000027000B0L});
    public static final BitSet FOLLOW_ruleRef_in_ruleTriple827 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_ID_in_ruleTriple845 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleLiteralValue_in_ruleTriple864 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleLiteralValue_in_entryRuleLiteralValue903 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleLiteralValue913 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNUMBER_in_ruleLiteralValue959 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_STRING_in_ruleLiteralValue982 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_20_in_ruleLiteralValue1013 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_21_in_ruleLiteralValue1042 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNUMBER_in_entryRuleNUMBER1101 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleNUMBER1112 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_22_in_ruleNUMBER1155 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_RULE_UNSIGNED_NUMBER_in_ruleNUMBER1172 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleRef_in_entryRuleRef1221 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleRef1231 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnName_in_ruleRef1277 = new BitSet(new long[]{0x0000000003800002L});
    public static final BitSet FOLLOW_23_in_ruleRef1295 = new BitSet(new long[]{0x0000000003000002L});
    public static final BitSet FOLLOW_ruleColumnName_in_ruleRef1326 = new BitSet(new long[]{0x0000000003000002L});
    public static final BitSet FOLLOW_24_in_ruleRef1345 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnName_in_entryRuleColumnName1396 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleColumnName1407 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_25_in_ruleColumnName1445 = new BitSet(new long[]{0x0000000000000120L});
    public static final BitSet FOLLOW_ruleColumnID_in_ruleColumnName1467 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_26_in_ruleColumnName1485 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnID_in_entryRuleColumnID1526 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleColumnID1537 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_ID_in_ruleColumnID1577 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_DIGITS_in_ruleColumnID1603 = new BitSet(new long[]{0x0000000000000002L});

}