package com.ge.research.sadl.ui.contentassist.antlr.internal; 

import java.io.InputStream;
import org.eclipse.xtext.*;
import org.eclipse.xtext.parser.*;
import org.eclipse.xtext.parser.impl.*;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;
import org.eclipse.xtext.parser.antlr.XtextTokenStream.HiddenTokens;
import org.eclipse.xtext.ui.editor.contentassist.antlr.internal.AbstractInternalContentAssistParser;
import org.eclipse.xtext.ui.editor.contentassist.antlr.internal.DFA;
import com.ge.research.sadl.services.MappingGrammarAccess;



import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class InternalMappingParser extends AbstractInternalContentAssistParser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_ID", "RULE_DIGITS", "RULE_EOS", "RULE_UNSIGNED_NUMBER", "RULE_STRING", "RULE_WS", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_ANY_OTHER", "'true'", "'false'", "'uri'", "'alias'", "'import'", "'as'", "'{'", "'}'", "'has'", "'-'", "'<'", "'>'", "'_'", "'()'"
    };
    public static final int RULE_ID=4;
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
    public static final int RULE_STRING=8;
    public static final int T__16=16;
    public static final int T__15=15;
    public static final int T__18=18;
    public static final int T__17=17;
    public static final int RULE_EOS=6;
    public static final int T__14=14;
    public static final int T__13=13;
    public static final int RULE_WS=9;
    public static final int RULE_DIGITS=5;

    // delegates
    // delegators


        public InternalMappingParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public InternalMappingParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return InternalMappingParser.tokenNames; }
    public String getGrammarFileName() { return "../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g"; }


     
     	private MappingGrammarAccess grammarAccess;
     	
        public void setGrammarAccess(MappingGrammarAccess grammarAccess) {
        	this.grammarAccess = grammarAccess;
        }
        
        @Override
        protected Grammar getGrammar() {
        	return grammarAccess.getGrammar();
        }
        
        @Override
        protected String getValueForTokenName(String tokenName) {
        	return tokenName;
        }




    // $ANTLR start "entryRuleModel"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:60:1: entryRuleModel : ruleModel EOF ;
    public final void entryRuleModel() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:61:1: ( ruleModel EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:62:1: ruleModel EOF
            {
             before(grammarAccess.getModelRule()); 
            pushFollow(FOLLOW_ruleModel_in_entryRuleModel61);
            ruleModel();

            state._fsp--;

             after(grammarAccess.getModelRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleModel68); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleModel"


    // $ANTLR start "ruleModel"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:69:1: ruleModel : ( ( rule__Model__Group__0 ) ) ;
    public final void ruleModel() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:73:2: ( ( ( rule__Model__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:74:1: ( ( rule__Model__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:74:1: ( ( rule__Model__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:75:1: ( rule__Model__Group__0 )
            {
             before(grammarAccess.getModelAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:76:1: ( rule__Model__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:76:2: rule__Model__Group__0
            {
            pushFollow(FOLLOW_rule__Model__Group__0_in_ruleModel94);
            rule__Model__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getModelAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleModel"


    // $ANTLR start "entryRuleNewModelNS"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:88:1: entryRuleNewModelNS : ruleNewModelNS EOF ;
    public final void entryRuleNewModelNS() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:89:1: ( ruleNewModelNS EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:90:1: ruleNewModelNS EOF
            {
             before(grammarAccess.getNewModelNSRule()); 
            pushFollow(FOLLOW_ruleNewModelNS_in_entryRuleNewModelNS121);
            ruleNewModelNS();

            state._fsp--;

             after(grammarAccess.getNewModelNSRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleNewModelNS128); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleNewModelNS"


    // $ANTLR start "ruleNewModelNS"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:97:1: ruleNewModelNS : ( ( rule__NewModelNS__Group__0 ) ) ;
    public final void ruleNewModelNS() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:101:2: ( ( ( rule__NewModelNS__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:102:1: ( ( rule__NewModelNS__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:102:1: ( ( rule__NewModelNS__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:103:1: ( rule__NewModelNS__Group__0 )
            {
             before(grammarAccess.getNewModelNSAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:104:1: ( rule__NewModelNS__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:104:2: rule__NewModelNS__Group__0
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group__0_in_ruleNewModelNS154);
            rule__NewModelNS__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getNewModelNSAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleNewModelNS"


    // $ANTLR start "entryRuleImport"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:116:1: entryRuleImport : ruleImport EOF ;
    public final void entryRuleImport() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:117:1: ( ruleImport EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:118:1: ruleImport EOF
            {
             before(grammarAccess.getImportRule()); 
            pushFollow(FOLLOW_ruleImport_in_entryRuleImport181);
            ruleImport();

            state._fsp--;

             after(grammarAccess.getImportRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleImport188); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleImport"


    // $ANTLR start "ruleImport"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:125:1: ruleImport : ( ( rule__Import__Group__0 ) ) ;
    public final void ruleImport() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:129:2: ( ( ( rule__Import__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:130:1: ( ( rule__Import__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:130:1: ( ( rule__Import__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:131:1: ( rule__Import__Group__0 )
            {
             before(grammarAccess.getImportAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:132:1: ( rule__Import__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:132:2: rule__Import__Group__0
            {
            pushFollow(FOLLOW_rule__Import__Group__0_in_ruleImport214);
            rule__Import__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getImportAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleImport"


    // $ANTLR start "entryRuleGroup"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:144:1: entryRuleGroup : ruleGroup EOF ;
    public final void entryRuleGroup() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:145:1: ( ruleGroup EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:146:1: ruleGroup EOF
            {
             before(grammarAccess.getGroupRule()); 
            pushFollow(FOLLOW_ruleGroup_in_entryRuleGroup241);
            ruleGroup();

            state._fsp--;

             after(grammarAccess.getGroupRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleGroup248); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleGroup"


    // $ANTLR start "ruleGroup"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:153:1: ruleGroup : ( ( rule__Group__Group__0 ) ) ;
    public final void ruleGroup() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:157:2: ( ( ( rule__Group__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:158:1: ( ( rule__Group__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:158:1: ( ( rule__Group__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:159:1: ( rule__Group__Group__0 )
            {
             before(grammarAccess.getGroupAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:160:1: ( rule__Group__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:160:2: rule__Group__Group__0
            {
            pushFollow(FOLLOW_rule__Group__Group__0_in_ruleGroup274);
            rule__Group__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getGroupAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleGroup"


    // $ANTLR start "entryRuleTriple"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:172:1: entryRuleTriple : ruleTriple EOF ;
    public final void entryRuleTriple() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:173:1: ( ruleTriple EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:174:1: ruleTriple EOF
            {
             before(grammarAccess.getTripleRule()); 
            pushFollow(FOLLOW_ruleTriple_in_entryRuleTriple301);
            ruleTriple();

            state._fsp--;

             after(grammarAccess.getTripleRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleTriple308); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleTriple"


    // $ANTLR start "ruleTriple"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:181:1: ruleTriple : ( ( rule__Triple__Group__0 ) ) ;
    public final void ruleTriple() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:185:2: ( ( ( rule__Triple__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:186:1: ( ( rule__Triple__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:186:1: ( ( rule__Triple__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:187:1: ( rule__Triple__Group__0 )
            {
             before(grammarAccess.getTripleAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:188:1: ( rule__Triple__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:188:2: rule__Triple__Group__0
            {
            pushFollow(FOLLOW_rule__Triple__Group__0_in_ruleTriple334);
            rule__Triple__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getTripleAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleTriple"


    // $ANTLR start "entryRuleLiteralValue"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:200:1: entryRuleLiteralValue : ruleLiteralValue EOF ;
    public final void entryRuleLiteralValue() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:201:1: ( ruleLiteralValue EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:202:1: ruleLiteralValue EOF
            {
             before(grammarAccess.getLiteralValueRule()); 
            pushFollow(FOLLOW_ruleLiteralValue_in_entryRuleLiteralValue361);
            ruleLiteralValue();

            state._fsp--;

             after(grammarAccess.getLiteralValueRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleLiteralValue368); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleLiteralValue"


    // $ANTLR start "ruleLiteralValue"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:209:1: ruleLiteralValue : ( ( rule__LiteralValue__Alternatives ) ) ;
    public final void ruleLiteralValue() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:213:2: ( ( ( rule__LiteralValue__Alternatives ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:214:1: ( ( rule__LiteralValue__Alternatives ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:214:1: ( ( rule__LiteralValue__Alternatives ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:215:1: ( rule__LiteralValue__Alternatives )
            {
             before(grammarAccess.getLiteralValueAccess().getAlternatives()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:216:1: ( rule__LiteralValue__Alternatives )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:216:2: rule__LiteralValue__Alternatives
            {
            pushFollow(FOLLOW_rule__LiteralValue__Alternatives_in_ruleLiteralValue394);
            rule__LiteralValue__Alternatives();

            state._fsp--;


            }

             after(grammarAccess.getLiteralValueAccess().getAlternatives()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleLiteralValue"


    // $ANTLR start "entryRuleNUMBER"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:228:1: entryRuleNUMBER : ruleNUMBER EOF ;
    public final void entryRuleNUMBER() throws RecognitionException {

        	HiddenTokens myHiddenTokenState = ((XtextTokenStream)input).setHiddenTokens();

        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:232:1: ( ruleNUMBER EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:233:1: ruleNUMBER EOF
            {
             before(grammarAccess.getNUMBERRule()); 
            pushFollow(FOLLOW_ruleNUMBER_in_entryRuleNUMBER426);
            ruleNUMBER();

            state._fsp--;

             after(grammarAccess.getNUMBERRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleNUMBER433); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	myHiddenTokenState.restore();

        }
        return ;
    }
    // $ANTLR end "entryRuleNUMBER"


    // $ANTLR start "ruleNUMBER"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:243:1: ruleNUMBER : ( ( rule__NUMBER__Group__0 ) ) ;
    public final void ruleNUMBER() throws RecognitionException {

        		HiddenTokens myHiddenTokenState = ((XtextTokenStream)input).setHiddenTokens();
        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:248:2: ( ( ( rule__NUMBER__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:249:1: ( ( rule__NUMBER__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:249:1: ( ( rule__NUMBER__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:250:1: ( rule__NUMBER__Group__0 )
            {
             before(grammarAccess.getNUMBERAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:251:1: ( rule__NUMBER__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:251:2: rule__NUMBER__Group__0
            {
            pushFollow(FOLLOW_rule__NUMBER__Group__0_in_ruleNUMBER463);
            rule__NUMBER__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getNUMBERAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);
            	myHiddenTokenState.restore();

        }
        return ;
    }
    // $ANTLR end "ruleNUMBER"


    // $ANTLR start "entryRuleRef"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:264:1: entryRuleRef : ruleRef EOF ;
    public final void entryRuleRef() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:265:1: ( ruleRef EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:266:1: ruleRef EOF
            {
             before(grammarAccess.getRefRule()); 
            pushFollow(FOLLOW_ruleRef_in_entryRuleRef490);
            ruleRef();

            state._fsp--;

             after(grammarAccess.getRefRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleRef497); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleRef"


    // $ANTLR start "ruleRef"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:273:1: ruleRef : ( ( rule__Ref__Group__0 ) ) ;
    public final void ruleRef() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:277:2: ( ( ( rule__Ref__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:278:1: ( ( rule__Ref__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:278:1: ( ( rule__Ref__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:279:1: ( rule__Ref__Group__0 )
            {
             before(grammarAccess.getRefAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:280:1: ( rule__Ref__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:280:2: rule__Ref__Group__0
            {
            pushFollow(FOLLOW_rule__Ref__Group__0_in_ruleRef523);
            rule__Ref__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getRefAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleRef"


    // $ANTLR start "entryRuleColumnName"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:292:1: entryRuleColumnName : ruleColumnName EOF ;
    public final void entryRuleColumnName() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:293:1: ( ruleColumnName EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:294:1: ruleColumnName EOF
            {
             before(grammarAccess.getColumnNameRule()); 
            pushFollow(FOLLOW_ruleColumnName_in_entryRuleColumnName550);
            ruleColumnName();

            state._fsp--;

             after(grammarAccess.getColumnNameRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleColumnName557); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleColumnName"


    // $ANTLR start "ruleColumnName"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:301:1: ruleColumnName : ( ( rule__ColumnName__Group__0 ) ) ;
    public final void ruleColumnName() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:305:2: ( ( ( rule__ColumnName__Group__0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:306:1: ( ( rule__ColumnName__Group__0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:306:1: ( ( rule__ColumnName__Group__0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:307:1: ( rule__ColumnName__Group__0 )
            {
             before(grammarAccess.getColumnNameAccess().getGroup()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:308:1: ( rule__ColumnName__Group__0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:308:2: rule__ColumnName__Group__0
            {
            pushFollow(FOLLOW_rule__ColumnName__Group__0_in_ruleColumnName583);
            rule__ColumnName__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getColumnNameAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleColumnName"


    // $ANTLR start "entryRuleColumnID"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:320:1: entryRuleColumnID : ruleColumnID EOF ;
    public final void entryRuleColumnID() throws RecognitionException {
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:321:1: ( ruleColumnID EOF )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:322:1: ruleColumnID EOF
            {
             before(grammarAccess.getColumnIDRule()); 
            pushFollow(FOLLOW_ruleColumnID_in_entryRuleColumnID610);
            ruleColumnID();

            state._fsp--;

             after(grammarAccess.getColumnIDRule()); 
            match(input,EOF,FOLLOW_EOF_in_entryRuleColumnID617); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleColumnID"


    // $ANTLR start "ruleColumnID"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:329:1: ruleColumnID : ( ( rule__ColumnID__Alternatives ) ) ;
    public final void ruleColumnID() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:333:2: ( ( ( rule__ColumnID__Alternatives ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:334:1: ( ( rule__ColumnID__Alternatives ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:334:1: ( ( rule__ColumnID__Alternatives ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:335:1: ( rule__ColumnID__Alternatives )
            {
             before(grammarAccess.getColumnIDAccess().getAlternatives()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:336:1: ( rule__ColumnID__Alternatives )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:336:2: rule__ColumnID__Alternatives
            {
            pushFollow(FOLLOW_rule__ColumnID__Alternatives_in_ruleColumnID643);
            rule__ColumnID__Alternatives();

            state._fsp--;


            }

             after(grammarAccess.getColumnIDAccess().getAlternatives()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleColumnID"


    // $ANTLR start "rule__Model__TriplesAlternatives_2_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:348:1: rule__Model__TriplesAlternatives_2_0 : ( ( ruleTriple ) | ( ruleGroup ) );
    public final void rule__Model__TriplesAlternatives_2_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:352:1: ( ( ruleTriple ) | ( ruleGroup ) )
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0==RULE_ID||LA1_0==21||LA1_0==23) ) {
                alt1=1;
            }
            else if ( (LA1_0==19) ) {
                alt1=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }
            switch (alt1) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:353:1: ( ruleTriple )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:353:1: ( ruleTriple )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:354:1: ruleTriple
                    {
                     before(grammarAccess.getModelAccess().getTriplesTripleParserRuleCall_2_0_0()); 
                    pushFollow(FOLLOW_ruleTriple_in_rule__Model__TriplesAlternatives_2_0679);
                    ruleTriple();

                    state._fsp--;

                     after(grammarAccess.getModelAccess().getTriplesTripleParserRuleCall_2_0_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:359:6: ( ruleGroup )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:359:6: ( ruleGroup )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:360:1: ruleGroup
                    {
                     before(grammarAccess.getModelAccess().getTriplesGroupParserRuleCall_2_0_1()); 
                    pushFollow(FOLLOW_ruleGroup_in_rule__Model__TriplesAlternatives_2_0696);
                    ruleGroup();

                    state._fsp--;

                     after(grammarAccess.getModelAccess().getTriplesGroupParserRuleCall_2_0_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__TriplesAlternatives_2_0"


    // $ANTLR start "rule__Group__GroupLinesAlternatives_1_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:370:1: rule__Group__GroupLinesAlternatives_1_0 : ( ( ruleTriple ) | ( ruleGroup ) );
    public final void rule__Group__GroupLinesAlternatives_1_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:374:1: ( ( ruleTriple ) | ( ruleGroup ) )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==RULE_ID||LA2_0==21||LA2_0==23) ) {
                alt2=1;
            }
            else if ( (LA2_0==19) ) {
                alt2=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:375:1: ( ruleTriple )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:375:1: ( ruleTriple )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:376:1: ruleTriple
                    {
                     before(grammarAccess.getGroupAccess().getGroupLinesTripleParserRuleCall_1_0_0()); 
                    pushFollow(FOLLOW_ruleTriple_in_rule__Group__GroupLinesAlternatives_1_0728);
                    ruleTriple();

                    state._fsp--;

                     after(grammarAccess.getGroupAccess().getGroupLinesTripleParserRuleCall_1_0_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:381:6: ( ruleGroup )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:381:6: ( ruleGroup )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:382:1: ruleGroup
                    {
                     before(grammarAccess.getGroupAccess().getGroupLinesGroupParserRuleCall_1_0_1()); 
                    pushFollow(FOLLOW_ruleGroup_in_rule__Group__GroupLinesAlternatives_1_0745);
                    ruleGroup();

                    state._fsp--;

                     after(grammarAccess.getGroupAccess().getGroupLinesGroupParserRuleCall_1_0_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__GroupLinesAlternatives_1_0"


    // $ANTLR start "rule__Triple__SubjAlternatives_0_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:392:1: rule__Triple__SubjAlternatives_0_0 : ( ( ruleRef ) | ( () ) );
    public final void rule__Triple__SubjAlternatives_0_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:396:1: ( ( ruleRef ) | ( () ) )
            int alt3=2;
            int LA3_0 = input.LA(1);

            if ( (LA3_0==23) ) {
                alt3=1;
            }
            else if ( (LA3_0==RULE_ID||LA3_0==21) ) {
                alt3=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 3, 0, input);

                throw nvae;
            }
            switch (alt3) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:397:1: ( ruleRef )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:397:1: ( ruleRef )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:398:1: ruleRef
                    {
                     before(grammarAccess.getTripleAccess().getSubjRefParserRuleCall_0_0_0()); 
                    pushFollow(FOLLOW_ruleRef_in_rule__Triple__SubjAlternatives_0_0777);
                    ruleRef();

                    state._fsp--;

                     after(grammarAccess.getTripleAccess().getSubjRefParserRuleCall_0_0_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:403:6: ( () )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:403:6: ( () )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:404:1: ()
                    {
                     before(grammarAccess.getTripleAccess().getSubjResourceNameCrossReference_0_0_1()); 
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:405:1: ()
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:407:1: 
                    {
                    }

                     after(grammarAccess.getTripleAccess().getSubjResourceNameCrossReference_0_0_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__SubjAlternatives_0_0"


    // $ANTLR start "rule__Triple__ObjvalAlternatives_3_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:416:1: rule__Triple__ObjvalAlternatives_3_0 : ( ( ruleRef ) | ( () ) | ( ruleLiteralValue ) );
    public final void rule__Triple__ObjvalAlternatives_3_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:420:1: ( ( ruleRef ) | ( () ) | ( ruleLiteralValue ) )
            int alt4=3;
            switch ( input.LA(1) ) {
            case 23:
                {
                int LA4_1 = input.LA(2);

                if ( (LA4_1==RULE_ID) ) {
                    int LA4_4 = input.LA(3);

                    if ( (LA4_4==24) ) {
                        alt4=1;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 4, 4, input);

                        throw nvae;
                    }
                }
                else if ( (LA4_1==RULE_DIGITS) ) {
                    int LA4_5 = input.LA(3);

                    if ( (LA4_5==24) ) {
                        alt4=1;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 4, 5, input);

                        throw nvae;
                    }
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 4, 1, input);

                    throw nvae;
                }
                }
                break;
            case EOF:
            case RULE_ID:
            case 19:
            case 20:
            case 21:
                {
                alt4=2;
                }
                break;
            case RULE_UNSIGNED_NUMBER:
            case RULE_STRING:
            case 13:
            case 14:
            case 22:
                {
                alt4=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }

            switch (alt4) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:421:1: ( ruleRef )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:421:1: ( ruleRef )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:422:1: ruleRef
                    {
                     before(grammarAccess.getTripleAccess().getObjvalRefParserRuleCall_3_0_0()); 
                    pushFollow(FOLLOW_ruleRef_in_rule__Triple__ObjvalAlternatives_3_0828);
                    ruleRef();

                    state._fsp--;

                     after(grammarAccess.getTripleAccess().getObjvalRefParserRuleCall_3_0_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:427:6: ( () )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:427:6: ( () )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:428:1: ()
                    {
                     before(grammarAccess.getTripleAccess().getObjvalResourceNameCrossReference_3_0_1()); 
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:429:1: ()
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:431:1: 
                    {
                    }

                     after(grammarAccess.getTripleAccess().getObjvalResourceNameCrossReference_3_0_1()); 

                    }


                    }
                    break;
                case 3 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:435:6: ( ruleLiteralValue )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:435:6: ( ruleLiteralValue )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:436:1: ruleLiteralValue
                    {
                     before(grammarAccess.getTripleAccess().getObjvalLiteralValueParserRuleCall_3_0_2()); 
                    pushFollow(FOLLOW_ruleLiteralValue_in_rule__Triple__ObjvalAlternatives_3_0864);
                    ruleLiteralValue();

                    state._fsp--;

                     after(grammarAccess.getTripleAccess().getObjvalLiteralValueParserRuleCall_3_0_2()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__ObjvalAlternatives_3_0"


    // $ANTLR start "rule__LiteralValue__Alternatives"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:446:1: rule__LiteralValue__Alternatives : ( ( ( rule__LiteralValue__LiteralNumberAssignment_0 ) ) | ( ( rule__LiteralValue__LiteralStringAssignment_1 ) ) | ( ( rule__LiteralValue__LiteralBooleanAssignment_2 ) ) );
    public final void rule__LiteralValue__Alternatives() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:450:1: ( ( ( rule__LiteralValue__LiteralNumberAssignment_0 ) ) | ( ( rule__LiteralValue__LiteralStringAssignment_1 ) ) | ( ( rule__LiteralValue__LiteralBooleanAssignment_2 ) ) )
            int alt5=3;
            switch ( input.LA(1) ) {
            case RULE_UNSIGNED_NUMBER:
            case 22:
                {
                alt5=1;
                }
                break;
            case RULE_STRING:
                {
                alt5=2;
                }
                break;
            case 13:
            case 14:
                {
                alt5=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }

            switch (alt5) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:451:1: ( ( rule__LiteralValue__LiteralNumberAssignment_0 ) )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:451:1: ( ( rule__LiteralValue__LiteralNumberAssignment_0 ) )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:452:1: ( rule__LiteralValue__LiteralNumberAssignment_0 )
                    {
                     before(grammarAccess.getLiteralValueAccess().getLiteralNumberAssignment_0()); 
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:453:1: ( rule__LiteralValue__LiteralNumberAssignment_0 )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:453:2: rule__LiteralValue__LiteralNumberAssignment_0
                    {
                    pushFollow(FOLLOW_rule__LiteralValue__LiteralNumberAssignment_0_in_rule__LiteralValue__Alternatives896);
                    rule__LiteralValue__LiteralNumberAssignment_0();

                    state._fsp--;


                    }

                     after(grammarAccess.getLiteralValueAccess().getLiteralNumberAssignment_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:457:6: ( ( rule__LiteralValue__LiteralStringAssignment_1 ) )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:457:6: ( ( rule__LiteralValue__LiteralStringAssignment_1 ) )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:458:1: ( rule__LiteralValue__LiteralStringAssignment_1 )
                    {
                     before(grammarAccess.getLiteralValueAccess().getLiteralStringAssignment_1()); 
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:459:1: ( rule__LiteralValue__LiteralStringAssignment_1 )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:459:2: rule__LiteralValue__LiteralStringAssignment_1
                    {
                    pushFollow(FOLLOW_rule__LiteralValue__LiteralStringAssignment_1_in_rule__LiteralValue__Alternatives914);
                    rule__LiteralValue__LiteralStringAssignment_1();

                    state._fsp--;


                    }

                     after(grammarAccess.getLiteralValueAccess().getLiteralStringAssignment_1()); 

                    }


                    }
                    break;
                case 3 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:463:6: ( ( rule__LiteralValue__LiteralBooleanAssignment_2 ) )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:463:6: ( ( rule__LiteralValue__LiteralBooleanAssignment_2 ) )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:464:1: ( rule__LiteralValue__LiteralBooleanAssignment_2 )
                    {
                     before(grammarAccess.getLiteralValueAccess().getLiteralBooleanAssignment_2()); 
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:465:1: ( rule__LiteralValue__LiteralBooleanAssignment_2 )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:465:2: rule__LiteralValue__LiteralBooleanAssignment_2
                    {
                    pushFollow(FOLLOW_rule__LiteralValue__LiteralBooleanAssignment_2_in_rule__LiteralValue__Alternatives932);
                    rule__LiteralValue__LiteralBooleanAssignment_2();

                    state._fsp--;


                    }

                     after(grammarAccess.getLiteralValueAccess().getLiteralBooleanAssignment_2()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__LiteralValue__Alternatives"


    // $ANTLR start "rule__LiteralValue__LiteralBooleanAlternatives_2_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:474:1: rule__LiteralValue__LiteralBooleanAlternatives_2_0 : ( ( 'true' ) | ( 'false' ) );
    public final void rule__LiteralValue__LiteralBooleanAlternatives_2_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:478:1: ( ( 'true' ) | ( 'false' ) )
            int alt6=2;
            int LA6_0 = input.LA(1);

            if ( (LA6_0==13) ) {
                alt6=1;
            }
            else if ( (LA6_0==14) ) {
                alt6=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 6, 0, input);

                throw nvae;
            }
            switch (alt6) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:479:1: ( 'true' )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:479:1: ( 'true' )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:480:1: 'true'
                    {
                     before(grammarAccess.getLiteralValueAccess().getLiteralBooleanTrueKeyword_2_0_0()); 
                    match(input,13,FOLLOW_13_in_rule__LiteralValue__LiteralBooleanAlternatives_2_0966); 
                     after(grammarAccess.getLiteralValueAccess().getLiteralBooleanTrueKeyword_2_0_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:487:6: ( 'false' )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:487:6: ( 'false' )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:488:1: 'false'
                    {
                     before(grammarAccess.getLiteralValueAccess().getLiteralBooleanFalseKeyword_2_0_1()); 
                    match(input,14,FOLLOW_14_in_rule__LiteralValue__LiteralBooleanAlternatives_2_0986); 
                     after(grammarAccess.getLiteralValueAccess().getLiteralBooleanFalseKeyword_2_0_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__LiteralValue__LiteralBooleanAlternatives_2_0"


    // $ANTLR start "rule__ColumnID__Alternatives"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:500:1: rule__ColumnID__Alternatives : ( ( RULE_ID ) | ( RULE_DIGITS ) );
    public final void rule__ColumnID__Alternatives() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:504:1: ( ( RULE_ID ) | ( RULE_DIGITS ) )
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( (LA7_0==RULE_ID) ) {
                alt7=1;
            }
            else if ( (LA7_0==RULE_DIGITS) ) {
                alt7=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 7, 0, input);

                throw nvae;
            }
            switch (alt7) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:505:1: ( RULE_ID )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:505:1: ( RULE_ID )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:506:1: RULE_ID
                    {
                     before(grammarAccess.getColumnIDAccess().getIDTerminalRuleCall_0()); 
                    match(input,RULE_ID,FOLLOW_RULE_ID_in_rule__ColumnID__Alternatives1020); 
                     after(grammarAccess.getColumnIDAccess().getIDTerminalRuleCall_0()); 

                    }


                    }
                    break;
                case 2 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:511:6: ( RULE_DIGITS )
                    {
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:511:6: ( RULE_DIGITS )
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:512:1: RULE_DIGITS
                    {
                     before(grammarAccess.getColumnIDAccess().getDIGITSTerminalRuleCall_1()); 
                    match(input,RULE_DIGITS,FOLLOW_RULE_DIGITS_in_rule__ColumnID__Alternatives1037); 
                     after(grammarAccess.getColumnIDAccess().getDIGITSTerminalRuleCall_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnID__Alternatives"


    // $ANTLR start "rule__Model__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:524:1: rule__Model__Group__0 : rule__Model__Group__0__Impl rule__Model__Group__1 ;
    public final void rule__Model__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:528:1: ( rule__Model__Group__0__Impl rule__Model__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:529:2: rule__Model__Group__0__Impl rule__Model__Group__1
            {
            pushFollow(FOLLOW_rule__Model__Group__0__Impl_in_rule__Model__Group__01067);
            rule__Model__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Model__Group__1_in_rule__Model__Group__01070);
            rule__Model__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__Group__0"


    // $ANTLR start "rule__Model__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:536:1: rule__Model__Group__0__Impl : ( ( rule__Model__UriAssignment_0 ) ) ;
    public final void rule__Model__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:540:1: ( ( ( rule__Model__UriAssignment_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:541:1: ( ( rule__Model__UriAssignment_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:541:1: ( ( rule__Model__UriAssignment_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:542:1: ( rule__Model__UriAssignment_0 )
            {
             before(grammarAccess.getModelAccess().getUriAssignment_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:543:1: ( rule__Model__UriAssignment_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:543:2: rule__Model__UriAssignment_0
            {
            pushFollow(FOLLOW_rule__Model__UriAssignment_0_in_rule__Model__Group__0__Impl1097);
            rule__Model__UriAssignment_0();

            state._fsp--;


            }

             after(grammarAccess.getModelAccess().getUriAssignment_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__Group__0__Impl"


    // $ANTLR start "rule__Model__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:553:1: rule__Model__Group__1 : rule__Model__Group__1__Impl rule__Model__Group__2 ;
    public final void rule__Model__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:557:1: ( rule__Model__Group__1__Impl rule__Model__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:558:2: rule__Model__Group__1__Impl rule__Model__Group__2
            {
            pushFollow(FOLLOW_rule__Model__Group__1__Impl_in_rule__Model__Group__11127);
            rule__Model__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Model__Group__2_in_rule__Model__Group__11130);
            rule__Model__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__Group__1"


    // $ANTLR start "rule__Model__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:565:1: rule__Model__Group__1__Impl : ( ( rule__Model__ImportsAssignment_1 )* ) ;
    public final void rule__Model__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:569:1: ( ( ( rule__Model__ImportsAssignment_1 )* ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:570:1: ( ( rule__Model__ImportsAssignment_1 )* )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:570:1: ( ( rule__Model__ImportsAssignment_1 )* )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:571:1: ( rule__Model__ImportsAssignment_1 )*
            {
             before(grammarAccess.getModelAccess().getImportsAssignment_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:572:1: ( rule__Model__ImportsAssignment_1 )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( (LA8_0==17) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:572:2: rule__Model__ImportsAssignment_1
            	    {
            	    pushFollow(FOLLOW_rule__Model__ImportsAssignment_1_in_rule__Model__Group__1__Impl1157);
            	    rule__Model__ImportsAssignment_1();

            	    state._fsp--;


            	    }
            	    break;

            	default :
            	    break loop8;
                }
            } while (true);

             after(grammarAccess.getModelAccess().getImportsAssignment_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__Group__1__Impl"


    // $ANTLR start "rule__Model__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:582:1: rule__Model__Group__2 : rule__Model__Group__2__Impl ;
    public final void rule__Model__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:586:1: ( rule__Model__Group__2__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:587:2: rule__Model__Group__2__Impl
            {
            pushFollow(FOLLOW_rule__Model__Group__2__Impl_in_rule__Model__Group__21188);
            rule__Model__Group__2__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__Group__2"


    // $ANTLR start "rule__Model__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:593:1: rule__Model__Group__2__Impl : ( ( rule__Model__TriplesAssignment_2 )* ) ;
    public final void rule__Model__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:597:1: ( ( ( rule__Model__TriplesAssignment_2 )* ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:598:1: ( ( rule__Model__TriplesAssignment_2 )* )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:598:1: ( ( rule__Model__TriplesAssignment_2 )* )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:599:1: ( rule__Model__TriplesAssignment_2 )*
            {
             before(grammarAccess.getModelAccess().getTriplesAssignment_2()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:600:1: ( rule__Model__TriplesAssignment_2 )*
            loop9:
            do {
                int alt9=2;
                int LA9_0 = input.LA(1);

                if ( (LA9_0==RULE_ID||LA9_0==19||LA9_0==21||LA9_0==23) ) {
                    alt9=1;
                }


                switch (alt9) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:600:2: rule__Model__TriplesAssignment_2
            	    {
            	    pushFollow(FOLLOW_rule__Model__TriplesAssignment_2_in_rule__Model__Group__2__Impl1215);
            	    rule__Model__TriplesAssignment_2();

            	    state._fsp--;


            	    }
            	    break;

            	default :
            	    break loop9;
                }
            } while (true);

             after(grammarAccess.getModelAccess().getTriplesAssignment_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__Group__2__Impl"


    // $ANTLR start "rule__NewModelNS__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:616:1: rule__NewModelNS__Group__0 : rule__NewModelNS__Group__0__Impl rule__NewModelNS__Group__1 ;
    public final void rule__NewModelNS__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:620:1: ( rule__NewModelNS__Group__0__Impl rule__NewModelNS__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:621:2: rule__NewModelNS__Group__0__Impl rule__NewModelNS__Group__1
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group__0__Impl_in_rule__NewModelNS__Group__01252);
            rule__NewModelNS__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__NewModelNS__Group__1_in_rule__NewModelNS__Group__01255);
            rule__NewModelNS__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__0"


    // $ANTLR start "rule__NewModelNS__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:628:1: rule__NewModelNS__Group__0__Impl : ( 'uri' ) ;
    public final void rule__NewModelNS__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:632:1: ( ( 'uri' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:633:1: ( 'uri' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:633:1: ( 'uri' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:634:1: 'uri'
            {
             before(grammarAccess.getNewModelNSAccess().getUriKeyword_0()); 
            match(input,15,FOLLOW_15_in_rule__NewModelNS__Group__0__Impl1283); 
             after(grammarAccess.getNewModelNSAccess().getUriKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__0__Impl"


    // $ANTLR start "rule__NewModelNS__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:647:1: rule__NewModelNS__Group__1 : rule__NewModelNS__Group__1__Impl rule__NewModelNS__Group__2 ;
    public final void rule__NewModelNS__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:651:1: ( rule__NewModelNS__Group__1__Impl rule__NewModelNS__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:652:2: rule__NewModelNS__Group__1__Impl rule__NewModelNS__Group__2
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group__1__Impl_in_rule__NewModelNS__Group__11314);
            rule__NewModelNS__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__NewModelNS__Group__2_in_rule__NewModelNS__Group__11317);
            rule__NewModelNS__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__1"


    // $ANTLR start "rule__NewModelNS__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:659:1: rule__NewModelNS__Group__1__Impl : ( ( rule__NewModelNS__BaseUriAssignment_1 ) ) ;
    public final void rule__NewModelNS__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:663:1: ( ( ( rule__NewModelNS__BaseUriAssignment_1 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:664:1: ( ( rule__NewModelNS__BaseUriAssignment_1 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:664:1: ( ( rule__NewModelNS__BaseUriAssignment_1 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:665:1: ( rule__NewModelNS__BaseUriAssignment_1 )
            {
             before(grammarAccess.getNewModelNSAccess().getBaseUriAssignment_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:666:1: ( rule__NewModelNS__BaseUriAssignment_1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:666:2: rule__NewModelNS__BaseUriAssignment_1
            {
            pushFollow(FOLLOW_rule__NewModelNS__BaseUriAssignment_1_in_rule__NewModelNS__Group__1__Impl1344);
            rule__NewModelNS__BaseUriAssignment_1();

            state._fsp--;


            }

             after(grammarAccess.getNewModelNSAccess().getBaseUriAssignment_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__1__Impl"


    // $ANTLR start "rule__NewModelNS__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:676:1: rule__NewModelNS__Group__2 : rule__NewModelNS__Group__2__Impl rule__NewModelNS__Group__3 ;
    public final void rule__NewModelNS__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:680:1: ( rule__NewModelNS__Group__2__Impl rule__NewModelNS__Group__3 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:681:2: rule__NewModelNS__Group__2__Impl rule__NewModelNS__Group__3
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group__2__Impl_in_rule__NewModelNS__Group__21374);
            rule__NewModelNS__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__NewModelNS__Group__3_in_rule__NewModelNS__Group__21377);
            rule__NewModelNS__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__2"


    // $ANTLR start "rule__NewModelNS__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:688:1: rule__NewModelNS__Group__2__Impl : ( ( rule__NewModelNS__Group_2__0 )? ) ;
    public final void rule__NewModelNS__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:692:1: ( ( ( rule__NewModelNS__Group_2__0 )? ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:693:1: ( ( rule__NewModelNS__Group_2__0 )? )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:693:1: ( ( rule__NewModelNS__Group_2__0 )? )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:694:1: ( rule__NewModelNS__Group_2__0 )?
            {
             before(grammarAccess.getNewModelNSAccess().getGroup_2()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:695:1: ( rule__NewModelNS__Group_2__0 )?
            int alt10=2;
            int LA10_0 = input.LA(1);

            if ( (LA10_0==16) ) {
                alt10=1;
            }
            switch (alt10) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:695:2: rule__NewModelNS__Group_2__0
                    {
                    pushFollow(FOLLOW_rule__NewModelNS__Group_2__0_in_rule__NewModelNS__Group__2__Impl1404);
                    rule__NewModelNS__Group_2__0();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getNewModelNSAccess().getGroup_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__2__Impl"


    // $ANTLR start "rule__NewModelNS__Group__3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:705:1: rule__NewModelNS__Group__3 : rule__NewModelNS__Group__3__Impl ;
    public final void rule__NewModelNS__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:709:1: ( rule__NewModelNS__Group__3__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:710:2: rule__NewModelNS__Group__3__Impl
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group__3__Impl_in_rule__NewModelNS__Group__31435);
            rule__NewModelNS__Group__3__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__3"


    // $ANTLR start "rule__NewModelNS__Group__3__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:716:1: rule__NewModelNS__Group__3__Impl : ( RULE_EOS ) ;
    public final void rule__NewModelNS__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:720:1: ( ( RULE_EOS ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:721:1: ( RULE_EOS )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:721:1: ( RULE_EOS )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:722:1: RULE_EOS
            {
             before(grammarAccess.getNewModelNSAccess().getEOSTerminalRuleCall_3()); 
            match(input,RULE_EOS,FOLLOW_RULE_EOS_in_rule__NewModelNS__Group__3__Impl1462); 
             after(grammarAccess.getNewModelNSAccess().getEOSTerminalRuleCall_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group__3__Impl"


    // $ANTLR start "rule__NewModelNS__Group_2__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:741:1: rule__NewModelNS__Group_2__0 : rule__NewModelNS__Group_2__0__Impl rule__NewModelNS__Group_2__1 ;
    public final void rule__NewModelNS__Group_2__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:745:1: ( rule__NewModelNS__Group_2__0__Impl rule__NewModelNS__Group_2__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:746:2: rule__NewModelNS__Group_2__0__Impl rule__NewModelNS__Group_2__1
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group_2__0__Impl_in_rule__NewModelNS__Group_2__01499);
            rule__NewModelNS__Group_2__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__NewModelNS__Group_2__1_in_rule__NewModelNS__Group_2__01502);
            rule__NewModelNS__Group_2__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group_2__0"


    // $ANTLR start "rule__NewModelNS__Group_2__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:753:1: rule__NewModelNS__Group_2__0__Impl : ( 'alias' ) ;
    public final void rule__NewModelNS__Group_2__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:757:1: ( ( 'alias' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:758:1: ( 'alias' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:758:1: ( 'alias' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:759:1: 'alias'
            {
             before(grammarAccess.getNewModelNSAccess().getAliasKeyword_2_0()); 
            match(input,16,FOLLOW_16_in_rule__NewModelNS__Group_2__0__Impl1530); 
             after(grammarAccess.getNewModelNSAccess().getAliasKeyword_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group_2__0__Impl"


    // $ANTLR start "rule__NewModelNS__Group_2__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:772:1: rule__NewModelNS__Group_2__1 : rule__NewModelNS__Group_2__1__Impl ;
    public final void rule__NewModelNS__Group_2__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:776:1: ( rule__NewModelNS__Group_2__1__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:777:2: rule__NewModelNS__Group_2__1__Impl
            {
            pushFollow(FOLLOW_rule__NewModelNS__Group_2__1__Impl_in_rule__NewModelNS__Group_2__11561);
            rule__NewModelNS__Group_2__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group_2__1"


    // $ANTLR start "rule__NewModelNS__Group_2__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:783:1: rule__NewModelNS__Group_2__1__Impl : ( ( rule__NewModelNS__PrefixAssignment_2_1 ) ) ;
    public final void rule__NewModelNS__Group_2__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:787:1: ( ( ( rule__NewModelNS__PrefixAssignment_2_1 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:788:1: ( ( rule__NewModelNS__PrefixAssignment_2_1 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:788:1: ( ( rule__NewModelNS__PrefixAssignment_2_1 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:789:1: ( rule__NewModelNS__PrefixAssignment_2_1 )
            {
             before(grammarAccess.getNewModelNSAccess().getPrefixAssignment_2_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:790:1: ( rule__NewModelNS__PrefixAssignment_2_1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:790:2: rule__NewModelNS__PrefixAssignment_2_1
            {
            pushFollow(FOLLOW_rule__NewModelNS__PrefixAssignment_2_1_in_rule__NewModelNS__Group_2__1__Impl1588);
            rule__NewModelNS__PrefixAssignment_2_1();

            state._fsp--;


            }

             after(grammarAccess.getNewModelNSAccess().getPrefixAssignment_2_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__Group_2__1__Impl"


    // $ANTLR start "rule__Import__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:804:1: rule__Import__Group__0 : rule__Import__Group__0__Impl rule__Import__Group__1 ;
    public final void rule__Import__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:808:1: ( rule__Import__Group__0__Impl rule__Import__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:809:2: rule__Import__Group__0__Impl rule__Import__Group__1
            {
            pushFollow(FOLLOW_rule__Import__Group__0__Impl_in_rule__Import__Group__01622);
            rule__Import__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Import__Group__1_in_rule__Import__Group__01625);
            rule__Import__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__0"


    // $ANTLR start "rule__Import__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:816:1: rule__Import__Group__0__Impl : ( 'import' ) ;
    public final void rule__Import__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:820:1: ( ( 'import' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:821:1: ( 'import' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:821:1: ( 'import' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:822:1: 'import'
            {
             before(grammarAccess.getImportAccess().getImportKeyword_0()); 
            match(input,17,FOLLOW_17_in_rule__Import__Group__0__Impl1653); 
             after(grammarAccess.getImportAccess().getImportKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__0__Impl"


    // $ANTLR start "rule__Import__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:835:1: rule__Import__Group__1 : rule__Import__Group__1__Impl rule__Import__Group__2 ;
    public final void rule__Import__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:839:1: ( rule__Import__Group__1__Impl rule__Import__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:840:2: rule__Import__Group__1__Impl rule__Import__Group__2
            {
            pushFollow(FOLLOW_rule__Import__Group__1__Impl_in_rule__Import__Group__11684);
            rule__Import__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Import__Group__2_in_rule__Import__Group__11687);
            rule__Import__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__1"


    // $ANTLR start "rule__Import__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:847:1: rule__Import__Group__1__Impl : ( ( rule__Import__ImportURIAssignment_1 ) ) ;
    public final void rule__Import__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:851:1: ( ( ( rule__Import__ImportURIAssignment_1 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:852:1: ( ( rule__Import__ImportURIAssignment_1 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:852:1: ( ( rule__Import__ImportURIAssignment_1 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:853:1: ( rule__Import__ImportURIAssignment_1 )
            {
             before(grammarAccess.getImportAccess().getImportURIAssignment_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:854:1: ( rule__Import__ImportURIAssignment_1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:854:2: rule__Import__ImportURIAssignment_1
            {
            pushFollow(FOLLOW_rule__Import__ImportURIAssignment_1_in_rule__Import__Group__1__Impl1714);
            rule__Import__ImportURIAssignment_1();

            state._fsp--;


            }

             after(grammarAccess.getImportAccess().getImportURIAssignment_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__1__Impl"


    // $ANTLR start "rule__Import__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:864:1: rule__Import__Group__2 : rule__Import__Group__2__Impl rule__Import__Group__3 ;
    public final void rule__Import__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:868:1: ( rule__Import__Group__2__Impl rule__Import__Group__3 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:869:2: rule__Import__Group__2__Impl rule__Import__Group__3
            {
            pushFollow(FOLLOW_rule__Import__Group__2__Impl_in_rule__Import__Group__21744);
            rule__Import__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Import__Group__3_in_rule__Import__Group__21747);
            rule__Import__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__2"


    // $ANTLR start "rule__Import__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:876:1: rule__Import__Group__2__Impl : ( ( rule__Import__Group_2__0 )? ) ;
    public final void rule__Import__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:880:1: ( ( ( rule__Import__Group_2__0 )? ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:881:1: ( ( rule__Import__Group_2__0 )? )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:881:1: ( ( rule__Import__Group_2__0 )? )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:882:1: ( rule__Import__Group_2__0 )?
            {
             before(grammarAccess.getImportAccess().getGroup_2()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:883:1: ( rule__Import__Group_2__0 )?
            int alt11=2;
            int LA11_0 = input.LA(1);

            if ( (LA11_0==18) ) {
                alt11=1;
            }
            switch (alt11) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:883:2: rule__Import__Group_2__0
                    {
                    pushFollow(FOLLOW_rule__Import__Group_2__0_in_rule__Import__Group__2__Impl1774);
                    rule__Import__Group_2__0();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getImportAccess().getGroup_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__2__Impl"


    // $ANTLR start "rule__Import__Group__3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:893:1: rule__Import__Group__3 : rule__Import__Group__3__Impl ;
    public final void rule__Import__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:897:1: ( rule__Import__Group__3__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:898:2: rule__Import__Group__3__Impl
            {
            pushFollow(FOLLOW_rule__Import__Group__3__Impl_in_rule__Import__Group__31805);
            rule__Import__Group__3__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__3"


    // $ANTLR start "rule__Import__Group__3__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:904:1: rule__Import__Group__3__Impl : ( RULE_EOS ) ;
    public final void rule__Import__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:908:1: ( ( RULE_EOS ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:909:1: ( RULE_EOS )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:909:1: ( RULE_EOS )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:910:1: RULE_EOS
            {
             before(grammarAccess.getImportAccess().getEOSTerminalRuleCall_3()); 
            match(input,RULE_EOS,FOLLOW_RULE_EOS_in_rule__Import__Group__3__Impl1832); 
             after(grammarAccess.getImportAccess().getEOSTerminalRuleCall_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group__3__Impl"


    // $ANTLR start "rule__Import__Group_2__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:929:1: rule__Import__Group_2__0 : rule__Import__Group_2__0__Impl rule__Import__Group_2__1 ;
    public final void rule__Import__Group_2__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:933:1: ( rule__Import__Group_2__0__Impl rule__Import__Group_2__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:934:2: rule__Import__Group_2__0__Impl rule__Import__Group_2__1
            {
            pushFollow(FOLLOW_rule__Import__Group_2__0__Impl_in_rule__Import__Group_2__01869);
            rule__Import__Group_2__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Import__Group_2__1_in_rule__Import__Group_2__01872);
            rule__Import__Group_2__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group_2__0"


    // $ANTLR start "rule__Import__Group_2__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:941:1: rule__Import__Group_2__0__Impl : ( 'as' ) ;
    public final void rule__Import__Group_2__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:945:1: ( ( 'as' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:946:1: ( 'as' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:946:1: ( 'as' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:947:1: 'as'
            {
             before(grammarAccess.getImportAccess().getAsKeyword_2_0()); 
            match(input,18,FOLLOW_18_in_rule__Import__Group_2__0__Impl1900); 
             after(grammarAccess.getImportAccess().getAsKeyword_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group_2__0__Impl"


    // $ANTLR start "rule__Import__Group_2__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:960:1: rule__Import__Group_2__1 : rule__Import__Group_2__1__Impl ;
    public final void rule__Import__Group_2__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:964:1: ( rule__Import__Group_2__1__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:965:2: rule__Import__Group_2__1__Impl
            {
            pushFollow(FOLLOW_rule__Import__Group_2__1__Impl_in_rule__Import__Group_2__11931);
            rule__Import__Group_2__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group_2__1"


    // $ANTLR start "rule__Import__Group_2__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:971:1: rule__Import__Group_2__1__Impl : ( ( rule__Import__AliasAssignment_2_1 ) ) ;
    public final void rule__Import__Group_2__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:975:1: ( ( ( rule__Import__AliasAssignment_2_1 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:976:1: ( ( rule__Import__AliasAssignment_2_1 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:976:1: ( ( rule__Import__AliasAssignment_2_1 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:977:1: ( rule__Import__AliasAssignment_2_1 )
            {
             before(grammarAccess.getImportAccess().getAliasAssignment_2_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:978:1: ( rule__Import__AliasAssignment_2_1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:978:2: rule__Import__AliasAssignment_2_1
            {
            pushFollow(FOLLOW_rule__Import__AliasAssignment_2_1_in_rule__Import__Group_2__1__Impl1958);
            rule__Import__AliasAssignment_2_1();

            state._fsp--;


            }

             after(grammarAccess.getImportAccess().getAliasAssignment_2_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__Group_2__1__Impl"


    // $ANTLR start "rule__Group__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:992:1: rule__Group__Group__0 : rule__Group__Group__0__Impl rule__Group__Group__1 ;
    public final void rule__Group__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:996:1: ( rule__Group__Group__0__Impl rule__Group__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:997:2: rule__Group__Group__0__Impl rule__Group__Group__1
            {
            pushFollow(FOLLOW_rule__Group__Group__0__Impl_in_rule__Group__Group__01992);
            rule__Group__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Group__Group__1_in_rule__Group__Group__01995);
            rule__Group__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__0"


    // $ANTLR start "rule__Group__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1004:1: rule__Group__Group__0__Impl : ( '{' ) ;
    public final void rule__Group__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1008:1: ( ( '{' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1009:1: ( '{' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1009:1: ( '{' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1010:1: '{'
            {
             before(grammarAccess.getGroupAccess().getLeftCurlyBracketKeyword_0()); 
            match(input,19,FOLLOW_19_in_rule__Group__Group__0__Impl2023); 
             after(grammarAccess.getGroupAccess().getLeftCurlyBracketKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__0__Impl"


    // $ANTLR start "rule__Group__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1023:1: rule__Group__Group__1 : rule__Group__Group__1__Impl rule__Group__Group__2 ;
    public final void rule__Group__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1027:1: ( rule__Group__Group__1__Impl rule__Group__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1028:2: rule__Group__Group__1__Impl rule__Group__Group__2
            {
            pushFollow(FOLLOW_rule__Group__Group__1__Impl_in_rule__Group__Group__12054);
            rule__Group__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Group__Group__2_in_rule__Group__Group__12057);
            rule__Group__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__1"


    // $ANTLR start "rule__Group__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1035:1: rule__Group__Group__1__Impl : ( ( ( rule__Group__GroupLinesAssignment_1 ) ) ( ( rule__Group__GroupLinesAssignment_1 )* ) ) ;
    public final void rule__Group__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1039:1: ( ( ( ( rule__Group__GroupLinesAssignment_1 ) ) ( ( rule__Group__GroupLinesAssignment_1 )* ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1040:1: ( ( ( rule__Group__GroupLinesAssignment_1 ) ) ( ( rule__Group__GroupLinesAssignment_1 )* ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1040:1: ( ( ( rule__Group__GroupLinesAssignment_1 ) ) ( ( rule__Group__GroupLinesAssignment_1 )* ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1041:1: ( ( rule__Group__GroupLinesAssignment_1 ) ) ( ( rule__Group__GroupLinesAssignment_1 )* )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1041:1: ( ( rule__Group__GroupLinesAssignment_1 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1042:1: ( rule__Group__GroupLinesAssignment_1 )
            {
             before(grammarAccess.getGroupAccess().getGroupLinesAssignment_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1043:1: ( rule__Group__GroupLinesAssignment_1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1043:2: rule__Group__GroupLinesAssignment_1
            {
            pushFollow(FOLLOW_rule__Group__GroupLinesAssignment_1_in_rule__Group__Group__1__Impl2086);
            rule__Group__GroupLinesAssignment_1();

            state._fsp--;


            }

             after(grammarAccess.getGroupAccess().getGroupLinesAssignment_1()); 

            }

            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1046:1: ( ( rule__Group__GroupLinesAssignment_1 )* )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1047:1: ( rule__Group__GroupLinesAssignment_1 )*
            {
             before(grammarAccess.getGroupAccess().getGroupLinesAssignment_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1048:1: ( rule__Group__GroupLinesAssignment_1 )*
            loop12:
            do {
                int alt12=2;
                int LA12_0 = input.LA(1);

                if ( (LA12_0==RULE_ID||LA12_0==19||LA12_0==21||LA12_0==23) ) {
                    alt12=1;
                }


                switch (alt12) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1048:2: rule__Group__GroupLinesAssignment_1
            	    {
            	    pushFollow(FOLLOW_rule__Group__GroupLinesAssignment_1_in_rule__Group__Group__1__Impl2098);
            	    rule__Group__GroupLinesAssignment_1();

            	    state._fsp--;


            	    }
            	    break;

            	default :
            	    break loop12;
                }
            } while (true);

             after(grammarAccess.getGroupAccess().getGroupLinesAssignment_1()); 

            }


            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__1__Impl"


    // $ANTLR start "rule__Group__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1059:1: rule__Group__Group__2 : rule__Group__Group__2__Impl rule__Group__Group__3 ;
    public final void rule__Group__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1063:1: ( rule__Group__Group__2__Impl rule__Group__Group__3 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1064:2: rule__Group__Group__2__Impl rule__Group__Group__3
            {
            pushFollow(FOLLOW_rule__Group__Group__2__Impl_in_rule__Group__Group__22131);
            rule__Group__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Group__Group__3_in_rule__Group__Group__22134);
            rule__Group__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__2"


    // $ANTLR start "rule__Group__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1071:1: rule__Group__Group__2__Impl : ( '}' ) ;
    public final void rule__Group__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1075:1: ( ( '}' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1076:1: ( '}' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1076:1: ( '}' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1077:1: '}'
            {
             before(grammarAccess.getGroupAccess().getRightCurlyBracketKeyword_2()); 
            match(input,20,FOLLOW_20_in_rule__Group__Group__2__Impl2162); 
             after(grammarAccess.getGroupAccess().getRightCurlyBracketKeyword_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__2__Impl"


    // $ANTLR start "rule__Group__Group__3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1090:1: rule__Group__Group__3 : rule__Group__Group__3__Impl ;
    public final void rule__Group__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1094:1: ( rule__Group__Group__3__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1095:2: rule__Group__Group__3__Impl
            {
            pushFollow(FOLLOW_rule__Group__Group__3__Impl_in_rule__Group__Group__32193);
            rule__Group__Group__3__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__3"


    // $ANTLR start "rule__Group__Group__3__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1101:1: rule__Group__Group__3__Impl : ( RULE_EOS ) ;
    public final void rule__Group__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1105:1: ( ( RULE_EOS ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1106:1: ( RULE_EOS )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1106:1: ( RULE_EOS )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1107:1: RULE_EOS
            {
             before(grammarAccess.getGroupAccess().getEOSTerminalRuleCall_3()); 
            match(input,RULE_EOS,FOLLOW_RULE_EOS_in_rule__Group__Group__3__Impl2220); 
             after(grammarAccess.getGroupAccess().getEOSTerminalRuleCall_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__Group__3__Impl"


    // $ANTLR start "rule__Triple__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1126:1: rule__Triple__Group__0 : rule__Triple__Group__0__Impl rule__Triple__Group__1 ;
    public final void rule__Triple__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1130:1: ( rule__Triple__Group__0__Impl rule__Triple__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1131:2: rule__Triple__Group__0__Impl rule__Triple__Group__1
            {
            pushFollow(FOLLOW_rule__Triple__Group__0__Impl_in_rule__Triple__Group__02257);
            rule__Triple__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Triple__Group__1_in_rule__Triple__Group__02260);
            rule__Triple__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__0"


    // $ANTLR start "rule__Triple__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1138:1: rule__Triple__Group__0__Impl : ( ( rule__Triple__SubjAssignment_0 ) ) ;
    public final void rule__Triple__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1142:1: ( ( ( rule__Triple__SubjAssignment_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1143:1: ( ( rule__Triple__SubjAssignment_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1143:1: ( ( rule__Triple__SubjAssignment_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1144:1: ( rule__Triple__SubjAssignment_0 )
            {
             before(grammarAccess.getTripleAccess().getSubjAssignment_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1145:1: ( rule__Triple__SubjAssignment_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1145:2: rule__Triple__SubjAssignment_0
            {
            pushFollow(FOLLOW_rule__Triple__SubjAssignment_0_in_rule__Triple__Group__0__Impl2287);
            rule__Triple__SubjAssignment_0();

            state._fsp--;


            }

             after(grammarAccess.getTripleAccess().getSubjAssignment_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__0__Impl"


    // $ANTLR start "rule__Triple__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1155:1: rule__Triple__Group__1 : rule__Triple__Group__1__Impl rule__Triple__Group__2 ;
    public final void rule__Triple__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1159:1: ( rule__Triple__Group__1__Impl rule__Triple__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1160:2: rule__Triple__Group__1__Impl rule__Triple__Group__2
            {
            pushFollow(FOLLOW_rule__Triple__Group__1__Impl_in_rule__Triple__Group__12317);
            rule__Triple__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Triple__Group__2_in_rule__Triple__Group__12320);
            rule__Triple__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__1"


    // $ANTLR start "rule__Triple__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1167:1: rule__Triple__Group__1__Impl : ( ( 'has' )? ) ;
    public final void rule__Triple__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1171:1: ( ( ( 'has' )? ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1172:1: ( ( 'has' )? )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1172:1: ( ( 'has' )? )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1173:1: ( 'has' )?
            {
             before(grammarAccess.getTripleAccess().getHasKeyword_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1174:1: ( 'has' )?
            int alt13=2;
            int LA13_0 = input.LA(1);

            if ( (LA13_0==21) ) {
                alt13=1;
            }
            switch (alt13) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1175:2: 'has'
                    {
                    match(input,21,FOLLOW_21_in_rule__Triple__Group__1__Impl2349); 

                    }
                    break;

            }

             after(grammarAccess.getTripleAccess().getHasKeyword_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__1__Impl"


    // $ANTLR start "rule__Triple__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1186:1: rule__Triple__Group__2 : rule__Triple__Group__2__Impl rule__Triple__Group__3 ;
    public final void rule__Triple__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1190:1: ( rule__Triple__Group__2__Impl rule__Triple__Group__3 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1191:2: rule__Triple__Group__2__Impl rule__Triple__Group__3
            {
            pushFollow(FOLLOW_rule__Triple__Group__2__Impl_in_rule__Triple__Group__22382);
            rule__Triple__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Triple__Group__3_in_rule__Triple__Group__22385);
            rule__Triple__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__2"


    // $ANTLR start "rule__Triple__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1198:1: rule__Triple__Group__2__Impl : ( ( rule__Triple__PredAssignment_2 ) ) ;
    public final void rule__Triple__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1202:1: ( ( ( rule__Triple__PredAssignment_2 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1203:1: ( ( rule__Triple__PredAssignment_2 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1203:1: ( ( rule__Triple__PredAssignment_2 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1204:1: ( rule__Triple__PredAssignment_2 )
            {
             before(grammarAccess.getTripleAccess().getPredAssignment_2()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1205:1: ( rule__Triple__PredAssignment_2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1205:2: rule__Triple__PredAssignment_2
            {
            pushFollow(FOLLOW_rule__Triple__PredAssignment_2_in_rule__Triple__Group__2__Impl2412);
            rule__Triple__PredAssignment_2();

            state._fsp--;


            }

             after(grammarAccess.getTripleAccess().getPredAssignment_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__2__Impl"


    // $ANTLR start "rule__Triple__Group__3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1215:1: rule__Triple__Group__3 : rule__Triple__Group__3__Impl ;
    public final void rule__Triple__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1219:1: ( rule__Triple__Group__3__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1220:2: rule__Triple__Group__3__Impl
            {
            pushFollow(FOLLOW_rule__Triple__Group__3__Impl_in_rule__Triple__Group__32442);
            rule__Triple__Group__3__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__3"


    // $ANTLR start "rule__Triple__Group__3__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1226:1: rule__Triple__Group__3__Impl : ( ( rule__Triple__ObjvalAssignment_3 ) ) ;
    public final void rule__Triple__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1230:1: ( ( ( rule__Triple__ObjvalAssignment_3 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1231:1: ( ( rule__Triple__ObjvalAssignment_3 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1231:1: ( ( rule__Triple__ObjvalAssignment_3 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1232:1: ( rule__Triple__ObjvalAssignment_3 )
            {
             before(grammarAccess.getTripleAccess().getObjvalAssignment_3()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1233:1: ( rule__Triple__ObjvalAssignment_3 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1233:2: rule__Triple__ObjvalAssignment_3
            {
            pushFollow(FOLLOW_rule__Triple__ObjvalAssignment_3_in_rule__Triple__Group__3__Impl2469);
            rule__Triple__ObjvalAssignment_3();

            state._fsp--;


            }

             after(grammarAccess.getTripleAccess().getObjvalAssignment_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__Group__3__Impl"


    // $ANTLR start "rule__NUMBER__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1251:1: rule__NUMBER__Group__0 : rule__NUMBER__Group__0__Impl rule__NUMBER__Group__1 ;
    public final void rule__NUMBER__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1255:1: ( rule__NUMBER__Group__0__Impl rule__NUMBER__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1256:2: rule__NUMBER__Group__0__Impl rule__NUMBER__Group__1
            {
            pushFollow(FOLLOW_rule__NUMBER__Group__0__Impl_in_rule__NUMBER__Group__02507);
            rule__NUMBER__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__NUMBER__Group__1_in_rule__NUMBER__Group__02510);
            rule__NUMBER__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NUMBER__Group__0"


    // $ANTLR start "rule__NUMBER__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1263:1: rule__NUMBER__Group__0__Impl : ( ( '-' )? ) ;
    public final void rule__NUMBER__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1267:1: ( ( ( '-' )? ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1268:1: ( ( '-' )? )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1268:1: ( ( '-' )? )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1269:1: ( '-' )?
            {
             before(grammarAccess.getNUMBERAccess().getHyphenMinusKeyword_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1270:1: ( '-' )?
            int alt14=2;
            int LA14_0 = input.LA(1);

            if ( (LA14_0==22) ) {
                alt14=1;
            }
            switch (alt14) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1271:2: '-'
                    {
                    match(input,22,FOLLOW_22_in_rule__NUMBER__Group__0__Impl2539); 

                    }
                    break;

            }

             after(grammarAccess.getNUMBERAccess().getHyphenMinusKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NUMBER__Group__0__Impl"


    // $ANTLR start "rule__NUMBER__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1282:1: rule__NUMBER__Group__1 : rule__NUMBER__Group__1__Impl ;
    public final void rule__NUMBER__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1286:1: ( rule__NUMBER__Group__1__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1287:2: rule__NUMBER__Group__1__Impl
            {
            pushFollow(FOLLOW_rule__NUMBER__Group__1__Impl_in_rule__NUMBER__Group__12572);
            rule__NUMBER__Group__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NUMBER__Group__1"


    // $ANTLR start "rule__NUMBER__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1293:1: rule__NUMBER__Group__1__Impl : ( RULE_UNSIGNED_NUMBER ) ;
    public final void rule__NUMBER__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1297:1: ( ( RULE_UNSIGNED_NUMBER ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1298:1: ( RULE_UNSIGNED_NUMBER )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1298:1: ( RULE_UNSIGNED_NUMBER )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1299:1: RULE_UNSIGNED_NUMBER
            {
             before(grammarAccess.getNUMBERAccess().getUNSIGNED_NUMBERTerminalRuleCall_1()); 
            match(input,RULE_UNSIGNED_NUMBER,FOLLOW_RULE_UNSIGNED_NUMBER_in_rule__NUMBER__Group__1__Impl2599); 
             after(grammarAccess.getNUMBERAccess().getUNSIGNED_NUMBERTerminalRuleCall_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NUMBER__Group__1__Impl"


    // $ANTLR start "rule__Ref__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1314:1: rule__Ref__Group__0 : rule__Ref__Group__0__Impl rule__Ref__Group__1 ;
    public final void rule__Ref__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1318:1: ( rule__Ref__Group__0__Impl rule__Ref__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1319:2: rule__Ref__Group__0__Impl rule__Ref__Group__1
            {
            pushFollow(FOLLOW_rule__Ref__Group__0__Impl_in_rule__Ref__Group__02632);
            rule__Ref__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Ref__Group__1_in_rule__Ref__Group__02635);
            rule__Ref__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__0"


    // $ANTLR start "rule__Ref__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1326:1: rule__Ref__Group__0__Impl : ( ( rule__Ref__RefAssignment_0 ) ) ;
    public final void rule__Ref__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1330:1: ( ( ( rule__Ref__RefAssignment_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1331:1: ( ( rule__Ref__RefAssignment_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1331:1: ( ( rule__Ref__RefAssignment_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1332:1: ( rule__Ref__RefAssignment_0 )
            {
             before(grammarAccess.getRefAccess().getRefAssignment_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1333:1: ( rule__Ref__RefAssignment_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1333:2: rule__Ref__RefAssignment_0
            {
            pushFollow(FOLLOW_rule__Ref__RefAssignment_0_in_rule__Ref__Group__0__Impl2662);
            rule__Ref__RefAssignment_0();

            state._fsp--;


            }

             after(grammarAccess.getRefAccess().getRefAssignment_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__0__Impl"


    // $ANTLR start "rule__Ref__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1343:1: rule__Ref__Group__1 : rule__Ref__Group__1__Impl rule__Ref__Group__2 ;
    public final void rule__Ref__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1347:1: ( rule__Ref__Group__1__Impl rule__Ref__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1348:2: rule__Ref__Group__1__Impl rule__Ref__Group__2
            {
            pushFollow(FOLLOW_rule__Ref__Group__1__Impl_in_rule__Ref__Group__12692);
            rule__Ref__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Ref__Group__2_in_rule__Ref__Group__12695);
            rule__Ref__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__1"


    // $ANTLR start "rule__Ref__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1355:1: rule__Ref__Group__1__Impl : ( ( rule__Ref__AddlcolsAssignment_1 )? ) ;
    public final void rule__Ref__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1359:1: ( ( ( rule__Ref__AddlcolsAssignment_1 )? ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1360:1: ( ( rule__Ref__AddlcolsAssignment_1 )? )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1360:1: ( ( rule__Ref__AddlcolsAssignment_1 )? )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1361:1: ( rule__Ref__AddlcolsAssignment_1 )?
            {
             before(grammarAccess.getRefAccess().getAddlcolsAssignment_1()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1362:1: ( rule__Ref__AddlcolsAssignment_1 )?
            int alt15=2;
            int LA15_0 = input.LA(1);

            if ( (LA15_0==25) ) {
                alt15=1;
            }
            switch (alt15) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1362:2: rule__Ref__AddlcolsAssignment_1
                    {
                    pushFollow(FOLLOW_rule__Ref__AddlcolsAssignment_1_in_rule__Ref__Group__1__Impl2722);
                    rule__Ref__AddlcolsAssignment_1();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getRefAccess().getAddlcolsAssignment_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__1__Impl"


    // $ANTLR start "rule__Ref__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1372:1: rule__Ref__Group__2 : rule__Ref__Group__2__Impl rule__Ref__Group__3 ;
    public final void rule__Ref__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1376:1: ( rule__Ref__Group__2__Impl rule__Ref__Group__3 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1377:2: rule__Ref__Group__2__Impl rule__Ref__Group__3
            {
            pushFollow(FOLLOW_rule__Ref__Group__2__Impl_in_rule__Ref__Group__22753);
            rule__Ref__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__Ref__Group__3_in_rule__Ref__Group__22756);
            rule__Ref__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__2"


    // $ANTLR start "rule__Ref__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1384:1: rule__Ref__Group__2__Impl : ( ( ruleColumnName )* ) ;
    public final void rule__Ref__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1388:1: ( ( ( ruleColumnName )* ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1389:1: ( ( ruleColumnName )* )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1389:1: ( ( ruleColumnName )* )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1390:1: ( ruleColumnName )*
            {
             before(grammarAccess.getRefAccess().getColumnNameParserRuleCall_2()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1391:1: ( ruleColumnName )*
            loop16:
            do {
                int alt16=2;
                int LA16_0 = input.LA(1);

                if ( (LA16_0==23) ) {
                    int LA16_2 = input.LA(2);

                    if ( (LA16_2==RULE_ID) ) {
                        int LA16_3 = input.LA(3);

                        if ( (LA16_3==24) ) {
                            alt16=1;
                        }


                    }
                    else if ( (LA16_2==RULE_DIGITS) ) {
                        int LA16_4 = input.LA(3);

                        if ( (LA16_4==24) ) {
                            alt16=1;
                        }


                    }


                }


                switch (alt16) {
            	case 1 :
            	    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1391:3: ruleColumnName
            	    {
            	    pushFollow(FOLLOW_ruleColumnName_in_rule__Ref__Group__2__Impl2784);
            	    ruleColumnName();

            	    state._fsp--;


            	    }
            	    break;

            	default :
            	    break loop16;
                }
            } while (true);

             after(grammarAccess.getRefAccess().getColumnNameParserRuleCall_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__2__Impl"


    // $ANTLR start "rule__Ref__Group__3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1401:1: rule__Ref__Group__3 : rule__Ref__Group__3__Impl ;
    public final void rule__Ref__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1405:1: ( rule__Ref__Group__3__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1406:2: rule__Ref__Group__3__Impl
            {
            pushFollow(FOLLOW_rule__Ref__Group__3__Impl_in_rule__Ref__Group__32815);
            rule__Ref__Group__3__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__3"


    // $ANTLR start "rule__Ref__Group__3__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1412:1: rule__Ref__Group__3__Impl : ( ( rule__Ref__RowAssignment_3 )? ) ;
    public final void rule__Ref__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1416:1: ( ( ( rule__Ref__RowAssignment_3 )? ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1417:1: ( ( rule__Ref__RowAssignment_3 )? )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1417:1: ( ( rule__Ref__RowAssignment_3 )? )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1418:1: ( rule__Ref__RowAssignment_3 )?
            {
             before(grammarAccess.getRefAccess().getRowAssignment_3()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1419:1: ( rule__Ref__RowAssignment_3 )?
            int alt17=2;
            int LA17_0 = input.LA(1);

            if ( (LA17_0==26) ) {
                alt17=1;
            }
            switch (alt17) {
                case 1 :
                    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1419:2: rule__Ref__RowAssignment_3
                    {
                    pushFollow(FOLLOW_rule__Ref__RowAssignment_3_in_rule__Ref__Group__3__Impl2842);
                    rule__Ref__RowAssignment_3();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getRefAccess().getRowAssignment_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__Group__3__Impl"


    // $ANTLR start "rule__ColumnName__Group__0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1437:1: rule__ColumnName__Group__0 : rule__ColumnName__Group__0__Impl rule__ColumnName__Group__1 ;
    public final void rule__ColumnName__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1441:1: ( rule__ColumnName__Group__0__Impl rule__ColumnName__Group__1 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1442:2: rule__ColumnName__Group__0__Impl rule__ColumnName__Group__1
            {
            pushFollow(FOLLOW_rule__ColumnName__Group__0__Impl_in_rule__ColumnName__Group__02881);
            rule__ColumnName__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__ColumnName__Group__1_in_rule__ColumnName__Group__02884);
            rule__ColumnName__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnName__Group__0"


    // $ANTLR start "rule__ColumnName__Group__0__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1449:1: rule__ColumnName__Group__0__Impl : ( '<' ) ;
    public final void rule__ColumnName__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1453:1: ( ( '<' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1454:1: ( '<' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1454:1: ( '<' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1455:1: '<'
            {
             before(grammarAccess.getColumnNameAccess().getLessThanSignKeyword_0()); 
            match(input,23,FOLLOW_23_in_rule__ColumnName__Group__0__Impl2912); 
             after(grammarAccess.getColumnNameAccess().getLessThanSignKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnName__Group__0__Impl"


    // $ANTLR start "rule__ColumnName__Group__1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1468:1: rule__ColumnName__Group__1 : rule__ColumnName__Group__1__Impl rule__ColumnName__Group__2 ;
    public final void rule__ColumnName__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1472:1: ( rule__ColumnName__Group__1__Impl rule__ColumnName__Group__2 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1473:2: rule__ColumnName__Group__1__Impl rule__ColumnName__Group__2
            {
            pushFollow(FOLLOW_rule__ColumnName__Group__1__Impl_in_rule__ColumnName__Group__12943);
            rule__ColumnName__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_rule__ColumnName__Group__2_in_rule__ColumnName__Group__12946);
            rule__ColumnName__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnName__Group__1"


    // $ANTLR start "rule__ColumnName__Group__1__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1480:1: rule__ColumnName__Group__1__Impl : ( ruleColumnID ) ;
    public final void rule__ColumnName__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1484:1: ( ( ruleColumnID ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1485:1: ( ruleColumnID )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1485:1: ( ruleColumnID )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1486:1: ruleColumnID
            {
             before(grammarAccess.getColumnNameAccess().getColumnIDParserRuleCall_1()); 
            pushFollow(FOLLOW_ruleColumnID_in_rule__ColumnName__Group__1__Impl2973);
            ruleColumnID();

            state._fsp--;

             after(grammarAccess.getColumnNameAccess().getColumnIDParserRuleCall_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnName__Group__1__Impl"


    // $ANTLR start "rule__ColumnName__Group__2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1497:1: rule__ColumnName__Group__2 : rule__ColumnName__Group__2__Impl ;
    public final void rule__ColumnName__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1501:1: ( rule__ColumnName__Group__2__Impl )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1502:2: rule__ColumnName__Group__2__Impl
            {
            pushFollow(FOLLOW_rule__ColumnName__Group__2__Impl_in_rule__ColumnName__Group__23002);
            rule__ColumnName__Group__2__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnName__Group__2"


    // $ANTLR start "rule__ColumnName__Group__2__Impl"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1508:1: rule__ColumnName__Group__2__Impl : ( '>' ) ;
    public final void rule__ColumnName__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1512:1: ( ( '>' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1513:1: ( '>' )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1513:1: ( '>' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1514:1: '>'
            {
             before(grammarAccess.getColumnNameAccess().getGreaterThanSignKeyword_2()); 
            match(input,24,FOLLOW_24_in_rule__ColumnName__Group__2__Impl3030); 
             after(grammarAccess.getColumnNameAccess().getGreaterThanSignKeyword_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__ColumnName__Group__2__Impl"


    // $ANTLR start "rule__Model__UriAssignment_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1534:1: rule__Model__UriAssignment_0 : ( ruleNewModelNS ) ;
    public final void rule__Model__UriAssignment_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1538:1: ( ( ruleNewModelNS ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1539:1: ( ruleNewModelNS )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1539:1: ( ruleNewModelNS )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1540:1: ruleNewModelNS
            {
             before(grammarAccess.getModelAccess().getUriNewModelNSParserRuleCall_0_0()); 
            pushFollow(FOLLOW_ruleNewModelNS_in_rule__Model__UriAssignment_03072);
            ruleNewModelNS();

            state._fsp--;

             after(grammarAccess.getModelAccess().getUriNewModelNSParserRuleCall_0_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__UriAssignment_0"


    // $ANTLR start "rule__Model__ImportsAssignment_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1549:1: rule__Model__ImportsAssignment_1 : ( ruleImport ) ;
    public final void rule__Model__ImportsAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1553:1: ( ( ruleImport ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1554:1: ( ruleImport )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1554:1: ( ruleImport )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1555:1: ruleImport
            {
             before(grammarAccess.getModelAccess().getImportsImportParserRuleCall_1_0()); 
            pushFollow(FOLLOW_ruleImport_in_rule__Model__ImportsAssignment_13103);
            ruleImport();

            state._fsp--;

             after(grammarAccess.getModelAccess().getImportsImportParserRuleCall_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__ImportsAssignment_1"


    // $ANTLR start "rule__Model__TriplesAssignment_2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1564:1: rule__Model__TriplesAssignment_2 : ( ( rule__Model__TriplesAlternatives_2_0 ) ) ;
    public final void rule__Model__TriplesAssignment_2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1568:1: ( ( ( rule__Model__TriplesAlternatives_2_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1569:1: ( ( rule__Model__TriplesAlternatives_2_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1569:1: ( ( rule__Model__TriplesAlternatives_2_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1570:1: ( rule__Model__TriplesAlternatives_2_0 )
            {
             before(grammarAccess.getModelAccess().getTriplesAlternatives_2_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1571:1: ( rule__Model__TriplesAlternatives_2_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1571:2: rule__Model__TriplesAlternatives_2_0
            {
            pushFollow(FOLLOW_rule__Model__TriplesAlternatives_2_0_in_rule__Model__TriplesAssignment_23134);
            rule__Model__TriplesAlternatives_2_0();

            state._fsp--;


            }

             after(grammarAccess.getModelAccess().getTriplesAlternatives_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Model__TriplesAssignment_2"


    // $ANTLR start "rule__NewModelNS__BaseUriAssignment_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1580:1: rule__NewModelNS__BaseUriAssignment_1 : ( RULE_STRING ) ;
    public final void rule__NewModelNS__BaseUriAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1584:1: ( ( RULE_STRING ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1585:1: ( RULE_STRING )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1585:1: ( RULE_STRING )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1586:1: RULE_STRING
            {
             before(grammarAccess.getNewModelNSAccess().getBaseUriSTRINGTerminalRuleCall_1_0()); 
            match(input,RULE_STRING,FOLLOW_RULE_STRING_in_rule__NewModelNS__BaseUriAssignment_13167); 
             after(grammarAccess.getNewModelNSAccess().getBaseUriSTRINGTerminalRuleCall_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__BaseUriAssignment_1"


    // $ANTLR start "rule__NewModelNS__PrefixAssignment_2_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1595:1: rule__NewModelNS__PrefixAssignment_2_1 : ( RULE_ID ) ;
    public final void rule__NewModelNS__PrefixAssignment_2_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1599:1: ( ( RULE_ID ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1600:1: ( RULE_ID )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1600:1: ( RULE_ID )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1601:1: RULE_ID
            {
             before(grammarAccess.getNewModelNSAccess().getPrefixIDTerminalRuleCall_2_1_0()); 
            match(input,RULE_ID,FOLLOW_RULE_ID_in_rule__NewModelNS__PrefixAssignment_2_13198); 
             after(grammarAccess.getNewModelNSAccess().getPrefixIDTerminalRuleCall_2_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__NewModelNS__PrefixAssignment_2_1"


    // $ANTLR start "rule__Import__ImportURIAssignment_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1610:1: rule__Import__ImportURIAssignment_1 : ( RULE_STRING ) ;
    public final void rule__Import__ImportURIAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1614:1: ( ( RULE_STRING ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1615:1: ( RULE_STRING )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1615:1: ( RULE_STRING )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1616:1: RULE_STRING
            {
             before(grammarAccess.getImportAccess().getImportURISTRINGTerminalRuleCall_1_0()); 
            match(input,RULE_STRING,FOLLOW_RULE_STRING_in_rule__Import__ImportURIAssignment_13229); 
             after(grammarAccess.getImportAccess().getImportURISTRINGTerminalRuleCall_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__ImportURIAssignment_1"


    // $ANTLR start "rule__Import__AliasAssignment_2_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1625:1: rule__Import__AliasAssignment_2_1 : ( RULE_ID ) ;
    public final void rule__Import__AliasAssignment_2_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1629:1: ( ( RULE_ID ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1630:1: ( RULE_ID )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1630:1: ( RULE_ID )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1631:1: RULE_ID
            {
             before(grammarAccess.getImportAccess().getAliasIDTerminalRuleCall_2_1_0()); 
            match(input,RULE_ID,FOLLOW_RULE_ID_in_rule__Import__AliasAssignment_2_13260); 
             after(grammarAccess.getImportAccess().getAliasIDTerminalRuleCall_2_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Import__AliasAssignment_2_1"


    // $ANTLR start "rule__Group__GroupLinesAssignment_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1640:1: rule__Group__GroupLinesAssignment_1 : ( ( rule__Group__GroupLinesAlternatives_1_0 ) ) ;
    public final void rule__Group__GroupLinesAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1644:1: ( ( ( rule__Group__GroupLinesAlternatives_1_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1645:1: ( ( rule__Group__GroupLinesAlternatives_1_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1645:1: ( ( rule__Group__GroupLinesAlternatives_1_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1646:1: ( rule__Group__GroupLinesAlternatives_1_0 )
            {
             before(grammarAccess.getGroupAccess().getGroupLinesAlternatives_1_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1647:1: ( rule__Group__GroupLinesAlternatives_1_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1647:2: rule__Group__GroupLinesAlternatives_1_0
            {
            pushFollow(FOLLOW_rule__Group__GroupLinesAlternatives_1_0_in_rule__Group__GroupLinesAssignment_13291);
            rule__Group__GroupLinesAlternatives_1_0();

            state._fsp--;


            }

             after(grammarAccess.getGroupAccess().getGroupLinesAlternatives_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Group__GroupLinesAssignment_1"


    // $ANTLR start "rule__Triple__SubjAssignment_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1656:1: rule__Triple__SubjAssignment_0 : ( ( rule__Triple__SubjAlternatives_0_0 ) ) ;
    public final void rule__Triple__SubjAssignment_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1660:1: ( ( ( rule__Triple__SubjAlternatives_0_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1661:1: ( ( rule__Triple__SubjAlternatives_0_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1661:1: ( ( rule__Triple__SubjAlternatives_0_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1662:1: ( rule__Triple__SubjAlternatives_0_0 )
            {
             before(grammarAccess.getTripleAccess().getSubjAlternatives_0_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1663:1: ( rule__Triple__SubjAlternatives_0_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1663:2: rule__Triple__SubjAlternatives_0_0
            {
            pushFollow(FOLLOW_rule__Triple__SubjAlternatives_0_0_in_rule__Triple__SubjAssignment_03324);
            rule__Triple__SubjAlternatives_0_0();

            state._fsp--;


            }

             after(grammarAccess.getTripleAccess().getSubjAlternatives_0_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__SubjAssignment_0"


    // $ANTLR start "rule__Triple__PredAssignment_2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1672:1: rule__Triple__PredAssignment_2 : ( ( RULE_ID ) ) ;
    public final void rule__Triple__PredAssignment_2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1676:1: ( ( ( RULE_ID ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1677:1: ( ( RULE_ID ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1677:1: ( ( RULE_ID ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1678:1: ( RULE_ID )
            {
             before(grammarAccess.getTripleAccess().getPredResourceNameCrossReference_2_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1679:1: ( RULE_ID )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1680:1: RULE_ID
            {
             before(grammarAccess.getTripleAccess().getPredResourceNameIDTerminalRuleCall_2_0_1()); 
            match(input,RULE_ID,FOLLOW_RULE_ID_in_rule__Triple__PredAssignment_23361); 
             after(grammarAccess.getTripleAccess().getPredResourceNameIDTerminalRuleCall_2_0_1()); 

            }

             after(grammarAccess.getTripleAccess().getPredResourceNameCrossReference_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__PredAssignment_2"


    // $ANTLR start "rule__Triple__ObjvalAssignment_3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1691:1: rule__Triple__ObjvalAssignment_3 : ( ( rule__Triple__ObjvalAlternatives_3_0 ) ) ;
    public final void rule__Triple__ObjvalAssignment_3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1695:1: ( ( ( rule__Triple__ObjvalAlternatives_3_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1696:1: ( ( rule__Triple__ObjvalAlternatives_3_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1696:1: ( ( rule__Triple__ObjvalAlternatives_3_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1697:1: ( rule__Triple__ObjvalAlternatives_3_0 )
            {
             before(grammarAccess.getTripleAccess().getObjvalAlternatives_3_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1698:1: ( rule__Triple__ObjvalAlternatives_3_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1698:2: rule__Triple__ObjvalAlternatives_3_0
            {
            pushFollow(FOLLOW_rule__Triple__ObjvalAlternatives_3_0_in_rule__Triple__ObjvalAssignment_33396);
            rule__Triple__ObjvalAlternatives_3_0();

            state._fsp--;


            }

             after(grammarAccess.getTripleAccess().getObjvalAlternatives_3_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Triple__ObjvalAssignment_3"


    // $ANTLR start "rule__LiteralValue__LiteralNumberAssignment_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1707:1: rule__LiteralValue__LiteralNumberAssignment_0 : ( ruleNUMBER ) ;
    public final void rule__LiteralValue__LiteralNumberAssignment_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1711:1: ( ( ruleNUMBER ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1712:1: ( ruleNUMBER )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1712:1: ( ruleNUMBER )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1713:1: ruleNUMBER
            {
             before(grammarAccess.getLiteralValueAccess().getLiteralNumberNUMBERParserRuleCall_0_0()); 
            pushFollow(FOLLOW_ruleNUMBER_in_rule__LiteralValue__LiteralNumberAssignment_03429);
            ruleNUMBER();

            state._fsp--;

             after(grammarAccess.getLiteralValueAccess().getLiteralNumberNUMBERParserRuleCall_0_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__LiteralValue__LiteralNumberAssignment_0"


    // $ANTLR start "rule__LiteralValue__LiteralStringAssignment_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1722:1: rule__LiteralValue__LiteralStringAssignment_1 : ( RULE_STRING ) ;
    public final void rule__LiteralValue__LiteralStringAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1726:1: ( ( RULE_STRING ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1727:1: ( RULE_STRING )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1727:1: ( RULE_STRING )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1728:1: RULE_STRING
            {
             before(grammarAccess.getLiteralValueAccess().getLiteralStringSTRINGTerminalRuleCall_1_0()); 
            match(input,RULE_STRING,FOLLOW_RULE_STRING_in_rule__LiteralValue__LiteralStringAssignment_13460); 
             after(grammarAccess.getLiteralValueAccess().getLiteralStringSTRINGTerminalRuleCall_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__LiteralValue__LiteralStringAssignment_1"


    // $ANTLR start "rule__LiteralValue__LiteralBooleanAssignment_2"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1737:1: rule__LiteralValue__LiteralBooleanAssignment_2 : ( ( rule__LiteralValue__LiteralBooleanAlternatives_2_0 ) ) ;
    public final void rule__LiteralValue__LiteralBooleanAssignment_2() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1741:1: ( ( ( rule__LiteralValue__LiteralBooleanAlternatives_2_0 ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1742:1: ( ( rule__LiteralValue__LiteralBooleanAlternatives_2_0 ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1742:1: ( ( rule__LiteralValue__LiteralBooleanAlternatives_2_0 ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1743:1: ( rule__LiteralValue__LiteralBooleanAlternatives_2_0 )
            {
             before(grammarAccess.getLiteralValueAccess().getLiteralBooleanAlternatives_2_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1744:1: ( rule__LiteralValue__LiteralBooleanAlternatives_2_0 )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1744:2: rule__LiteralValue__LiteralBooleanAlternatives_2_0
            {
            pushFollow(FOLLOW_rule__LiteralValue__LiteralBooleanAlternatives_2_0_in_rule__LiteralValue__LiteralBooleanAssignment_23491);
            rule__LiteralValue__LiteralBooleanAlternatives_2_0();

            state._fsp--;


            }

             after(grammarAccess.getLiteralValueAccess().getLiteralBooleanAlternatives_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__LiteralValue__LiteralBooleanAssignment_2"


    // $ANTLR start "rule__Ref__RefAssignment_0"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1753:1: rule__Ref__RefAssignment_0 : ( ruleColumnName ) ;
    public final void rule__Ref__RefAssignment_0() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1757:1: ( ( ruleColumnName ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1758:1: ( ruleColumnName )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1758:1: ( ruleColumnName )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1759:1: ruleColumnName
            {
             before(grammarAccess.getRefAccess().getRefColumnNameParserRuleCall_0_0()); 
            pushFollow(FOLLOW_ruleColumnName_in_rule__Ref__RefAssignment_03524);
            ruleColumnName();

            state._fsp--;

             after(grammarAccess.getRefAccess().getRefColumnNameParserRuleCall_0_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__RefAssignment_0"


    // $ANTLR start "rule__Ref__AddlcolsAssignment_1"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1768:1: rule__Ref__AddlcolsAssignment_1 : ( ( '_' ) ) ;
    public final void rule__Ref__AddlcolsAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1772:1: ( ( ( '_' ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1773:1: ( ( '_' ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1773:1: ( ( '_' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1774:1: ( '_' )
            {
             before(grammarAccess.getRefAccess().getAddlcols_Keyword_1_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1775:1: ( '_' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1776:1: '_'
            {
             before(grammarAccess.getRefAccess().getAddlcols_Keyword_1_0()); 
            match(input,25,FOLLOW_25_in_rule__Ref__AddlcolsAssignment_13560); 
             after(grammarAccess.getRefAccess().getAddlcols_Keyword_1_0()); 

            }

             after(grammarAccess.getRefAccess().getAddlcols_Keyword_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__AddlcolsAssignment_1"


    // $ANTLR start "rule__Ref__RowAssignment_3"
    // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1791:1: rule__Ref__RowAssignment_3 : ( ( '()' ) ) ;
    public final void rule__Ref__RowAssignment_3() throws RecognitionException {

        		int stackSize = keepStackSize();
            
        try {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1795:1: ( ( ( '()' ) ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1796:1: ( ( '()' ) )
            {
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1796:1: ( ( '()' ) )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1797:1: ( '()' )
            {
             before(grammarAccess.getRefAccess().getRowLeftParenthesisRightParenthesisKeyword_3_0()); 
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1798:1: ( '()' )
            // ../com.ge.research.sadl.mapping.ui/src-gen/com/ge/research/sadl/ui/contentassist/antlr/internal/InternalMapping.g:1799:1: '()'
            {
             before(grammarAccess.getRefAccess().getRowLeftParenthesisRightParenthesisKeyword_3_0()); 
            match(input,26,FOLLOW_26_in_rule__Ref__RowAssignment_33604); 
             after(grammarAccess.getRefAccess().getRowLeftParenthesisRightParenthesisKeyword_3_0()); 

            }

             after(grammarAccess.getRefAccess().getRowLeftParenthesisRightParenthesisKeyword_3_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Ref__RowAssignment_3"

    // Delegated rules


 

    public static final BitSet FOLLOW_ruleModel_in_entryRuleModel61 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleModel68 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__Group__0_in_ruleModel94 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNewModelNS_in_entryRuleNewModelNS121 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleNewModelNS128 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__0_in_ruleNewModelNS154 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleImport_in_entryRuleImport181 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleImport188 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group__0_in_ruleImport214 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleGroup_in_entryRuleGroup241 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleGroup248 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Group__Group__0_in_ruleGroup274 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleTriple_in_entryRuleTriple301 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleTriple308 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__Group__0_in_ruleTriple334 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleLiteralValue_in_entryRuleLiteralValue361 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleLiteralValue368 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__LiteralValue__Alternatives_in_ruleLiteralValue394 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNUMBER_in_entryRuleNUMBER426 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleNUMBER433 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NUMBER__Group__0_in_ruleNUMBER463 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleRef_in_entryRuleRef490 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleRef497 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__Group__0_in_ruleRef523 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnName_in_entryRuleColumnName550 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleColumnName557 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__ColumnName__Group__0_in_ruleColumnName583 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnID_in_entryRuleColumnID610 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_entryRuleColumnID617 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__ColumnID__Alternatives_in_ruleColumnID643 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleTriple_in_rule__Model__TriplesAlternatives_2_0679 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleGroup_in_rule__Model__TriplesAlternatives_2_0696 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleTriple_in_rule__Group__GroupLinesAlternatives_1_0728 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleGroup_in_rule__Group__GroupLinesAlternatives_1_0745 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleRef_in_rule__Triple__SubjAlternatives_0_0777 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleRef_in_rule__Triple__ObjvalAlternatives_3_0828 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleLiteralValue_in_rule__Triple__ObjvalAlternatives_3_0864 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__LiteralValue__LiteralNumberAssignment_0_in_rule__LiteralValue__Alternatives896 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__LiteralValue__LiteralStringAssignment_1_in_rule__LiteralValue__Alternatives914 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__LiteralValue__LiteralBooleanAssignment_2_in_rule__LiteralValue__Alternatives932 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_13_in_rule__LiteralValue__LiteralBooleanAlternatives_2_0966 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_14_in_rule__LiteralValue__LiteralBooleanAlternatives_2_0986 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_ID_in_rule__ColumnID__Alternatives1020 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_DIGITS_in_rule__ColumnID__Alternatives1037 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__Group__0__Impl_in_rule__Model__Group__01067 = new BitSet(new long[]{0x00000000008A0000L});
    public static final BitSet FOLLOW_rule__Model__Group__1_in_rule__Model__Group__01070 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__UriAssignment_0_in_rule__Model__Group__0__Impl1097 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__Group__1__Impl_in_rule__Model__Group__11127 = new BitSet(new long[]{0x00000000008A0000L});
    public static final BitSet FOLLOW_rule__Model__Group__2_in_rule__Model__Group__11130 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__ImportsAssignment_1_in_rule__Model__Group__1__Impl1157 = new BitSet(new long[]{0x0000000000020002L});
    public static final BitSet FOLLOW_rule__Model__Group__2__Impl_in_rule__Model__Group__21188 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__TriplesAssignment_2_in_rule__Model__Group__2__Impl1215 = new BitSet(new long[]{0x0000000000880002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__0__Impl_in_rule__NewModelNS__Group__01252 = new BitSet(new long[]{0x0000000000000100L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__1_in_rule__NewModelNS__Group__01255 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_15_in_rule__NewModelNS__Group__0__Impl1283 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__1__Impl_in_rule__NewModelNS__Group__11314 = new BitSet(new long[]{0x0000000000010040L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__2_in_rule__NewModelNS__Group__11317 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__BaseUriAssignment_1_in_rule__NewModelNS__Group__1__Impl1344 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__2__Impl_in_rule__NewModelNS__Group__21374 = new BitSet(new long[]{0x0000000000010040L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__3_in_rule__NewModelNS__Group__21377 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group_2__0_in_rule__NewModelNS__Group__2__Impl1404 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group__3__Impl_in_rule__NewModelNS__Group__31435 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_EOS_in_rule__NewModelNS__Group__3__Impl1462 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group_2__0__Impl_in_rule__NewModelNS__Group_2__01499 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group_2__1_in_rule__NewModelNS__Group_2__01502 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_16_in_rule__NewModelNS__Group_2__0__Impl1530 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__Group_2__1__Impl_in_rule__NewModelNS__Group_2__11561 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NewModelNS__PrefixAssignment_2_1_in_rule__NewModelNS__Group_2__1__Impl1588 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group__0__Impl_in_rule__Import__Group__01622 = new BitSet(new long[]{0x0000000000000100L});
    public static final BitSet FOLLOW_rule__Import__Group__1_in_rule__Import__Group__01625 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_17_in_rule__Import__Group__0__Impl1653 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group__1__Impl_in_rule__Import__Group__11684 = new BitSet(new long[]{0x0000000000040040L});
    public static final BitSet FOLLOW_rule__Import__Group__2_in_rule__Import__Group__11687 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__ImportURIAssignment_1_in_rule__Import__Group__1__Impl1714 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group__2__Impl_in_rule__Import__Group__21744 = new BitSet(new long[]{0x0000000000040040L});
    public static final BitSet FOLLOW_rule__Import__Group__3_in_rule__Import__Group__21747 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group_2__0_in_rule__Import__Group__2__Impl1774 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group__3__Impl_in_rule__Import__Group__31805 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_EOS_in_rule__Import__Group__3__Impl1832 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group_2__0__Impl_in_rule__Import__Group_2__01869 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_rule__Import__Group_2__1_in_rule__Import__Group_2__01872 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_18_in_rule__Import__Group_2__0__Impl1900 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__Group_2__1__Impl_in_rule__Import__Group_2__11931 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Import__AliasAssignment_2_1_in_rule__Import__Group_2__1__Impl1958 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Group__Group__0__Impl_in_rule__Group__Group__01992 = new BitSet(new long[]{0x0000000000880000L});
    public static final BitSet FOLLOW_rule__Group__Group__1_in_rule__Group__Group__01995 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_19_in_rule__Group__Group__0__Impl2023 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Group__Group__1__Impl_in_rule__Group__Group__12054 = new BitSet(new long[]{0x0000000000100000L});
    public static final BitSet FOLLOW_rule__Group__Group__2_in_rule__Group__Group__12057 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Group__GroupLinesAssignment_1_in_rule__Group__Group__1__Impl2086 = new BitSet(new long[]{0x0000000000880002L});
    public static final BitSet FOLLOW_rule__Group__GroupLinesAssignment_1_in_rule__Group__Group__1__Impl2098 = new BitSet(new long[]{0x0000000000880002L});
    public static final BitSet FOLLOW_rule__Group__Group__2__Impl_in_rule__Group__Group__22131 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_rule__Group__Group__3_in_rule__Group__Group__22134 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_20_in_rule__Group__Group__2__Impl2162 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Group__Group__3__Impl_in_rule__Group__Group__32193 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_EOS_in_rule__Group__Group__3__Impl2220 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__Group__0__Impl_in_rule__Triple__Group__02257 = new BitSet(new long[]{0x0000000000200010L});
    public static final BitSet FOLLOW_rule__Triple__Group__1_in_rule__Triple__Group__02260 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__SubjAssignment_0_in_rule__Triple__Group__0__Impl2287 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__Group__1__Impl_in_rule__Triple__Group__12317 = new BitSet(new long[]{0x0000000000200010L});
    public static final BitSet FOLLOW_rule__Triple__Group__2_in_rule__Triple__Group__12320 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_21_in_rule__Triple__Group__1__Impl2349 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__Group__2__Impl_in_rule__Triple__Group__22382 = new BitSet(new long[]{0x0000000000C06180L});
    public static final BitSet FOLLOW_rule__Triple__Group__3_in_rule__Triple__Group__22385 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__PredAssignment_2_in_rule__Triple__Group__2__Impl2412 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__Group__3__Impl_in_rule__Triple__Group__32442 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__ObjvalAssignment_3_in_rule__Triple__Group__3__Impl2469 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NUMBER__Group__0__Impl_in_rule__NUMBER__Group__02507 = new BitSet(new long[]{0x0000000000400080L});
    public static final BitSet FOLLOW_rule__NUMBER__Group__1_in_rule__NUMBER__Group__02510 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_22_in_rule__NUMBER__Group__0__Impl2539 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__NUMBER__Group__1__Impl_in_rule__NUMBER__Group__12572 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_UNSIGNED_NUMBER_in_rule__NUMBER__Group__1__Impl2599 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__Group__0__Impl_in_rule__Ref__Group__02632 = new BitSet(new long[]{0x0000000006800000L});
    public static final BitSet FOLLOW_rule__Ref__Group__1_in_rule__Ref__Group__02635 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__RefAssignment_0_in_rule__Ref__Group__0__Impl2662 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__Group__1__Impl_in_rule__Ref__Group__12692 = new BitSet(new long[]{0x0000000006800000L});
    public static final BitSet FOLLOW_rule__Ref__Group__2_in_rule__Ref__Group__12695 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__AddlcolsAssignment_1_in_rule__Ref__Group__1__Impl2722 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__Group__2__Impl_in_rule__Ref__Group__22753 = new BitSet(new long[]{0x0000000006800000L});
    public static final BitSet FOLLOW_rule__Ref__Group__3_in_rule__Ref__Group__22756 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnName_in_rule__Ref__Group__2__Impl2784 = new BitSet(new long[]{0x0000000000800002L});
    public static final BitSet FOLLOW_rule__Ref__Group__3__Impl_in_rule__Ref__Group__32815 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Ref__RowAssignment_3_in_rule__Ref__Group__3__Impl2842 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__ColumnName__Group__0__Impl_in_rule__ColumnName__Group__02881 = new BitSet(new long[]{0x0000000000000030L});
    public static final BitSet FOLLOW_rule__ColumnName__Group__1_in_rule__ColumnName__Group__02884 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_23_in_rule__ColumnName__Group__0__Impl2912 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__ColumnName__Group__1__Impl_in_rule__ColumnName__Group__12943 = new BitSet(new long[]{0x0000000001000000L});
    public static final BitSet FOLLOW_rule__ColumnName__Group__2_in_rule__ColumnName__Group__12946 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnID_in_rule__ColumnName__Group__1__Impl2973 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__ColumnName__Group__2__Impl_in_rule__ColumnName__Group__23002 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_24_in_rule__ColumnName__Group__2__Impl3030 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNewModelNS_in_rule__Model__UriAssignment_03072 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleImport_in_rule__Model__ImportsAssignment_13103 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Model__TriplesAlternatives_2_0_in_rule__Model__TriplesAssignment_23134 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_STRING_in_rule__NewModelNS__BaseUriAssignment_13167 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_ID_in_rule__NewModelNS__PrefixAssignment_2_13198 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_STRING_in_rule__Import__ImportURIAssignment_13229 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_ID_in_rule__Import__AliasAssignment_2_13260 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Group__GroupLinesAlternatives_1_0_in_rule__Group__GroupLinesAssignment_13291 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__SubjAlternatives_0_0_in_rule__Triple__SubjAssignment_03324 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_ID_in_rule__Triple__PredAssignment_23361 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__Triple__ObjvalAlternatives_3_0_in_rule__Triple__ObjvalAssignment_33396 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleNUMBER_in_rule__LiteralValue__LiteralNumberAssignment_03429 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_RULE_STRING_in_rule__LiteralValue__LiteralStringAssignment_13460 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_rule__LiteralValue__LiteralBooleanAlternatives_2_0_in_rule__LiteralValue__LiteralBooleanAssignment_23491 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ruleColumnName_in_rule__Ref__RefAssignment_03524 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_25_in_rule__Ref__AddlcolsAssignment_13560 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_26_in_rule__Ref__RowAssignment_33604 = new BitSet(new long[]{0x0000000000000002L});

}