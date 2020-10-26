package com.ge.research.sadl.ui.syntaxcoloring;

import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultAntlrTokenToAttributeIdMapper;

import com.ge.research.sadl.ui.contentassist.antlr.internal.InternalSadlParser;

public class SadlTokenToAttributeIdMapper extends DefaultAntlrTokenToAttributeIdMapper {

    // Sadl.xtext defines only two new token terminals: NUMBER and EOS.
    // EOS is just punctuation, so we don't need another style for it.
    @Override
    protected String calculateId(String tokenName, int tokenType) {

        // We could hardcode the comparison to use "RULE_NUMBER" but
        // we want to ensure a compilation error if someone renames 
        // the NUMBER terminal without updating this place.
        String ruleNumber = InternalSadlParser.tokenNames[InternalSadlParser.RULE_UNSIGNED_NUMBER];

        if (ruleNumber.equals(tokenName)) {
            return SadlHighlightingConfiguration.NUMBER_ID;
        }

        return super.calculateId(tokenName, tokenType);
    }

}
