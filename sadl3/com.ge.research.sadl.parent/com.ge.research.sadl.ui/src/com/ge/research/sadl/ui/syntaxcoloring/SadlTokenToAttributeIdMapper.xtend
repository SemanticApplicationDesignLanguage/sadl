package com.ge.research.sadl.ui.syntaxcoloring

import org.eclipse.xtext.ide.editor.syntaxcoloring.DefaultAntlrTokenToAttributeIdMapper
import com.ge.research.sadl.ide.contentassist.antlr.internal.InternalSADLParser

class SadlTokenToAttributeIdMapper extends DefaultAntlrTokenToAttributeIdMapper {
	// Sadl.xtext defines only two new token terminals: NUMBER and EOS.
	// EOS is just punctuation, so we don't need another style for it.
	override protected String calculateId(String tokenName, int tokenType) {
		// We could hardcode the comparison to use "RULE_NUMBER" but
		// we want to ensure a compilation error if someone renames 
		// the NUMBER terminal without updating this place.
		var String ruleNumber = {
			val _rdIndx_tmpNode = InternalSADLParser.RULE_UNSIGNED_NUMBER
			InternalSADLParser.tokenNames.get(_rdIndx_tmpNode)
		}
		if (ruleNumber.equals(tokenName)) {
			return SadlHighlightingConfiguration.NUMBER_ID
		}
		return super.calculateId(tokenName, tokenType)
	}
}
