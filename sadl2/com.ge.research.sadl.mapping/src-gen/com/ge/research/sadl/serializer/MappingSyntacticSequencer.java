package com.ge.research.sadl.serializer;

import com.ge.research.sadl.services.MappingGrammarAccess;
import com.google.inject.Inject;
import java.util.List;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.IGrammarAccess;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.AbstractElementAlias;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.TokenAlias;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynNavigable;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynTransition;
import org.eclipse.xtext.serializer.sequencer.AbstractSyntacticSequencer;

@SuppressWarnings("all")
public class MappingSyntacticSequencer extends AbstractSyntacticSequencer {

	protected MappingGrammarAccess grammarAccess;
	protected AbstractElementAlias match_Ref_ColumnNameParserRuleCall_2_a;
	protected AbstractElementAlias match_Triple_HasKeyword_1_q;
	
	@Inject
	protected void init(IGrammarAccess access) {
		grammarAccess = (MappingGrammarAccess) access;
		match_Ref_ColumnNameParserRuleCall_2_a = new TokenAlias(true, true, grammarAccess.getRefAccess().getColumnNameParserRuleCall_2());
		match_Triple_HasKeyword_1_q = new TokenAlias(false, true, grammarAccess.getTripleAccess().getHasKeyword_1());
	}
	
	@Override
	protected String getUnassignedRuleCallToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if(ruleCall.getRule() == grammarAccess.getColumnNameRule())
			return getColumnNameToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getEOSRule())
			return getEOSToken(semanticObject, ruleCall, node);
		return "";
	}
	
	/**
	 * ColumnName:
	 * 	'<' ColumnID '>'
	 * ;
	 */
	protected String getColumnNameToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "<>";
	}
	
	/**
	 * terminal EOS :
	 *     '.' (' '|'\t'|'\r'|'\n'|EOF);
	 */
	protected String getEOSToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return ". ";
	}
	
	@Override
	protected void emitUnassignedTokens(EObject semanticObject, ISynTransition transition, INode fromNode, INode toNode) {
		if (transition.getAmbiguousSyntaxes().isEmpty()) return;
		List<INode> transitionNodes = collectNodes(fromNode, toNode);
		for (AbstractElementAlias syntax : transition.getAmbiguousSyntaxes()) {
			List<INode> syntaxNodes = getNodesFor(transitionNodes, syntax);
			if(match_Ref_ColumnNameParserRuleCall_2_a.equals(syntax))
				emit_Ref_ColumnNameParserRuleCall_2_a(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_Triple_HasKeyword_1_q.equals(syntax))
				emit_Triple_HasKeyword_1_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else acceptNodes(getLastNavigableState(), syntaxNodes);
		}
	}

	/**
	 * Syntax:
	 *     ColumnName*
	 */
	protected void emit_Ref_ColumnNameParserRuleCall_2_a(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'has'?
	 */
	protected void emit_Triple_HasKeyword_1_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
}
