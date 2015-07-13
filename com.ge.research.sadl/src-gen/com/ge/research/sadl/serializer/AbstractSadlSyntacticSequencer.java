package com.ge.research.sadl.serializer;

import com.ge.research.sadl.services.SadlGrammarAccess;
import com.google.inject.Inject;
import java.util.List;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.IGrammarAccess;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.AbstractElementAlias;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.AlternativeAlias;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.TokenAlias;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynNavigable;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynTransition;
import org.eclipse.xtext.serializer.sequencer.AbstractSyntacticSequencer;

@SuppressWarnings("all")
public abstract class AbstractSadlSyntacticSequencer extends AbstractSyntacticSequencer {

	protected SadlGrammarAccess grammarAccess;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_0_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_1_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_2_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_3_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_4_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_5_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_6_0_q;
	protected AbstractElementAlias match_AdditionalPropertyInfo_CommaKeyword_7_0_q;
	protected AbstractElementAlias match_AddlClassInfo_CommaKeyword_0_0_q;
	protected AbstractElementAlias match_AddlClassInfo_CommaKeyword_1_0_q;
	protected AbstractElementAlias match_CardCondition_HasKeyword_0_0_or_WithKeyword_0_1;
	protected AbstractElementAlias match_CardCondition_ValueKeyword_3_0_or_ValuesKeyword_3_1;
	protected AbstractElementAlias match_ClassDeclaration_TopLevelKeyword_0_3_q;
	protected AbstractElementAlias match_ClassDeclaration_TopLevelKeyword_1_2_q;
	protected AbstractElementAlias match_DataTypeRestriction_CommaKeyword_1_2_0_0_or_OrKeyword_1_2_0_1;
	protected AbstractElementAlias match_DataTypeRestriction___AKeyword_0_0_0_or_AnKeyword_0_0_1__q;
	protected AbstractElementAlias match_ElementSet_CommaKeyword_1_0_q;
	protected AbstractElementAlias match_EnumeratedInstances_CommaKeyword_0_q;
	protected AbstractElementAlias match_Facets_CommaKeyword_3_2_0_q;
	protected AbstractElementAlias match_HasValueCondition_HasKeyword_1_0_or_WithKeyword_1_1;
	protected AbstractElementAlias match_InstAttrSPV_CommaKeyword_4_0_q;
	protected AbstractElementAlias match_InstAttrSPV_HasKeyword_4_1_0_or_WithKeyword_4_1_1;
	protected AbstractElementAlias match_InstAttrSPV___HasKeyword_1_0_or_WithKeyword_1_1__q;
	protected AbstractElementAlias match_MaxCardCondition_HasKeyword_0_0_or_WithKeyword_0_1;
	protected AbstractElementAlias match_MaxCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1;
	protected AbstractElementAlias match_MinCardCondition_HasKeyword_0_0_or_WithKeyword_0_1;
	protected AbstractElementAlias match_MinCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1;
	protected AbstractElementAlias match_OrderList_CommaKeyword_1_0_q;
	protected AbstractElementAlias match_PropValPartialTriple_CommaKeyword_0_q;
	protected AbstractElementAlias match_PropValPartialTriple___HasKeyword_1_0_or_WithKeyword_1_1__q;
	protected AbstractElementAlias match_Range_HasKeyword_0_0_or_WithKeyword_0_1;
	protected AbstractElementAlias match_Rule_ColonKeyword_2_q;
	protected AbstractElementAlias match_Rule_GivenKeyword_3_0_q;
	protected AbstractElementAlias match_Rule_IfKeyword_4_0_q;
	protected AbstractElementAlias match_SomeValuesCondition_HasKeyword_0_0_or_WithKeyword_0_1;
	protected AbstractElementAlias match_ValueTable_CommaKeyword_4_0_q;
	protected AbstractElementAlias match_VariableList_CommaKeyword_1_0_q;
	protected AbstractElementAlias match_WithPhrase_CommaKeyword_0_0_q;
	protected AbstractElementAlias match_WithPhrase_CommaKeyword_1_0_q;
	protected AbstractElementAlias match_WithPhrase_CommaKeyword_2_0_q;
	protected AbstractElementAlias match_WithPhrase_HasKeyword_0_1_0_or_WithKeyword_0_1_1;
	protected AbstractElementAlias match_WithPhrase_HasKeyword_1_1_0_or_WithKeyword_1_1_1;
	protected AbstractElementAlias match_WithPhrase_HasKeyword_2_1_0_or_WithKeyword_2_1_1;
	
	@Inject
	protected void init(IGrammarAccess access) {
		grammarAccess = (SadlGrammarAccess) access;
		match_AdditionalPropertyInfo_CommaKeyword_0_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_0_0());
		match_AdditionalPropertyInfo_CommaKeyword_1_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_1_0());
		match_AdditionalPropertyInfo_CommaKeyword_2_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_2_0());
		match_AdditionalPropertyInfo_CommaKeyword_3_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_3_0());
		match_AdditionalPropertyInfo_CommaKeyword_4_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_4_0());
		match_AdditionalPropertyInfo_CommaKeyword_5_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_5_0());
		match_AdditionalPropertyInfo_CommaKeyword_6_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_6_0());
		match_AdditionalPropertyInfo_CommaKeyword_7_0_q = new TokenAlias(false, true, grammarAccess.getAdditionalPropertyInfoAccess().getCommaKeyword_7_0());
		match_AddlClassInfo_CommaKeyword_0_0_q = new TokenAlias(false, true, grammarAccess.getAddlClassInfoAccess().getCommaKeyword_0_0());
		match_AddlClassInfo_CommaKeyword_1_0_q = new TokenAlias(false, true, grammarAccess.getAddlClassInfoAccess().getCommaKeyword_1_0());
		match_CardCondition_HasKeyword_0_0_or_WithKeyword_0_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getCardConditionAccess().getHasKeyword_0_0()), new TokenAlias(false, false, grammarAccess.getCardConditionAccess().getWithKeyword_0_1()));
		match_CardCondition_ValueKeyword_3_0_or_ValuesKeyword_3_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getCardConditionAccess().getValueKeyword_3_0()), new TokenAlias(false, false, grammarAccess.getCardConditionAccess().getValuesKeyword_3_1()));
		match_ClassDeclaration_TopLevelKeyword_0_3_q = new TokenAlias(false, true, grammarAccess.getClassDeclarationAccess().getTopLevelKeyword_0_3());
		match_ClassDeclaration_TopLevelKeyword_1_2_q = new TokenAlias(false, true, grammarAccess.getClassDeclarationAccess().getTopLevelKeyword_1_2());
		match_DataTypeRestriction_CommaKeyword_1_2_0_0_or_OrKeyword_1_2_0_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getDataTypeRestrictionAccess().getCommaKeyword_1_2_0_0()), new TokenAlias(false, false, grammarAccess.getDataTypeRestrictionAccess().getOrKeyword_1_2_0_1()));
		match_DataTypeRestriction___AKeyword_0_0_0_or_AnKeyword_0_0_1__q = new AlternativeAlias(false, true, new TokenAlias(false, false, grammarAccess.getDataTypeRestrictionAccess().getAKeyword_0_0_0()), new TokenAlias(false, false, grammarAccess.getDataTypeRestrictionAccess().getAnKeyword_0_0_1()));
		match_ElementSet_CommaKeyword_1_0_q = new TokenAlias(false, true, grammarAccess.getElementSetAccess().getCommaKeyword_1_0());
		match_EnumeratedInstances_CommaKeyword_0_q = new TokenAlias(false, true, grammarAccess.getEnumeratedInstancesAccess().getCommaKeyword_0());
		match_Facets_CommaKeyword_3_2_0_q = new TokenAlias(false, true, grammarAccess.getFacetsAccess().getCommaKeyword_3_2_0());
		match_HasValueCondition_HasKeyword_1_0_or_WithKeyword_1_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getHasValueConditionAccess().getHasKeyword_1_0()), new TokenAlias(false, false, grammarAccess.getHasValueConditionAccess().getWithKeyword_1_1()));
		match_InstAttrSPV_CommaKeyword_4_0_q = new TokenAlias(false, true, grammarAccess.getInstAttrSPVAccess().getCommaKeyword_4_0());
		match_InstAttrSPV_HasKeyword_4_1_0_or_WithKeyword_4_1_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getInstAttrSPVAccess().getHasKeyword_4_1_0()), new TokenAlias(false, false, grammarAccess.getInstAttrSPVAccess().getWithKeyword_4_1_1()));
		match_InstAttrSPV___HasKeyword_1_0_or_WithKeyword_1_1__q = new AlternativeAlias(false, true, new TokenAlias(false, false, grammarAccess.getInstAttrSPVAccess().getHasKeyword_1_0()), new TokenAlias(false, false, grammarAccess.getInstAttrSPVAccess().getWithKeyword_1_1()));
		match_MaxCardCondition_HasKeyword_0_0_or_WithKeyword_0_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getMaxCardConditionAccess().getHasKeyword_0_0()), new TokenAlias(false, false, grammarAccess.getMaxCardConditionAccess().getWithKeyword_0_1()));
		match_MaxCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getMaxCardConditionAccess().getValueKeyword_4_0()), new TokenAlias(false, false, grammarAccess.getMaxCardConditionAccess().getValuesKeyword_4_1()));
		match_MinCardCondition_HasKeyword_0_0_or_WithKeyword_0_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getMinCardConditionAccess().getHasKeyword_0_0()), new TokenAlias(false, false, grammarAccess.getMinCardConditionAccess().getWithKeyword_0_1()));
		match_MinCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getMinCardConditionAccess().getValueKeyword_4_0()), new TokenAlias(false, false, grammarAccess.getMinCardConditionAccess().getValuesKeyword_4_1()));
		match_OrderList_CommaKeyword_1_0_q = new TokenAlias(false, true, grammarAccess.getOrderListAccess().getCommaKeyword_1_0());
		match_PropValPartialTriple_CommaKeyword_0_q = new TokenAlias(false, true, grammarAccess.getPropValPartialTripleAccess().getCommaKeyword_0());
		match_PropValPartialTriple___HasKeyword_1_0_or_WithKeyword_1_1__q = new AlternativeAlias(false, true, new TokenAlias(false, false, grammarAccess.getPropValPartialTripleAccess().getHasKeyword_1_0()), new TokenAlias(false, false, grammarAccess.getPropValPartialTripleAccess().getWithKeyword_1_1()));
		match_Range_HasKeyword_0_0_or_WithKeyword_0_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getRangeAccess().getHasKeyword_0_0()), new TokenAlias(false, false, grammarAccess.getRangeAccess().getWithKeyword_0_1()));
		match_Rule_ColonKeyword_2_q = new TokenAlias(false, true, grammarAccess.getRuleAccess().getColonKeyword_2());
		match_Rule_GivenKeyword_3_0_q = new TokenAlias(false, true, grammarAccess.getRuleAccess().getGivenKeyword_3_0());
		match_Rule_IfKeyword_4_0_q = new TokenAlias(false, true, grammarAccess.getRuleAccess().getIfKeyword_4_0());
		match_SomeValuesCondition_HasKeyword_0_0_or_WithKeyword_0_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getSomeValuesConditionAccess().getHasKeyword_0_0()), new TokenAlias(false, false, grammarAccess.getSomeValuesConditionAccess().getWithKeyword_0_1()));
		match_ValueTable_CommaKeyword_4_0_q = new TokenAlias(false, true, grammarAccess.getValueTableAccess().getCommaKeyword_4_0());
		match_VariableList_CommaKeyword_1_0_q = new TokenAlias(false, true, grammarAccess.getVariableListAccess().getCommaKeyword_1_0());
		match_WithPhrase_CommaKeyword_0_0_q = new TokenAlias(false, true, grammarAccess.getWithPhraseAccess().getCommaKeyword_0_0());
		match_WithPhrase_CommaKeyword_1_0_q = new TokenAlias(false, true, grammarAccess.getWithPhraseAccess().getCommaKeyword_1_0());
		match_WithPhrase_CommaKeyword_2_0_q = new TokenAlias(false, true, grammarAccess.getWithPhraseAccess().getCommaKeyword_2_0());
		match_WithPhrase_HasKeyword_0_1_0_or_WithKeyword_0_1_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getWithPhraseAccess().getHasKeyword_0_1_0()), new TokenAlias(false, false, grammarAccess.getWithPhraseAccess().getWithKeyword_0_1_1()));
		match_WithPhrase_HasKeyword_1_1_0_or_WithKeyword_1_1_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getWithPhraseAccess().getHasKeyword_1_1_0()), new TokenAlias(false, false, grammarAccess.getWithPhraseAccess().getWithKeyword_1_1_1()));
		match_WithPhrase_HasKeyword_2_1_0_or_WithKeyword_2_1_1 = new AlternativeAlias(false, false, new TokenAlias(false, false, grammarAccess.getWithPhraseAccess().getHasKeyword_2_1_0()), new TokenAlias(false, false, grammarAccess.getWithPhraseAccess().getWithKeyword_2_1_1()));
	}
	
	@Override
	protected String getUnassignedRuleCallToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if(ruleCall.getRule() == grammarAccess.getEOSRule())
			return getEOSToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getIsFunctionalRule())
			return getIsFunctionalToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getIsInverseFunctionalRule())
			return getIsInverseFunctionalToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getIsSymmetricalRule())
			return getIsSymmetricalToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getIsTransitiveRule())
			return getIsTransitiveToken(semanticObject, ruleCall, node);
		return "";
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
	
	/**
	 * IsFunctional :	'has' 'a' 'single' 'value';
	 */
	protected String getIsFunctionalToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "hasasinglevalue";
	}
	
	/**
	 * IsInverseFunctional :	'has' 'a' 'single' 'subject';
	 */
	protected String getIsInverseFunctionalToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "hasasinglesubject";
	}
	
	/**
	 * IsSymmetrical : 'is' 'symmetrical';
	 */
	protected String getIsSymmetricalToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "issymmetrical";
	}
	
	/**
	 * IsTransitive :	'is' 'transitive';
	 */
	protected String getIsTransitiveToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "istransitive";
	}
	
	@Override
	protected void emitUnassignedTokens(EObject semanticObject, ISynTransition transition, INode fromNode, INode toNode) {
		if (transition.getAmbiguousSyntaxes().isEmpty()) return;
		List<INode> transitionNodes = collectNodes(fromNode, toNode);
		for (AbstractElementAlias syntax : transition.getAmbiguousSyntaxes()) {
			List<INode> syntaxNodes = getNodesFor(transitionNodes, syntax);
			if(match_AdditionalPropertyInfo_CommaKeyword_0_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_0_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_1_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_1_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_2_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_2_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_3_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_3_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_4_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_4_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_5_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_5_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_6_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_6_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AdditionalPropertyInfo_CommaKeyword_7_0_q.equals(syntax))
				emit_AdditionalPropertyInfo_CommaKeyword_7_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AddlClassInfo_CommaKeyword_0_0_q.equals(syntax))
				emit_AddlClassInfo_CommaKeyword_0_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AddlClassInfo_CommaKeyword_1_0_q.equals(syntax))
				emit_AddlClassInfo_CommaKeyword_1_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_CardCondition_HasKeyword_0_0_or_WithKeyword_0_1.equals(syntax))
				emit_CardCondition_HasKeyword_0_0_or_WithKeyword_0_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_CardCondition_ValueKeyword_3_0_or_ValuesKeyword_3_1.equals(syntax))
				emit_CardCondition_ValueKeyword_3_0_or_ValuesKeyword_3_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_ClassDeclaration_TopLevelKeyword_0_3_q.equals(syntax))
				emit_ClassDeclaration_TopLevelKeyword_0_3_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_ClassDeclaration_TopLevelKeyword_1_2_q.equals(syntax))
				emit_ClassDeclaration_TopLevelKeyword_1_2_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_DataTypeRestriction_CommaKeyword_1_2_0_0_or_OrKeyword_1_2_0_1.equals(syntax))
				emit_DataTypeRestriction_CommaKeyword_1_2_0_0_or_OrKeyword_1_2_0_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_DataTypeRestriction___AKeyword_0_0_0_or_AnKeyword_0_0_1__q.equals(syntax))
				emit_DataTypeRestriction___AKeyword_0_0_0_or_AnKeyword_0_0_1__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_ElementSet_CommaKeyword_1_0_q.equals(syntax))
				emit_ElementSet_CommaKeyword_1_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_EnumeratedInstances_CommaKeyword_0_q.equals(syntax))
				emit_EnumeratedInstances_CommaKeyword_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_Facets_CommaKeyword_3_2_0_q.equals(syntax))
				emit_Facets_CommaKeyword_3_2_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_HasValueCondition_HasKeyword_1_0_or_WithKeyword_1_1.equals(syntax))
				emit_HasValueCondition_HasKeyword_1_0_or_WithKeyword_1_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_InstAttrSPV_CommaKeyword_4_0_q.equals(syntax))
				emit_InstAttrSPV_CommaKeyword_4_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_InstAttrSPV_HasKeyword_4_1_0_or_WithKeyword_4_1_1.equals(syntax))
				emit_InstAttrSPV_HasKeyword_4_1_0_or_WithKeyword_4_1_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_InstAttrSPV___HasKeyword_1_0_or_WithKeyword_1_1__q.equals(syntax))
				emit_InstAttrSPV___HasKeyword_1_0_or_WithKeyword_1_1__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_MaxCardCondition_HasKeyword_0_0_or_WithKeyword_0_1.equals(syntax))
				emit_MaxCardCondition_HasKeyword_0_0_or_WithKeyword_0_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_MaxCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1.equals(syntax))
				emit_MaxCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_MinCardCondition_HasKeyword_0_0_or_WithKeyword_0_1.equals(syntax))
				emit_MinCardCondition_HasKeyword_0_0_or_WithKeyword_0_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_MinCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1.equals(syntax))
				emit_MinCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_OrderList_CommaKeyword_1_0_q.equals(syntax))
				emit_OrderList_CommaKeyword_1_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_PropValPartialTriple_CommaKeyword_0_q.equals(syntax))
				emit_PropValPartialTriple_CommaKeyword_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_PropValPartialTriple___HasKeyword_1_0_or_WithKeyword_1_1__q.equals(syntax))
				emit_PropValPartialTriple___HasKeyword_1_0_or_WithKeyword_1_1__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_Range_HasKeyword_0_0_or_WithKeyword_0_1.equals(syntax))
				emit_Range_HasKeyword_0_0_or_WithKeyword_0_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_Rule_ColonKeyword_2_q.equals(syntax))
				emit_Rule_ColonKeyword_2_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_Rule_GivenKeyword_3_0_q.equals(syntax))
				emit_Rule_GivenKeyword_3_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_Rule_IfKeyword_4_0_q.equals(syntax))
				emit_Rule_IfKeyword_4_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SomeValuesCondition_HasKeyword_0_0_or_WithKeyword_0_1.equals(syntax))
				emit_SomeValuesCondition_HasKeyword_0_0_or_WithKeyword_0_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_ValueTable_CommaKeyword_4_0_q.equals(syntax))
				emit_ValueTable_CommaKeyword_4_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_VariableList_CommaKeyword_1_0_q.equals(syntax))
				emit_VariableList_CommaKeyword_1_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_WithPhrase_CommaKeyword_0_0_q.equals(syntax))
				emit_WithPhrase_CommaKeyword_0_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_WithPhrase_CommaKeyword_1_0_q.equals(syntax))
				emit_WithPhrase_CommaKeyword_1_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_WithPhrase_CommaKeyword_2_0_q.equals(syntax))
				emit_WithPhrase_CommaKeyword_2_0_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_WithPhrase_HasKeyword_0_1_0_or_WithKeyword_0_1_1.equals(syntax))
				emit_WithPhrase_HasKeyword_0_1_0_or_WithKeyword_0_1_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_WithPhrase_HasKeyword_1_1_0_or_WithKeyword_1_1_1.equals(syntax))
				emit_WithPhrase_HasKeyword_1_1_0_or_WithKeyword_1_1_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_WithPhrase_HasKeyword_2_1_0_or_WithKeyword_2_1_1.equals(syntax))
				emit_WithPhrase_HasKeyword_2_1_0_or_WithKeyword_2_1_1(semanticObject, getLastNavigableState(), syntaxNodes);
			else acceptNodes(getLastNavigableState(), syntaxNodes);
		}
	}

	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_0_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_1_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_2_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_3_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_4_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_5_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_6_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AdditionalPropertyInfo_CommaKeyword_7_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AddlClassInfo_CommaKeyword_0_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_AddlClassInfo_CommaKeyword_1_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'has' | 'with'
	 */
	protected void emit_CardCondition_HasKeyword_0_0_or_WithKeyword_0_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'values' | 'value'
	 */
	protected void emit_CardCondition_ValueKeyword_3_0_or_ValuesKeyword_3_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'top-level'?
	 */
	protected void emit_ClassDeclaration_TopLevelKeyword_0_3_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'top-level'?
	 */
	protected void emit_ClassDeclaration_TopLevelKeyword_1_2_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'or' | ','
	 */
	protected void emit_DataTypeRestriction_CommaKeyword_1_2_0_0_or_OrKeyword_1_2_0_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('an' | 'a')?
	 */
	protected void emit_DataTypeRestriction___AKeyword_0_0_0_or_AnKeyword_0_0_1__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_ElementSet_CommaKeyword_1_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_EnumeratedInstances_CommaKeyword_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_Facets_CommaKeyword_3_2_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'with' | 'has'
	 */
	protected void emit_HasValueCondition_HasKeyword_1_0_or_WithKeyword_1_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_InstAttrSPV_CommaKeyword_4_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'with' | 'has'
	 */
	protected void emit_InstAttrSPV_HasKeyword_4_1_0_or_WithKeyword_4_1_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('has' | 'with')?
	 */
	protected void emit_InstAttrSPV___HasKeyword_1_0_or_WithKeyword_1_1__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'with' | 'has'
	 */
	protected void emit_MaxCardCondition_HasKeyword_0_0_or_WithKeyword_0_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'value' | 'values'
	 */
	protected void emit_MaxCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'has' | 'with'
	 */
	protected void emit_MinCardCondition_HasKeyword_0_0_or_WithKeyword_0_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'values' | 'value'
	 */
	protected void emit_MinCardCondition_ValueKeyword_4_0_or_ValuesKeyword_4_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_OrderList_CommaKeyword_1_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_PropValPartialTriple_CommaKeyword_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('with' | 'has')?
	 */
	protected void emit_PropValPartialTriple___HasKeyword_1_0_or_WithKeyword_1_1__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'with' | 'has'
	 */
	protected void emit_Range_HasKeyword_0_0_or_WithKeyword_0_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ':'?
	 */
	protected void emit_Rule_ColonKeyword_2_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'given'?
	 */
	protected void emit_Rule_GivenKeyword_3_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'if'?
	 */
	protected void emit_Rule_IfKeyword_4_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'with' | 'has'
	 */
	protected void emit_SomeValuesCondition_HasKeyword_0_0_or_WithKeyword_0_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_ValueTable_CommaKeyword_4_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_VariableList_CommaKeyword_1_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_WithPhrase_CommaKeyword_0_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_WithPhrase_CommaKeyword_1_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ','?
	 */
	protected void emit_WithPhrase_CommaKeyword_2_0_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'with' | 'has'
	 */
	protected void emit_WithPhrase_HasKeyword_0_1_0_or_WithKeyword_0_1_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'has' | 'with'
	 */
	protected void emit_WithPhrase_HasKeyword_1_1_0_or_WithKeyword_1_1_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     'has' | 'with'
	 */
	protected void emit_WithPhrase_HasKeyword_2_1_0_or_WithKeyword_2_1_1(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
}
