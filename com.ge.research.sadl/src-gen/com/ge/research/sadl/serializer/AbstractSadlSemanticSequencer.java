package com.ge.research.sadl.serializer;

import com.ge.research.sadl.sadl.AdditionalPropertyInfo;
import com.ge.research.sadl.sadl.AddlClassInfo;
import com.ge.research.sadl.sadl.AllValuesCondition;
import com.ge.research.sadl.sadl.AllValuesFrom;
import com.ge.research.sadl.sadl.AskQueryExpression;
import com.ge.research.sadl.sadl.BinaryOpExpression;
import com.ge.research.sadl.sadl.CardCondition;
import com.ge.research.sadl.sadl.Cardinality;
import com.ge.research.sadl.sadl.ClassDeclaration;
import com.ge.research.sadl.sadl.ComplementOfClass;
import com.ge.research.sadl.sadl.ConstructExpression;
import com.ge.research.sadl.sadl.ContentList;
import com.ge.research.sadl.sadl.DataTypeRestriction;
import com.ge.research.sadl.sadl.DefaultValue;
import com.ge.research.sadl.sadl.DisjointClasses;
import com.ge.research.sadl.sadl.Display;
import com.ge.research.sadl.sadl.ElementSet;
import com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom;
import com.ge.research.sadl.sadl.EnumeratedAllValuesFrom;
import com.ge.research.sadl.sadl.EnumeratedInstances;
import com.ge.research.sadl.sadl.EquivalentConcepts;
import com.ge.research.sadl.sadl.ExistingInstanceAttribution;
import com.ge.research.sadl.sadl.ExistingResourceList;
import com.ge.research.sadl.sadl.Explanation;
import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.Expr;
import com.ge.research.sadl.sadl.Expression;
import com.ge.research.sadl.sadl.Facets;
import com.ge.research.sadl.sadl.FunctionalProperty;
import com.ge.research.sadl.sadl.HasValue;
import com.ge.research.sadl.sadl.HasValueCondition;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.InstAttrPSV;
import com.ge.research.sadl.sadl.InstAttrSPV;
import com.ge.research.sadl.sadl.InstanceDeclaration;
import com.ge.research.sadl.sadl.InstanceDifferentFrom;
import com.ge.research.sadl.sadl.InstancesAllDifferent;
import com.ge.research.sadl.sadl.IntersectionResource;
import com.ge.research.sadl.sadl.IntervalValue;
import com.ge.research.sadl.sadl.InverseFunctionalProperty;
import com.ge.research.sadl.sadl.InverseProperty;
import com.ge.research.sadl.sadl.IsInverseOf;
import com.ge.research.sadl.sadl.JunctionExpression;
import com.ge.research.sadl.sadl.LiteralList;
import com.ge.research.sadl.sadl.LiteralValue;
import com.ge.research.sadl.sadl.MaxCardCondition;
import com.ge.research.sadl.sadl.MaxCardinality;
import com.ge.research.sadl.sadl.MergedTriples;
import com.ge.research.sadl.sadl.MinCardCondition;
import com.ge.research.sadl.sadl.MinCardinality;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.sadl.NecessaryAndSufficient;
import com.ge.research.sadl.sadl.OfPatternReturningValues;
import com.ge.research.sadl.sadl.OfPhrase;
import com.ge.research.sadl.sadl.OrderElement;
import com.ge.research.sadl.sadl.OrderList;
import com.ge.research.sadl.sadl.PropOfSubj;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.PropertyDeclaration;
import com.ge.research.sadl.sadl.PropertyOfClass;
import com.ge.research.sadl.sadl.Query;
import com.ge.research.sadl.sadl.Range;
import com.ge.research.sadl.sadl.RangeType;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceByRestriction;
import com.ge.research.sadl.sadl.ResourceList;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.Rule;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.SelectExpression;
import com.ge.research.sadl.sadl.SomeValuesCondition;
import com.ge.research.sadl.sadl.SomeValuesFrom;
import com.ge.research.sadl.sadl.SubTypeOf;
import com.ge.research.sadl.sadl.SubjProp;
import com.ge.research.sadl.sadl.SymmetricalProperty;
import com.ge.research.sadl.sadl.Test;
import com.ge.research.sadl.sadl.TransitiveProperty;
import com.ge.research.sadl.sadl.TypeDeclaration;
import com.ge.research.sadl.sadl.TypedBNode;
import com.ge.research.sadl.sadl.UnaryOpExpression;
import com.ge.research.sadl.sadl.UnionResource;
import com.ge.research.sadl.sadl.UserDefinedDataType;
import com.ge.research.sadl.sadl.ValueRow;
import com.ge.research.sadl.sadl.ValueTable;
import com.ge.research.sadl.sadl.VariableList;
import com.ge.research.sadl.sadl.WithChain;
import com.ge.research.sadl.sadl.WithPhrase;
import com.ge.research.sadl.services.SadlGrammarAccess;
import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.serializer.acceptor.ISemanticSequenceAcceptor;
import org.eclipse.xtext.serializer.acceptor.SequenceFeeder;
import org.eclipse.xtext.serializer.diagnostic.ISemanticSequencerDiagnosticProvider;
import org.eclipse.xtext.serializer.diagnostic.ISerializationDiagnostic.Acceptor;
import org.eclipse.xtext.serializer.sequencer.AbstractDelegatingSemanticSequencer;
import org.eclipse.xtext.serializer.sequencer.GenericSequencer;
import org.eclipse.xtext.serializer.sequencer.ISemanticNodeProvider.INodesForEObjectProvider;
import org.eclipse.xtext.serializer.sequencer.ISemanticSequencer;
import org.eclipse.xtext.serializer.sequencer.ITransientValueService;
import org.eclipse.xtext.serializer.sequencer.ITransientValueService.ValueTransient;

@SuppressWarnings("all")
public abstract class AbstractSadlSemanticSequencer extends AbstractDelegatingSemanticSequencer {

	@Inject
	private SadlGrammarAccess grammarAccess;
	
	public void createSequence(EObject context, EObject semanticObject) {
		if(semanticObject.eClass().getEPackage() == SadlPackage.eINSTANCE) switch(semanticObject.eClass().getClassifierID()) {
			case SadlPackage.ADDITIONAL_PROPERTY_INFO:
				if(context == grammarAccess.getAdditionalPropertyInfoRule()) {
					sequence_AdditionalPropertyInfo(context, (AdditionalPropertyInfo) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ADDL_CLASS_INFO:
				if(context == grammarAccess.getAddlClassInfoRule()) {
					sequence_AddlClassInfo(context, (AddlClassInfo) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ALL_VALUES_CONDITION:
				if(context == grammarAccess.getAllValuesConditionRule() ||
				   context == grammarAccess.getConditionRule()) {
					sequence_AllValuesCondition(context, (AllValuesCondition) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ALL_VALUES_FROM:
				if(context == grammarAccess.getAllValuesFromRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_AllValuesFrom(context, (AllValuesFrom) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ASK_QUERY_EXPRESSION:
				if(context == grammarAccess.getAskQueryExpressionRule() ||
				   context == grammarAccess.getExpressionRule()) {
					sequence_AskQueryExpression(context, (AskQueryExpression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.BINARY_OP_EXPRESSION:
				if(context == grammarAccess.getAdditiveExpressionRule() ||
				   context == grammarAccess.getAdditiveExpressionAccess().getBinaryOpExpressionLeftAction_1_0()) {
					sequence_AdditiveExpression_MultiplicativeExpression(context, (BinaryOpExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getAndExpressionRule() ||
				   context == grammarAccess.getAndExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getExpressionRule() ||
				   context == grammarAccess.getOrExpressionRule() ||
				   context == grammarAccess.getOrExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getRelationalExpressionRule() ||
				   context == grammarAccess.getRelationalExpressionAccess().getBinaryOpExpressionLeftAction_1_0()) {
					sequence_AdditiveExpression_MultiplicativeExpression_RelationalExpression(context, (BinaryOpExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getLimitedAdditiveExpressionRule() ||
				   context == grammarAccess.getLimitedAdditiveExpressionAccess().getBinaryOpExpressionLeftAction_1_0()) {
					sequence_LimitedAdditiveExpression_LimitedMultiplicativeExpression(context, (BinaryOpExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getLimitedAndExpressionRule() ||
				   context == grammarAccess.getLimitedAndExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedExpressionRule() ||
				   context == grammarAccess.getLimitedOrExpressionRule() ||
				   context == grammarAccess.getLimitedOrExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedRelationalExpressionRule() ||
				   context == grammarAccess.getLimitedRelationalExpressionAccess().getBinaryOpExpressionLeftAction_1_0()) {
					sequence_LimitedAdditiveExpression_LimitedMultiplicativeExpression_LimitedRelationalExpression(context, (BinaryOpExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getLimitedMultiplicativeExpressionRule() ||
				   context == grammarAccess.getLimitedMultiplicativeExpressionAccess().getBinaryOpExpressionLeftAction_1_0()) {
					sequence_LimitedMultiplicativeExpression(context, (BinaryOpExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getMultiplicativeExpressionRule() ||
				   context == grammarAccess.getMultiplicativeExpressionAccess().getBinaryOpExpressionLeftAction_1_0()) {
					sequence_MultiplicativeExpression(context, (BinaryOpExpression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.CARD_CONDITION:
				if(context == grammarAccess.getCardConditionRule() ||
				   context == grammarAccess.getConditionRule()) {
					sequence_CardCondition(context, (CardCondition) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.CARDINALITY:
				if(context == grammarAccess.getCardinalityRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_Cardinality(context, (Cardinality) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.CLASS_DECLARATION:
				if(context == grammarAccess.getClassDeclarationRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_ClassDeclaration(context, (ClassDeclaration) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.COMPLEMENT_OF_CLASS:
				if(context == grammarAccess.getComplementOfClassRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_ComplementOfClass(context, (ComplementOfClass) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.CONSTRUCT_EXPRESSION:
				if(context == grammarAccess.getConstructExpressionRule() ||
				   context == grammarAccess.getExpressionRule()) {
					sequence_ConstructExpression(context, (ConstructExpression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.CONTENT_LIST:
				if(context == grammarAccess.getContentListRule()) {
					sequence_ContentList(context, (ContentList) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.DATA_TYPE_RESTRICTION:
				if(context == grammarAccess.getDataTypeRestrictionRule()) {
					sequence_DataTypeRestriction(context, (DataTypeRestriction) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.DEFAULT_VALUE:
				if(context == grammarAccess.getDefaultValueRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_DefaultValue(context, (DefaultValue) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.DISJOINT_CLASSES:
				if(context == grammarAccess.getDisjointClassesRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_DisjointClasses(context, (DisjointClasses) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.DISPLAY:
				if(context == grammarAccess.getDisplayRule() ||
				   context == grammarAccess.getModelElementRule()) {
					sequence_Display(context, (Display) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ELEMENT_SET:
				if(context == grammarAccess.getElementSetRule()) {
					sequence_ElementSet(context, (ElementSet) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ENUMERATED_ALL_AND_SOME_VALUES_FROM:
				if(context == grammarAccess.getEnumeratedAllAndSomeValuesFromRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_EnumeratedAllAndSomeValuesFrom(context, (EnumeratedAllAndSomeValuesFrom) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ENUMERATED_ALL_VALUES_FROM:
				if(context == grammarAccess.getEnumeratedAllValuesFromRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_EnumeratedAllValuesFrom(context, (EnumeratedAllValuesFrom) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ENUMERATED_INSTANCES:
				if(context == grammarAccess.getEnumeratedInstancesRule()) {
					sequence_EnumeratedInstances(context, (EnumeratedInstances) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EQUIVALENT_CONCEPTS:
				if(context == grammarAccess.getEquivalentConceptsRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_EquivalentConcepts(context, (EquivalentConcepts) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION:
				if(context == grammarAccess.getExistingInstanceAttributionRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_ExistingInstanceAttribution(context, (ExistingInstanceAttribution) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EXISTING_RESOURCE_LIST:
				if(context == grammarAccess.getExistingResourceListRule()) {
					sequence_ExistingResourceList(context, (ExistingResourceList) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EXPLANATION:
				if(context == grammarAccess.getExplanationRule() ||
				   context == grammarAccess.getModelElementRule()) {
					sequence_Explanation(context, (Explanation) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EXPLICIT_VALUE:
				if(context == grammarAccess.getExplicitValueRule()) {
					sequence_ExplicitValue(context, (ExplicitValue) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EXPR:
				if(context == grammarAccess.getExprRule() ||
				   context == grammarAccess.getModelElementRule()) {
					sequence_Expr(context, (Expr) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.EXPRESSION:
				if(context == grammarAccess.getLimitedAdditiveExpressionRule() ||
				   context == grammarAccess.getLimitedAdditiveExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedAndExpressionRule() ||
				   context == grammarAccess.getLimitedAndExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedExpressionRule() ||
				   context == grammarAccess.getLimitedMultiplicativeExpressionRule() ||
				   context == grammarAccess.getLimitedMultiplicativeExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedOrExpressionRule() ||
				   context == grammarAccess.getLimitedOrExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedPrimaryExpressionRule() ||
				   context == grammarAccess.getLimitedRelationalExpressionRule() ||
				   context == grammarAccess.getLimitedRelationalExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedUnaryOrPrimaryExpressionRule()) {
					sequence_LimitedPrimaryExpression(context, (Expression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getAdditiveExpressionRule() ||
				   context == grammarAccess.getAdditiveExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getAndExpressionRule() ||
				   context == grammarAccess.getAndExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getExpressionRule() ||
				   context == grammarAccess.getMultiplicativeExpressionRule() ||
				   context == grammarAccess.getMultiplicativeExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getOrExpressionRule() ||
				   context == grammarAccess.getOrExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getPrimaryExpressionRule() ||
				   context == grammarAccess.getRelationalExpressionRule() ||
				   context == grammarAccess.getRelationalExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getUnaryOrPrimaryExpressionRule()) {
					sequence_PrimaryExpression(context, (Expression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.FACETS:
				if(context == grammarAccess.getFacetsRule()) {
					sequence_Facets(context, (Facets) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.FUNCTIONAL_PROPERTY:
				if(context == grammarAccess.getFunctionalPropertyRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_FunctionalProperty(context, (FunctionalProperty) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.HAS_VALUE:
				if(context == grammarAccess.getHasValueRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_HasValue(context, (HasValue) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.HAS_VALUE_CONDITION:
				if(context == grammarAccess.getConditionRule() ||
				   context == grammarAccess.getHasValueConditionRule()) {
					sequence_HasValueCondition(context, (HasValueCondition) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.IMPORT:
				if(context == grammarAccess.getImportRule()) {
					sequence_Import(context, (Import) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INST_ATTR_PSV:
				if(context == grammarAccess.getGraphPatternRule() ||
				   context == grammarAccess.getInstAttrPSVRule()) {
					sequence_InstAttrPSV(context, (InstAttrPSV) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INST_ATTR_SPV:
				if(context == grammarAccess.getGraphPatternRule() ||
				   context == grammarAccess.getInstAttrSPVRule()) {
					sequence_InstAttrSPV(context, (InstAttrSPV) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INSTANCE_DECLARATION:
				if(context == grammarAccess.getEmbeddedInstanceDeclarationRule() ||
				   context == grammarAccess.getInstanceDeclarationRule() ||
				   context == grammarAccess.getInstanceDeclarationStatementRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_InstanceDeclaration(context, (InstanceDeclaration) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INSTANCE_DIFFERENT_FROM:
				if(context == grammarAccess.getInstanceDifferentFromRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_InstanceDifferentFrom(context, (InstanceDifferentFrom) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INSTANCES_ALL_DIFFERENT:
				if(context == grammarAccess.getInstancesAllDifferentRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_InstancesAllDifferent(context, (InstancesAllDifferent) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INTERSECTION_RESOURCE:
				if(context == grammarAccess.getIntersectionResourceRule() ||
				   context == grammarAccess.getResourceBySetOpRule()) {
					sequence_IntersectionResource(context, (IntersectionResource) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getResourceIdentifierRule()) {
					sequence_IntersectionResource_ResourceIdentifier(context, (IntersectionResource) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INTERVAL_VALUE:
				if(context == grammarAccess.getIntervalValueRule()) {
					sequence_IntervalValue(context, (IntervalValue) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INVERSE_FUNCTIONAL_PROPERTY:
				if(context == grammarAccess.getInverseFunctionalPropertyRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_InverseFunctionalProperty(context, (InverseFunctionalProperty) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.INVERSE_PROPERTY:
				if(context == grammarAccess.getInversePropertyRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_InverseProperty(context, (InverseProperty) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.IS_INVERSE_OF:
				if(context == grammarAccess.getIsInverseOfRule()) {
					sequence_IsInverseOf(context, (IsInverseOf) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.JUNCTION_EXPRESSION:
				if(context == grammarAccess.getAndExpressionRule() ||
				   context == grammarAccess.getAndExpressionAccess().getJunctionExpressionLeftAction_1_0()) {
					sequence_AndExpression(context, (JunctionExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getExpressionRule() ||
				   context == grammarAccess.getOrExpressionRule() ||
				   context == grammarAccess.getOrExpressionAccess().getJunctionExpressionLeftAction_1_0()) {
					sequence_AndExpression_OrExpression(context, (JunctionExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getLimitedAndExpressionRule() ||
				   context == grammarAccess.getLimitedAndExpressionAccess().getJunctionExpressionLeftAction_1_0()) {
					sequence_LimitedAndExpression(context, (JunctionExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getLimitedExpressionRule() ||
				   context == grammarAccess.getLimitedOrExpressionRule() ||
				   context == grammarAccess.getLimitedOrExpressionAccess().getJunctionExpressionLeftAction_1_0()) {
					sequence_LimitedAndExpression_LimitedOrExpression(context, (JunctionExpression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.LITERAL_LIST:
				if(context == grammarAccess.getLiteralListRule()) {
					sequence_LiteralList(context, (LiteralList) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.LITERAL_VALUE:
				if(context == grammarAccess.getLiteralValueRule()) {
					sequence_LiteralValue(context, (LiteralValue) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MAX_CARD_CONDITION:
				if(context == grammarAccess.getConditionRule() ||
				   context == grammarAccess.getMaxCardConditionRule()) {
					sequence_MaxCardCondition(context, (MaxCardCondition) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MAX_CARDINALITY:
				if(context == grammarAccess.getMaxCardinalityRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_MaxCardinality(context, (MaxCardinality) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MERGED_TRIPLES:
				if(context == grammarAccess.getGraphPatternRule() ||
				   context == grammarAccess.getMergedTriplesRule()) {
					sequence_MergedTriples(context, (MergedTriples) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MIN_CARD_CONDITION:
				if(context == grammarAccess.getConditionRule() ||
				   context == grammarAccess.getMinCardConditionRule()) {
					sequence_MinCardCondition(context, (MinCardCondition) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MIN_CARDINALITY:
				if(context == grammarAccess.getMinCardinalityRule() ||
				   context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_MinCardinality(context, (MinCardinality) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MODEL:
				if(context == grammarAccess.getModelRule()) {
					sequence_Model(context, (Model) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.MODEL_NAME:
				if(context == grammarAccess.getModelNameRule()) {
					sequence_ModelName(context, (ModelName) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.NECESSARY_AND_SUFFICIENT:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getNecessaryAndSufficientRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_NecessaryAndSufficient(context, (NecessaryAndSufficient) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.OBJECT:
				if(context == grammarAccess.getObjectRule()) {
					sequence_Object(context, (com.ge.research.sadl.sadl.Object) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.OF_PATTERN_RETURNING_VALUES:
				if(context == grammarAccess.getOfPatternReturningValuesRule()) {
					sequence_OfPatternReturningValues(context, (OfPatternReturningValues) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.OF_PHRASE:
				if(context == grammarAccess.getOfPhraseRule()) {
					sequence_OfPhrase(context, (OfPhrase) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ORDER_ELEMENT:
				if(context == grammarAccess.getOrderElementRule()) {
					sequence_OrderElement(context, (OrderElement) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.ORDER_LIST:
				if(context == grammarAccess.getOrderListRule()) {
					sequence_OrderList(context, (OrderList) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.PROP_OF_SUBJ:
				if(context == grammarAccess.getGraphPatternRule() ||
				   context == grammarAccess.getPropOfSubjRule()) {
					sequence_PropOfSubj(context, (PropOfSubj) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.PROP_VAL_PARTIAL_TRIPLE:
				if(context == grammarAccess.getPropValPartialTripleRule()) {
					sequence_PropValPartialTriple(context, (PropValPartialTriple) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.PROPERTY_DECLARATION:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getPropertyDeclarationRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_PropertyDeclaration(context, (PropertyDeclaration) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.PROPERTY_OF_CLASS:
				if(context == grammarAccess.getPropertyOfClassRule()) {
					sequence_PropertyOfClass(context, (PropertyOfClass) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.QUERY:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getQueryRule()) {
					sequence_Query(context, (Query) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RANGE:
				if(context == grammarAccess.getRangeRule()) {
					sequence_Range(context, (Range) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RANGE_TYPE:
				if(context == grammarAccess.getRangeTypeRule()) {
					sequence_RangeType(context, (RangeType) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RESOURCE_BY_NAME:
				if(context == grammarAccess.getResourceByNameRule() ||
				   context == grammarAccess.getResourceIdentifierRule()) {
					sequence_ResourceByName(context, (ResourceByName) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RESOURCE_BY_RESTRICTION:
				if(context == grammarAccess.getResourceByRestrictionRule()) {
					sequence_ResourceByRestriction(context, (ResourceByRestriction) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getResourceIdentifierRule()) {
					sequence_ResourceByRestriction_ResourceIdentifier(context, (ResourceByRestriction) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RESOURCE_LIST:
				if(context == grammarAccess.getResourceListRule()) {
					sequence_ResourceList(context, (ResourceList) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RESOURCE_NAME:
				if(context == grammarAccess.getResourceNameRule()) {
					sequence_ResourceName(context, (ResourceName) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.RULE:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getRuleRule()) {
					sequence_Rule(context, (Rule) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.SELECT_EXPRESSION:
				if(context == grammarAccess.getExpressionRule() ||
				   context == grammarAccess.getSelectExpressionRule()) {
					sequence_SelectExpression(context, (SelectExpression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.SOME_VALUES_CONDITION:
				if(context == grammarAccess.getConditionRule() ||
				   context == grammarAccess.getSomeValuesConditionRule()) {
					sequence_SomeValuesCondition(context, (SomeValuesCondition) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.SOME_VALUES_FROM:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getSomeValuesFromRule() ||
				   context == grammarAccess.getStatementRule()) {
					sequence_SomeValuesFrom(context, (SomeValuesFrom) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.SUB_TYPE_OF:
				if(context == grammarAccess.getGraphPatternRule() ||
				   context == grammarAccess.getSubTypeOfRule()) {
					sequence_SubTypeOf(context, (SubTypeOf) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.SUBJ_PROP:
				if(context == grammarAccess.getGraphPatternRule() ||
				   context == grammarAccess.getSubjPropRule()) {
					sequence_SubjProp(context, (SubjProp) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.SYMMETRICAL_PROPERTY:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule() ||
				   context == grammarAccess.getSymmetricalPropertyRule()) {
					sequence_SymmetricalProperty(context, (SymmetricalProperty) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.TEST:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getTestRule()) {
					sequence_Test(context, (Test) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.TRANSITIVE_PROPERTY:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule() ||
				   context == grammarAccess.getTransitivePropertyRule()) {
					sequence_TransitiveProperty(context, (TransitiveProperty) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.TYPE_DECLARATION:
				if(context == grammarAccess.getTypeDeclarationRule()) {
					sequence_TypeDeclaration(context, (TypeDeclaration) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.TYPED_BNODE:
				if(context == grammarAccess.getTypedBNodeRule()) {
					sequence_TypedBNode(context, (TypedBNode) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.UNARY_OP_EXPRESSION:
				if(context == grammarAccess.getLimitedAdditiveExpressionRule() ||
				   context == grammarAccess.getLimitedAdditiveExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedAndExpressionRule() ||
				   context == grammarAccess.getLimitedAndExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedExpressionRule() ||
				   context == grammarAccess.getLimitedMultiplicativeExpressionRule() ||
				   context == grammarAccess.getLimitedMultiplicativeExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedOrExpressionRule() ||
				   context == grammarAccess.getLimitedOrExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedRelationalExpressionRule() ||
				   context == grammarAccess.getLimitedRelationalExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getLimitedUnaryOrPrimaryExpressionRule()) {
					sequence_LimitedUnaryOrPrimaryExpression(context, (UnaryOpExpression) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getAdditiveExpressionRule() ||
				   context == grammarAccess.getAdditiveExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getAndExpressionRule() ||
				   context == grammarAccess.getAndExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getExpressionRule() ||
				   context == grammarAccess.getMultiplicativeExpressionRule() ||
				   context == grammarAccess.getMultiplicativeExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getOrExpressionRule() ||
				   context == grammarAccess.getOrExpressionAccess().getJunctionExpressionLeftAction_1_0() ||
				   context == grammarAccess.getRelationalExpressionRule() ||
				   context == grammarAccess.getRelationalExpressionAccess().getBinaryOpExpressionLeftAction_1_0() ||
				   context == grammarAccess.getUnaryOrPrimaryExpressionRule()) {
					sequence_UnaryOrPrimaryExpression(context, (UnaryOpExpression) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.UNION_RESOURCE:
				if(context == grammarAccess.getResourceIdentifierRule()) {
					sequence_ResourceIdentifier_UnionResource(context, (UnionResource) semanticObject); 
					return; 
				}
				else if(context == grammarAccess.getResourceBySetOpRule() ||
				   context == grammarAccess.getUnionResourceRule()) {
					sequence_UnionResource(context, (UnionResource) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.USER_DEFINED_DATA_TYPE:
				if(context == grammarAccess.getModelElementRule() ||
				   context == grammarAccess.getStatementRule() ||
				   context == grammarAccess.getUserDefinedDataTypeRule()) {
					sequence_UserDefinedDataType(context, (UserDefinedDataType) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.VALUE_ROW:
				if(context == grammarAccess.getValueRowRule()) {
					sequence_ValueRow(context, (ValueRow) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.VALUE_TABLE:
				if(context == grammarAccess.getValueTableRule()) {
					sequence_ValueTable(context, (ValueTable) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.VARIABLE_LIST:
				if(context == grammarAccess.getVariableListRule()) {
					sequence_VariableList(context, (VariableList) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.WITH_CHAIN:
				if(context == grammarAccess.getWithChainRule()) {
					sequence_WithChain(context, (WithChain) semanticObject); 
					return; 
				}
				else break;
			case SadlPackage.WITH_PHRASE:
				if(context == grammarAccess.getWithPhraseRule()) {
					sequence_WithPhrase(context, (WithPhrase) semanticObject); 
					return; 
				}
				else break;
			}
		if (errorAcceptor != null) errorAcceptor.accept(diagnosticProvider.createInvalidContextOrTypeDiagnostic(semanticObject, context));
	}
	
	/**
	 * Constraint:
	 *     (
	 *         domain=ResourceIdentifier | 
	 *         cond=Condition | 
	 *         range=Range | 
	 *         isfunc=IsFunctional | 
	 *         isinvfunc=IsInverseFunctional | 
	 *         isSym=IsSymmetrical | 
	 *         isTrans=IsTransitive | 
	 *         isInvOf=IsInverseOf
	 *     )
	 */
	protected void sequence_AdditionalPropertyInfo(EObject context, AdditionalPropertyInfo semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (left=AdditiveExpression_BinaryOpExpression_1_0 (op='+' | op='-') right=MultiplicativeExpression) | 
	 *         (left=MultiplicativeExpression_BinaryOpExpression_1_0 (op='*' | op='/' | op='^' | op='%') right=UnaryOrPrimaryExpression)
	 *     )
	 */
	protected void sequence_AdditiveExpression_MultiplicativeExpression(EObject context, BinaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (left=AdditiveExpression_BinaryOpExpression_1_0 (op='+' | op='-') right=MultiplicativeExpression) | 
	 *         (left=MultiplicativeExpression_BinaryOpExpression_1_0 (op='*' | op='/' | op='^' | op='%') right=UnaryOrPrimaryExpression) | 
	 *         (
	 *             left=RelationalExpression_BinaryOpExpression_1_0 
	 *             (
	 *                 op='=' | 
	 *                 op='==' | 
	 *                 op='is' | 
	 *                 op=ShallBe | 
	 *                 op='!=' | 
	 *                 op='<' | 
	 *                 op='<=' | 
	 *                 op='>' | 
	 *                 op='>='
	 *             ) 
	 *             right=AdditiveExpression
	 *         )
	 *     )
	 */
	protected void sequence_AdditiveExpression_MultiplicativeExpression_RelationalExpression(EObject context, BinaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (propertyByName=ResourceByName | (propertyName=ResourceName range=Range) | (propertyByName=ResourceByName restriction=Condition))
	 */
	protected void sequence_AddlClassInfo(EObject context, AddlClassInfo semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     restriction=ResourceIdentifier
	 */
	protected void sequence_AllValuesCondition(EObject context, AllValuesCondition semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.ALL_VALUES_CONDITION__RESTRICTION) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.ALL_VALUES_CONDITION__RESTRICTION));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getAllValuesConditionAccess().getRestrictionResourceIdentifierParserRuleCall_5_0(), semanticObject.getRestriction());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     ((restricted=PropertyOfClass cond=AllValuesCondition) | (className=ResourceIdentifier propertyName=ResourceByName cond=AllValuesCondition))
	 */
	protected void sequence_AllValuesFrom(EObject context, AllValuesFrom semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (left=AndExpression_JunctionExpression_1_0 (op='&&' | op='and') right=RelationalExpression)
	 */
	protected void sequence_AndExpression(EObject context, JunctionExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (left=OrExpression_JunctionExpression_1_0 (op='||' | op='or') right=AndExpression) | 
	 *         (left=AndExpression_JunctionExpression_1_0 (op='&&' | op='and') right=RelationalExpression)
	 *     )
	 */
	protected void sequence_AndExpression_OrExpression(EObject context, JunctionExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     expr=OrExpression
	 */
	protected void sequence_AskQueryExpression(EObject context, AskQueryExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (card=NUMBER classQualifier=ResourceIdentifier?)
	 */
	protected void sequence_CardCondition(EObject context, CardCondition semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((restricted=PropertyOfClass cond=CardCondition) | (className=ResourceIdentifier propertyName=ResourceByName cond=CardCondition))
	 */
	protected void sequence_Cardinality(EObject context, Cardinality semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (className=ResourceName mustBeOneOf=EnumeratedInstances? describedBy+=AddlClassInfo*) | 
	 *         (classList=ResourceList describedBy+=AddlClassInfo*) | 
	 *         (className=ResourceName classIdentifier=ResourceIdentifier mustBeOneOf=EnumeratedInstances? describedBy+=AddlClassInfo*) | 
	 *         (classList=ResourceList classIdentifier=ResourceIdentifier describedBy+=AddlClassInfo*)
	 *     )
	 */
	protected void sequence_ClassDeclaration(EObject context, ClassDeclaration semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (class1=ResourceByName class2=ResourceIdentifier)
	 */
	protected void sequence_ComplementOfClass(EObject context, ComplementOfClass semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.COMPLEMENT_OF_CLASS__CLASS1) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.COMPLEMENT_OF_CLASS__CLASS1));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.COMPLEMENT_OF_CLASS__CLASS2) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.COMPLEMENT_OF_CLASS__CLASS2));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getComplementOfClassAccess().getClass1ResourceByNameParserRuleCall_0_0(), semanticObject.getClass1());
		feeder.accept(grammarAccess.getComplementOfClassAccess().getClass2ResourceIdentifierParserRuleCall_6_0(), semanticObject.getClass2());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (subj=ResourceName pred=ResourceName obj=ResourceName expr=OrExpression)
	 */
	protected void sequence_ConstructExpression(EObject context, ConstructExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (annContent+=STRING annContent+=STRING*)
	 */
	protected void sequence_ContentList(EObject context, ContentList semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((basetype=NAME facets=Facets) | (basetypes+=NAME basetypes+=NAME+))
	 */
	protected void sequence_DataTypeRestriction(EObject context, DataTypeRestriction semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (defValueClass=PropertyOfClass level=NUMBER? defValue=ExplicitValue)
	 */
	protected void sequence_DefaultValue(EObject context, DefaultValue semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((class1=ResourceByName class2=ResourceIdentifier) | classes=ExistingResourceList)
	 */
	protected void sequence_DisjointClasses(EObject context, DisjointClasses semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (displayString=STRING | model='Deductions' | model='Model')
	 */
	protected void sequence_Display(EObject context, Display semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (elements+=Expression elements+=Expression*)
	 */
	protected void sequence_ElementSet(EObject context, ElementSet semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (restricted=PropertyOfClass (enumeration=ExistingResourceList | enumeration=LiteralList))
	 */
	protected void sequence_EnumeratedAllAndSomeValuesFrom(EObject context, EnumeratedAllAndSomeValuesFrom semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (restricted=PropertyOfClass (enumeration=ExistingResourceList | enumeration=LiteralList))
	 */
	protected void sequence_EnumeratedAllValuesFrom(EObject context, EnumeratedAllValuesFrom semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     instanceList=ResourceList
	 */
	protected void sequence_EnumeratedInstances(EObject context, EnumeratedInstances semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.ENUMERATED_INSTANCES__INSTANCE_LIST) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.ENUMERATED_INSTANCES__INSTANCE_LIST));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getEnumeratedInstancesAccess().getInstanceListResourceListParserRuleCall_5_0(), semanticObject.getInstanceList());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (class1=ResourceByName class2=ResourceIdentifier)
	 */
	protected void sequence_EquivalentConcepts(EObject context, EquivalentConcepts semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.EQUIVALENT_CONCEPTS__CLASS1) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.EQUIVALENT_CONCEPTS__CLASS1));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.EQUIVALENT_CONCEPTS__CLASS2) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.EQUIVALENT_CONCEPTS__CLASS2));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getEquivalentConceptsAccess().getClass1ResourceByNameParserRuleCall_0_0(), semanticObject.getClass1());
		feeder.accept(grammarAccess.getEquivalentConceptsAccess().getClass2ResourceIdentifierParserRuleCall_5_0(), semanticObject.getClass2());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (subj=ResourceByName addlInfoItems+=PropValPartialTriple+) | 
	 *         (pOfS=OfPatternReturningValues obj=Object) | 
	 *         (obj=ExplicitValue pOfS=OfPatternReturningValues)
	 *     )
	 */
	protected void sequence_ExistingInstanceAttribution(EObject context, ExistingInstanceAttribution semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((names+=ResourceByName | names+=ResourceBySetOp) (names+=ResourceByName | names+=ResourceBySetOp)*)
	 */
	protected void sequence_ExistingResourceList(EObject context, ExistingResourceList semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (expr=InstanceDeclaration | expr=ExistingInstanceAttribution | expr=Expression | rulename=NAME)
	 */
	protected void sequence_Explanation(EObject context, Explanation semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (instName=ResourceByName | litValue=LiteralValue | term='PI' | term='known')
	 */
	protected void sequence_ExplicitValue(EObject context, ExplicitValue semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     expr=Expression
	 */
	protected void sequence_Expr(EObject context, Expr semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.EXPR__EXPR) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.EXPR__EXPR));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getExprAccess().getExprExpressionParserRuleCall_1_0(), semanticObject.getExpr());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         ((minexin='(' | minexin='[') min=NUMBER? max=NUMBER? (maxexin=']' | maxexin=')')) | 
	 *         regex=STRING | 
	 *         len=NUMBER | 
	 *         (minlen=NUMBER maxlen=NUMBER) | 
	 *         ((values+=STRING | values+=NUMBER) (values+=STRING | values+=NUMBER)*)
	 *     )
	 */
	protected void sequence_Facets(EObject context, Facets semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     propertyName=ResourceByName
	 */
	protected void sequence_FunctionalProperty(EObject context, FunctionalProperty semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.FUNCTIONAL_PROPERTY__PROPERTY_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.FUNCTIONAL_PROPERTY__PROPERTY_NAME));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getFunctionalPropertyAccess().getPropertyNameResourceByNameParserRuleCall_0_0(), semanticObject.getPropertyName());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     restriction=ExplicitValue
	 */
	protected void sequence_HasValueCondition(EObject context, HasValueCondition semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.HAS_VALUE_CONDITION__RESTRICTION) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.HAS_VALUE_CONDITION__RESTRICTION));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getHasValueConditionAccess().getRestrictionExplicitValueParserRuleCall_3_0(), semanticObject.getRestriction());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     ((restricted=PropertyOfClass cond=HasValueCondition) | (className=ResourceIdentifier propertyName=ResourceByName cond=HasValueCondition))
	 */
	protected void sequence_HasValue(EObject context, HasValue semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (importURI=STRING alias=NAME?)
	 */
	protected void sequence_Import(EObject context, Import semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (prop=PropOfSubj val=ExplicitValue)
	 */
	protected void sequence_InstAttrPSV(EObject context, InstAttrPSV semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INST_ATTR_PSV__PROP) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INST_ATTR_PSV__PROP));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INST_ATTR_PSV__VAL) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INST_ATTR_PSV__VAL));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getInstAttrPSVAccess().getPropPropOfSubjParserRuleCall_0_0(), semanticObject.getProp());
		feeder.accept(grammarAccess.getInstAttrPSVAccess().getValExplicitValueParserRuleCall_2_0(), semanticObject.getVal());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (subj=ResourceByName props+=ResourceByName vals+=AdditiveExpression (props+=ResourceByName vals+=AdditiveExpression)*)
	 */
	protected void sequence_InstAttrSPV(EObject context, InstAttrSPV semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (typeDecl=TypeDeclaration addlInfoItems+=PropValPartialTriple*) | 
	 *         (article=IndefiniteArticle className=ResourceByName instanceName=ResourceName? addlInfoItems+=PropValPartialTriple*)
	 *     )
	 */
	protected void sequence_InstanceDeclaration(EObject context, InstanceDeclaration semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (instName1=ResourceByName instName2=ResourceByName)
	 */
	protected void sequence_InstanceDifferentFrom(EObject context, InstanceDifferentFrom semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INSTANCE_DIFFERENT_FROM__INST_NAME1) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INSTANCE_DIFFERENT_FROM__INST_NAME1));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INSTANCE_DIFFERENT_FROM__INST_NAME2) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INSTANCE_DIFFERENT_FROM__INST_NAME2));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getInstanceDifferentFromAccess().getInstName1ResourceByNameParserRuleCall_0_0(), semanticObject.getInstName1());
		feeder.accept(grammarAccess.getInstanceDifferentFromAccess().getInstName2ResourceByNameParserRuleCall_6_0(), semanticObject.getInstName2());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     instances=ExistingResourceList
	 */
	protected void sequence_InstancesAllDifferent(EObject context, InstancesAllDifferent semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INSTANCES_ALL_DIFFERENT__INSTANCES) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INSTANCES_ALL_DIFFERENT__INSTANCES));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getInstancesAllDifferentAccess().getInstancesExistingResourceListParserRuleCall_0_0(), semanticObject.getInstances());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (names+=ResourceIdentifier (op+='and' names+=ResourceIdentifier)+)
	 */
	protected void sequence_IntersectionResource(EObject context, IntersectionResource semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (names+=ResourceIdentifier (op+='and' names+=ResourceIdentifier)+ ((annType='alias' | annType='note') annContent=STRING)?)
	 */
	protected void sequence_IntersectionResource_ResourceIdentifier(EObject context, IntersectionResource semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((op='==' | op='<' | op='<=' | op='>' | op='>=') expr=AdditiveExpression)
	 */
	protected void sequence_IntervalValue(EObject context, IntervalValue semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     propertyName=ResourceByName
	 */
	protected void sequence_InverseFunctionalProperty(EObject context, InverseFunctionalProperty semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INVERSE_FUNCTIONAL_PROPERTY__PROPERTY_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INVERSE_FUNCTIONAL_PROPERTY__PROPERTY_NAME));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getInverseFunctionalPropertyAccess().getPropertyNameResourceByNameParserRuleCall_0_0(), semanticObject.getPropertyName());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (propertyName1=ResourceByName invOf=IsInverseOf)
	 */
	protected void sequence_InverseProperty(EObject context, InverseProperty semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INVERSE_PROPERTY__PROPERTY_NAME1) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INVERSE_PROPERTY__PROPERTY_NAME1));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.INVERSE_PROPERTY__INV_OF) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.INVERSE_PROPERTY__INV_OF));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getInversePropertyAccess().getPropertyName1ResourceByNameParserRuleCall_0_0(), semanticObject.getPropertyName1());
		feeder.accept(grammarAccess.getInversePropertyAccess().getInvOfIsInverseOfParserRuleCall_1_0(), semanticObject.getInvOf());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     propertyName2=ResourceByName
	 */
	protected void sequence_IsInverseOf(EObject context, IsInverseOf semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.IS_INVERSE_OF__PROPERTY_NAME2) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.IS_INVERSE_OF__PROPERTY_NAME2));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getIsInverseOfAccess().getPropertyName2ResourceByNameParserRuleCall_4_0(), semanticObject.getPropertyName2());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (left=LimitedAdditiveExpression_BinaryOpExpression_1_0 (op='+' | op='-') right=LimitedMultiplicativeExpression) | 
	 *         (left=LimitedMultiplicativeExpression_BinaryOpExpression_1_0 (op='*' | op='/' | op='^' | op='%') right=LimitedUnaryOrPrimaryExpression)
	 *     )
	 */
	protected void sequence_LimitedAdditiveExpression_LimitedMultiplicativeExpression(EObject context, BinaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (left=LimitedAdditiveExpression_BinaryOpExpression_1_0 (op='+' | op='-') right=LimitedMultiplicativeExpression) | 
	 *         (left=LimitedMultiplicativeExpression_BinaryOpExpression_1_0 (op='*' | op='/' | op='^' | op='%') right=LimitedUnaryOrPrimaryExpression) | 
	 *         (
	 *             left=LimitedRelationalExpression_BinaryOpExpression_1_0 
	 *             (
	 *                 op='=' | 
	 *                 op='==' | 
	 *                 op='is' | 
	 *                 op='!=' | 
	 *                 op='<' | 
	 *                 op='<=' | 
	 *                 op='>' | 
	 *                 op='>='
	 *             ) 
	 *             right=LimitedAdditiveExpression
	 *         )
	 *     )
	 */
	protected void sequence_LimitedAdditiveExpression_LimitedMultiplicativeExpression_LimitedRelationalExpression(EObject context, BinaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (left=LimitedAndExpression_JunctionExpression_1_0 (op='&&' | op='and') right=LimitedRelationalExpression)
	 */
	protected void sequence_LimitedAndExpression(EObject context, JunctionExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (left=LimitedOrExpression_JunctionExpression_1_0 (op='||' | op='or') right=LimitedAndExpression) | 
	 *         (left=LimitedAndExpression_JunctionExpression_1_0 (op='&&' | op='and') right=LimitedRelationalExpression)
	 *     )
	 */
	protected void sequence_LimitedAndExpression_LimitedOrExpression(EObject context, JunctionExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (left=LimitedMultiplicativeExpression_BinaryOpExpression_1_0 (op='*' | op='/' | op='^' | op='%') right=LimitedUnaryOrPrimaryExpression)
	 */
	protected void sequence_LimitedMultiplicativeExpression(EObject context, BinaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (expr=Expression | (func=ID (args+=Expression args+=Expression*)?) | ivalue=IntervalValue | value=ExplicitValue | valueTable=ValueTable)
	 */
	protected void sequence_LimitedPrimaryExpression(EObject context, Expression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((op='!' | op='-') expr=LimitedPrimaryExpression)
	 */
	protected void sequence_LimitedUnaryOrPrimaryExpression(EObject context, UnaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (literals+=LiteralValue literals+=LiteralValue*)
	 */
	protected void sequence_LiteralList(EObject context, LiteralList semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (literalNumber=NUMBER | literalString=STRING | literalBoolean='true' | literalBoolean='false')
	 */
	protected void sequence_LiteralValue(EObject context, LiteralValue semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (card=NUMBER classQualifier=ResourceIdentifier?)
	 */
	protected void sequence_MaxCardCondition(EObject context, MaxCardCondition semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((restricted=PropertyOfClass cond=MaxCardCondition) | (className=ResourceIdentifier propertyName=ResourceByName cond=MaxCardCondition))
	 */
	protected void sequence_MaxCardinality(EObject context, MaxCardinality semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (ops+=OfPhrase* pivot=TypedBNode wcs+=WithChain*)
	 */
	protected void sequence_MergedTriples(EObject context, MergedTriples semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (card=NUMBER classQualifier=ResourceIdentifier?)
	 */
	protected void sequence_MinCardCondition(EObject context, MinCardCondition semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((restricted=PropertyOfClass cond=MinCardCondition) | (className=ResourceIdentifier propertyName=ResourceByName cond=MinCardCondition))
	 */
	protected void sequence_MinCardinality(EObject context, MinCardinality semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (baseUri=STRING alias=NAME? version=STRING?)
	 */
	protected void sequence_ModelName(EObject context, ModelName semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (modelName=ModelName imports+=Import* elements+=ModelElement*)
	 */
	protected void sequence_Model(EObject context, Model semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (left=MultiplicativeExpression_BinaryOpExpression_1_0 (op='*' | op='/' | op='^' | op='%') right=UnaryOrPrimaryExpression)
	 */
	protected void sequence_MultiplicativeExpression(EObject context, BinaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         superClass=TypedBNode 
	 *         article=IndefiniteArticle 
	 *         subClass=ResourceName 
	 *         propertyName+=ResourceByName 
	 *         cond+=Condition 
	 *         (propertyName+=ResourceByName cond+=Condition)*
	 *     )
	 */
	protected void sequence_NecessaryAndSufficient(EObject context, NecessaryAndSufficient semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (val=LiteralValue | val=ResourceByName | val=InstanceDeclaration)
	 */
	protected void sequence_Object(EObject context, com.ge.research.sadl.sadl.Object semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((ofphrs+=OfPhrase+ subject=ResourceByName) | type=TypedBNode)
	 */
	protected void sequence_OfPatternReturningValues(EObject context, OfPatternReturningValues semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (article=AnArticle? propertyName=ResourceByName)
	 */
	protected void sequence_OfPhrase(EObject context, OfPhrase semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((order='asc' | order='desc')? name=ResourceName)
	 */
	protected void sequence_OrderElement(EObject context, OrderElement semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (orderList+=OrderElement orderList+=OrderElement*)
	 */
	protected void sequence_OrderList(EObject context, OrderList semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         expr=Expression | 
	 *         (func=ID (args+=Expression args+=Expression*)?) | 
	 *         gp=GraphPattern | 
	 *         ivalue=IntervalValue | 
	 *         value=ExplicitValue | 
	 *         valueTable=ValueTable
	 *     )
	 */
	protected void sequence_PrimaryExpression(EObject context, Expression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (ofPhr+=OfPhrase+ subj=ResourceByName)
	 */
	protected void sequence_PropOfSubj(EObject context, PropOfSubj semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (propertyName=ResourceByName (objectValue=ExplicitValue | objectValueBNode=InstanceDeclaration))
	 */
	protected void sequence_PropValPartialTriple(EObject context, PropValPartialTriple semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (propertyName=ResourceName superPropName=ResourceByName? addlPropInfo+=AdditionalPropertyInfo+) | 
	 *         (article=AnArticle? domain=ResourceIdentifier rangeResource=ResourceIdentifier propertyName=ResourceName) | 
	 *         annotationProperty=ResourceName
	 *     )
	 */
	protected void sequence_PropertyDeclaration(EObject context, PropertyDeclaration semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (propertyName=ResourceByName className=ResourceIdentifier)
	 */
	protected void sequence_PropertyOfClass(EObject context, PropertyOfClass semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.PROPERTY_OF_CLASS__PROPERTY_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.PROPERTY_OF_CLASS__PROPERTY_NAME));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.PROPERTY_OF_CLASS__CLASS_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.PROPERTY_OF_CLASS__CLASS_NAME));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getPropertyOfClassAccess().getPropertyNameResourceByNameParserRuleCall_0_0(), semanticObject.getPropertyName());
		feeder.accept(grammarAccess.getPropertyOfClassAccess().getClassNameResourceIdentifierParserRuleCall_2_0(), semanticObject.getClassName());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     expr=Expression
	 */
	protected void sequence_Query(EObject context, Query semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.QUERY__EXPR) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.QUERY__EXPR));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getQueryAccess().getExprExpressionParserRuleCall_1_0(), semanticObject.getExpr());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     classIdentifier=ResourceIdentifier
	 */
	protected void sequence_RangeType(EObject context, RangeType semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.RANGE_TYPE__CLASS_IDENTIFIER) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.RANGE_TYPE__CLASS_IDENTIFIER));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getRangeTypeAccess().getClassIdentifierResourceIdentifierParserRuleCall_0(), semanticObject.getClassIdentifier());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (single='single'? type=RangeType)
	 */
	protected void sequence_Range(EObject context, Range semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     name=[ResourceName|NAME]
	 */
	protected void sequence_ResourceByName(EObject context, ResourceByName semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.RESOURCE_BY_NAME__NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.RESOURCE_BY_NAME__NAME));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getResourceByNameAccess().getNameResourceNameNAMEParserRuleCall_0_1(), semanticObject.getName());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (propName=ResourceByName cond=Condition)
	 */
	protected void sequence_ResourceByRestriction(EObject context, ResourceByRestriction semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (propName=ResourceByName cond=Condition ((annType='alias' | annType='note') annContent=STRING)?)
	 */
	protected void sequence_ResourceByRestriction_ResourceIdentifier(EObject context, ResourceByRestriction semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (names+=ResourceIdentifier ((op+=',' | op+='or') names+=ResourceIdentifier)+ ((annType='alias' | annType='note') annContent=STRING)?)
	 */
	protected void sequence_ResourceIdentifier_UnionResource(EObject context, UnionResource semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (names+=ResourceName names+=ResourceName*)
	 */
	protected void sequence_ResourceList(EObject context, ResourceList semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (name=NAME ((annType+='alias' | annType+='note') annContent+=ContentList)*)
	 */
	protected void sequence_ResourceName(EObject context, ResourceName semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (name=NAME givens=ElementSet? ifs=ElementSet? thens=ElementSet)
	 */
	protected void sequence_Rule(EObject context, Rule semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (distinct='distinct'? (allVars='*' | varList=VariableList) expr=OrExpression (orderby='order by' orderList=OrderList)?)
	 */
	protected void sequence_SelectExpression(EObject context, SelectExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (restriction=ResourceIdentifier | restriction=ExistingResourceList)
	 */
	protected void sequence_SomeValuesCondition(EObject context, SomeValuesCondition semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((restricted=PropertyOfClass cond=SomeValuesCondition) | (className=ResourceIdentifier propertyName=ResourceByName cond=SomeValuesCondition))
	 */
	protected void sequence_SomeValuesFrom(EObject context, SomeValuesFrom semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (subclass=ResourceByName superclass=ResourceByName)
	 */
	protected void sequence_SubTypeOf(EObject context, SubTypeOf semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.SUB_TYPE_OF__SUBCLASS) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.SUB_TYPE_OF__SUBCLASS));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.SUB_TYPE_OF__SUPERCLASS) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.SUB_TYPE_OF__SUPERCLASS));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getSubTypeOfAccess().getSubclassResourceByNameParserRuleCall_0_0(), semanticObject.getSubclass());
		feeder.accept(grammarAccess.getSubTypeOfAccess().getSuperclassResourceByNameParserRuleCall_5_0(), semanticObject.getSuperclass());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (subj=ResourceByName hwPhr+=WithPhrase+)
	 */
	protected void sequence_SubjProp(EObject context, SubjProp semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     propertyName=ResourceByName
	 */
	protected void sequence_SymmetricalProperty(EObject context, SymmetricalProperty semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.SYMMETRICAL_PROPERTY__PROPERTY_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.SYMMETRICAL_PROPERTY__PROPERTY_NAME));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getSymmetricalPropertyAccess().getPropertyNameResourceByNameParserRuleCall_0_0(), semanticObject.getPropertyName());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     expr=Expression
	 */
	protected void sequence_Test(EObject context, Test semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.TEST__EXPR) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.TEST__EXPR));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getTestAccess().getExprExpressionParserRuleCall_1_0(), semanticObject.getExpr());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     propertyName=ResourceByName
	 */
	protected void sequence_TransitiveProperty(EObject context, TransitiveProperty semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.TRANSITIVE_PROPERTY__PROPERTY_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.TRANSITIVE_PROPERTY__PROPERTY_NAME));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getTransitivePropertyAccess().getPropertyNameResourceByNameParserRuleCall_0_0(), semanticObject.getPropertyName());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (instName=ResourceName type=TypedBNode)
	 */
	protected void sequence_TypeDeclaration(EObject context, TypeDeclaration semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.TYPE_DECLARATION__INST_NAME) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.TYPE_DECLARATION__INST_NAME));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.TYPE_DECLARATION__TYPE) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.TYPE_DECLARATION__TYPE));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getTypeDeclarationAccess().getInstNameResourceNameParserRuleCall_0_0(), semanticObject.getInstName());
		feeder.accept(grammarAccess.getTypeDeclarationAccess().getTypeTypedBNodeParserRuleCall_2_0(), semanticObject.getType());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     ((article=IndefiniteArticle classIdentifier=ResourceIdentifier) | (article='any' classIdentifier=ResourceIdentifier))
	 */
	protected void sequence_TypedBNode(EObject context, TypedBNode semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((op='!' | op='not' | op='only' | op=NotOnly | op='-') expr=PrimaryExpression)
	 */
	protected void sequence_UnaryOrPrimaryExpression(EObject context, UnaryOpExpression semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (names+=ResourceIdentifier ((op+=',' | op+='or') names+=ResourceIdentifier)+)
	 */
	protected void sequence_UnionResource(EObject context, UnionResource semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (udt=NAME restriction=DataTypeRestriction)
	 */
	protected void sequence_UserDefinedDataType(EObject context, UserDefinedDataType semanticObject) {
		if(errorAcceptor != null) {
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.USER_DEFINED_DATA_TYPE__UDT) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.USER_DEFINED_DATA_TYPE__UDT));
			if(transientValues.isValueTransient(semanticObject, SadlPackage.Literals.USER_DEFINED_DATA_TYPE__RESTRICTION) == ValueTransient.YES)
				errorAcceptor.accept(diagnosticProvider.createFeatureValueMissing(semanticObject, SadlPackage.Literals.USER_DEFINED_DATA_TYPE__RESTRICTION));
		}
		INodesForEObjectProvider nodes = createNodeProvider(semanticObject);
		SequenceFeeder feeder = createSequencerFeeder(semanticObject, nodes);
		feeder.accept(grammarAccess.getUserDefinedDataTypeAccess().getUdtNAMEParserRuleCall_0_0(), semanticObject.getUdt());
		feeder.accept(grammarAccess.getUserDefinedDataTypeAccess().getRestrictionDataTypeRestrictionParserRuleCall_7_0(), semanticObject.getRestriction());
		feeder.finish();
	}
	
	
	/**
	 * Constraint:
	 *     (explicitValues+=ExplicitValue explicitValues+=ExplicitValue*)
	 */
	protected void sequence_ValueRow(EObject context, ValueRow semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (row=ValueRow | (rows+=ValueRow rows+=ValueRow*))
	 */
	protected void sequence_ValueTable(EObject context, ValueTable semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (names+=ResourceName names+=ResourceName*)
	 */
	protected void sequence_VariableList(EObject context, VariableList semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (wps+=WithPhrase+ | wps+=WithPhrase+)
	 */
	protected void sequence_WithChain(EObject context, WithChain semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (
	 *         (propertyName=ResourceByName value=ExplicitValue) | 
	 *         (propertyName=ResourceByName value=IntervalValue) | 
	 *         (propertyName=ResourceByName (value=EmbeddedInstanceDeclaration | value=WithPhrase | value=LimitedExpression))
	 *     )
	 */
	protected void sequence_WithPhrase(EObject context, WithPhrase semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
}
