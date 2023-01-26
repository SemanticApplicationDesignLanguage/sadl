/************************************************************************
 * Copyright © 2022 - Natural Semantics, LLC. All Rights Reserved.
 *
 * Project: SADL
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
package com.naturalsemantics.sadl.jena;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.XSD;
import org.eclipse.emf.ecore.EObject;

import com.ge.research.sadl.jena.IntermediateFormTranslator;
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor;
import com.ge.research.sadl.jena.JenaBasedSadlModelValidator;
import com.ge.research.sadl.jena.TypeCheckInfo;
import com.ge.research.sadl.jena.UtilsForJena;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.Literal.LiteralType;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.I_IntermediateFormTranslator;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.ArgumentTypeValidationException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.naturalsemantics.sadl.processing.ISadlUnittedQuantityHandler;

public class SadlSimpleUnittedQuantityHanderForJena implements ISadlUnittedQuantityHandler {
	
	public static final String COMBINE_UNITS = "combineUnits";
	private I_IntermediateFormTranslator ifTranslator;
	
	class BuiltinElementAndUnits {
		BuiltinElement builtin;
		Node[] argUnits;
		
		public BuiltinElementAndUnits(BuiltinElement be, Node[] units) {
			builtin = be;
			argUnits = units;
		}
		
		BuiltinElement getBuiltinElement() {
			return builtin;
		}
		
		Node[] getArgUnits() {
			return argUnits;
		}
	}
	
	private List<BuiltinElementAndUnits> modifiedBuiltinElementsAndUnits;
	
	public SadlSimpleUnittedQuantityHanderForJena() {
		
	}

	/**
	 * Class to contain a new TripleElement and the GraphPatternElement after which it should
	 * eventually appear.
	 */
	class NewTripleAndPredecessor {
		private TripleElement newTriple;
		private GraphPatternElement predecessor;
		
		public NewTripleAndPredecessor(TripleElement tr, GraphPatternElement pr) {
			setNewTriple(tr);
			setPredecessor(pr);
		}

		TripleElement getNewTriple() {
			return newTriple;
		}

		void setNewTriple(TripleElement newTriple) {
			this.newTriple = newTriple;
		}

		GraphPatternElement getPredecessor() {
			return predecessor;
		}

		void setPredecessor(GraphPatternElement predecessor) {
			this.predecessor = predecessor;
		}
		
		public String toString() {
			StringBuffer sb = new StringBuffer();
			sb.append(getNewTriple().toDescriptiveString());
			sb.append(" after ");
			sb.append(getPredecessor().toDescriptiveString());
			return sb.toString();
		}
	}
	
	public SadlSimpleUnittedQuantityHanderForJena(IntermediateFormTranslator ift) {
		setIfTranslator(ift);
	}

	private I_IntermediateFormTranslator getIfTranslator() {
		return ifTranslator;
	}

	private void setIfTranslator(IntermediateFormTranslator ifTranslator) {
		this.ifTranslator = ifTranslator;
	}
	
	@Override
	public Rule checkRuleForNewUnittedQuantityCreation(Rule rule) throws TranslationException {
		// Using the modifiedBuiltinElementsAndUnits, add any combineUnits and there exists constructs to the rule
		//
		// For each BuiltinElementAndUnits in the list, we need to make sure there is a unit for the
		// returned value of the built-in. That returned-value unit can be paired with the returned value
		// variable, creating a new variable if necessary.
		// If the rule conclusion assigns a returned value in a TripleElement, and the range of the
		// triple predicate is UnittedQuantity, then that conclusion triple must be converted to a 
		// thereExists built-in.
		List<BuiltinElementAndUnits> modifiedBEs = getModifiedBuiltinElementsAndUnits();
		if (modifiedBEs != null && modifiedBEs.size() > 0) {
			Node returnedValue = null;
			GraphPatternElement usesReturnedValue = null;
			Node returnedValueUnit = null;
			boolean returnedValueUsageFound = false;
			Iterator<BuiltinElementAndUnits> keyItr = modifiedBEs.iterator();
			while (keyItr.hasNext()) {
				BuiltinElementAndUnits beau = keyItr.next();
				BuiltinElement be = beau.getBuiltinElement();
				if (be.getArguments().size() < 2) {
					continue;
				}
				if (be.getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME)) {
					// Find a GraphPatternElement that uses returned value as input
					usesReturnedValue = findGpeUsingReturnedValue(rule, be.getArguments().get(2));
					returnedValue = be.getArguments().get(0);
					returnedValueUnit = be.getArguments().get(1);
				}
				else {
					returnedValue = be.getArguments().get(be.getArguments().size() - 1);
					Node[] unitVars = beau.getArgUnits();
					if (unitVars != null) {
						// Find a GraphPatternElement that uses returned value as input
						usesReturnedValue = findGpeUsingReturnedValue(rule, returnedValue);
						if (usesReturnedValue instanceof BuiltinElement) {
							for (BuiltinElementAndUnits beauUsing : modifiedBEs) {
								if (beauUsing.getBuiltinElement().equals(usesReturnedValue)) {
									Node[] beauUsingUnits = beauUsing.getArgUnits();
									List<Node> args = ((BuiltinElement)usesReturnedValue).getArguments();
									for (int i = 0; i < args.size(); i++) {
										if (args.get(i).equals(returnedValue) && i < beauUsingUnits.length) {
											returnedValueUnit = beauUsingUnits[i];
											returnedValueUsageFound = true;
											break;
										}
									}
								}
							}
							if (returnedValueUnit != null) {
								if (be.getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
									if (unitVars.length == 2 && unitVars[0] != null && unitVars[1] != null) {
										BuiltinElement combineUnitsBe = new BuiltinElement();
										combineUnitsBe.setFuncName(COMBINE_UNITS);
										combineUnitsBe.addArgument(new Literal(be.getFuncName(), null, LiteralType.StringLiteral));
										combineUnitsBe.addArgument(unitVars[0]);
										combineUnitsBe.addArgument(unitVars[1]);
										combineUnitsBe.addArgument(returnedValueUnit);
										rule.getIfs().add(combineUnitsBe);
									}
								}
							}
							else {
								returnedValueUnit = unitVars[0];
							}
						}
						else if (usesReturnedValue instanceof TripleElement) {
							if (be.getUnittedQuantityStatus() != null && be.getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
								if (unitVars.length == 2 && unitVars[0] != null && unitVars[1] != null) {
									returnedValueUnit = new VariableNode(getIfTranslator().getNewVar());
									BuiltinElement combineUnitsBe = new BuiltinElement();
									combineUnitsBe.setFuncName(COMBINE_UNITS);
									combineUnitsBe.addArgument(new Literal(be.getFuncName(), null, LiteralType.StringLiteral));
									combineUnitsBe.addArgument(unitVars[0]);
									combineUnitsBe.addArgument(unitVars[1]);
									combineUnitsBe.addArgument(returnedValueUnit);
									rule.getIfs().add(combineUnitsBe);
								}
								else {
									returnedValueUnit = unitVars[0];								
								}
							}
							else if (be.getUnittedQuantityStatus() != null && 
									be.getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.SingleArgument) &&
									isUnittedQuantity(be.getArguments().get(0))) {
								returnedValueUnit = new VariableNode(getIfTranslator().getNewVar());
								BuiltinElement combineUnitsBe = new BuiltinElement();
								combineUnitsBe.setFuncName(COMBINE_UNITS);
								combineUnitsBe.addArgument(new Literal(be.getFuncName(), null, LiteralType.StringLiteral));
								combineUnitsBe.addArgument(be.getArguments().get(0));
								combineUnitsBe.addArgument(be.getArgumentTypes().get(0));
								combineUnitsBe.addArgument(returnedValueUnit);
								rule.getIfs().add(combineUnitsBe);
							}
							else {
								returnedValueUnit = unitVars[0];
							}			
						}
					}
				}
			}
			if (returnedValue != null && usesReturnedValue != null) {
				if (!returnedValueUsageFound) {
					// we need to go farther 
					Node nextReturnedValue = returnedValue;
					GraphPatternElement nextUsesReturnedValue = usesReturnedValue;					
					do {
						if (nextUsesReturnedValue instanceof BuiltinElement) {
							// for a BuiltinElement, 
							List<Node> args = ((BuiltinElement)nextUsesReturnedValue).getArguments();
							nextReturnedValue = args.get(args.size() - 1);	// wouldn't have a usesReturnedValue unless it had args
						}
						else if (nextUsesReturnedValue instanceof TripleElement) {
							if (((TripleElement)nextUsesReturnedValue).getSubject().equals(returnedValue)) {
								nextReturnedValue = ((TripleElement)nextUsesReturnedValue).getObject();
							}
							else {
								returnedValue = nextReturnedValue;
								nextReturnedValue = null;
							}
						}
						if (nextReturnedValue != null) {
							nextUsesReturnedValue = findGpeUsingReturnedValue(rule, nextReturnedValue);
						}
						else {
							usesReturnedValue = nextUsesReturnedValue;
							nextUsesReturnedValue = null;
						}
					} while (nextUsesReturnedValue != null);
				}
//				if (usesReturnedValue instanceof TripleElement) {
//					BuiltinElement thereExistsBE = new BuiltinElement();
//					thereExistsBE.setFuncName("thereExists");
//					Node pred = ((TripleElement)usesReturnedValue).getPredicate();
//					Object theModel = getIfTranslator().getTheModel();
//					if (!(theModel instanceof OntModel)) {
//						throw new TranslationException("The model was not a Jena OntModel as expected.");
//					}
//					OntProperty prop = ((OntModel)theModel).getOntProperty(pred.getURI());
//					NamedNode uQClass = null;
//					if (prop != null) {
//						OntResource rng = prop.getRange();
//						if (rng != null) {
//							uQClass = new NamedNode(rng.getURI());
//						}
//					}
//					if (uQClass == null) {
//						uQClass = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
//					}
//					uQClass.setNodeType(NodeType.ClassNode);
//					thereExistsBE.addArgument(uQClass);
//					NamedNode valuePred = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI);
//					valuePred.setNodeType(NodeType.DataTypeProperty);
//					thereExistsBE.addArgument(valuePred);
//					thereExistsBE.addArgument(returnedValue);
//					NamedNode unitPred = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI);
//					unitPred.setNodeType(NodeType.DataTypeProperty);
//					thereExistsBE.addArgument(unitPred);
//					thereExistsBE.addArgument(returnedValueUnit);
//					NamedNode plusNode = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_URI + "#Plus");
//					plusNode.setNodeType(NodeType.InstanceNode);
//					thereExistsBE.addArgument(plusNode);
//					thereExistsBE.addArgument(((TripleElement)usesReturnedValue).getSubject());
//					thereExistsBE.addArgument(((TripleElement)usesReturnedValue).getPredicate());
//					int thenIdx = rule.getThens().indexOf(usesReturnedValue);
//					rule.getThens().set(thenIdx, thereExistsBE);		
//				}
			}
		}
		return rule;
	}

	/**
	 * Method to find a GraphPatternElement that uses a given Node.
	 * For a BuiltinElement, this will be one that has the given Node as an input argument
	 * For a TripleElement, this will be one in the rule body that has the given Node as a subject or 
	 * one in the rule conclusion that has the given node as an subject or object
	 * @param rule
	 * @param returnedValue
	 * @return
	 */
	private GraphPatternElement findGpeUsingReturnedValue(Rule rule, Node returnedValue) {
		if (rule.getGivens() != null) {
			for (GraphPatternElement gpe : rule.getGivens()) {
				if (gpe instanceof BuiltinElement) {
					List<Node> args = ((BuiltinElement)gpe).getArguments();
					if (args.contains(returnedValue) && 
							!(args.indexOf(returnedValue) == args.size() - 1)) {
						return gpe;
					}
				}
				else if (gpe instanceof TripleElement) {
					if (((TripleElement)gpe).getSubject().equals(returnedValue)) {
						return gpe;
					}
				}
			}
		}
		if (rule.getIfs() != null) {
			for (GraphPatternElement gpe : rule.getIfs()) {
				if (gpe instanceof BuiltinElement) {
					List<Node> args = ((BuiltinElement)gpe).getArguments();
					if (args.contains(returnedValue) && 
							!(args.indexOf(returnedValue) == args.size() - 1)) {
						return gpe;
					}
				}
				else if (gpe instanceof TripleElement) {
					if (((TripleElement)gpe).getSubject().equals(returnedValue)) {
						return gpe;
					}
				}
			}
		}
		if (rule.getThens() != null) {
			for (GraphPatternElement gpe : rule.getThens()) {
				if (gpe instanceof BuiltinElement) {
					List<Node> args = ((BuiltinElement)gpe).getArguments();
					if (args.contains(returnedValue) && 
							!(args.indexOf(returnedValue) == args.size() - 1)) {
						return gpe;
					}
				}
				else if (gpe instanceof TripleElement) {
					if (((TripleElement)gpe).getSubject().equals(returnedValue)) {
						return gpe;
					}
					else if (((TripleElement)gpe).getObject().equals(returnedValue)) {
						return gpe;
					}
				}
			}
		}
		return null;
	}

	@Override
	public List<GraphPatternElement> expandUnittedQuantities(List<GraphPatternElement> patterns, BuiltinElement be,
			boolean isRuleThen) throws TranslationException {
		if (be.getArguments() == null || be.getArguments().size() == 0) {
			// nothing to expand
			return patterns;
		}
		List<Node> beargs = be.getArguments();
		// To get to here, there is at least 1 argument
		Node lhsArg = beargs.get(0);
		boolean lhsUQ = lhsArg != null ? getIfTranslator().isUnittedQuantity(lhsArg) : false;
		if (beargs.size() == 1) {
			// there is only one argument
			if (lhsUQ) {
				if (isList(lhsArg, be.getContext())) {
					if (be.isCanProcessUnittedQuantity() && be.isCanProcessListArgument()) {
						// UnittedQuantity argument can and must be handled by built-in
						return patterns;
					}
					throw new TranslationException("Neither built-in nor expansion of UnittedQuantities handles UnittedQuantity list input.");
				}
				return expandUnittedQuantitiesSingleArgument(patterns, be, isRuleThen);
			}
		}
		
		if (be.getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME)) {
			// This built-in captures something in the grammar that has explicit units, either
			//  1) a number followed by a unit, or
			//  2) an expression followed by a unit
			// Make sure that it has unitted quantity status UnitsNotSupported (args must be of type number, string)
			be.setUnittedQuantityStatus(BuiltinUnittedQuantityStatus.UnitsNotSupported);
			return patterns;
		}

		if (be.getArguments().size() != 2) {
			// all BuiltinElements of interest are binary at this point (before any 3rd output arg for a math operation is added later)
			return patterns;
		}
		int beIdx = patterns != null ? patterns.indexOf(be) : -1;
		Node rhsArg = beargs.get(1);
		boolean rhsUQ = rhsArg != null ? getIfTranslator().isUnittedQuantity(rhsArg) : false;
		Node lhsUnitValueNode = null;
		Node rhsUnitValueNode = null;
		if (lhsArg instanceof Literal && rhsArg instanceof Literal) {
			if (lhsUQ || rhsUQ) {
				if (lhsUQ) {
					lhsUnitValueNode = new Literal(((Literal)lhsArg).getUnits(), null, LiteralType.StringLiteral);
					((Literal)lhsArg).setUnits(null);
				}
				if (rhsUQ) {
					rhsUnitValueNode = new Literal(((Literal)rhsArg).getUnits(), null, LiteralType.StringLiteral);
					((Literal)rhsArg).setUnits(null);
				}
				try {
					Node valNode = new ProxyNode(be);
					Node unitNode;
					if (lhsUnitValueNode.equals(rhsUnitValueNode)) {
						unitNode = lhsUnitValueNode;
					}
					else {
						BuiltinElement combineUnitsBe = (BuiltinElement) getIfTranslator().getModelProcessor().createBinaryBuiltin(COMBINE_UNITS, 
								lhsUnitValueNode, rhsUnitValueNode, (EObject) be.getContext());
						unitNode = new ProxyNode(combineUnitsBe);
					}
					BuiltinElement newUQBi = (BuiltinElement) getIfTranslator().getModelProcessor().createBinaryBuiltin(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME, 
							valNode, unitNode, null);
					patterns.set(beIdx, newUQBi);
					addModifiedBuiltinElementAndUnits(be, lhsUnitValueNode, rhsUnitValueNode);
					return patterns;
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvalidTypeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (TranslationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			else {
				// 	if both args are simple Literals we don't want to do anything. 
				return patterns;
			}
		}

		if (lhsUQ || rhsUQ) {
			if (!isComparisonOperation(be) && !isCommonMathOperation(be) && !isOtherExpandableOperation(be)) {
				SadlModelProcessor mp = getIfTranslator().getModelProcessor();
				if (mp instanceof JenaBasedSadlModelProcessor) {
					((JenaBasedSadlModelProcessor)mp).addTypeCheckingError("Built-in function '" + be.getFuncName() + "' unexpectedly has UnittedQuantity inputs.", (EObject) be.getContext());
				}
				return patterns;
			}
		}

		boolean isBooleanReturn = false;
		BuiltinUnittedQuantityStatus bestatus = be.getUnittedQuantityStatus();
		if (bestatus == null) {
			if (getIfTranslator().isComparisonBuiltin(be.getFuncName())) {
				// All comparisons require the same units, no need to go to the built-in
				bestatus = BuiltinUnittedQuantityStatus.SameUnitsRequired;
				be.setUnittedQuantityStatus(bestatus);
				isBooleanReturn = true;
			}
			else {
				try {
					validateArgumentTypes(be, getIfTranslator().getTheModel(), be.getArgumentTypes());
					if (be.getUnittedQuantityStatus() == null) {
						// if the built-in isn't type-enabled, validation of arguments will not have supplied the unitted quantity status
						//	so see if it is a common built-in
						bestatus = getBuiltinUnittedQuantityStatusOfCommonBuiltins(be);
						if (bestatus != null) {
							be.setUnittedQuantityStatus(bestatus);
						}
						else {
							SadlModelProcessor mp = getIfTranslator().getModelProcessor();
							if (mp instanceof JenaBasedSadlModelProcessor) {
								((JenaBasedSadlModelProcessor)mp).addTypeCheckingError("UnittedQuantity input but function does not appear to handle UnittedQuantities", (EObject) be.getContext());
							}
						}
					}
					else {
						bestatus = be.getUnittedQuantityStatus();
					}
				} catch (UnittedQuantityHandlerException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ArgumentTypeValidationException e) {
					if (be.getContext() != null) {
						SadlModelProcessor mp = getIfTranslator().getModelProcessor();
						if (mp instanceof JenaBasedSadlModelProcessor) {
							((JenaBasedSadlModelProcessor)mp).addTypeCheckingError(e.getMessage(), (EObject) be.getContext());
						}
					}
					bestatus = be.getUnittedQuantityStatus();
				} catch (TranslationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		boolean noUQArgs = false;
		NamedNode valuePredNode = null;
		NamedNode unitPredNode = null;

		if (!lhsUQ && !rhsUQ) {
			noUQArgs = true;
			// check to see if this was originally a UQ-returning BuiltinElement but the arguments have been replaced 
			SadlModelProcessor processor = getIfTranslator().getModelProcessor();
			if (processor instanceof JenaBasedSadlModelProcessor) {
				try {
					TypeCheckInfo beTci = ((JenaBasedSadlModelProcessor)processor).getModelValidator().getType((EObject) be.getContext());
					if (beTci != null) {
						Node beType = beTci.getTypeCheckType();
						if (isUnittedQuantity(beType)) {
							Node unitNode = null;
							valuePredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI));
							valuePredNode.setNodeType(NodeType.DataTypeProperty);
							unitPredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI));
							unitPredNode.setNodeType(NodeType.DataTypeProperty);
							// need to find the unit associated with this built-in's return
							lhsUnitValueNode = getUnitFromPrevious(bestatus, patterns, lhsArg, valuePredNode, unitPredNode, null);
							rhsUnitValueNode = getUnitFromPrevious(bestatus, patterns, rhsArg, valuePredNode, unitPredNode, null);
							if (lhsUnitValueNode == null && rhsUnitValueNode == null) {
								// is this an type checking error?
								return patterns;
							}
							else {
								addModifiedBuiltinElementAndUnits(be, lhsUnitValueNode, rhsUnitValueNode);
							}
						}
						return patterns;
					}
					else {
						return patterns;
					}
				} catch (TranslationException e) {
					throw e;
				} catch (Exception e) {
					throw new TranslationException(e.getMessage(), e);
				}
			}
		}

		if (beIdx < 0) {
			// the BuiltinElement be should be in the patterns list--otherwise it is an unexpected error
			throw new TranslationException("Unexpected error: BuiltinElement is not in the list of GraphPatternElements");
		}
		else {
			// Get the predicate nodes for the value and unit of the UnittedQuantity
			if (valuePredNode == null) {
				valuePredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI));
				valuePredNode.setNodeType(NodeType.DataTypeProperty);
			}
			if (unitPredNode == null) {
				unitPredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI));
				unitPredNode.setNodeType(NodeType.DataTypeProperty);
			}

			Node newLhsArg = null;	// If we expand either or both sides, the results will be a new argument for that side of the builtin
			Node newRhsArg = null;
			
			Node lhsUnitsNode = null;
			Node rhsUnitsNode = null;
			
			List<NewTripleAndPredecessor> newTriples = 		// This is a List of NewTripleAndPredecessors containing the new TripleElement
					new ArrayList<NewTripleAndPredecessor>();	// and the GraphPatternElement it should follow in the completed patterns
			
			List<GraphPatternElement> unittedQuantityBuiltinsToBeRemoved =   // this is a list of any GraphPatternElements that should be removed
					new ArrayList<GraphPatternElement>();
			
			Map<Node, TripleElement> unitTriplesByValueNode = new HashMap<Node, TripleElement>();	// this keeps a record of what unit triples may need completing, indexed by the value of the UQ
			
			if (!noUQArgs) {
				for (GraphPatternElement gpe : patterns) {
					if (gpe instanceof TripleElement) {
						Node currentTripleObject = ((TripleElement)gpe).getObject();
						if (currentTripleObject != null && currentTripleObject.equals(lhsArg) &&
								currentTripleObject instanceof VariableNode && 	
								// last 2 conditionals are to make sure that property is an ObjectProperty, 
								//	cheaper than checking that type of variable is UnittedQuantity or subclass
								((TripleElement)gpe).getPredicate() instanceof NamedNode &&
								((NamedNode)((TripleElement)gpe).getPredicate()).getNodeType().equals(NodeType.ObjectProperty)) {
							// the lhsArg is obtained from a triple so need to expand to get value and unit
							GraphPatternElement lhsValueGpe = findValueGpeAlreadyPresent(patterns, lhsArg);
							if (lhsValueGpe == null) {
								newLhsArg = new VariableNode(getIfTranslator().getNewVar());
								((VariableNode)newLhsArg).setType(new NamedNode(XSD.decimal.getURI()));
								TripleElement addedTriple1 = new TripleElement(lhsArg, valuePredNode, newLhsArg);
								addNewTriple(newTriples, gpe, addedTriple1);
								// create incomplete triple for unit constraint
								TripleElement addedTriple2 = new TripleElement(lhsArg, unitPredNode, null);
								addNewTriple(newTriples, addedTriple1, addedTriple2);
								unitTriplesByValueNode.put(newLhsArg, addedTriple2);
							}
							else if (lhsValueGpe instanceof TripleElement){
								newLhsArg = ((TripleElement) lhsValueGpe).getObject();
							}
						}
						else if (currentTripleObject != null && currentTripleObject.equals(rhsArg) &&
								((TripleElement)gpe).getPredicate() instanceof NamedNode &&
								((NamedNode)((TripleElement)gpe).getPredicate()).getNodeType().equals(NodeType.ObjectProperty)) {
							// the rhsArg is obtained from a triple so need to expand to get value and unit
							GraphPatternElement rhsValueGpe = findValueGpeAlreadyPresent(patterns, rhsArg);
							if (rhsValueGpe == null) {
								newRhsArg = new VariableNode(getIfTranslator().getNewVar());
								((VariableNode)newRhsArg).setType(new NamedNode(XSD.decimal.getURI()));
								TripleElement addedTriple1 = new TripleElement(rhsArg, valuePredNode, newRhsArg);
								addNewTriple(newTriples, gpe, addedTriple1);
								// create incomplete triple for unit constraint
								TripleElement addedTriple2 = new TripleElement(rhsArg, unitPredNode, null);
								addNewTriple(newTriples, addedTriple1, addedTriple2);
								unitTriplesByValueNode.put(newRhsArg, addedTriple2);
							}
							else if (rhsValueGpe instanceof TripleElement){
								newRhsArg = ((TripleElement) rhsValueGpe).getObject();
							}
						} else if (currentTripleObject != null && getIfTranslator().isUnittedQuantity(currentTripleObject) && 
								!patternsExpandCurrentObject(patterns, currentTripleObject, valuePredNode, unitPredNode)) {	
							// This gpe is a TripleElement whose object is a UnittedQuantity which has not yet been expanded in the patterns
							VariableNode replacementArg = new VariableNode(getIfTranslator().getNewVar());
							TripleElement addedTriple1 = new TripleElement(currentTripleObject, valuePredNode, replacementArg);
							addNewTriple(newTriples,gpe, addedTriple1);
							// create incomplete triple for unit constraint
							TripleElement addedTriple2 = new TripleElement(currentTripleObject, unitPredNode, null);
							addNewTriple(newTriples, addedTriple1, addedTriple2);						
							unitTriplesByValueNode.put(replacementArg, addedTriple2);
							GraphPatternElement referencingGpe = getReferencingGpe(patterns, patterns.indexOf(gpe), currentTripleObject);
							if (referencingGpe != null) {
								replaceReferencingGpeArg(referencingGpe, currentTripleObject, replacementArg);
							}
						}
						else if (getIfTranslator().isUnittedQuantity(((TripleElement)gpe).getSubject()) && 
								!patternsExpandCurrentObject(patterns, ((TripleElement)gpe).getSubject(), valuePredNode, unitPredNode)) {
							// This gpe is a TripleElement whose subject is a UnittedQuantity which has not been expanded in the patterns 
							Node currentTripleSubject = ((TripleElement)gpe).getSubject();
							GraphPatternElement referencingGpe = getReferencingGpe(patterns, patterns.indexOf(gpe), currentTripleSubject);
							VariableNode replacementArg = new VariableNode(getIfTranslator().getNewVar());
							TripleElement addedTriple1 = new TripleElement(currentTripleSubject, valuePredNode, replacementArg);
							addNewTriple(newTriples, gpe, addedTriple1);
							// create incomplete triple for unit constraint
							TripleElement addedTriple2 = new TripleElement(currentTripleSubject, unitPredNode, null);
							addNewTriple(newTriples, addedTriple1, addedTriple2);
							unitTriplesByValueNode.put(replacementArg, addedTriple2);
							if (referencingGpe != null) {
								replaceReferencingGpeArg(referencingGpe, currentTripleSubject, replacementArg);
							}
						}
					}
					else if (gpe instanceof BuiltinElement) {
//						if (((BuiltinElement)gpe).getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME)) {
//							List<Node> uqArgs = ((BuiltinElement)gpe).getArguments();
//							if (uqArgs.size() == 2 && uqArgs.get(0).equals(lhsArg)) {
//								newLhsArg = uqArgs.get(0);
////								lhsUnits = SadlUtils.stripQuotes(uqArgs.get(1).toString());
//							}
//							else if (uqArgs.size() > 2 && uqArgs.get(2).equals(rhsArg)) {
//								newRhsArg = uqArgs.get(0);
////								rhsUnits = SadlUtils.stripQuotes(uqArgs.get(1).toString());
//							}
//							unittedQuantityBuiltinsToBeRemoved.add(gpe);
//						}
//						else 
						if (isCommonMathOperation((BuiltinElement)gpe)) {
							// see if the units of each side have already been specified
							lhsUnitValueNode = getUnitFromPrevious(bestatus, patterns, lhsArg, valuePredNode, unitPredNode, null);
							rhsUnitValueNode = getUnitFromPrevious(bestatus, patterns, rhsArg, valuePredNode, unitPredNode, null);

							// If the output of a math operation is a variable used in the
							//	BuiltinElement be, then the unit of the math operation output must also
							//	be the unit of the other comparison operation argument.
							String unit = getUnitFromMathOperation(patterns, isRuleThen, (BuiltinElement)gpe);
							if (unit != null) {
								Node unitValueNode = new Literal(unit, null, LiteralType.StringLiteral);
								List<Node> args = ((BuiltinElement)gpe).getArguments();
								boolean allArgsUnitsRemoved = true;
								for (int i = 0; i <= args.size() - 1; i++) {
									Node arg = args.get(i);
									if (arg instanceof Literal && ((Literal)arg).getUnits() != null) {
										((Literal)arg).setUnits(null);
									}
									else if (arg instanceof NamedNode) {
										if (arg instanceof VariableNode && 
												((BuiltinElement)gpe).getUnittedQuantityStatus() != null) {
											if (((BuiltinElement)gpe).getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)) {
												if (i == 0) {
													lhsUnitValueNode = unitValueNode;
													setMissingUnits(newTriples, lhsArg, lhsUnitValueNode);
												}
												else if (i == 1) {
													rhsUnitValueNode = unitValueNode;
													setMissingUnits(newTriples, rhsArg, rhsUnitValueNode);
												}
												if (arg.equals(lhsArg)) {
													lhsUQ = false;
												}
												else if (arg.equals(rhsArg)) {
													rhsUQ = false;
												}
											}
											else if (((BuiltinElement)gpe).getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
												if (i == 0) {
													lhsUnitValueNode = unitValueNode;
												}
												else if (i == 1) {
													rhsUnitValueNode = unitValueNode;
												}
												setMissingUnits(newTriples, arg, unitValueNode);
												if (i <= 1) {
													allArgsUnitsRemoved = false;
												}
												else {
													if (allArgsUnitsRemoved) {
														rhsUQ = false;
													}
												}
											}
											else {
												System.err.println("Unhandled BuiltinUnittedQuantityStatus: " + ((BuiltinElement)gpe).getUnittedQuantityStatus().toString());
											}
										}
										else {
											VariableNode newArg = new VariableNode(getIfTranslator().getNewVar());
											TripleElement addedTriple1 = new TripleElement(arg, valuePredNode, newArg);
											addNewTriple(newTriples, gpe, addedTriple1);
											args.set(args.indexOf(arg), newArg);
											TripleElement addedTriple2 = new TripleElement(arg, unitPredNode, new Literal(unit, null, LiteralType.StringLiteral));
											addNewTriple(newTriples, addedTriple1, addedTriple2);
										}
									}
								}
								if (args.size() > 2 && args.get(2).equals(lhsArg)) {
//									lhsUnits = unit;
								}
								else if (args.size() > 2 && args.get(2).equals(rhsArg)) {
//									rhsUnits = unit;
								}
							}
						}
						if (gpe.equals(be)) {
							if (lhsUQ && newLhsArg == null) {
								if (lhsArg instanceof VariableNode) {
									// this is a UQ but we haven't expanded it yet
									GraphPatternElement lhsValueGpe = findValueGpeAlreadyPresent(patterns, lhsArg);
									if (lhsValueGpe == null) {
										newLhsArg = new VariableNode(getIfTranslator().getNewVar());
										((VariableNode)newLhsArg).setType(new NamedNode(XSD.decimal.getURI()));
										TripleElement addedTriple1 = new TripleElement(lhsArg, valuePredNode, newLhsArg);
										addNewTriple(newTriples, gpe, addedTriple1);
										if (lhsUnitValueNode == null) {
											// create incom		plete triple for unit constraint
											TripleElement addedTriple2 = new TripleElement(lhsArg, unitPredNode, null);
											addNewTriple(newTriples, addedTriple1, addedTriple2);
											unitTriplesByValueNode.put(newLhsArg, addedTriple2);
										}
									}	
								}
								else if (lhsArg instanceof ProxyNode && ((ProxyNode)lhsArg).getProxyFor() instanceof BuiltinElement && 
										((BuiltinElement)((ProxyNode)lhsArg).getProxyFor()).getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME)) {
									 lhsUnitsNode = ((BuiltinElement)((ProxyNode)lhsArg).getProxyFor()).getArguments().get(1);
									 newLhsArg = ((BuiltinElement)((ProxyNode)lhsArg).getProxyFor()).getArguments().get(0);
								}
							}
							if (rhsUQ && newRhsArg == null) {
								if (rhsArg instanceof VariableNode) {
									GraphPatternElement rhsValueGpe = findValueGpeAlreadyPresent(patterns, rhsArg);
									if (rhsValueGpe == null) {
										newRhsArg = new VariableNode(getIfTranslator().getNewVar());
										((VariableNode)newRhsArg).setType(new NamedNode(XSD.decimal.getURI()));
										TripleElement addedTriple1 = new TripleElement(rhsArg, valuePredNode, newRhsArg);
										addNewTriple(newTriples, gpe, addedTriple1);
										if (rhsUnitValueNode == null) {
											// create incomplete triple for unit constraint
											TripleElement addedTriple2 = new TripleElement(rhsArg, unitPredNode, null);
											addNewTriple(newTriples, addedTriple1, addedTriple2);
											unitTriplesByValueNode.put(newRhsArg, addedTriple2);
										}
									}
								}
								else if (rhsArg instanceof ProxyNode && ((ProxyNode)rhsArg).getProxyFor() instanceof BuiltinElement && 
										((BuiltinElement)((ProxyNode)rhsArg).getProxyFor()).getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME)) {
									 rhsUnitsNode = ((BuiltinElement)((ProxyNode)rhsArg).getProxyFor()).getArguments().get(1);
									 newRhsArg = ((BuiltinElement)((ProxyNode)rhsArg).getProxyFor()).getArguments().get(0);
								}
							}
							if (newLhsArg != null) {
								be.getArguments().set(0, newLhsArg);
							}
							else if (lhsArg instanceof Literal && ((Literal)lhsArg).getUnits() != null) {
								lhsUnitsNode = new Literal(((Literal)lhsArg).getUnits(), null, LiteralType.StringLiteral);
								((Literal)lhsArg).setUnits(null);
							}
							if (newRhsArg != null) {
								be.getArguments().set(1,  newRhsArg);
							}
							else if (rhsArg instanceof Literal && ((Literal)rhsArg).getUnits() != null) {
								rhsUnitsNode = new Literal(((Literal)rhsArg).getUnits(), null, LiteralType.StringLiteral);
								((Literal)rhsArg).setUnits(null);
							}
							if (isCommonMathOperation((BuiltinElement)gpe) || isBooleanReturn) {
								// add a return type to be used later in the expansion process.
								List<Node> retTypes = be.getReturnTypes();
								if (retTypes == null) {
									retTypes = new ArrayList<Node>();
								}
								else {
									// clear old value for modified return value
									retTypes.clear();
								}
								if (isBooleanReturn) {
									retTypes.add(new NamedNode(XSD.xboolean.getURI()));
								}
								else {
									retTypes.add(new NamedNode(XSD.decimal.getURI()));
								}
								be.setReturnTypes(retTypes);
							}
						}
					}
				}
			}
			
			if (unitTriplesByValueNode.size() > 0) {
				Iterator<Node> utvn = unitTriplesByValueNode.keySet().iterator();
				while (utvn.hasNext()) {
					Node uqValueNode = utvn.next();
					TripleElement uqUnitTriple =  unitTriplesByValueNode.get(uqValueNode);
					Node uqNode = uqUnitTriple.getSubject();
					TripleElement lhsUnitTriple = unitTriplesByValueNode.get(newLhsArg);
					TripleElement rhsUnitTriple = unitTriplesByValueNode.get(newRhsArg);
					int i = 0;
				}
			}
			if (lhsUnitsNode != null) {
				if (!(lhsArg instanceof Literal)) {
					updateUnitTripleObject(patterns, newTriples, lhsArg, lhsUnitsNode);
				}
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired) && rhsArg instanceof VariableNode) {
					updateUnitTripleObject(patterns, newTriples, rhsArg, lhsUnitsNode);
				}
			}
			if (rhsUnitsNode != null) {
				if (!(rhsArg instanceof Literal)) {
					updateUnitTripleObject(patterns, newTriples, rhsArg, rhsUnitsNode);
				}
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired) && lhsArg instanceof VariableNode) {
					updateUnitTripleObject(patterns, newTriples, lhsArg, rhsUnitsNode);
				}
			}
			if (lhsUnitsNode == null && rhsUnitsNode == null) {
				if (lhsUnitValueNode == null) {
					lhsUnitValueNode = getUnitFromPrevious(bestatus, patterns, lhsArg, valuePredNode, unitPredNode, null);
				}
				if (rhsUnitValueNode == null && !noUQArgs) {
					rhsUnitValueNode = getUnitFromPrevious(bestatus, patterns, rhsArg, valuePredNode, unitPredNode, null);
				}
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)) {
					if (lhsUnitValueNode == null && rhsUnitValueNode == null) {
						// we don't know what the units are, but they need to be the same
						VariableNode unitVar = new VariableNode(getIfTranslator().getNewVar());
						updateUnitTripleObject(patterns, newTriples, lhsArg, unitVar);
						updateUnitTripleObject(patterns, newTriples, rhsArg, unitVar);
						lhsUnitValueNode = unitVar;
						rhsUnitValueNode = unitVar;
					}
					else if (lhsUnitValueNode != null) {
						updateUnitTripleObject(patterns, newTriples, rhsArg, lhsUnitValueNode);
					}
					else if (rhsUnitValueNode != null) {
						updateUnitTripleObject(patterns, newTriples, lhsArg, rhsUnitValueNode);
					}
					if (hasReturnedValue(be)) {
						addModifiedBuiltinElementAndUnits(be, lhsUnitValueNode, rhsUnitValueNode);
					}
				}
				else if (bestatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)){
					// this must be a math operation
					if (lhsUnitValueNode == null) {
						lhsUnitValueNode = new VariableNode(getIfTranslator().getNewVar());
					}
					if (rhsUQ && rhsUnitValueNode == null) {
						rhsUnitValueNode = new VariableNode(getIfTranslator().getNewVar());
					}
					// cache modified be with units of lhs, rhs
					if (lhsUnitValueNode != null) {
						if (!updateUnitTripleObject(patterns, newTriples, lhsArg, lhsUnitValueNode)) {
							// there isn't any triple to update. We need to create 							
						};
					}
					if (rhsUnitValueNode != null) {
						updateUnitTripleObject(patterns, newTriples, rhsArg, rhsUnitValueNode);
					}
					addModifiedBuiltinElementAndUnits(be, lhsUnitValueNode, rhsUnitValueNode);
				}
			}
			for (GraphPatternElement gpe : unittedQuantityBuiltinsToBeRemoved) {
				if (gpe instanceof BuiltinElement) {
					fillMissingUnitsInNewTriplesFromUnittedQuantityBuiltins(be, bestatus, newTriples, (BuiltinElement)gpe);
				}
				patterns.remove(gpe);
			}

			if (newTriples != null && newTriples.size() > 0) {
				Iterator<NewTripleAndPredecessor> keyItr = newTriples.iterator();
				while (keyItr.hasNext()) {
					NewTripleAndPredecessor npt = keyItr.next();
					boolean success = addNewTripleToPatterns(npt, newTriples, patterns, be);
				}
			}
		}
		return patterns;
	}

	private BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatusOfCommonBuiltins(BuiltinElement be) {
		SadlModelProcessor mp = getIfTranslator().getModelProcessor();
		if (mp instanceof JenaBasedSadlModelProcessor) {
			if (((JenaBasedSadlModelProcessor)mp).isComparisonBuiltin(be.getFuncName())) {
				return BuiltinUnittedQuantityStatus.SameUnitsRequired;
			}
			else if (((JenaBasedSadlModelProcessor)mp).isNumericOperator(be.getFuncName()) || 
					 ((JenaBasedSadlModelProcessor)mp).canBeNumericOperator(be.getFuncName())) {
				if (be.getFuncType().equals(BuiltinType.Minus) || be.getFuncType().equals(BuiltinType.Plus)) {
					return BuiltinUnittedQuantityStatus.SameUnitsRequired;
				}
				else if (be.getFuncType().equals(BuiltinType.Divide) || be.getFuncType().equals(BuiltinType.Multiply)) {
					return BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly;
				}
			}
		}
		return BuiltinUnittedQuantityStatus.UnitsNotSupported;
	}

	protected boolean isList(Node typenode, Object expr) {
		if (typenode != null && typenode instanceof NamedNode && ((NamedNode)typenode).isList()) {
			return true;
		}
		if (expr instanceof Declaration) {
			return isList(null, ((Declaration) expr).getType());
		}
		if (expr instanceof SadlSimpleTypeReference && ((SadlSimpleTypeReference) expr).isList()) {
			return true;
		}
		if (expr instanceof SubjHasProp) {
			return isList(null, ((SubjHasProp) expr).getLeft());
		}
		return false;
	}

	private List<GraphPatternElement> expandUnittedQuantitiesSingleArgument(List<GraphPatternElement> patterns,
			BuiltinElement be, boolean isRuleThen) throws TranslationException {
		// This should be called only if the single argument is known to be a UnittedQuantity
		Node singleArg = be.getArguments().get(0);
		SadlModelProcessor processor = getIfTranslator().getModelProcessor();
		NamedNode valuePredNode = null;
		NamedNode unitPredNode = null;
		Node singleArgUnitVar = null;
//		Node rhsUnitVar = null;
		BuiltinUnittedQuantityStatus bestatus = be.getUnittedQuantityStatus();
		if (bestatus ==  null) {
			bestatus = BuiltinUnittedQuantityStatus.UnitsNotSupported;
			be.setUnittedQuantityStatus(bestatus);
		}
		int beIdx = patterns != null ? patterns.indexOf(be) : -1;
		if (beIdx >= 0) {
			if (valuePredNode == null) {
				valuePredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI));
				valuePredNode.setNodeType(NodeType.DataTypeProperty);
			}
			if (unitPredNode == null) {
				unitPredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI));
				unitPredNode.setNodeType(NodeType.DataTypeProperty);
			}

			Node newSingleArg = null;
			String singleArgUnits = null;

			List<NewTripleAndPredecessor> newTriples = 		// This is a List of NewTripleAndPredecessors containing the new TripleElement
					new ArrayList<NewTripleAndPredecessor>();	// and the GraphPatternElement it should follow in the completed patterns

			List<GraphPatternElement> unittedQuantityBuiltinsToBeRemoved = new ArrayList<GraphPatternElement>();
			for (GraphPatternElement gpe : patterns) {
				if (gpe instanceof TripleElement) {
					Node currentTripleObject = ((TripleElement)gpe).getObject();
					if (currentTripleObject != null && currentTripleObject.equals(singleArg) &&
							currentTripleObject instanceof VariableNode && 	
							// last 2 conditionals are to make sure that property is an ObjectProperty, 
							//	cheaper than checking that type of variable is UnittedQuantity or subclass
							((TripleElement)gpe).getPredicate() instanceof NamedNode &&
							((NamedNode)((TripleElement)gpe).getPredicate()).getNodeType().equals(NodeType.ObjectProperty)) {
						//							((VariableNode)currentTripleObject).getType().getNamespace().equals(XSD.getURI())) {
						// the new arg is obtained from a triple so need to expand to get value and unit
						GraphPatternElement lhsValueGpe = findValueGpeAlreadyPresent(patterns, singleArg);
						if (lhsValueGpe == null) {
							newSingleArg = new VariableNode(getIfTranslator().getNewVar());
							((VariableNode)newSingleArg).setType(new NamedNode(XSD.decimal.getURI()));
							TripleElement addedTriple1 = new TripleElement(singleArg, valuePredNode, newSingleArg);
							addNewTriple(newTriples, gpe, addedTriple1);
							singleArgUnitVar = new VariableNode(getIfTranslator().getNewVar());
							((VariableNode)singleArgUnitVar).setType(new NamedNode(XSD.xstring.getURI()));
							TripleElement addedTriple2 = new TripleElement(singleArg, unitPredNode, singleArgUnitVar);
							addNewTriple(newTriples, addedTriple1, addedTriple2);
						}
						else if (lhsValueGpe instanceof GraphPatternElement){
							newSingleArg = ((TripleElement) lhsValueGpe).getObject();
						}
					} else if (currentTripleObject != null && getIfTranslator().isUnittedQuantity(currentTripleObject) && 
							!patternsExpandCurrentObject(patterns, currentTripleObject, valuePredNode, unitPredNode)) {	
						// This gpe is a TripleElement whose object is a UnittedQuantity which has not yet been expanded in the patterns
						GraphPatternElement referencingGpe = getReferencingGpe(patterns, patterns.indexOf(gpe), currentTripleObject);
						VariableNode replacementArg = new VariableNode(getIfTranslator().getNewVar());
						TripleElement addedTriple1 = new TripleElement(currentTripleObject, valuePredNode, replacementArg);
						addNewTriple(newTriples,gpe, addedTriple1);
						singleArgUnitVar = new VariableNode(getIfTranslator().getNewVar());
						TripleElement addedTriple2 = new TripleElement(currentTripleObject, unitPredNode, singleArgUnitVar);
						addNewTriple(newTriples, addedTriple1, addedTriple2);						
						if (referencingGpe != null) {
							replaceReferencingGpeArg(referencingGpe, currentTripleObject, replacementArg);
						}
					}
					else if (getIfTranslator().isUnittedQuantity(((TripleElement)gpe).getSubject()) && 
							!patternsExpandCurrentObject(patterns, ((TripleElement)gpe).getSubject(), valuePredNode, unitPredNode)) {
						// This gpe is a TripleElement whose subject is a UnittedQuantity which has not been expanded in the patterns 
						Node currentTripleSubject = ((TripleElement)gpe).getSubject();
						GraphPatternElement referencingGpe = getReferencingGpe(patterns, patterns.indexOf(gpe), currentTripleSubject);
						VariableNode replacementArg = new VariableNode(getIfTranslator().getNewVar());
						TripleElement addedTriple1 = new TripleElement(currentTripleSubject, valuePredNode, replacementArg);
						addNewTriple(newTriples, gpe, addedTriple1);
						// create incomplete triple for unit constraint
						TripleElement addedTriple2 = new TripleElement(currentTripleSubject, unitPredNode, null);
						addNewTriple(newTriples, addedTriple1, addedTriple2);
						if (referencingGpe != null) {
							replaceReferencingGpeArg(referencingGpe, currentTripleSubject, replacementArg);
						}
					}
				}
				else if (gpe instanceof BuiltinElement) {
					if (((BuiltinElement)gpe).getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME)) {
						List<Node> uqArgs = ((BuiltinElement)gpe).getArguments();
						if (uqArgs.size() > 2 && uqArgs.get(2).equals(singleArg)) {
							newSingleArg = uqArgs.get(0);
							singleArgUnits = SadlUtils.stripQuotes(uqArgs.get(1).toString());
						}
						unittedQuantityBuiltinsToBeRemoved.add(gpe);
					}
					else if (isCommonMathOperation((BuiltinElement)gpe)) {
						// If the output of a math operation is a variable used in the
						//	BuiltinElement be, then the unit of the math operation output must also
						//	be the unit of the other comparison operation argument.
						String unit = getUnitFromMathOperation(patterns, isRuleThen, (BuiltinElement)gpe);
						if (unit != null) {
							List<Node> args = ((BuiltinElement)gpe).getArguments();
							for (int i = 0; i <= args.size() - 1; i++) {
								Node arg = args.get(i);
								if (arg instanceof Literal && ((Literal)arg).getUnits() != null) {
									((Literal)arg).setUnits(null);
								}
								else if (arg instanceof NamedNode) {
									if (arg instanceof VariableNode && bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)) {
										setMissingUnits(newTriples, arg, new Literal(unit, null, LiteralType.StringLiteral));
										if (i == 0) {
											singleArgUnits = unit;
										}
									}
									else {
										VariableNode newArg = new VariableNode(getIfTranslator().getNewVar());
										TripleElement addedTriple1 = new TripleElement(arg, valuePredNode, newArg);
										addNewTriple(newTriples, gpe, addedTriple1);
										args.set(args.indexOf(arg), newArg);
										TripleElement addedTriple2 = new TripleElement(arg, unitPredNode, new Literal(unit, null, LiteralType.StringLiteral));
										addNewTriple(newTriples, addedTriple1, addedTriple2);
									}
								}
							}
							if (args.size() > 2 && args.get(2).equals(singleArg)) {
								singleArgUnits = unit;
							}
						}
					}
					if (gpe.equals(be)) {
						if (isComparisonOperation(be) || isCommonMathOperation(be) || isOtherExpandableOperation(be)) {
							if (newSingleArg != null) {
								be.getArguments().set(0, newSingleArg);
							}
							else if (singleArg instanceof Literal && ((Literal)singleArg).getUnits() != null) {
								singleArgUnits = ((Literal)singleArg).getUnits();
								((Literal)singleArg).setUnits(null);
							}
							// add a return type to be used later in the expansion process.
							List<Node> retTypes = be.getReturnTypes();
							if (retTypes == null) {
								retTypes = new ArrayList<Node>();
								retTypes.add(new NamedNode(XSD.decimal.getURI()));
								be.setReturnTypes(retTypes);
							}
						}
					}
				}
			}

			if (singleArgUnits != null) {
				Literal lhsUnuitLiteral = new Literal(singleArgUnits, null, LiteralType.StringLiteral);
				updateUnitTripleObject(patterns, newTriples, singleArg, lhsUnuitLiteral);
			}
			if (singleArgUnits == null) {
				if (singleArgUnitVar == null) {
					singleArgUnitVar = getUnitFromPrevious(bestatus, patterns, singleArg, valuePredNode, unitPredNode, null);
				}
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SingleArgument)) {
					if (hasReturnedValue(be)) {
						addModifiedBuiltinElementAndUnits(be, singleArgUnitVar, null);
					}
				}
			}
			for (GraphPatternElement gpe : unittedQuantityBuiltinsToBeRemoved) {
				if (gpe instanceof BuiltinElement) {
					fillMissingUnitsInNewTriplesFromUnittedQuantityBuiltins(be, bestatus, newTriples, (BuiltinElement)gpe);
				}
				patterns.remove(gpe);
			}

			if (newTriples != null && newTriples.size() > 0) {
				Iterator<NewTripleAndPredecessor> keyItr = newTriples.iterator();
				while (keyItr.hasNext()) {
					NewTripleAndPredecessor npt = keyItr.next();
					boolean success = addNewTripleToPatterns(npt, newTriples, patterns, be);
				}
			}
		}
		else {
			throw new TranslationException("Unexpected condition: BuiltinElement is not in the list of GraphPatternElements");
		}
		return patterns;
	}

	private boolean isUnittedQuantity(Node beType) {
		return getIfTranslator().isUnittedQuantity(beType);
	}

	private boolean hasReturnedValue(BuiltinElement be) {
		if (isComparisonOperation(be)) {
			return false;
		}
		return true;
	}

	/**
	 * Method to find and update a unit triple for a given subject with the given unit Node
	 * @param patterns 
	 * @param newTriples
	 * @param subj
	 * @param unitNode
	 * @return
	 */
	private boolean updateUnitTripleObject(List<GraphPatternElement> patterns, List<NewTripleAndPredecessor> newTriples, Node subj,
			Node unitNode) {
		Iterator<NewTripleAndPredecessor> itr = newTriples.iterator();
		while (itr.hasNext()) {
			NewTripleAndPredecessor ntp = itr.next();
			TripleElement tr = ntp.getNewTriple();
			if (tr.getSubject().equals(subj) && 
					tr.getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI)) {
				tr.setObject(unitNode);
				return true;
			}
		}
		for (GraphPatternElement gpe : patterns) {
			if (gpe instanceof TripleElement && 
					((TripleElement)gpe).getSubject().equals(subj) && 
					((TripleElement)gpe).getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI)) {
				((TripleElement)gpe).setObject(unitNode);
				return true;
			}
		}
		return false;
	}

	/**
	 * Method to add a new triple, encapsulated in a NewTripleAndPredecessor instance, to patterns. If the 
	 * predecessor isn't in patterns then it should be in the newTriples list so recurse
	 * @param npt -- the NewTripleAndPredecessor instance to be processed
	 * @param newTriples -- the list of NewTripleAndPredecessor instances being processed
	 * @param patterns -- the list of GraphPatternElements to which we are adding a new triple
	 * @param be -- the BuiltinElement for which we are expanding UnittedQuantities
	 * @return -- true if successful else false
	 * @throws TranslationException
	 */
	private boolean addNewTripleToPatterns(NewTripleAndPredecessor npt, List<NewTripleAndPredecessor> newTriples,
			List<GraphPatternElement> patterns, BuiltinElement be) throws TranslationException {
		TripleElement newTriple = npt.getNewTriple();
		GraphPatternElement predecessor = npt.getPredecessor();
		if (!patterns.contains(predecessor)) {
			// the predecessor isn't yet in patterns; try to find it in newTriples
			NewTripleAndPredecessor predecessorsPredecessor = findPredecessorNTP(newTriples, predecessor);
			if (predecessorsPredecessor != null) {
				boolean status = addNewTripleToPatterns(predecessorsPredecessor, newTriples, patterns, be);
				// now the predecessor should be in patterns
				if (!patterns.contains(predecessor)) {
					throw new TranslationException("Unable to find or add predecessor '" + predecessor.toDescriptiveString() + "' to patterns");
				}
			}
		}
		// patterns contains predecessor
		int predecessorIdx = patterns.indexOf(predecessor);
		int beIdx = patterns.indexOf(be);	// latest position
		if (predecessorIdx == beIdx) {
			// the new triple must be before the built-in
			patterns.add(beIdx, newTriple);
		}
		else if (predecessorIdx > beIdx) {
			patterns.add(beIdx, newTriple);
		}
		else {
			patterns.add(predecessorIdx + 1, newTriple);
		}	
		return true;
	}

	/**
	 * Method to find the predecessor for a new TripleElement
	 * @param newTriples -- the List of new triples and predecessors
	 * @param newTriple
	 * @return
	 */
	private NewTripleAndPredecessor findPredecessorNTP(List<NewTripleAndPredecessor> newTriples,
			GraphPatternElement newTriple) {
		for (NewTripleAndPredecessor ntp : newTriples) {
			TripleElement tr = ntp.getNewTriple();
			if (newTriple instanceof TripleElement &&
				((TripleElement)tr).getSubject().equals(((TripleElement)newTriple).getSubject()) &&
				((TripleElement)tr).getPredicate().equals(((TripleElement)newTriple).getPredicate()) &&
				(((TripleElement)tr).getObject() == null || ((TripleElement)tr).getObject().equals(((TripleElement)newTriple).getObject()))) {
				return ntp;
			}
		}
		return null;
	}

	/**
	 * Method to use a unittedQuantity BuiltinElement to fill in missing units in new triples
	 * @param be
	 * @param bestatus 
	 * @param newTriples
	 * @param uqBe
	 * @throws TranslationException
	 */
	private void fillMissingUnitsInNewTriplesFromUnittedQuantityBuiltins(BuiltinElement be, BuiltinUnittedQuantityStatus bestatus, List<NewTripleAndPredecessor> newTriples, BuiltinElement uqBe) throws TranslationException {
		List<Node> args = uqBe.getArguments();
		if (args == null || args.size() < 2) {
			throw new TranslationException("Unexpected unittedQuantity built-in without expected arguments");
		}
		Node arg1 = args.get(0);
		Node arg2 = args.get(1);
		setMissingUnits(newTriples, arg1, arg2);
		if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)) {
			for (Node arg : be.getArguments()) {
				setMissingUnits(newTriples, arg, arg2);
			}
		}
	}

	/**
	 * Method to fill in missing units
	 * @param newTriples
	 * @param subj
	 * @param unit
	 */
	private void setMissingUnits(List<NewTripleAndPredecessor> newTriples, Node subj, Node unit) {
		if (newTriples !=  null) {
			Node valueSubject = null;
			for (NewTripleAndPredecessor ntp : newTriples) {
				TripleElement nt = ntp.getNewTriple();
				if (nt.getSubject().equals(subj) && 
						nt.getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI)) {
					if (nt.getObject() == null) {
						nt.setObject(unit);
					}
					else if (!nt.getObject().equals(unit)){
						System.err.println("Encountered conflicting units: " + nt.toDescriptiveString());
					}
				}
				else if (nt.getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI) &&
						nt.getObject().equals(subj)) {
					valueSubject = nt.getSubject();
				}
				else if (nt.getSubject().equals(valueSubject) && 
						nt.getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI)) {
					if (nt.getObject() == null) {
						nt.setObject(unit);
					}
					else if (!nt.getObject().equals(unit)){
						System.err.println("Encountered conflicting units: " + nt.toDescriptiveString());
					}
				}
			}
		}
		
	}

	@Override
	public void addModifiedBuiltinElementAndUnits(BuiltinElement be, Node lhsUnitNode, Node rhsUnitNode) {
		if (modifiedBuiltinElementsAndUnits == null) {
			modifiedBuiltinElementsAndUnits = new ArrayList<BuiltinElementAndUnits>();
		}
		Node[] units = new Node[2];
		units[0] = lhsUnitNode;
		units[1] = rhsUnitNode;
		BuiltinElementAndUnits mbeau = new BuiltinElementAndUnits(be, units);
		modifiedBuiltinElementsAndUnits.add(mbeau);
	}
	
	/** Method to retrieve the remembered list of BuiltinElements and unit Nodes
	 * @return
	 */
	private List<BuiltinElementAndUnits> getModifiedBuiltinElementsAndUnits() {
		return modifiedBuiltinElementsAndUnits;
	}
	
	@Override
	public void reset() {
		clearModifiedBuiltinElementsAndUnits();
	}
	
	private void clearModifiedBuiltinElementsAndUnits() {
		if (modifiedBuiltinElementsAndUnits != null) {
			modifiedBuiltinElementsAndUnits.clear();
		}
	}

	/**
	 * Method to determine if the argument Node is already the subject of a TripleElement in patterns with "value" as the predicate
	 * @param patterns
	 * @param arg
	 * @return -- the TripleElement found, if any
	 */
	private GraphPatternElement findValueGpeAlreadyPresent(List<GraphPatternElement> patterns, Node arg) {
		Iterator<GraphPatternElement> gpeItr = patterns.iterator();
		while (gpeItr.hasNext()) {
			GraphPatternElement gpe = gpeItr.next();
			if (gpe instanceof TripleElement && 
					((TripleElement)gpe).getSubject() != null &&
					((TripleElement)gpe).getSubject().equals(arg)) {
				if (((TripleElement)gpe).getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI)) {
					return (TripleElement)gpe;
				}
			}
			else if (gpe instanceof BuiltinElement && 
					((BuiltinElement)gpe).getArguments() != null) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				if (args != null && args.size() > 0) {
					if (args.get(args.size() - 1).equals(arg)) {
						Object ctx = ((BuiltinElement)gpe).getContext();
						if (ctx instanceof BinaryOperation && 
								isDefinition(((BinaryOperation)ctx).getRight())) {
							return gpe;
						}
					}
				}
			}
		}
		return null;
	}
	
	private boolean isDefinition(EObject eobj) {
		I_IntermediateFormTranslator ift = getIfTranslator();
		if (ift instanceof IntermediateFormTranslator) {
			SadlModelProcessor mp = ((IntermediateFormTranslator)ift).getModelProcessor();
			return mp.isDeclaration(eobj);
		}
		return false;
	}

	/**
	 * Method to add a TripleElement to the Map as key and the GraphPatternElement that is to follow as value
	 * @param newTriples
	 * @param predecessorGpe
	 * @param addedTriple1
	 */
	private void addNewTriple(List<NewTripleAndPredecessor> newTriples, GraphPatternElement predecessorGpe, TripleElement tripleToAdd) {
		newTriples.add(new NewTripleAndPredecessor(tripleToAdd, predecessorGpe));
	}

	/**
	 * Method to find a {@link GraphPatternElement} in a list of such that references a given Node
	 * @param patterns
	 * @param startingAtIndex
	 * @param node
	 * @return
	 */
	private GraphPatternElement getReferencingGpe(List<GraphPatternElement> patterns, int startingAtIndex, Node node) {
		for (int i = startingAtIndex + 1; i < patterns.size(); i++) {
			GraphPatternElement gpe = patterns.get(i);
			if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getObject() != null &&
						((TripleElement)gpe).getObject().equals(node)) {
					return gpe;
				}
				else if (((TripleElement)gpe).getSubject().equals(node)) {
					return gpe;
				}
			}
			else if (gpe instanceof BuiltinElement) {
				for (Node arg : ((BuiltinElement)gpe).getArguments()) {
					if (arg.equals(node)) {
						return gpe;
					}
				}
			}
		}
		return null;
	}

	/**
	 * Method to determine if the patterns List already contains an expansion of the given UnittedQuantity node
	 * @param patterns
	 * @param node
	 * @param valuePredNode
	 * @param unitPredNode
	 * @return
	 */
	private boolean patternsExpandCurrentObject(List<GraphPatternElement> patterns, Node node, NamedNode valuePredNode, NamedNode unitPredNode) {
		boolean valueTripleFound = false;
		boolean unitTripleFound = false;
		for (GraphPatternElement gpe : patterns) {
			if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getPredicate().equals(valuePredNode) && ((TripleElement)gpe).getSubject().equals(node)) {
					valueTripleFound = true;
				}
				else if (((TripleElement)gpe).getPredicate().equals(unitPredNode) && ((TripleElement)gpe).getSubject().equals(node)) {
					unitTripleFound = true;
				}
				if (valueTripleFound && unitTripleFound) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Method to replace a Node in a GrpahPatternElement with a different Node
	 * @param gpe
	 * @param currentNode
	 * @param replacementNode
	 */
	private void replaceReferencingGpeArg(GraphPatternElement gpe, Node currentNode, VariableNode replacementNode) {
		if (gpe instanceof TripleElement) {
			if (((TripleElement)gpe).getObject().equals(currentNode)) {
				((TripleElement)gpe).setObject(replacementNode);
				return;
			}
			else if (((TripleElement)gpe).getSubject().equals(currentNode)) {
				((TripleElement)gpe).setSubject(replacementNode);
				return;
			}
		}
		else if (gpe instanceof BuiltinElement) {
			for (Node arg : ((BuiltinElement)gpe).getArguments()) {
				if (arg.equals(currentNode)) {
					List<Node> args = ((BuiltinElement)gpe).getArguments();
					args.set(args.indexOf(currentNode), replacementNode);
					return;
				}
			}
		}
	}

	/**
	 * Method to compute unit of result of common math operation for Literal arguments
	 * @param patterns 
	 * @param isRuleThen 
	 * @param be
	 * @return
	 */
	private String getUnitFromMathOperation(List<GraphPatternElement> patterns, boolean isRuleThen, BuiltinElement be) {
		String arg1Unit = null;
		String arg2Unit = null;
		if (be.getArguments().get(0) instanceof Literal &&
				((Literal)be.getArguments().get(0)).getUnits() != null) {
			arg1Unit = ((Literal)be.getArguments().get(0)).getUnits();
		}
		if (be.getArguments().size() > 1 && be.getArguments().get(1) instanceof Literal &&
				((Literal)be.getArguments().get(1)).getUnits() != null) {
			arg2Unit = ((Literal)be.getArguments().get(1)).getUnits();
		}
		if (arg1Unit == null && arg2Unit == null) {
			for (GraphPatternElement gpe : patterns) {
				if (gpe instanceof TripleElement && 
						((TripleElement)gpe).getPredicate().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI)) {
					if (((TripleElement)gpe).getSubject().equals(be.getArguments().get(0))) {
						if (((TripleElement)gpe).getObject() instanceof Literal) {
							arg1Unit = ((Literal)((TripleElement)gpe).getObject()).getValue().toString();
						}
					}
					if (((TripleElement)gpe).getSubject().equals(be.getArguments().get(1))) {
						if (((TripleElement)gpe).getObject() instanceof Literal) {
							arg2Unit = ((Literal)((TripleElement)gpe).getObject()).getValue().toString();
						}
					}
				}
			}
		}
		Object target = getIfTranslator().getTarget();
		if (target instanceof Rule) {
			if (arg1Unit == null) {
				if (((Rule)target).getGivens() != null && !((Rule)target).getGivens().equals(patterns)) {
					arg1Unit = getUnitFromMathOperation(((Rule)target).getGivens(), isRuleThen, be);
				}
				if (arg1Unit == null) {
					if (((Rule)target).getIfs() != null && !((Rule)target).getIfs().equals(patterns)) {
						arg1Unit = getUnitFromMathOperation(((Rule)target).getIfs(), isRuleThen, be);
					}
				}
			}
			if (arg2Unit == null && isRuleThen && !((Rule)target).getThens().equals(patterns)) {
				arg2Unit = getUnitFromMathOperation(((Rule)target).getThens(), isRuleThen, be);
				if (arg2Unit == null) {
					arg2Unit = getUnitFromMathOperation(((Rule)target).getThens(), isRuleThen, be);
				}
			}
		}
		if (be.getUnittedQuantityStatus() == BuiltinUnittedQuantityStatus.SameUnitsRequired) {
			if (arg1Unit != null) {
				return arg1Unit;
			}
			else if (arg2Unit != null) {
				return arg2Unit;
			}
			else {
				return null;
			}
		}
//		else if (arg1Unit != null && arg2Unit != null && 
//			(fn.equals("*") || fn.equals("/") || fn.equals("%") || fn.equals("^"))) {
//			return arg1Unit + fn + arg2Unit;
//		}
		return null;
	}

	/**
	 * Method to determine if BuiltinElement is a common math operation
	 * @param be
	 * @return
	 */
	private boolean isCommonMathOperation(BuiltinElement be) {
		return getIfTranslator().getModelProcessor().isNumericOperator(be.getFuncName());
	}

	/**
	 * Method to determine if BuiltinElement is a comparison operation
	 * @param be
	 * @return
	 */
	private boolean isComparisonOperation(BuiltinElement be) {
		return getIfTranslator().getModelProcessor().isComparisonOperator(be.getFuncName());
	}

	/**
	 * Method to determine if BuiltinElement is another UnittedQuantity-enabled operation
	 * @param be
	 * @return
	 */
	private boolean isOtherExpandableOperation(BuiltinElement be) {
		return getIfTranslator().getModelProcessor().isOtherExpandableOperator(be.getFuncName());
	}

	/**
	 * Method to find the unit of a variable from the patterns GraphPatternElement List.
	 * @param bestatus 
	 * @param patterns
	 * @param target
	 * @param unitPredNode
	 * @return
	 * @throws TranslationException 
	 */
	private Node getUnitFromPrevious(BuiltinUnittedQuantityStatus bestatus, List<GraphPatternElement> patterns, Node target,
			NamedNode valuePredNode, NamedNode unitPredNode, List<Node> targets) throws TranslationException {
		if (target instanceof Literal) {
			if (((Literal)target).getUnits() != null) {
				return new Literal(((Literal)target).getUnits(), null, LiteralType.StringLiteral);
			}
			return null;
		}
		for (GraphPatternElement gpe : patterns) {
			if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getSubject() != null && ((TripleElement)gpe).getSubject().equals(target) &&
						((TripleElement)gpe).getPredicate().equals(unitPredNode)) {
					Node unit = ((TripleElement)gpe).getObject();
					if (unit != null) {
						return unit;
					}
				}
			}
			else if (gpe instanceof BuiltinElement && 
					((BuiltinElement)gpe).getFuncName().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_BUILTIN_NAME) &&
					((BuiltinElement)gpe).getArguments().get(2).equals(target)) {
				return ((BuiltinElement)gpe).getArguments().get(1);
			}
		}
		Object container = getIfTranslator().getTarget();
		if (container instanceof Rule) {
			Rule rule = (Rule)container;
			if (rule.getGivens() != null) {
				for (GraphPatternElement gpe : rule.getGivens()) {
					if (gpe instanceof TripleElement) {
						if (((TripleElement)gpe).getSubject() != null && ((TripleElement)gpe).getSubject().equals(target) &&
								((TripleElement)gpe).getPredicate().equals(unitPredNode)) {
							Node unit = ((TripleElement)gpe).getObject();
							if (unit != null) {
								return unit;
							}
						}
					}
				}
			}
			if (rule.getIfs() != null) {
				for (GraphPatternElement gpe : rule.getIfs()) {
					if (gpe instanceof TripleElement) {
						if (((TripleElement)gpe).getSubject() != null && ((TripleElement)gpe).getSubject().equals(target) &&
								((TripleElement)gpe).getPredicate().equals(unitPredNode)) {
							Node unit = ((TripleElement)gpe).getObject();
							if (unit != null) {
								return unit;
							}
						}
					}
				}
			}
			if (rule.getThens() != null) {
				for (GraphPatternElement gpe : rule.getThens()) {
					if (gpe instanceof TripleElement) {
						if (((TripleElement)gpe).getSubject() != null && ((TripleElement)gpe).getSubject().equals(target) &&
								((TripleElement)gpe).getPredicate().equals(unitPredNode)) {
							Node unit = ((TripleElement)gpe).getObject();
							if (unit != null) {
								return unit;
							}
						}
					}
				}
			}
		}
		List<Node> thingsWithSameUnits = new ArrayList<Node>();
		for (GraphPatternElement gpe : patterns) {
			if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				if (args.contains(target)) {
					BuiltinUnittedQuantityStatus thisBeStatus = ((BuiltinElement)gpe).getUnittedQuantityStatus();
					if (thisBeStatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)){
						for (Node arg : args) {
							if (!(arg instanceof Literal) && !arg.equals(target) && (targets == null || !targets.contains(arg))) {
								thingsWithSameUnits.add(arg);
							}
						}
					}
					else if (thisBeStatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)){
						boolean leftOnly = isLeftOnly((BuiltinElement)gpe);
						if (leftOnly) {
							if (targets == null) {
								targets = new ArrayList<Node>();
							}
							if (!targets.contains(target)) {
								targets.add(target);
								Node indirect = getUnitFromPrevious(bestatus, patterns, args.get(0), valuePredNode, unitPredNode, targets);
								if (indirect != null) {
									return indirect;
								}
							}
						}
						else {
							return null;
						}
					}
					else if (thisBeStatus.equals(BuiltinUnittedQuantityStatus.SingleArgument)) {
						// ??
					}
				}
			}
			else if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getObject() != null && ((TripleElement)gpe).getObject().equals(target)
						&& ((TripleElement)gpe).getPredicate().equals(valuePredNode)) {
					if (targets == null || !targets.contains(((TripleElement)gpe).getSubject())) {
						thingsWithSameUnits.add(((TripleElement)gpe).getSubject());
					}
				}
			}
		}
		for (Node su : thingsWithSameUnits) {
			if (su instanceof Literal && ((Literal)su).getUnits() != null) {
				Node units = new Literal(((Literal) su).getUnits(), null, LiteralType.StringLiteral);
				return units;
			}
			if (targets == null) {
				targets = new ArrayList<Node>();
			}
			if (!targets.contains(target)) {
				targets.add(target);
				Node indirect = getUnitFromPrevious(bestatus, patterns, su, valuePredNode, unitPredNode, targets);
				if (indirect != null) {
					return indirect;
				}
			}
		}
		return null;
	}

	private boolean isLeftOnly(BuiltinElement be) throws TranslationException {
		Object ctx = be.getContext();
		if (ctx instanceof BinaryOperation) {
			SadlModelProcessor mp = getIfTranslator().getModelProcessor();
			if (mp instanceof JenaBasedSadlModelProcessor) {
				JenaBasedSadlModelValidator mv = ((JenaBasedSadlModelProcessor)mp).getModelValidator();
				try {
					TypeCheckInfo leftTci = mv.getType(((BinaryOperation)ctx).getLeft());
					TypeCheckInfo rightTci = mv.getType(((BinaryOperation)ctx).getRight());
					if (isUnittedQuantity(leftTci.getTypeCheckType()) && !isUnittedQuantity(rightTci.getTypeCheckType())) {
						return true;
					}
				} catch (TranslationException e) {
					throw e;
				} catch (Exception e) {
					throw new TranslationException(e.getMessage(), e);
				}
			}
		}
		return false;
	}

//	@Override
//	public
//	BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatus(BuiltinElement be, List<Node> argTypes) throws TranslationException {
//		if (be.getUnittedQuantityStatus() != null) {
//			return be.getUnittedQuantityStatus();
//		}
//		try {
//			ITranslator trans = getIfTranslator().getModelProcessor().getConfigMgr().getTranslator();
//			String biUri = SadlConstants.SADL_BUILTIN_FUNCTIONS_URI + "#" + trans.builtinTypeToString(be);
//			if (getIfTranslator() instanceof IntermediateFormTranslator) {
//				OntModel m = ((IntermediateFormTranslator)getIfTranslator()).getTheModel();
//				Individual subject = m.getIndividual(biUri);
//				if (subject != null) {
//					be.setFuncUri(biUri);
//					StmtIterator sitr = m.listStatements(subject, m.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI), (RDFNode)null);
//					if (sitr.hasNext()) {
//						String euri = sitr.nextStatement().getObject().toString();
//						be.setExternalUri(euri);
//						return trans.getBuiltinElementUQStatus(be);
//					}
//				}
////				else {
////					throw new TranslationException("Built-in '" + biUri + "' not found in semantic model.");
////				}
//			}
//			return trans.getBuiltinElementUQStatus(be);
//		} catch (IllegalArgumentException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (SecurityException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (ConfigurationException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		return BuiltinUnittedQuantityStatus.UnitsNotSupported;
//	}

	/**
	 * Method to compute the units of the output of a binary operation that generates units, e.g., multiply and divide.
	 * @param operator
	 * @param arg1
	 * @param arg2
	 * @return
	 */
	public static String combineUnits(org.apache.jena.graph.Node operator, org.apache.jena.graph.Node arg1,
			org.apache.jena.graph.Node arg2) {
		StringBuilder sb = new StringBuilder();
		Object v2 = arg1.getLiteralValue();
		sb.append(v2.toString());
		Object v1 = operator.getLiteralValue();
		sb.append(v1.toString());
		Object v3 = arg2.getLiteralValue();
		sb.append(v3.toString());
		return sb.toString();
	}

	@Override
	public void setIntermediateFormTranslator(I_IntermediateFormTranslator ift) {
		ifTranslator = ift;
	}

	@Override
	public String getUnittedQuantityInferenceHelperClassname() {
		return "com.naturalsemanticsllc.sadl.reasoner.JenaUnittedQuantityInferenceHelper";
	}

	@Override
	public Node computeBuiltinReturnType(BuiltinElement be, List<Node> argTypes) throws TranslationException {
		Node retType;
		try {
			retType = (Node) validateArgumentTypes(be, getIfTranslator().getTheModel(), argTypes);
		} catch (TranslationException e) {
			throw e;
		} catch (Exception e) {
			throw new TranslationException(e.getMessage(), e);
		}
		return retType;
	}

	@Override
	public Node validateArgumentTypes(BuiltinElement be, Object model, List<Node> argTypes) throws UnittedQuantityHandlerException, ConfigurationException, TranslationException {
		ITranslator trans = getIfTranslator().getModelProcessor().getConfigMgr().getTranslator();
		if (getIfTranslator() instanceof IntermediateFormTranslator) {
//			model = ((IntermediateFormTranslator)getIfTranslator()).getTheModel();
			if (be.getExternalUri() == null) {
				if (be.getFuncUri() == null) {
					String biUri = SadlConstants.SADL_BUILTIN_FUNCTIONS_URI + "#" + trans.builtinTypeToString(be);
					be.setFuncUri(biUri);
				}
				try {
					addExternalUri(be, (OntModel) model);
				}
				catch (UnittedQuantityHandlerException e) {
					// is this a implied operator?
				}
			}
			Node retType = trans.validateArgumentTypes(be, (OntModel)model, argTypes);
			if (retType != null) {
				be.addReturnType(retType);
				return retType;
			}
		}
		return null;
	}

	private void addExternalUri(BuiltinElement be, OntModel m) throws UnittedQuantityHandlerException {
		if (getIfTranslator() instanceof IntermediateFormTranslator) {
			Individual subject = m.getIndividual(be.getFuncUri());
			if (subject != null) {
				StmtIterator sitr = m.listStatements(subject, m.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI), (RDFNode)null);
				if (sitr.hasNext()) {
					String euri = sitr.nextStatement().getObject().toString();
					be.setExternalUri(euri);
				}
			}
			else {
				throw new UnittedQuantityHandlerException(be.getFuncUri() + " not found in model.");
			}
		}
	}

}
