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
import java.util.Iterator;
import java.util.List;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.XSD;
import org.eclipse.emf.ecore.EObject;

import com.ge.research.sadl.jena.IntermediateFormTranslator;
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor;
import com.ge.research.sadl.jena.JenaBasedSadlModelValidator;
import com.ge.research.sadl.jena.TypeCheckInfo;
import com.ge.research.sadl.jena.UtilsForJena;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.Literal.LiteralType;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.I_IntermediateFormTranslator;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.naturalsemantics.sadl.processing.ISadlUnittedQuantityHandler;

public class SadlSimpleUnittedQuantityHanderForJena implements ISadlUnittedQuantityHandler {
	
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
			Iterator<BuiltinElementAndUnits> keyItr = modifiedBEs.iterator();
			while (keyItr.hasNext()) {
				BuiltinElementAndUnits beau = keyItr.next();
				BuiltinElement be = beau.getBuiltinElement();
				if (be.getArguments().size() < 3) {
					continue;
				}
				Node returnedValue = be.getArguments().get(2);
				Node returnedValueUnit = null;
				Node[] unitVars = beau.getArgUnits();
				if (unitVars != null) {
					// Find a GraphPatternElement that uses returned value as input
					GraphPatternElement usesReturnedValue = findGpeUsingReturnedValue(rule, returnedValue);
					if (usesReturnedValue instanceof BuiltinElement) {
						for (BuiltinElementAndUnits beauUsing : modifiedBEs) {
							if (beauUsing.getBuiltinElement().equals(usesReturnedValue)) {
								Node[] beauUsingUnits = beauUsing.getArgUnits();
								List<Node> args = ((BuiltinElement)usesReturnedValue).getArguments();
								for (int i = 0; i < args.size(); i++) {
									if (args.get(i).equals(returnedValue) && i < beauUsingUnits.length) {
										returnedValueUnit = beauUsingUnits[i];
										break;
									}
								}
							}
						}
						if (returnedValueUnit == null) {
							// unit isn't in the modifiedBEs
							
						}
						if (returnedValueUnit != null) {
							if (be.getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
								if (unitVars.length == 2 && unitVars[0] != null && unitVars[1] != null) {
									BuiltinElement combineUnitsBe = new BuiltinElement();
									combineUnitsBe.setFuncName("combineUnits");
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
						if (be.getUnittedQuantityStatus().equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
							if (unitVars.length == 2 && unitVars[0] != null && unitVars[1] != null) {
								returnedValueUnit = new VariableNode(getIfTranslator().getNewVar());
								BuiltinElement combineUnitsBe = new BuiltinElement();
								combineUnitsBe.setFuncName("combineUnits");
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
						else {
							returnedValueUnit = unitVars[0];
						}
						
						BuiltinElement thereExistsBE = new BuiltinElement();
						thereExistsBE.setFuncName("thereExists");
						Node pred = ((TripleElement)usesReturnedValue).getPredicate();
						Object theModel = getIfTranslator().getTheModel();
						if (!(theModel instanceof OntModel)) {
							throw new TranslationException("The model was not a Jena OntModel as expected.");
						}
						OntProperty prop = ((OntModel)theModel).getOntProperty(pred.getURI());
						NamedNode uQClass = null;
						if (prop != null) {
							OntResource rng = prop.getRange();
							if (rng != null) {
								uQClass = new NamedNode(rng.getURI());
							}
						}
						if (uQClass == null) {
							uQClass = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
						}
						uQClass.setNodeType(NodeType.ClassNode);
						thereExistsBE.addArgument(uQClass);
						NamedNode valuePred = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI);
						valuePred.setNodeType(NodeType.DataTypeProperty);
						thereExistsBE.addArgument(valuePred);
						thereExistsBE.addArgument(returnedValue);
						NamedNode unitPred = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI);
						unitPred.setNodeType(NodeType.DataTypeProperty);
						thereExistsBE.addArgument(unitPred);
						thereExistsBE.addArgument(returnedValueUnit);
						NamedNode plusNode = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_URI + "#Plus");
						plusNode.setNodeType(NodeType.InstanceNode);
						thereExistsBE.addArgument(plusNode);
						thereExistsBE.addArgument(((TripleElement)usesReturnedValue).getSubject());
						thereExistsBE.addArgument(((TripleElement)usesReturnedValue).getPredicate());
						int thenIdx = rule.getThens().indexOf(usesReturnedValue);
						rule.getThens().set(thenIdx, thereExistsBE);

					}
				}
			}
		}
		return rule;
	}

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
		if (be.getArguments() == null) {
			return patterns;
		}
		List<Node> beargs = be.getArguments();
		Node lhsArg = beargs.size() > 0 ? beargs.get(0) : null;
		boolean lhsUQ = lhsArg != null ? getIfTranslator().isUnittedQuantity(lhsArg) : false;
		Node rhsArg = beargs.size() > 1 ? beargs.get(1) : null;
		boolean rhsUQ = rhsArg != null ? getIfTranslator().isUnittedQuantity(rhsArg) : false;
		boolean noUQArgs = false;
		NamedNode valuePredNode = null;
		NamedNode unitPredNode = null;
		Node lhsUnitVar = null;
		Node rhsUnitVar = null;

		if (be.getArguments() == null || be.getArguments().size() != 2) {
			// all BuiltinElements of interest are binary at this point (before any 3rd output arg for a math operation is added later)
			return patterns;
		}
		if (be.getArguments().get(0) instanceof Literal && be.getArguments().get(1) instanceof Literal) {
			// if both args are Literals we don't want to do anything. If the units aren't the same that will be detected later (?? verify with test case ??).
			return patterns;
		}

		if (!lhsUQ && !rhsUQ) {
			noUQArgs = true;
			// check to see if this was originally a UQ-returning BuiltinElement
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
							lhsUnitVar = getUnitFromPrevious(patterns, lhsArg, valuePredNode, unitPredNode, true);
							rhsUnitVar = getUnitFromPrevious(patterns, rhsArg, valuePredNode, unitPredNode, true);
							if (lhsUnitVar == null && rhsUnitVar == null) {
								return patterns;
							}
							else {
								addModifiedBuiltinElementAndUnits(be, lhsUnitVar, rhsUnitVar);
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

		BuiltinUnittedQuantityStatus bestatus = getBuiltinUnittedQuantityStatusForExpansion(be);
		if (bestatus == null) {
			// try to get it from the built-in itself
			bestatus = getBuiltinUnittedQuantityStatus(be);
		}
		be.setUnittedQuantityStatus(bestatus);
		int beIdx = patterns != null ? patterns.indexOf(be) : -1;
		if (beIdx >= 0) {
			// the BuiltinElement be should be in the patterns list--otherwise it is an unexpected error
			// if the BuiltinElement be is a comparison operator it must have 2 args
			if (be.getArguments().size() != 2) {
				throw new TranslationException("Unexpectd number of BuiltinElement arguments (" + be.getArguments().size() + ")");
			}
			if (valuePredNode == null) {
				valuePredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI));
				valuePredNode.setNodeType(NodeType.DataTypeProperty);
			}
			if (unitPredNode == null) {
				unitPredNode = UtilsForJena.validateNamedNode(getIfTranslator().getModelProcessor().getConfigMgr(), getIfTranslator().getModelProcessor().getModelName() + "#" , new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI));
				unitPredNode.setNodeType(NodeType.DataTypeProperty);
			}

			Node newLhsArg = null;
			Node newRhsArg = null;
			String lhsUnits = null;
			String rhsUnits = null;
			
			List<NewTripleAndPredecessor> newTriples = 		// This is a List of NewTripleAndPredecessors containing the new TripleElement
					new ArrayList<NewTripleAndPredecessor>();	// and the GraphPatternElement it should follow in the completed patterns
			
			List<GraphPatternElement> unittedQuantityBuiltinsToBeRemoved = new ArrayList<GraphPatternElement>();
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
	//							((VariableNode)currentTripleObject).getType().getNamespace().equals(XSD.getURI())) {
							// the lhsArg is obtained from a triple so need to expand to get value and unit
							TripleElement lhsValueTriple = findValueTripleAlreadyAdded(patterns, lhsArg);
							if (lhsValueTriple == null) {
								newLhsArg = new VariableNode(getIfTranslator().getNewVar());
								((VariableNode)newLhsArg).setType(new NamedNode(XSD.decimal.getURI()));
								TripleElement addedTriple1 = new TripleElement(lhsArg, valuePredNode, newLhsArg);
								addNewTriple(newTriples, gpe, addedTriple1);
								// create incomplete triple for unit constraint
								TripleElement addedTriple2 = new TripleElement(lhsArg, unitPredNode, null);
								addNewTriple(newTriples, addedTriple1, addedTriple2);
							}
							else {
								newLhsArg = lhsValueTriple.getObject();
							}
						}
						else if (currentTripleObject != null && currentTripleObject.equals(rhsArg)) {
							// the rhsArg is obtained from a triple so need to expand to get value and unit
							TripleElement rhsValueTriple = findValueTripleAlreadyAdded(patterns, rhsArg);
							if (rhsValueTriple == null) {
								newRhsArg = new VariableNode(getIfTranslator().getNewVar());
								((VariableNode)newRhsArg).setType(new NamedNode(XSD.decimal.getURI()));
								TripleElement addedTriple1 = new TripleElement(rhsArg, valuePredNode, newRhsArg);
								addNewTriple(newTriples, gpe, addedTriple1);
								// create incomplete triple for unit constraint
								TripleElement addedTriple2 = new TripleElement(rhsArg, unitPredNode, null);
								addNewTriple(newTriples, addedTriple1, addedTriple2);
							}
							else {
								newRhsArg = rhsValueTriple.getObject();
							}
						} else if (currentTripleObject != null && getIfTranslator().isUnittedQuantity(currentTripleObject) && 
								!patternsExpandCurrentObject(patterns, currentTripleObject, valuePredNode, unitPredNode)) {	
							// This gpe is a TripleElement whose object is a UnittedQuantity which has not yet been expanded in the patterns
							GraphPatternElement referencingGpe = getReferencingGpe(patterns, patterns.indexOf(gpe), currentTripleObject);
							VariableNode replacementArg = new VariableNode(getIfTranslator().getNewVar());
							TripleElement addedTriple1 = new TripleElement(currentTripleObject, valuePredNode, replacementArg);
							addNewTriple(newTriples,gpe, addedTriple1);
							// create incomplete triple for unit constraint
							TripleElement addedTriple2 = new TripleElement(currentTripleObject, unitPredNode, null);
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
							if (uqArgs.size() > 2 && uqArgs.get(2).equals(lhsArg)) {
								newLhsArg = uqArgs.get(0);
								lhsUnits = SadlUtils.stripQuotes(uqArgs.get(1).toString());
							}
							else if (uqArgs.size() > 2 && uqArgs.get(2).equals(rhsArg)) {
								newRhsArg = uqArgs.get(0);
								rhsUnits = SadlUtils.stripQuotes(uqArgs.get(1).toString());
							}
							unittedQuantityBuiltinsToBeRemoved.add(gpe);
						}
						else if (isCommonMathOperation((BuiltinElement)gpe)) {
							// If the output of a math operation is a variable used in the
							//	BuiltinElement be, then the unit of the math operation output must also
							//	be the unit of the other comparison operation argument.
							String unit = getUnitFromMathOperation(patterns, (BuiltinElement)gpe);
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
												lhsUnits = unit;
											}
											else if (i == 1) {
												rhsUnits = unit;
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
									lhsUnits = unit;
								}
								else if (args.size() > 2 && args.get(2).equals(rhsArg)) {
									rhsUnits = unit;
								}
							}
						}
						if (gpe.equals(be)) {
							if (isComparisonOperation(be) || isCommonMathOperation(be) || isOtherExpandableOperation(be)) {
								if (newLhsArg != null) {
									be.getArguments().set(0, newLhsArg);
								}
								else if (lhsArg instanceof Literal && ((Literal)lhsArg).getUnits() != null) {
									lhsUnits = ((Literal)lhsArg).getUnits();
									((Literal)lhsArg).setUnits(null);
								}
								if (newRhsArg != null) {
									be.getArguments().set(1,  newRhsArg);
								}
								else if (rhsArg instanceof Literal && ((Literal)rhsArg).getUnits() != null) {
									rhsUnits = ((Literal)rhsArg).getUnits();
									((Literal)rhsArg).setUnits(null);
								}
								// add a return type to be used later in the expansion process.
								List<Node> retTypes = be.getReturnTypes();
								if (retTypes == null) {
									retTypes = new ArrayList<Node>();
								}
								retTypes.add(new NamedNode(XSD.decimal.getURI()));
								be.setReturnTypes(retTypes);
							}
						}
					}
				}
			}
			if (lhsUnits != null) {
				Literal lhsUnuitLiteral = new Literal(lhsUnits, null, LiteralType.StringLiteral);
				updateUnitTripleObject(patterns, newTriples, lhsArg, lhsUnuitLiteral);
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired) && rhsArg instanceof VariableNode) {
					updateUnitTripleObject(patterns, newTriples, rhsArg, lhsUnuitLiteral);
				}
			}
			if (rhsUnits != null) {
				Literal rhsUnuitLiteral = new Literal(rhsUnits, null, LiteralType.StringLiteral);
				updateUnitTripleObject(patterns, newTriples, rhsArg, new Literal(rhsUnits, null, LiteralType.StringLiteral));
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired) && lhsArg instanceof VariableNode) {
					updateUnitTripleObject(patterns, newTriples, lhsArg, rhsUnuitLiteral);
				}
			}
			if (lhsUnits == null && rhsUnits == null) {
				if (lhsUnitVar == null) {
					lhsUnitVar = getUnitFromPrevious(patterns, lhsArg, valuePredNode, unitPredNode, true);
				}
				if (rhsUnitVar == null && !noUQArgs) {
					rhsUnitVar = getUnitFromPrevious(patterns, rhsArg, valuePredNode, unitPredNode, true);
				}
				if (bestatus.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)) {
					if (lhsUnitVar == null && rhsUnitVar == null) {
						// we don't know what the units are, but they need to be the same
						VariableNode unitVar = new VariableNode(getIfTranslator().getNewVar());
						updateUnitTripleObject(patterns, newTriples, lhsArg, unitVar);
						updateUnitTripleObject(patterns, newTriples, rhsArg, unitVar);
						lhsUnitVar = unitVar;
						rhsUnitVar = unitVar;
					}
					else if (lhsUnitVar != null) {
						updateUnitTripleObject(patterns, newTriples, rhsArg, lhsUnitVar);
					}
					else if (rhsUnitVar != null) {
						updateUnitTripleObject(patterns, newTriples, lhsArg, rhsUnitVar);
					}
					if (hasReturnedValue(be)) {
						addModifiedBuiltinElementAndUnits(be, lhsUnitVar, rhsUnitVar);
					}
				}
				else if (bestatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)){
					// this must be a math operation
					if (lhsUnitVar == null) {
						lhsUnitVar = new VariableNode(getIfTranslator().getNewVar());
					}
					if (rhsUQ && rhsUnitVar == null) {
						rhsUnitVar = new VariableNode(getIfTranslator().getNewVar());
					}
					// cache modified be with units of lhs, rhs
					if (lhsUnitVar != null) {
						if (!updateUnitTripleObject(patterns, newTriples, lhsArg, lhsUnitVar)) {
							// there isn't any triple to update. We need to create 							
						};
					}
					if (rhsUnitVar != null) {
						updateUnitTripleObject(patterns, newTriples, rhsArg, rhsUnitVar);
					}
					addModifiedBuiltinElementAndUnits(be, lhsUnitVar, rhsUnitVar);
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
		if (predecessorIdx > beIdx) {
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

	/**
	 * Method to add a BuiltinElement with the LHS unit Node and RHS unit Node for latter retrieval
	 * @param be -- the BuiltinElement to remember
	 * @param lhsUnitNode -- its LHS unit Node
	 * @param rhsUnitNode -- its RHS unit Node
	 */
	private void addModifiedBuiltinElementAndUnits(BuiltinElement be, Node lhsUnitNode, Node rhsUnitNode) {
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
	private TripleElement findValueTripleAlreadyAdded(List<GraphPatternElement> patterns, Node arg) {
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
		}
		return null;
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
	 * @param be
	 * @return
	 */
	private String getUnitFromMathOperation(List<GraphPatternElement> patterns, BuiltinElement be) {
		String fn = be.getFuncName();
		String arg1Unit = null;
		String arg2Unit = null;
		if (be.getArguments().get(0) instanceof Literal &&
				((Literal)be.getArguments().get(0)).getUnits() != null) {
			arg1Unit = ((Literal)be.getArguments().get(0)).getUnits();
		}
		if (be.getArguments().get(1) instanceof Literal &&
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
		if (fn.equals("+") || fn.equals("-") ) {
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
		else if (arg1Unit != null && arg2Unit != null && 
			(fn.equals("*") || fn.equals("/") || fn.equals("%") || fn.equals("^"))) {
			return arg1Unit + fn + arg2Unit;
		}
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
	 * @param patterns
	 * @param target
	 * @param unitPredNode
	 * @return
	 * @throws TranslationException 
	 */
	private Node getUnitFromPrevious(List<GraphPatternElement> patterns, Node target,
			NamedNode valuePredNode, NamedNode unitPredNode, boolean firstCall) throws TranslationException {
		if (target instanceof Literal) {
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
		}
		List<Node> thingsWithSameUnits = new ArrayList<Node>();
		for (GraphPatternElement gpe : patterns) {
			if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				if (args.contains(target)) {
					if (getBuiltinUnittedQuantityStatus((BuiltinElement) gpe).equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)){
						for (Node arg : args) {
							if (!arg.equals(target)) {
								thingsWithSameUnits.add(arg);
							}
						}
					}
					else if (getBuiltinUnittedQuantityStatus((BuiltinElement) gpe).equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)){
						boolean leftOnly = isLeftOnly((BuiltinElement)gpe);
						if (leftOnly) {
							if (firstCall) {
								Node indirect = getUnitFromPrevious(patterns, args.get(0), valuePredNode, unitPredNode, false);
								if (indirect != null) {
									return indirect;
								}
							}
						}
						else {
							return null;
						}
					}
				}
			}
			else if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getObject() != null && ((TripleElement)gpe).getObject().equals(target)
						&& ((TripleElement)gpe).getPredicate().equals(valuePredNode)) {
					thingsWithSameUnits.add(((TripleElement)gpe).getSubject());
				}
			}
		}
		for (Node su : thingsWithSameUnits) {
			Node indirect = getUnitFromPrevious(patterns, su, valuePredNode, unitPredNode, false);
			if (indirect != null) {
				return indirect;
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

	@Override
	public
	BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatus(BuiltinElement be) throws TranslationException {
		if (be.getUnittedQuantityStatus() != null) {
			return be.getUnittedQuantityStatus();
		}
		try {
			ITranslator trans = getIfTranslator().getModelProcessor().getConfigMgr().getTranslator();
			String biUri = SadlConstants.SADL_BUILTIN_FUNCTIONS_URI + "#" + trans.builtinTypeToString(be);
			if (getIfTranslator() instanceof IntermediateFormTranslator) {
				OntModel m = ((IntermediateFormTranslator)getIfTranslator()).getTheModel();
				Individual subject = m.getIndividual(biUri);
				if (subject != null) {
					be.setFuncUri(biUri);
					StmtIterator sitr = m.listStatements(subject, m.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI), (RDFNode)null);
					if (sitr.hasNext()) {
						String euri = sitr.nextStatement().getObject().toString();
						be.setExternalUri(euri);
						return trans.getBuiltinElementUQStatus(be);
					}
				}
//				else {
//					throw new TranslationException("Built-in '" + biUri + "' not found in semantic model.");
//				}
			}
			return trans.getBuiltinElementUQStatus(be);
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return BuiltinUnittedQuantityStatus.UnitsNotSupported;
	}

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
	public Object computeBuiltinReturnType(BuiltinElement be, List<Object> argTcis) throws TranslationException {
		be.setUnittedQuantityStatus(getBuiltinUnittedQuantityStatus(be));
		BuiltinUnittedQuantityStatus bes = be.getUnittedQuantityStatus();
		if (bes != null) {
			if (bes.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired) || 
					bes.equals(BuiltinUnittedQuantityStatus.LeftUnitsOnly) ||
					bes.equals(BuiltinUnittedQuantityStatus.SingleArgument)) {
				if (argTcis != null && argTcis.size() > 0 && argTcis.get(0) instanceof TypeCheckInfo) {
					TypeCheckInfo fctTci = new TypeCheckInfo(new ConceptName(be.getFuncUri(), ConceptType.INDIVIDUAL));
					fctTci.setTypeCheckType(((TypeCheckInfo)argTcis.get(0)).getTypeCheckType());
					fctTci.setTypeToExprRelationship(TypeCheckInfo.FUNCTION_RETURN);
					return fctTci;
				}
			}
			else if (bes.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
				if (leftOnlyUQ(be, argTcis)) {
					if (argTcis != null && argTcis.size() > 0 && argTcis.get(0) instanceof TypeCheckInfo) {
						TypeCheckInfo fctTci = new TypeCheckInfo(new ConceptName(be.getFuncUri(), ConceptType.INDIVIDUAL));
						fctTci.setTypeCheckType(((TypeCheckInfo)argTcis.get(0)).getTypeCheckType());
						fctTci.setTypeToExprRelationship(TypeCheckInfo.FUNCTION_RETURN);
						return fctTci;
					}
				}
				else {
					TypeCheckInfo fctTci = new TypeCheckInfo(new ConceptName(be.getFuncUri(), ConceptType.INDIVIDUAL));
					fctTci.setTypeCheckType(new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI, NodeType.ClassNode));
					fctTci.setTypeToExprRelationship(TypeCheckInfo.FUNCTION_RETURN);
					return fctTci;
					}
			}
		}
		return null;
	}

	/**
	 * Method to determine if only the left-most (first) argument is a UnittedQuantity
	 * @param be
	 * @param argTcis
	 * @return
	 */
	private boolean leftOnlyUQ(BuiltinElement be, List<Object> argTcis) {
		boolean leftOnly = true;
		for (int i = 0; i < argTcis.size(); i++) {
			Object tci = argTcis.get(i);
			if (tci instanceof TypeCheckInfo) {
				if (((TypeCheckInfo)tci).getTypeCheckType().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
					if (i > 0) {
						leftOnly = false;
						break;
					}
				}
			}
		}
		return leftOnly;
	}

}
