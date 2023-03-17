/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.jena.reasoner.builtin;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.jena.reasoner.builtin.utils.UnittedQuantity;
import com.ge.research.sadl.jena.reasoner.builtin.utils.Utils;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.TranslationException;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;
import com.naturalsemanticsllc.sadl.reasoner.TypedBuiltinFunctionException;

/**

 */
public class Product extends org.apache.jena.reasoner.rulesys.builtins.Product implements ITypedBaseBuiltin {

	private int argLength = 0;

    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return argLength;
    }
    
    /**
     * This method is invoked when the builtin is called in a rule body.
     * @param args the array of argument values for the builtin, this is an array 
     * of Nodes, some of which may be Node_RuleVariables.
     * @param length the length of the argument list, may be less than the length of the args array
     * for some rule engines
     * @param context an execution context giving access to other relevant data
     * @return return true if the buildin predicate is deemed to have succeeded in
     * the current environment
     */
    public boolean bodyCall(Node[] args, int length, RuleContext context) {
        checkArgs(length, context);
        BindingEnvironment env = context.getEnv();
        /*
         * There are three cases to consider:
         * 1. The arguments (except the last) represent a graph pattern and the nodes that match that graph
         *    pattern is a list of items to be multiplied together. The product is assigned to the last argument,
         *    which must be a variable.
         * 2. There are three arguments, the first two of which are to be multiplied together and assigned
         *    to the third, which must be a variable
         * 3. The first argument is a list of items to be multiplied together, the product of which is to
         *    be assigned to the second argument, which must be a variable.
         *    
         * In any of these three cases, the elements of the list may be instances of the SadlImplicitModel's
         * UnittedQuantity, in which case the result is an instance of a UnittedQuantity whose value is the
         * product of the values of the multiplicands and the unit is the product of the units.
         * 
         */
    	if (GeUtils.isGraphPatternInput(this, args, length, context)) {
        	Node[] nodes = GeUtils.matchNonSparqlPattern(this, args, length, true, context);
        	Number nProd = Long.valueOf(1);
        	Node prod = null;
        	List<Node> nodeLst = Arrays.asList(nodes);
        	try {
				if (Utils.listContainsUnittedQuantity(nodeLst, context)) {
					prod = createUnittedQuantityProduct(context, nProd, nodeLst);
				}
				else {
					nProd = multiplyList(nProd, nodeLst, context);
					if (nProd instanceof Float || nProd instanceof Double) {
						prod = Util.makeDoubleNode(nProd.doubleValue());
					}
					else if (nProd instanceof Integer) {
						prod = Util.makeIntNode(nProd.intValue());
					}
					else {
						prod = Util.makeLongNode(nProd.longValue());
					}
				}
			} catch (TypedBuiltinFunctionException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
        	return env.bind(args[length - 1], prod);
        }
        else if (length == 3) {
        	if (Utils.isUnittedQuantity(getArg(0, args, context), context) ||
					Utils.isUnittedQuantity(getArg(1, args, context), context)) {
				Number nProd = Long.valueOf(1);
				List<Node> nodeLst = new ArrayList<Node>();
				nodeLst.add(getArg(0, args, context));
				nodeLst.add(getArg(1, args, context));
				Node prod = createUnittedQuantityProduct(context, nProd, nodeLst);
			   	return env.bind(args[length - 1], prod);
			}
			else {
				// this is just the normal case implemented by HP Labs (standard Jena) (except Jena doesn't handle
				//	BigDecimal so we convert BigDecimal to Double before making the call)
				if (getArg(0, args, context).isURI()) {
					throw new BuiltinException(this, context, "First argument to product is a URI: " + getArg(0, args, context).getURI());
				}
				if (getArg(1, args, context).isURI()) {
					throw new BuiltinException(this, context, "Second argument to product is a URI: " + getArg(1, args, context).getURI());
				}
				for (int i = 0; i < args.length - 1; i++) {
			        Node n = getArg(i, args, context);
			        if (n.isLiteral()) {
			        	Object v;
			        	if (n.getLiteralDatatypeURI().equals(XSD.decimal.getURI())) {
			        		v = Double.parseDouble(n.getLiteralValue().toString());
							args[i] = Util.makeDoubleNode((double) v);
			        	}
			        	else {
			        		v = n.getLiteralValue();
			        	}
			        	if (v instanceof Number) {
			        		if (v instanceof BigDecimal) {
			        			if (((BigDecimal)v).scale() > 0) {
			        				Double d = ((BigDecimal)v).doubleValue();
			        				args[i] = Util.makeDoubleNode(d);
			        			}
			        		}
			        	}
			        }
				}
				return super.bodyCall(args, length, context);
			}
    	}
    	
    	// Not a graph pattern and not 3 arguments, a multiplicand, a multiplier, and a variable, so 
    	// this case must be either a list (length = 2) or more than 3 args.
        Node prod = getArg(0, args, context);
        if (prod.isVariable()) {
        	return false;
        }
        
        if (length == 2) {
        	// this may be a list of multiplicands
            if (!args[length - 1].isVariable()) {
            	return false;
            }
            if (prod == null || prod.equals(RDF.Nodes.nil)) {
                return false;
            } else {
            	Number nProd = Long.valueOf(1);
            	java.util.List<Node> nodeLst = Util.convertList(prod, context);
            	try {
					if (Utils.listContainsUnittedQuantity(nodeLst, context)) {
						prod = createUnittedQuantityProduct(context, nProd, nodeLst);
					}
					else {
						nProd = multiplyList(nProd, nodeLst, context);
						if (nProd instanceof Float || nProd instanceof Double || nProd instanceof BigDecimal) {
							prod = Util.makeDoubleNode(nProd.doubleValue());
						}
						else if (nProd instanceof Integer) {
							prod = Util.makeIntNode(nProd.intValue());
						}
						else {
							prod = Util.makeLongNode(nProd.longValue());
						}
					}
				} catch (TypedBuiltinFunctionException e) {
					throw new BuiltinException(this, context, e.getMessage());
				}
            	return env.bind(args[length - 1], prod);
            }
        }
        else {
        	// so this must be the one remaining case--more than three arguments, more than two multipliers
        	List<Node> nodeLst = Arrays.asList(args);
        	try {
				if (Utils.listContainsUnittedQuantity(nodeLst, context)) {
					Number nProd = Long.valueOf(1);
					List<Node> multiplierNodeLst = new ArrayList<Node>(nodeLst.size() - 1);
					for (int i = 0; i < (nodeLst.size() - 1); i++) {
						multiplierNodeLst.add(nodeLst.get(i));
					}
					prod = createUnittedQuantityProduct(context, nProd, multiplierNodeLst);
				}
				else {
					if (!prod.isLiteral()) {
				        throw new BuiltinException(this, context, "builtin " + getName() + " has a multiplier (" + prod + ") which is not a Literal");
					}
				    Object prodObj = prod.getLiteralValue();
				    if (!(prodObj instanceof Number)) {
				        throw new BuiltinException(this, context, "builtin " + getName() + " has a multiplier (" + prodObj + ") which is not a number");
				    }
				    Number nProd = Long.valueOf(1);
					java.util.List<Node> lst = new ArrayList<Node>();
					for (int i = 0; i < (length - 1); i++) {
						lst.add(getArg(i, args, context));
					}
					nProd = multiplyList(nProd, lst, context);
				   	if (nProd instanceof Float || nProd instanceof Double || nProd instanceof BigDecimal) {
						prod = Util.makeDoubleNode(nProd.doubleValue());
					}
					else if (nProd instanceof Integer) {
						prod = Util.makeIntNode(nProd.intValue());
					}
					else {
						prod = Util.makeLongNode(nProd.longValue());
					}
				}
			} catch (TypedBuiltinFunctionException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
        	return env.bind(args[length - 1], prod);
        }
    }
    
    private Node createUnittedQuantityProduct(RuleContext context, Number nProduct, List<Node> nodeLst) {
		String opStr = "*";
		Node operator = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(opStr));
     	List<UnittedQuantity> uqs;
		uqs = Utils.getUnittedQuantityArgumentList(context, nodeLst, getUnittedQuantityProcessingConstraint());
		List<Node> values = new ArrayList<Node>();
		List<Node> units = new ArrayList<Node>(); 
		for (UnittedQuantity uq : uqs) {
			values.add(uq.getValue());
			units.add(uq.getUnit());
		}
		nProduct = multiplyList(nProduct, values, context);
		Node valNode;
		if (nProduct instanceof Float || nProduct instanceof Double) {
			valNode = Util.makeDoubleNode(nProduct.doubleValue());
		}
		else if (nProduct instanceof Integer) {
			valNode = Util.makeIntNode(nProduct.intValue());
		}
		else {
			valNode = Util.makeLongNode(nProduct.longValue());
		}
		Node lastUnit = null;
		for (Node unit : units) {
			if (lastUnit != null) {
				try {
					if (unit != null) {
						lastUnit = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(Utils.combineUnits(context, operator, lastUnit, unit)));
					}
				} catch (TypedBuiltinFunctionException e) {
					throw new BuiltinException(this, context, e.getMessage());
				}
			}
			else {
				lastUnit = unit;
			}
		}
		Node uQinst = Utils.createUnittedQuantity(context, valNode, lastUnit);
		return uQinst;
	}

    private Number multiplyList(Number prod, java.util.List<Node> l, RuleContext context) {
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
             	if (v1 instanceof Number) {
                   	String ldturi = elt.getLiteralDatatypeURI();
            		if (v1 instanceof Float || ldturi.equals(XSD.xfloat.getURI())) {
            			prod = (Float)v1 * prod.floatValue();
            		}
            		else if (v1 instanceof Double || ldturi.equals(XSD.xdouble.getURI())) {
            			prod = (Double)v1 * prod.doubleValue();
            		}
            		else if (v1 instanceof Integer || ldturi.equals(XSD.xint)) {
            			prod = ((Number)v1).intValue() * prod.doubleValue();
            		}
            		else if (ldturi.equals(XSD.decimal.getURI())) {
            			if (elt.getLiteralLexicalForm().indexOf(".") >= 0) {
            				prod = ((Number) v1).doubleValue() * prod.doubleValue();
            			}
            			else {
            				prod = ((Number)v1).longValue() * prod.doubleValue();
            			}
            		}
            		else {
            			prod = ((Number)v1).longValue() * prod.longValue();
            		}
            	}
            	else {
            		throw new BuiltinException(this, context, "Element of list input to product not a number: " + v1.toString());
            	}
            }
            else {
           		throw new BuiltinException(this, context, "Element of list input to product not a Literal: " + elt.toString());
            }
    	}
    	return prod;
    }

	@Override
	public UnittedQuantityBuiltinHandlingType getUnittedQuantityProcessingConstraint() {
		return UnittedQuantityBuiltinHandlingType.DifferentUnitsAllowedOrLeftOnly;
	}

	@Override
	public String getFunctionSignatureString() {
		return "product(decimal, ...)decimal";
	}

	@Override
	public boolean canProcessListArgument() {
		return true;
	}

	@Override
	public boolean canProcessUnittedQuantityArguments() {
		return true;
	}

	@Override
	public boolean canProcessGraphPatternArguments() {
		return true;
	}

	@Override
	public com.ge.research.sadl.model.gp.Node[] validateArgumentTypes(OntModel context, BuiltinElement be, List<com.ge.research.sadl.model.gp.Node> args, List<com.ge.research.sadl.model.gp.Node> argTypes) throws TranslationException {
		be.setCanProcessListArgument(canProcessListArgument());
		be.setCanProcessUnittedQuantityArguments(canProcessUnittedQuantityArguments());
		be.setUnittedQuantityProcessingCapability(getUnittedQuantityProcessingConstraint());
		be.setCanprocessGraphPatternArguments(canProcessGraphPatternArguments());
		if (argTypes == null) {
			return null;
		}
		if (args != null && ITypedBaseBuiltin.isGraphPattern(context, args)) {
			if (!be.isCanProcessUnittedQuantityArguments()) {
				throw new TranslationException("Function arguments appear to be a graph pattern, but the built-in function '" + be.getFuncName() + "' does not support graph pattern arguments.");
			}
			else {
				// return type is the argType of the last argument
				com.ge.research.sadl.model.gp.Node[] retNodes = new com.ge.research.sadl.model.gp.Node[1];
				if (isUnittedQuantity(argTypes.get(argTypes.size() - 1), context)) {
					retNodes[0] = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI, NodeType.ClassNode);
				}
				else {
					retNodes[0] = argTypes.get(argTypes.size() - 1);
				}
				return retNodes;
			}
		}

		com.ge.research.sadl.model.gp.Node result = validateBuiltinAcceptingVarNumListOrGraphPattern(context, argTypes, true);
		if (result != null) {
			com.ge.research.sadl.model.gp.Node[] retNodes = new com.ge.research.sadl.model.gp.Node[1];
			retNodes[0] = result;
			return retNodes;
		}
		return null;
	}

	/**
	 * Method to validate arguments for built-ins like com.ge.research.sadl.jena.reasoner.builtin.Product that can accept
	 *   1) any number of numeric inputs
	 *   2) a List of numeric inputs
	 *   3) a Graph Pattern that generates a List of numeric inputs
	 * @param product 
	 * @param model
	 * @param argTypes
	 * @return
	 * @throws UnittedQuantityHandlerException 
	 */
	public com.ge.research.sadl.model.gp.Node validateBuiltinAcceptingVarNumListOrGraphPattern(OntModel context, List<com.ge.research.sadl.model.gp.Node> argTypes, boolean leftOnlyOK) throws TypedBuiltinFunctionException {
		int numArgs = argTypes.size();
		if (numArgs == 1) {
			com.ge.research.sadl.model.gp.Node argType = argTypes.get(0);
			// must be a List
			if (!(argType instanceof NamedNode && ((NamedNode)argType).isList())) {
				ClosableIterator<Triple> itr = context.getGraph().find(null, RDF.Nodes.type, NodeFactory.createURI(argType.getURI()));
				if (itr.hasNext()) {
					Node subj = itr.next().getSubject();
					if (subj != null) {
						boolean isList = context.getGraph().contains(subj, RDFS.subClassOf.asNode(), NodeFactory.createURI(SadlConstants.SADL_LIST_MODEL_LIST_URI));
						if (!isList) {
							itr.close();
							throw new TypedBuiltinFunctionException("A single argument must be a List");
						}
					}
				}
				itr.close();
			}
			// get type of list, should be number or UnittedQuantity, and return it
			if (isUnittedQuantity(argType, context)) {
				NamedNode retNN =  new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
				retNN.setNodeType(NodeType.ClassNode);
				return retNN;
			}
			else {
			 	return argType;
			}
		}
		else if (numArgs >= 2) {
			int prodIdx = 1;	// 2nd argument
			boolean graphPattern = false;
			while (prodIdx < numArgs) {
				String prodUri = argTypes.get(prodIdx).getURI();
				Node n = NodeFactory.createURI(prodUri);
				ClosableIterator<Triple> itr = context.getGraph().find(n, RDF.type.asNode(), null);
				boolean isProp = false;
				while (itr.hasNext()) {
					Node on = itr.next().getObject();
					if (!on.isURI()) {
						isProp = false;
					}
					if (on.equals(OWL.ObjectProperty.asNode()) ||
							on.equals(OWL.DatatypeProperty.asNode()) ||
							on.equals(OWL.AnnotationProperty.asNode()) ||
							on.equals(RDF.Property.asNode())) {
						itr.close();
						isProp = true;
						break;
					}
				}
				itr.close();
				if (isProp) {
					graphPattern = true;
				}
				else {
					graphPattern = false;
					break;
				}
				prodIdx = prodIdx + 3;
			}
			if (graphPattern) {
				// if this is a graph pattern, the range of the final property should be numeric or a subclass of UnittedQuantity
				String lastProdUri = argTypes.get(prodIdx - 3).getURI();
				Node n = NodeFactory.createURI(lastProdUri);
				ClosableIterator<Triple> itr = context.getGraph().find(n, RDFS.range.asNode(), null);
				while (itr.hasNext()) {
					Node rng = itr.next().getObject();
					if (rng.isURI()) {
						if (rng.getLiteralDatatypeURI() != null) {
							// this is numeric?
							String rngUri = rng.getLiteralDatatypeURI();
							if (rngUri.equals(XSD.decimal.getURI()) ||
									rngUri.equals(XSD.xdouble.getURI()) ||
									rngUri.equals(XSD.xfloat.getURI()) ||
									rngUri.equals(XSD.xint.getURI()) ||
									rngUri.equals(XSD.xlong.getURI())) {
								NamedNode retNN = new NamedNode(XSD.decimal.getURI());
								retNN.setNodeType(NodeType.DataTypeNode);
								return retNN;
							}
						}
						else {
							boolean isUQ = context.getGraph().contains(rng, RDFS.subClassOf.asNode(), NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI));
							if (isUQ) {
								NamedNode retNN =  new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
								retNN.setNodeType(NodeType.ClassNode);
								return retNN;
							}
						}
					}

				}
			}
			
			// not a graph pattern, so must be two or more operands.
			boolean uqFound = false;
			boolean returnTypeOfFirstArg = false;
			for (int i = 0; i < argTypes.size(); i++) {
				boolean isUQ = isUnittedQuantity(argTypes.get(i), context);
				if (isUQ) {
					uqFound = true;
				}
				else {
					if (uqFound) {
						// this one isn't UQ but a previous one was
						if (numArgs != 2 || !leftOnlyOK) {
							throw new TypedBuiltinFunctionException("Arguments are an invalid mix of UnittedQuantity and non-UnittedQuantity");
						}
						returnTypeOfFirstArg = true;
					}
				}
			}
			if (uqFound) {
				if (returnTypeOfFirstArg) {
					return argTypes.get(0);
				}
				else {
					NamedNode retNN =  new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
					retNN.setNodeType(NodeType.ClassNode);
					return retNN;
				}
			}
			NamedNode retNN = new NamedNode(XSD.decimal.getURI());
			retNN.setNodeType(NodeType.DataTypeNode);
			return retNN;
		}
		return null;
	}
	
	private boolean isUnittedQuantity(com.ge.research.sadl.model.gp.Node node, OntModel model) {
		com.ge.research.sadl.model.gp.Node target;
		if (node instanceof VariableNode) {
			target = ((VariableNode)node).getType();
		}
		else {
			target = node;
		}
		if (target instanceof com.ge.research.sadl.model.gp.Literal) {
			if (((com.ge.research.sadl.model.gp.Literal)node).getUnits() != null) {
				return true;
			}
		}
		else if (target instanceof NamedNode) {
			OntClass cls = model.getOntClass(target.getURI());
			if (cls != null) {
				OntClass uQCls = model.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
				if (uQCls != null && (cls.equals(uQCls) || cls.hasSuperClass(uQCls, false))) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public int numOutputArgs() {
		return 1;
	}
}
