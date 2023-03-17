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
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.Builtin;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.jena.reasoner.builtin.utils.UnittedQuantity;
import com.ge.research.sadl.jena.reasoner.builtin.utils.Utils;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.reasoner.TranslationException;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;
import com.naturalsemanticsllc.sadl.reasoner.TypedBuiltinFunctionException;

/**

 */
public class Sum extends org.apache.jena.reasoner.rulesys.builtins.Sum implements ITypedBaseBuiltin {

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
         *    pattern is a list of items to be added together. The sum is assigned to the last argument,
         *    which must be a variable.
         * 2. There are three arguments, the first two of which are to be added together and assigned
         *    to the third, which must be a variable
         * 3. The first argument is a list of items to be added together, the sum of which is to
         *    be assigned to the second argument, which must be a variable.
         *    
         * In any of these three cases, the elements of the list may be instances of the SadlImplicitModel's
         * UnittedQuantity, in which case the result is an instance of a UnittedQuantity whose value is the
         * sum of the values of the addends and the unit is that of any addend, all of which must be the same.
         * 
         */
        if (GeUtils.isGraphPatternInput(this, args, length, context)) {
        	Node[] nodes = GeUtils.matchNonSparqlPattern(this, args, length, true, context);
           	Number nSum = Long.valueOf(0);
        	Node sum = null;
        	List<Node> nodeLst = Arrays.asList(nodes);
        	try {
				if (Utils.listContainsUnittedQuantity(nodeLst, context)) {
					sum = createUnittedQuantitySum(context, nSum, nodeLst);
				}
				else {
				 	nSum = addList(nSum, Arrays.asList(nodes), context);
				   	if (nSum instanceof Float || nSum instanceof Double) {
						sum = Util.makeDoubleNode(nSum.doubleValue());
					}
				   	else if (nSum instanceof Integer) {
				   		sum = Util.makeIntNode(nSum.intValue());
				   	}
					else {
						sum = Util.makeLongNode(nSum.longValue());
					}
				}
			} catch (TypedBuiltinFunctionException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
        	return env.bind(args[length - 1], sum);
        }
        else if (length == 3) {
        	if (Utils.isUnittedQuantity(getArg(0, args, context), context) ||
					Utils.isUnittedQuantity(getArg(1, args, context), context)) {
        		List<Node> nodeLst = new ArrayList<Node>();
        		nodeLst.add(getArg(0, args, context));
        		nodeLst.add(getArg(1, args, context));
				Number nSum = Long.valueOf(0);
				Node sum = createUnittedQuantitySum(context, nSum, nodeLst);
				return env.bind(args[length - 1], sum);
			}
			else {
				// this is just the normal case implemented by HP Labs (standard Jena) (except Jena doesn't handle
				//	BigDecimal so we convert BigDecimal to Double before making the call)
				if (getArg(0, args, context).isURI()) {
					throw new BuiltinException(this, context, "First argument to sum is a URI: " + getArg(0, args, context).getURI());
				}
				if (getArg(1, args, context).isURI()) {
					throw new BuiltinException(this, context, "Second argument to sum is a URI: " + getArg(1, args, context).getURI());
				}
				for (int i = 0; i < args.length - 1; i++) {
			        Node n = getArg(i, args, context);
			        if (n.isLiteral()) {
			        	Object v = n.getLiteralValue();
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
        Node n1 = getArg(0, args, context);
        Node n2 = getArg(1, args, context);
        if (n1.isVariable()) {
        	return false;
        }
        
        if (length == 2) {
        	// this may be a list of addends
        	if (!args[length - 1].isVariable()) {
        		return false;
        	}
        	if (n1 == null ||n1.equals(RDF.Nodes.nil)) {
        		return false;
        	}
        	else {
        		Number nSum = Long.valueOf(0);
            	java.util.List<Node> nodeLst = Util.convertList(n1, context);
            	try {
					if (Utils.listContainsUnittedQuantity(nodeLst, context)) {
						n1 = createUnittedQuantitySum(context, nSum, nodeLst);
					}
					else {
						nSum = sumList(nSum, nodeLst, context);
						if (nSum instanceof Float || nSum instanceof Double || nSum instanceof BigDecimal) {
							n1 = Util.makeDoubleNode(nSum.doubleValue());
						}
						else if (nSum instanceof Integer) {
							n1 = Util.makeIntNode(nSum.intValue());
						}
						else {
							n1 = Util.makeLongNode(nSum.longValue());
						}
					}
				} catch (TypedBuiltinFunctionException e) {
					throw new BuiltinException(this, context, e.getMessage());
				}
            	return env.bind(args[length - 1], n1);
        	}
        }
        else {
        	// so this must be the one remaining case--more than three arguments, more than two addends
        	List<Node> nodeLst = Arrays.asList(args);
        	try {
				if (Utils.listContainsUnittedQuantity(nodeLst, context)) {
					Number nSum = Long.valueOf(0);
					List<Node> sumNodeLst = new ArrayList<Node>(nodeLst.size() - 1);
					for (int i = 0; i < (nodeLst.size() - 1); i++) {
						sumNodeLst.add(nodeLst.get(i));
					}
					n1 = createUnittedQuantitySum(context, nSum, sumNodeLst);
					
				}
				else {
					if (!n1.isLiteral()) {
				        throw new BuiltinException(this, context, "builtin " + getName() + " has an addend (" + n1 + ") which is not a Literal");
					}
				    Object sumObj = n1.getLiteralValue();
				    if (!(sumObj instanceof Number)) {
				        throw new BuiltinException(this, context, "builtin " + getName() + " has an addend (" + sumObj + ") which is not a number");
				    }
				    Number nSum = Long.valueOf(0);
					java.util.List<Node> lst = new ArrayList<Node>();
					for (int i = 0; i < (length - 1); i++) {
						lst.add(getArg(i, args, context));
					}
					nSum = sumList(nSum, lst, context);
				   	if (nSum instanceof Float || nSum instanceof Double || nSum instanceof BigDecimal) {
						n1 = Util.makeDoubleNode(nSum.doubleValue());
					}
					else if (nSum instanceof Integer) {
						n1 = Util.makeIntNode(nSum.intValue());
					}
					else {
						n1 = Util.makeLongNode(nSum.longValue());
					}
				}
			} catch (TypedBuiltinFunctionException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
        	return env.bind(args[length - 1], n1);
        }
    }
    
    private Node createUnittedQuantitySum(RuleContext context, Number nSum, List<Node> nodeLst) {
    	try {
    		Utils.checkUnittedQuantityListSameUnits(context, nodeLst);
		} catch (TypedBuiltinFunctionException e) {
			throw new BuiltinException(this, context, e.getMessage());
		}
    	List<UnittedQuantity> uqs;
		uqs = Utils.getUnittedQuantityArgumentList(context, nodeLst, getUnittedQuantityProcessingConstraint());
		List<Node> values = new ArrayList<Node>();
		Node units = null; 
		for (UnittedQuantity uq : uqs) {
			values.add(uq.getValue());
			if (units == null) {
				units = uq.getUnit();
			}
		}
		nSum = sumList(nSum, values, context);
		Node valNode;
		if (nSum instanceof Float || nSum instanceof Double) {
			valNode = Util.makeDoubleNode(nSum.doubleValue());
		}
		else if (nSum instanceof Integer) {
			valNode = Util.makeIntNode(nSum.intValue());
		}
		else {
			valNode = Util.makeLongNode(nSum.longValue());
		}
		Node uQinst = Utils.createUnittedQuantity(context, valNode, units);
		return uQinst;
	}

	private Number sumList(Number sum, List<Node> l, RuleContext context) {
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
             	if (v1 instanceof Number) {
                   	String ldturi = elt.getLiteralDatatypeURI();
            		if (v1 instanceof Float || ldturi.equals(XSD.xfloat.getURI())) {
            			sum = (Float)v1 + sum.floatValue();
            		}
            		else if (v1 instanceof Double || ldturi.equals(XSD.xdouble.getURI())) {
            			sum = (Double)v1 + sum.doubleValue();
            		}
            		else if (v1 instanceof Integer || ldturi.equals(XSD.xint.getURI())) {
            			sum = ((Number)v1).intValue() + sum.doubleValue();
            		}
            		else if (ldturi.equals(XSD.decimal.getURI())) {
            			if (elt.getLiteralLexicalForm().indexOf(".") >= 0) {
            				sum = ((Number) v1).doubleValue() + sum.doubleValue();
            			}
            			else {
            				sum = ((Number)v1).longValue() + sum.doubleValue();
            			}
            		}
            		else {
            			sum = ((Number)v1).longValue() + sum.longValue();
            		}
            	}
            	else {
            		throw new BuiltinException(this, context, "Element of list input to sum not a number: " + v1.toString());
            	}
            }
            else {
           		throw new BuiltinException(this, context, "Element of list input to sum not a Literal: " + elt.toString());
            }
    	}
		return sum;
	}

    private Number addList(Number sum, java.util.List<Node> l, RuleContext context) {
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
            	if (v1 instanceof Number) {
            		if (v1 instanceof Float) {
            			sum = (Float)v1 + sum.floatValue();
            		}
            		else if (v1 instanceof Double) {
            			sum = (Double)v1 + sum.doubleValue();
            		}
            		else {
            			sum = ((Number)v1).longValue() + sum.longValue();
            		}
            	}
            	else {
            		throw new BuiltinException(this, context, "Element of list input to sum not a number: " + v1.toString());
            	}
            }
            else {
           		throw new BuiltinException(this, context, "Element of list input to sum not a Literal: " + elt.toString());
            }
    	}
        return sum;
    }

	@Override
	public UnittedQuantityBuiltinHandlingType getUnittedQuantityProcessingConstraint() {
		return UnittedQuantityBuiltinHandlingType.SameUnitsRequired;
	}
	@Override

	public String getFunctionSignatureString() {
		return "sum(decimal, ...)decimal";
	}

	@Override
	public boolean canProcessListArgument() {
		return true;
	}

	@Override
	public com.ge.research.sadl.model.gp.Node[] validateArgumentTypes(OntModel context, BuiltinElement be, List<com.ge.research.sadl.model.gp.Node> args, List<com.ge.research.sadl.model.gp.Node> argTypes) throws TranslationException {
		be.setCanProcessListArgument(canProcessListArgument());
		be.setCanProcessUnittedQuantityArguments(canProcessUnittedQuantityArguments());
		be.setUnittedQuantityProcessingCapability(getUnittedQuantityProcessingConstraint());
		be.setCanprocessGraphPatternArguments(canProcessGraphPatternArguments());
		com.ge.research.sadl.model.gp.Node lastType = null;
		boolean isList = false;
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
				retNodes[0] = argTypes.get(argTypes.size() - 1);
				return retNodes;
			}
		}
		for (com.ge.research.sadl.model.gp.Node argType : argTypes) {
			if (lastType != null) {
				if (!lastType.equals(argType)) {
					if (!Utils.isNumber(lastType, context) || !Utils.isNumber(argType, context)) {
						throw new TranslationException("Arguments are not all of the same type.");
					}
					else {
						argType = Utils.moreGeneralNumericType(lastType, argType, context);
					}
				}
			}
			lastType = argType;
			if (lastType instanceof NamedNode) {
				isList = ((NamedNode)lastType).isList();
			}
		}
		if (isList) {
			if (lastType instanceof NamedNode) {
				// type of list should be lastType, but the return won't be a list.
				NamedNode listType = new NamedNode(((NamedNode)lastType).getURI());
				listType.setContext(be.getContext());
				if (((NamedNode) lastType).getNodeType().equals(NodeType.ClassListNode)) {
					listType.setNodeType(NodeType.ClassNode);
				}
				else if (((NamedNode) lastType).getNodeType().equals(NodeType.DataTypeListNode)) {
					listType.setNodeType(NodeType.DataTypeNode);
				}
				else {
					throw new TypedBuiltinFunctionException("List type isn't Class or Datatype; this is unexpected.");
				}
				lastType = listType;
			}
			else {
				// ??
			}
		}
		com.ge.research.sadl.model.gp.Node[] rettypes = new com.ge.research.sadl.model.gp.Node[1];
		rettypes[0] = lastType;
		return rettypes;
	}

	public List<UnittedQuantity> getUnittedQuantityArgumentList(Object bi, List<Node> nodes,
			UnittedQuantityBuiltinHandlingType builtinUqStatus, Object context) throws TypedBuiltinFunctionException {
		if (bi instanceof Builtin && context instanceof RuleContext) {
			return getUnittedQuantityArgumentList((Builtin)bi, nodes, builtinUqStatus, (RuleContext)context);
		}
		throw new TypedBuiltinFunctionException("Invalid Builtin and/or RuleContext passed to getUnittedQuantityArgumentList");
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
	public int numOutputArgs() {
		return 1;
	}
}
