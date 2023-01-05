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
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.UnittedQuantity;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;
import com.naturalsemanticsllc.sadl.reasoner.JenaUnittedQuantityInferenceHelper;

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
        
        JenaUnittedQuantityInferenceHelper juqih = new JenaUnittedQuantityInferenceHelper();
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
				if (juqih.listContainsUnittedQuantity(nodeLst, context)) {
					prod = createUnittedQuantityProduct(context, nProd, nodeLst, juqih);
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
			} catch (UnittedQuantityHandlerException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
//        	System.out.println("builtin product assigning value: " + sum);
        	return env.bind(args[length - 1], prod);
        }
        else if (length == 3) {
        	try {
				if (juqih.isUnittedQuantity(getArg(0, args, context), context) ||
						juqih.isUnittedQuantity(getArg(1, args, context), context)) {
					Number nProd = Long.valueOf(1);
					List<Node> nodeLst = new ArrayList<Node>();
					nodeLst.add(getArg(0, args, context));
					nodeLst.add(getArg(1, args, context));
					Node prod = createUnittedQuantityProduct(context, nProd, nodeLst, juqih);
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
			} catch (UnittedQuantityHandlerException e) {
				throw new BuiltinException(this, context, e.getMessage());
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
					if (juqih.listContainsUnittedQuantity(nodeLst, context)) {
						prod = createUnittedQuantityProduct(context, nProd, nodeLst, juqih);
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
				} catch (UnittedQuantityHandlerException e) {
					throw new BuiltinException(this, context, e.getMessage());
				}
//            	System.out.println("builtin product assigning value: " + sum);
            	return env.bind(args[length - 1], prod);
            }
        }
        else {
        	// so this must be the one remaining case--more than three arguments, more than two multipliers
        	List<Node> nodeLst = Arrays.asList(args);
        	try {
				if (juqih.listContainsUnittedQuantity(nodeLst, context)) {
					Number nProd = Long.valueOf(1);
					List<Node> multiplierNodeLst = new ArrayList<Node>(nodeLst.size() - 1);
					for (int i = 0; i < (nodeLst.size() - 1); i++) {
						multiplierNodeLst.add(nodeLst.get(i));
					}
					prod = createUnittedQuantityProduct(context, nProd, multiplierNodeLst, juqih);
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
			} catch (UnittedQuantityHandlerException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
//        	System.out.println("builtin product assigning value: " + sum);
        	return env.bind(args[length - 1], prod);
        }
    }

	private Node createUnittedQuantityProduct(RuleContext context, Number nProd, List<Node> nodeLst, JenaUnittedQuantityInferenceHelper juqih) {
		String opStr = "*";
		Node operator = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(opStr));
    	List<UnittedQuantity> uqs;
		try {
			uqs = juqih.getUnittedQuantityArgumentList(this, nodeLst, getBuiltinUnittedQuantityStatus(), context);
		} catch (UnittedQuantityHandlerException e) {
			throw new BuiltinException(this, context, e.getMessage());
		}
		List<Node> values = new ArrayList<Node>();
		List<Node> units = new ArrayList<Node>(); 
		for (UnittedQuantity uq : uqs) {
			values.add(uq.getValue());
			units.add(uq.getUnit());
		}
    	try {
    		juqih.validateUnittedQuantityArgs(context, BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly, units);
		} catch (UnittedQuantityHandlerException e) {
			throw new BuiltinException(this, context, e.getMessage());
		}
		nProd = multiplyList(nProd, values, context);
		Node valNode;
		if (nProd instanceof Float || nProd instanceof Double) {
			valNode = Util.makeDoubleNode(nProd.doubleValue());
		}
		else if (nProd instanceof Integer) {
			valNode = Util.makeIntNode(nProd.intValue());
		}
		else {
			valNode = Util.makeLongNode(nProd.longValue());
		}
		Node lastUnit = null;
		for (Node unit : units) {
			if (lastUnit != null) {
				try {
					lastUnit = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(Utils.combineUnits(context, operator, lastUnit, unit)));
				} catch (UnittedQuantityHandlerException e) {
					throw new BuiltinException(this, context, e.getMessage());
				}
			}
			else {
				lastUnit = unit;
			}
		}
		Node uQinst = juqih.createUnittedQuantity(context, valNode, lastUnit);
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
//            			System.out.println("multiplying " + v1 + " by prod (" + prod.floatValue() + ")");
            			prod = (Float)v1 * prod.floatValue();
            		}
            		else if (v1 instanceof Double || ldturi.equals(XSD.xdouble.getURI())) {
//            			System.out.println("multiplying " + v1 + " by prod (" + prod.doubleValue() + ")");
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
//            			System.out.println("multiplying " + v1 + " by prod (" + prod.longValue() + ")");
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

    private BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatus() {
		return BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly;
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
	public boolean canProcessUnittedQuantity() {
		return true;
	}

	@Override
	public com.ge.research.sadl.model.gp.Node validateArgumentTypes(OntModel model, BuiltinElement be,
			List<com.ge.research.sadl.model.gp.Node> argTypes)
			throws UnittedQuantityHandlerException, TranslationException {
		be.setCanProcessListArgument(canProcessListArgument());
		be.setCanProcessUnittedQuantity(canProcessUnittedQuantity());
		be.setUnittedQuantityStatus(getBuiltinUnittedQuantityStatus());
		return (new Utils()).validateBuiltinAcceptingVarNumListOrGraphPattern(model, argTypes, true);
	}

}
