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

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.Node_RuleVariable;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.vocabulary.RDF;

import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.UnittedQuantity;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;
import com.naturalsemanticsllc.sadl.reasoner.JenaUnittedQuantityInferenceHelper;

public class Min extends TypedBaseBuiltin {
	
	private int argLength = 0;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "min";
    }
    
    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return argLength;
    }
    
    private void setArgLength(int len) {
    	argLength = len;
    }

	@Override
	public String getFunctionSignatureString() {
		return "min(decimal,decimal,...)decimal";
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
        Object minVal = null;
        Node min = null;
        boolean allLongs = true;
        Node[] nodes = null;
        boolean hasUnittedQuantityArgument = false;
        JenaUnittedQuantityInferenceHelper juqih = new JenaUnittedQuantityInferenceHelper();
        Node unit0 = null;

        if (GeUtils.isGraphPatternInput(this, args, length, context)) {
        	nodes = GeUtils.matchNonSparqlPattern(this, args, length, true, context);
        }
        else if (length >= 3) {
        	nodes = new Node[length];
        	for (int i = 0; i < length; i++) {
        		nodes[i] = getArg(i, args, context);
        		try {
					if (!hasUnittedQuantityArgument && juqih.isUnittedQuantity(nodes[i], context)) {
						hasUnittedQuantityArgument = true;
					}
				} catch (UnittedQuantityHandlerException e) {
					throw new BuiltinException(this, context, e.getMessage());
				}
        	}
        }
        else {
            Node n1 = getArg(0, args, context);
            if (n1 == null || n1.equals(RDF.Nodes.nil)) {
                return false;
            } else {
            	minVal = minOfList(n1, context);
             }
        }
        if (hasUnittedQuantityArgument) {
        	List<Node> nodeLst = new ArrayList<Node>();
        	for (Node n : nodes) {
        		if (!(n instanceof Node_RuleVariable)) {
        			nodeLst.add(n);
        		}
        	}
        	List<UnittedQuantity> uqlist = null;
			try {
				uqlist = juqih.getUnittedQuantityArgumentList(this, nodeLst, getBuiltinUnittedQuantityStatus(), context);
			} catch (UnittedQuantityHandlerException e) {
				throw new BuiltinException(this, context, e.getMessage());
			}
        	if (uqlist !=  null && uqlist.size() >= 1) {
        		unit0 = uqlist.get(0).getUnit();
        		Node[] values = new Node[nodes.length];
        		values[0] = uqlist.get(0).getValue();
        		for (int i = 1; i < uqlist.size(); i++) {
        			Node unit = uqlist.get(i).getUnit();
        			try {
						juqih.validateOperation(context, getBuiltinUnittedQuantityStatus(), unit0, unit);
					} catch (UnittedQuantityHandlerException e) {
						throw new BuiltinException(this, context, e.getMessage());
					}
        			values[i] = uqlist.get(i).getValue();
        		}
        		nodes = values;
        	}
        }
        if (nodes != null) {
	        for (int i = 0; i < nodes.length; i++) {
	        	Node n1 = nodes[i]; //getArg(i, args, context);
	        	if (n1 != null && n1.isLiteral()) {
	        		Object v1 = n1.getLiteralValue();
	        		if (v1 instanceof Number) {
	        			Number nv1 = (Number)v1;
	        			if (minVal == null) {
	        				minVal = nv1;
	        			}
	        			else {
			                if (v1 instanceof Float || v1 instanceof Double) {
			                	double pwd = nv1.doubleValue();
			                	if (pwd < ((Number) minVal).doubleValue()) {
			                		minVal = nv1;
			                	}
			                	allLongs = false;
			                }
			                else if (v1 instanceof Integer || v1 instanceof Long) {
			                	long pwd = nv1.longValue();
			                	if (pwd < ((Number) minVal).doubleValue()) {
			                		minVal = nv1;
			                	}
			                }
	        			}
	        		}
	        		else if (v1 instanceof XSDDateTime) {
	        			if (minVal == null) {
	        				minVal = v1;
	        			}
	        			else if (minVal instanceof XSDDateTime) {
		            		if (((XSDDateTime) minVal).compareTo((XSDDateTime)v1) > 0) {
		            			minVal = v1;
		            		}
	        			}
	        			else {
	        				throw new BuiltinException(this, context, "Can't compare datetime (" + v1.toString() + 
	        						") with non-datetime value (" + minVal.toString() + ")");
	        			}
	        		}
	        		else {
	        			return false;
	        		}
	        	}
	        }	// end for
        }

        if (minVal == null) {
        	return false;
        }
    	if (minVal instanceof Float) {
    		min = Utils.makeFloatNode((Float) minVal);
    	}
    	else if ( minVal instanceof Double) {
    		min = Util.makeDoubleNode(((Double) minVal).doubleValue());
    	}
    	else if (minVal instanceof XSDDateTime) {
    		min = Utils.makeXSDDateTimeNode((XSDDateTime) minVal);
    	}
    	else if ( minVal instanceof Integer) {
    		min = Util.makeIntNode(((Integer) minVal).intValue());
    	}
    	else {
    		min = Util.makeLongNode(((Number) minVal).longValue());
    	}
    	if (hasUnittedQuantityArgument) {
    		min = juqih.createUnittedQuantity(context, min, unit0);
    	}
        return env.bind(args[length - 1], min);
    }

    private Object minOfList(Node lst, RuleContext context) {
    	java.util.List<Node> l = Util.convertList(lst, context);
    	Number min = null;
    	XSDDateTime minDate = null;
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
            	if (v1 instanceof Number) {
            		if (min == null || min.doubleValue() > ((Number)v1).doubleValue()) {
            			min = (Number) v1;
            		}
             	}
            	else if (v1 instanceof XSDDateTime) {
            		if (minDate == null || minDate.compareTo((XSDDateTime)v1) > 0) {
            			minDate = (XSDDateTime) v1;
            		}
            	}
            	else {
            		throw new BuiltinException(this, context, "Element of list input to min not a number: " + v1.toString());
            	}
            }
            else {
           		throw new BuiltinException(this, context, "Element of list input to min not a Literal: " + elt.toString());
            }
    	}
    	if (minDate != null) {
    		return minDate;
    	}
        return min;
    }

	@Override
	public BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatus() {
		return BuiltinUnittedQuantityStatus.SameUnitsRequired;
	}

	@Override
	public boolean canProcessListArgument() {
		return true;
	}

}
