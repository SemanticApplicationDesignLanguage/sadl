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

import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.vocabulary.RDF;

public class Max extends TypedBaseBuiltin {
	
	private int argLength = 0;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "max";
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
		return "max(decimal,decimal,...)decimal";
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
        Object maxVal = null;
        Node min = null;
        boolean allLongs = true;
        Node[] nodes = null;
        if (GeUtils.isGraphPatternInput(this, args, length, context)) {
        	nodes = GeUtils.matchNonSparqlPattern(this, args, length, true, context);
        }
        else if (length >= 3) {
        	nodes = new Node[length];
        	for (int i = 0; i < length; i++) {
        		nodes[i] = getArg(i, args, context);
        	}
        }
        else {
            Node n1 = getArg(0, args, context);
            if (n1 == null || n1.equals(RDF.Nodes.nil)) {
                return false;
            } else {
            	 maxVal = maxOfList(n1, context);
            }
        }
        if (nodes != null) {
	        for (int i = 0; i < nodes.length; i++) {
	        	Node n1 = nodes[i]; //getArg(i, args, context);
	        	if (n1.isLiteral()) {
	        		Object v1 = n1.getLiteralValue();
	        		if (v1 instanceof Number) {
	        			Number nv1 = (Number)v1;
	        			if (maxVal == null) {
	        				maxVal = nv1;
	        			}
	        			else {
			                if (v1 instanceof Float || v1 instanceof Double) {
			                	double pwd = nv1.doubleValue();
			                	if (pwd > ((Number)maxVal).doubleValue()) {
			                		maxVal = nv1;
			                	}
			                	allLongs = false;
			                }
			                else if (v1 instanceof Integer || v1 instanceof Long) {
			                	long pwd = nv1.longValue();
			                	if (pwd > ((Number)maxVal).doubleValue()) {
			                		maxVal = nv1;
			                	}
			                }
	        			}
	        		}
	        		else if (v1 instanceof XSDDateTime) {
	        			if (maxVal == null) {
	        				maxVal = v1;
	        			}
	        			else if (maxVal instanceof XSDDateTime) {
		            		if (((XSDDateTime) maxVal).compareTo((XSDDateTime)v1) < 0) {
		            			maxVal = v1;
		            		}
	        			}
	        			else {
	        				throw new BuiltinException(this, context, "Can't compare datetime (" + v1.toString() + 
	        						") with non-datetime value (" + maxVal.toString() + ")");
	        			}
	        		}
	        		else {
	        			return false;
	        		}
	        	}
	        }	// end for
        }
        if (maxVal == null) {
        	return false;
        }
    	if (maxVal instanceof Float) {
    		min = Utils.makeFloatNode((Float) maxVal);
    	}
    	else if ( maxVal instanceof Double) {
    		min = Util.makeDoubleNode(((Double) maxVal).doubleValue());
    	}
    	else if (maxVal instanceof XSDDateTime) {
    		min = Utils.makeXSDDateTimeNode((XSDDateTime) maxVal);
    	}
    	else if ( maxVal instanceof Integer) {
    		min = Util.makeIntNode(((Integer) maxVal).intValue());
    	}
    	else {
    		min = Util.makeLongNode(((Number) maxVal).longValue());
    	}
        return env.bind(args[length - 1], min);
    }

    private Object maxOfList(Node lst, RuleContext context) {
    	java.util.List<Node> l = Util.convertList(lst, context);
    	Number max = null;
    	XSDDateTime maxDate = null;
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
            	if (v1 instanceof Number) {
            		if (max == null || max.doubleValue() < ((Number)v1).doubleValue()) {
            			max = (Number) v1;
            		}
             	}
            	else if (v1 instanceof XSDDateTime) {
            		if (maxDate == null || maxDate.compareTo((XSDDateTime)v1) < 0) {
            			maxDate = (XSDDateTime) v1;
            		}
            	}
            	else {
            		throw new BuiltinException(this, context, "Element of list input to max not a number: " + v1.toString());
            	}
            }
            else {
           		throw new BuiltinException(this, context, "Element of list input to max not a Literal: " + elt.toString());
            }
    	}
    	if (maxDate != null) {
    		return maxDate;
    	}
        return max;
    }

}
