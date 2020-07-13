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

import java.util.Arrays;

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;
import org.apache.jena.vocabulary.RDF;

public class Average extends BaseBuiltin {
	
	private int argLength = 0;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "average";
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
        double avg = 0;
        if (GeUtils.isGraphPatternInput(this, args, length, context)) {
        	Node[] nodes = GeUtils.matchNonSparqlPattern(this, args, length, true, context);
        	java.util.List<Node> nodeLst = Arrays.asList(nodes);
        	avg = averageOfList(nodeLst, context);
        }
        else if (length >= 3) {
        	double sum = 0;
	        for (int i = 0; i < length; i++) {
	        	Node n1 = getArg(i, args, context);
	        	if (n1.isLiteral()) {
	        		Object v1 = n1.getLiteralValue();
	        		if (v1 instanceof Number) {
	        			Number nv1 = (Number)v1;
	        			if (v1 instanceof Float || v1 instanceof Double) {
			                sum+= nv1.doubleValue();
		                }
		                else if (v1 instanceof Integer || v1 instanceof Long) {
		                	sum += nv1.longValue();
		                }
	        		}
	        		else {
	        			return false;
	        		}
	        	}
	        }	// end for
	        avg = sum / (length - 1);
        }
        else {
            Node n1 = getArg(0, args, context);
            if (n1 == null || n1.equals(RDF.Nodes.nil)) {
                return false;
            } else {
            	avg = averageOfList(n1, context);
             }
        }
        Node avgnode = Util.makeDoubleNode(avg);
        return env.bind(args[length - 1], avgnode);
    }

    private double averageOfList(Node lst, RuleContext context) {
    	java.util.List<Node> l = Util.convertList(lst, context);
    	return averageOfList(l, context);
    }

	private double averageOfList(java.util.List<Node> l, RuleContext context) {
		int cnt = 0;
    	double sum = 0;
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
            	if (v1 instanceof Number) {
            		sum += ((Number) v1).doubleValue();
            		cnt++;
             	}
            	else {
            		throw new BuiltinException(this, context, "Element of list input to max not a number: " + v1.toString());
            	}
            }
            else {
           		throw new BuiltinException(this, context, "Element of list input to max not a Literal: " + elt.toString());
            }
    	}
    	if (cnt < 1) {
    		throw new BuiltinException(this, context, "Can't average an empty List!");
    	}
        return sum / cnt;
	}

}
