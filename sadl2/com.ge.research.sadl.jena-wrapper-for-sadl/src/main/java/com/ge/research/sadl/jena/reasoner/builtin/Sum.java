/************************************************************************
 * Copyright \u00a9 2007, 2008 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/31 15:41:06 $
 ***********************************************************************/

package com.ge.research.sadl.jena.reasoner.builtin;

import com.hp.hpl.jena.reasoner.rulesys.*;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.graph.*;

/**

 */
public class Sum extends com.hp.hpl.jena.reasoner.rulesys.builtins.Sum {

	private int argLength = 0;

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
    	if (length == 3) {
    		// this is just the normal case implemented by HP Labs (standard Jena)
    		if (getArg(0, args, context).isURI()) {
    			throw new BuiltinException(this, context, "First argument to sum is a URI: " + getArg(0, args, context).getURI());
    		}
    		if (getArg(1, args, context).isURI()) {
    			throw new BuiltinException(this, context, "Second argument to sum is a URI: " + getArg(1, args, context).getURI());
    		}
    		return super.bodyCall(args, length, context);
    	}
        checkArgs(length, context);
        BindingEnvironment env = context.getEnv();
        Node n1 = getArg(0, args, context);
        Node n2 = getArg(1, args, context);
        if (n1.isVariable()) {
        	return false;
        }
        
        if (n1.isLiteral() && n2.isLiteral()) {
            Object v1 = n1.getLiteralValue();
            Object v2 = n2.getLiteralValue();
            Node sum = null;
            if (v1 instanceof Number && v2 instanceof Number) {
                Number nv1 = (Number)v1;
                Number nv2 = (Number)v2;
                if (v1 instanceof Float || v1 instanceof Double 
                ||  v2 instanceof Float || v2 instanceof Double) {
                    sum = Util.makeDoubleNode(nv1.doubleValue() + nv2.doubleValue());
                } else {
                    sum = Util.makeLongNode(nv1.longValue() + nv2.longValue());
                }
                return env.bind(args[2], sum);
            }
        }
        else {
//        	System.out.println("builtin sum called with first arg list: " + Utils.listToString(Util.convertList(n1, context)));
            if (!args[length - 1].isVariable()) {
            	return false;
            }
        	if (n1 == null || n1.equals(RDF.Nodes.nil)) {
                return false;
            } else {
            	Number nSum = new Long(0);
            	nSum = addList(nSum, n1, context);
            	Node sum = null;
            	if (nSum instanceof Float || nSum instanceof Double) {
            		sum = Util.makeDoubleNode(nSum.doubleValue());
            	}
            	else {
            		sum = Util.makeLongNode(nSum.longValue());
            	}
//            	System.out.println("builtin sum assigning value: " + sum);
            	return env.bind(args[length - 1], sum);
            }
        }
        // Doesn't (yet) handle partially bound cases
        return false;
    }
    
    private Number addList(Number sum, Node lst, RuleContext context) {
    	java.util.List<Node> l = Util.convertList(lst, context);
    	for (int i = 0; l != null && i < l.size(); i++) {
    		Node elt = (Node) l.get(i);
            if (elt != null && elt.isLiteral()) {
            	Object v1 = elt.getLiteralValue();
            	if (v1 instanceof Number) {
            		if (v1 instanceof Float) {
//            			System.out.println("adding " + v1 + " to sum (" + sum.floatValue() + ")");
            			sum = (Float)v1 + sum.floatValue();
            		}
            		else if (v1 instanceof Double) {
//            			System.out.println("adding " + v1 + " to sum (" + sum.doubleValue() + ")");
            			sum = (Double)v1 + sum.doubleValue();
            		}
            		else {
//            			System.out.println("adding " + v1 + " to sum (" + sum.longValue() + ")");
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
    
}
