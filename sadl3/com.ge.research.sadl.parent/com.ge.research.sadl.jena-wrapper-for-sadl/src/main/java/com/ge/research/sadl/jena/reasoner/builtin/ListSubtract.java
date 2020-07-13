/************************************************************************
 * Copyright � 2007-2010 - General Electric Company, All Rights Reserved
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
 * Last revised by: $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/11/03 19:26:32 $
 ***********************************************************************/
package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;
import org.apache.jena.vocabulary.RDF;

public class ListSubtract extends  BaseBuiltin {

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
	public String getName() {
		return "listSubtract";
	}

    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return 3;
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
        Node listA = getArg(0, args, context);
        List<Node> listAlist = GeUtils.getListItems(context.getGraph(), null, listA);
        List<Node> diffList = listAlist;
        Node listB = getArg(1, args, context);
        List<Node> listBlist = GeUtils.getListItems(context.getGraph(), null, listB);
        
        for (int i = 0; listBlist != null && i < listBlist.size(); i++) {
        	Node n = listBlist.get(i);
        	if (diffList.contains(n)) {
        		diffList.remove(n);
        	}
        }
        
        if (diffList != null && diffList.size() > 0) {
	        Node ul = GeUtils.makeList(diffList.toArray(new Node[diffList.size()]), context);
	    	return context.getEnv().bind(args[length - 1], ul);	     
        }
        else {
        	return context.getEnv().bind(args[length - 1], RDF.Nodes.nil);
        }
    }

	/**
     * Returns false if this builtin has side effects when run in a body clause,
     * other than the binding of environment variables.
     */
    public boolean isSafe() {
         return true;
    }
}
