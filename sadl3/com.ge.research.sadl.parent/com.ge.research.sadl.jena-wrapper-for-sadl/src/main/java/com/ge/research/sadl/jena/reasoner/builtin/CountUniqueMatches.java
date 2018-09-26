/************************************************************************
 * Copyright � 2007, 2008 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/31 15:29:27 $
 ***********************************************************************/

package com.ge.research.sadl.jena.reasoner.builtin;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.reasoner.rulesys.BindingEnvironment;
import com.hp.hpl.jena.reasoner.rulesys.BuiltinException;
import com.hp.hpl.jena.reasoner.rulesys.RuleContext;
import com.hp.hpl.jena.reasoner.rulesys.Util;
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin;

public class CountUniqueMatches extends BaseBuiltin {
    protected static final Logger logger = LoggerFactory.getLogger(CountUniqueMatches.class);
	
	private int argLength = 0;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "countUniqueMatches";
    }
    
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
       	if (length < 2) {
            throw new BuiltinException(this, context, "builtin '" + getName() + "' requires at least 2 arguments.");
      	}
       	Node[] nodes = GeUtils.matchNonSparqlPattern(this, args, length, context);
        if (nodes == null || nodes.length == 0) {
        	logger.debug("countUniqueMatches returning 0, no matches found");
        	return env.bind(args[length - 1], Util.makeIntNode(0));
        }
        else {
        	nodes = GeUtils.removeDuplicatesFromList(nodes);
	        logger.debug("countUniqueMatches returning " + nodes.length + ", the number of matching values.");
	    	return env.bind(args[length - 1], Util.makeIntNode(nodes.length));
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
