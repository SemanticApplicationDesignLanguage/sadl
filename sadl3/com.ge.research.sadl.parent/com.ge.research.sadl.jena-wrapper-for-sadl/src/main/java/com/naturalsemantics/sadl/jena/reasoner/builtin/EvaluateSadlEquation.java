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
package com.naturalsemantics.sadl.jena.reasoner.builtin;

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.CancellableBuiltin;

/**
 * Class to wrap a SADL equation for execution in the Jena built-in environment.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class EvaluateSadlEquation extends CancellableBuiltin {

	@Override
	public String getName() {
		return "evaluateSadlEquation";
	}

    private static final Logger _logger = LoggerFactory.getLogger (EvaluateSadlEquation.class) ;
	private boolean doTransitiveClosure = true;

	/**
     * This method is invoked when the builtin is called in a rule head.
     * Such a use is only valid in a forward rule.
     * Exected args are the instance to be annotated, the property to use and the type
     * of the resulting bNode.
     * @param allArgs the array of argument values for the builtin, this is an array 
     * of Nodes.
     * @param context an execution context giving access to other relevant data
     */
    public void headAction(Node[] allArgs, int length, RuleContext context) {
		checkCanceled(allArgs, context);
		Node method = getArg(0, allArgs, context);
		if (method.isLiteral()) {
			String exturi = method.getLiteralValue().toString();
			List<Node> restOfArgs =  new ArrayList<Node>();
			for (int i = 1; i < allArgs.length; i++) {
				restOfArgs.add(allArgs[i]);
			}
			(new EvaluateSadlEquationUtils()).evaluateSadlEquation(exturi, restOfArgs);
		}
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
		Node method = getArg(0, args, context);
		if (method.isLiteral()) {
			String exturi = method.getLiteralValue().toString();
			List<Node> restOfArgs =  new ArrayList<Node>();
			for (int i = 1; i < args.length - 1; i++) {
				restOfArgs.add(args[i]);
			}
			Node result = (new EvaluateSadlEquationUtils()).evaluateSadlEquation(exturi, restOfArgs);
       		return context.getEnv().bind(args[length - 1], result);	     
		}
		return false;
    }
    
    /**
     * Returns false if this builtin has side effects when run in a body clause,
     * other than the binding of environment variables.
     */
    public boolean isSafe() {
         return true;
    }
    
    /**
     * Returns false if this builtin is non-monotonic. This includes non-monotonic checks like noValue
     * and non-monotonic actions like remove/drop. A non-monotonic call in a head is assumed to 
     * be an action and makes the overall rule and ruleset non-monotonic. 
     * Most JenaRules are monotonic deductive closure rules in which this should be false.
     */
    public boolean isMonotonic() {
        return true;
    }
 }
