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

import org.apache.jena.reasoner.rulesys.*;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.graph.*;

/**
 * Has only three arg form (X, P, V). 
 * Succeeds if the triple  (X, P, V) is currently present and there ARE other values of V present.
 * 
 * @author Crapo, Andrew W.
 * @version $Revision: 1.1 $ on $Date: 2014/01/31 15:41:06 $
 */
public class NotOnlyValue extends BaseBuiltin {

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "notOnlyValue";
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
        if (length != 3) {
            throw new BuiltinException(this, context, "builtin " + getName() + " requires 3 arguments but saw " + length);
        }
        Node obj = getArg(2, args, context);
        Node subj = getArg(0, args, context);
        // Allow variables in subject position to correspond to a wild card
        if (subj.isVariable()) {
            subj = null;
        }
        
        // Allow variables in the predicate position to correspond to a wild card
        Node pred = getArg(1, args, context);
        if (pred.isVariable()) {
            pred = null;
        }
        boolean bContainsObj = context.contains(subj, pred, obj);
        if (!bContainsObj) {
        	return false;
        }
        // does it contain anything else?
        ClosableIterator<Triple> citr = context.find(subj, pred, (Node)null);
        boolean otherValue = false;
        while (citr.hasNext()) {
        	Triple o = citr.next();
        	Node objval = o.getObject();
        	if (!objval.equals(obj)) {
        		otherValue = true;
        		break;
        	}
        }
        citr.close();
        return otherValue;
    }
    
    /**
     * Flag as non-monotonic so the guard clause will get rerun after deferal
     * as part of a non-trivial conflict set.
     */
    public boolean isMonotonic() {
        return false;
    }
    
}
