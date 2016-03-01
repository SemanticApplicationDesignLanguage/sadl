/************************************************************************
 * Copyright \u00a9 2007, 2008, 2009 - General Electric Company, All Rights Reserved
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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.reasoner.rulesys.*;
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin;
import com.hp.hpl.jena.util.iterator.ClosableIterator;
import com.hp.hpl.jena.graph.*;

/**
 * Has only five arg form (S, OP, P2). 
 * Succeeds if for every triple matching (S, OP, ?) the matching object values are the subject of at least one triple (Oi, P2, ?).
 * 
 * @author Crapo, Andrew W.
 * @version $Revision: 1.1 $ on $Date: 2014/01/31 15:41:06 $
 */
public class NoUnknownValues extends BaseBuiltin {
	private static final Logger _logger = LoggerFactory.getLogger(NoUnknownValues.class);

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "noUnknownValues";
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
        Node subj = getArg(0, args, context);
        Node op1 = getArg(1, args, context);
        Node p2 = getArg(2, args, context);
		if (_logger.isDebugEnabled()) {
			_logger.debug("in Rule " + context.getRule().getName() + " called with args (" + subj.toString() + ", " + op1.toString() + ", " + p2.toString() + ")");
		}        
        ClosableIterator<Triple> citr = context.find(subj, op1, (Node)null);
         while (citr.hasNext()) {
        	Object o = citr.next();
        	Node objval = ((Triple) o).getObject();
            boolean bContainsObj = context.contains(objval, p2, (Node)null);
            if (!bContainsObj) {
            	citr.close();
    			if (_logger.isDebugEnabled()) {
    				_logger.debug("in Rule " + context.getRule().getName() + " returning false on value '" + objval.toString() + "'");
    			}
            	return false;
            }       	
        }
        citr.close();
		if (_logger.isDebugEnabled()) {
			_logger.debug("in Rule " + context.getRule().getName() + " returning true");
		}
        return true;
    }
    
    /**
     * Flag as non-monotonic so the guard clause will get rerun after deferal
     * as part of a non-trivial conflict set.
     */
    public boolean isMonotonic() {
        return true;
    }
    
    public boolean isSafe() {
    	return true;
    }
    
}
