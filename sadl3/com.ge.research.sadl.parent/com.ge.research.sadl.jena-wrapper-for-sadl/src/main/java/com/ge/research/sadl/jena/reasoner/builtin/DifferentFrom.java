package com.ge.research.sadl.jena.reasoner.builtin;

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Check to see if the two nodes are URI resources with different URIs (different Resources)
 * 
 * @author crapo
 *
 */
public class DifferentFrom extends CancellableBuiltin {

	@Override
	public String getName() {
		return "differentFrom";
	}

    private static final Logger _logger = LoggerFactory.getLogger (DifferentFrom.class) ;

    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return 2;
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
        Node n1 = getArg(0, args, context);
        Node n2 = getArg(1, args, context);
        if (n1.isURI() && n2.isURI()) {
        	 String uri1 = n1.getURI();
        	 String uri2 = n2.getURI();
             _logger.debug(uri1 + " different from " + uri2 +  "?");
             return uri1 != null && uri2 != null && !uri1.equals(uri2);
        }
        else {
        	 _logger.debug("Arguments to differentFrom not both URI resources");
        }
        return false;
    }
}
