/************************************************************************
 * Copyright � 2008, 2009 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL-GE
 * 
 * Description: The Semantic Application Design Language (SADL) is a 
 * language for building semantic models and expressing rules that 
 * capture additional domain knowledge. The SADL-IDE (integrated 
 * development environment) is a set of Eclipse plug-ins that 
 * support the editing and testing of semantic models using the 
 * SADL language. The SADLServer facilitates delivery of semantic models
 * and domain knowledge via a Service interface.
 * 
 * The information contained in this document is General Electric Company (GE) 
 * proprietary information and is disclosed in confidence. It is the property 
 * of GE and shall not be used, disclosed to others or reproduced without the 
 * express written consent of GE. If consent is given for reproduction in whole 
 * or in part, this notice and the notice set forth on each page of this document 
 * shall appear in any such reproduction in whole or in part. The information 
 * contained in this document may also be controlled by the U.S. export control 
 * laws.  Unauthorized export or re-export is prohibited.
 *
 * 
 * Package: com.ge.jena.builtin
 * Filename: NoValue.java
 * 
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/31 15:29:27 $
 ***********************************************************************/
package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.Utils;
import com.hp.hpl.jena.reasoner.rulesys.*;
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin;
import com.hp.hpl.jena.util.iterator.ClosableIterator;
import com.hp.hpl.jena.graph.*;

/**
 * Can be used in two arg form (X, P) or three arg form (X, P, V). 
 * In three arg form it succeeds if the triple  (X, P, V) is not
 * currently present, in two arg form it succeeds if there is not value
 * for (X, P, *).
 * 
 * @author <a href="mailto:der@hplb.hpl.hp.com">Dave Reynolds</a>
 * @version $Revision: 1.1 $ on $Date: 2014/01/31 15:29:27 $
 */
public class NoValue extends BaseBuiltin {
	private static final Logger _logger = LoggerFactory.getLogger(NoValue.class);

	/**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "noValue";
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
        if (GeUtils.isGraphPatternInput(this, args, length, context)) {
        	Node[] nodes = GeUtils.matchNonSparqlPattern(this, args, length, false, context);
        	if (nodes == null || nodes.length == 0) {
        		return true;
        	}
        	else {
        		return false;
        	}
        }
        else if (length >= 3) {
    		Node n3 = getArg(2, args, context);
    		if (n3 != null && n3.isURI()) {
    			if (Utils.isOntProperty(context, n3)) {
    	    		// this could be of the form
    	    		//	noValue(subj, pred1, pred2 [, pred3,...,[value]])
    	    		//	iff 3rd argument is a property
	    			// start with the single subject for the first pattern
	    			List<Node> subjects = new ArrayList<Node>();
	    			subjects.add(getArg(0, args, context));		
	    			
	    			List<Node> values = new ArrayList<Node>();
	
	    			// iterate over the arguments in the arg array (starting with arg[1])
	    			for (int ip = 1; ip < length; ip++) {
	    				Node n = getArg(ip, args, context);
	    				if (n != null && n.isURI()) {
	    					// this is a predicate or a value
	    					if (Utils.isOntProperty(context, n)) {
	    						// this is a predicate
	    						Node pred = n;
	    	    				// iterate over the subjects in the current 
	    	        			for (int is = 0; is < subjects.size(); is++) {
	    		    				ClosableIterator<Triple> itr = context.find(subjects.get(is), pred, null);
	    		    				while (itr.hasNext()) {
	    		    					Triple t = itr.next();
	    		    					values.add(t.getObject());
	    		    				}
	    		    				itr.close();
	    		    			}
	    	        			if (values.isEmpty()) {
	    	        				// no point continuing: even if there are more edges there are no subjects
	    	        				break;
	    	        			}
	    	        			if (ip < (length - 1)) {
	    	        				// we have more edges to walk or we have a value to compare
	    	        				subjects.clear();
	    	        				subjects.addAll(values);
	    	        				values.clear();
	    	        			}
	    					}
	    					else {
	    						// this must be a value; values have been put in subjects
	    						if (subjects.contains(n)) {
	    							// we have a match
	    							return false;
	    						}
	    						else {
	    							// there is no match to give value
	    							return true;
	    						}
	    					}  				
	    				}
	    			}
	    			if (values.size() > 0) {
	    				// at least one matching value was found
	    				return false;
	    			}
	    	        return true;
    			}
    			else {
    	    		// this could be of the form
    	    		//	noValue(subj, pred1, obj1, pred2, obj2 [, pred3, obj3,...]])
    	    		//	iff 3rd argument is not a property
    				Node subj = getArg(0, args, context);
    				for (int patternCnt = 0; patternCnt < args.length; patternCnt = patternCnt + 3) {
	    				Node pred = getArg(patternCnt + 1, args, context);
	    				Node obj = null;
	    				if ((patternCnt + 2) < args.length) {
	    					obj = getArg(patternCnt + 2, args, context);
	    					if (context.contains(subj, pred, obj)) {
    		    				ClosableIterator<Triple> itr = context.find(subj, pred, obj);
    		    				while (itr.hasNext()) {
    		    					Triple t = itr.next();
    		    					subj = t.getSubject();
    		    					// matches so far; keep going
    		    					break;
    		    				}
    		    				itr.close();
	    					}
	    					else {
	    						return true;	// this condition wasn't matched so there is no value; return true
	    					}
	    				}
    				}
    				return false;	// all conditions were matched so there is a value; return false
    			}
    		}
    	}
    	
    	// the rest of this is a snapshot of the Jena NoValue built-in (as of 6/25/09) except for the warning to _logger
        if (length !=2 && length != 3) {
            throw new BuiltinException(this, context, "regular builtin " + getName() + " requires 2 or 3 arguments but saw " + length);
        }
        int nullCount = 0;
        Node obj = null;
        if (length == 3) {
            obj = getArg(2, args, context);
        }
        if (obj == null) {
        	nullCount++;
        }
        Node subj = getArg(0, args, context);
        // Allow variables in subject position to correspond to wild cards
        if (subj.isVariable()) {
            subj = null;
            nullCount++;
        }
        Node pred = getArg(1, args, context);
        if (pred.isVariable()) {
            pred = null;
            nullCount++;
        }
        if (nullCount >= 2) {
        	_logger.warn("noValue has two or more unbound variable members of triple; are you sure that's what is desired?");
        }
        return !context.contains(subj, pred, obj);
    }
    
    /**
     * Flag as non-monotonic so the guard clause will get rerun after deferal
     * as part of a non-trivial conflict set.
     */
    public boolean isMonotonic() {
        return false;
    }
    
}
