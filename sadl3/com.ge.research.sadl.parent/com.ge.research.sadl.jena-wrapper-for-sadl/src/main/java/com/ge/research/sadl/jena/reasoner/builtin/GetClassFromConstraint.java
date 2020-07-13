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
 * Filename: CheckType.java
 * 
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/31 15:29:27 $
 ***********************************************************************/

package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDFS;

/**
 * This built-in returns a Class (X) in the model that has a restriction matching the constraints 
 * identified by the first two arguments:
 * 	 arg[0] is a property (P)
 *   arg[1] is a value (V)
 * The constraint might be one of the following:
 *   an allValuesFrom constraint on the property (P) on some Class (X) containing the value (V)
 *   a someValuesFrom constraint on the property (P) on some Class (X) containing the value (V)
 *   
 * This class is a work in progress and is not yet complete.  
 * 
 * @author <a href="mailto:der@hplb.hpl.hp.com">Dave Reynolds</a>
 * @version $Revision: 1.1 $ on $Date: 2014/01/31 15:29:27 $
 */
public class GetClassFromConstraint extends BaseBuiltin {

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "getClassFromConstraint";
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
        checkArgs(length, context);
        Node prop = getArg(0, args, context);
        Node val = getArg(1, args, context);
    	Node restrict = null;
    	Node owlOnProp = OWL.onProperty.asNode();
        Node theClass = null;

        ClosableIterator<Triple> citr0 = context.find(null, owlOnProp, prop);
    	if (!citr0.hasNext()) {
    		ClosableIterator<Triple> citr0b = context.find(null, owlOnProp, null);
    		while (citr0b.hasNext()) {
    			Triple tr0b = citr0b.next();
    			System.out.println(tr0b);
    		}
    	}
    	while (citr0.hasNext()) {
    		Triple tr0 = citr0.next();
    		restrict = tr0.getSubject();
        	if (restrict != null) {
        		// we found a restriction on the property
        		ClosableIterator<Triple> citr1 = context.find(restrict, OWL.allValuesFrom.asNode(), null);
        		if (citr1.hasNext()) {
        			// there are allValuesFrom restrictions...
    	    		while (citr1.hasNext() && theClass == null) {
    	    			theClass = getClassOfRestriction(context, restrict, val, citr1.next());
    	    		}
    	    		citr1.close();
        		}
        		if (theClass == null) {
        			citr1 = context.find(restrict, OWL.someValuesFrom.asNode(), null);
        			if (citr1.hasNext()) {
        				// there are someValueFrom restrictions
        	    		while (citr1.hasNext() && theClass == null) {
        	    			theClass = getClassOfRestriction(context, restrict, val, citr1.next());
        	    		}
        			}
    	    		citr1.close();
        		}
        		
            }
        	if (theClass != null) {
        		break;
        	}
    	}
		citr0.close();

		if (theClass != null) {
    		return context.getEnv().bind(args[length - 1], theClass);  
    	}
    	return false;
    }

	private Node getClassOfRestriction(RuleContext context, Node restrict, Node val, Triple tr) {
		Node theClass = null;
		Node dr = tr.getObject();
        if (val.isLiteral()) {
        	// this can only be a DataRange oneOf
        	ClosableIterator<Triple> citr2 = context.find(dr, OWL.oneOf.asNode(), null);
        	while (citr2.hasNext() && theClass == null) {
        		Triple oftr = citr2.next();
        		Node lst = oftr.getObject();
        		List<Node> oflist = Util.convertList(lst, context);
        		for (int i = 0; oflist != null && i < oflist.size(); i++) {
        			Node lstitem = oflist.get(i);
        			if (lstitem.isLiteral() && (lstitem.equals(val) || 
        					lstitem.sameValueAs(val))) {
        				ClosableIterator<Triple> citr3 = context.find(null, RDFS.subClassOf.asNode(), restrict);
        				while (citr3.hasNext()) {
        					Triple thetr = citr3.next();
        					theClass = thetr.getSubject();
        					if (theClass.isURI()) {
        						citr3.close();
        						break;
        					}
        				}
        				citr3.close();
        			}
        		}
        	}
   			citr2.close();
        }
        else {
        	// this will be a restriction to a class
        	if (val.equals(dr)) {
				ClosableIterator<Triple> citr2 = context.find(null, RDFS.subClassOf.asNode(), restrict);
				if (citr2.hasNext()) {
					Triple thetr = citr2.next();
					theClass = thetr.getSubject();
				}
				citr2.close();
        	}
        }
		return theClass;
	}
    
}
