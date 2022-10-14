/************************************************************************
 * Copyright © 2007-2022 - General Electric Company, All Rights Reserved
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

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;
import org.apache.jena.vocabulary.RDF;

/**
 * Built-in to take an input RDF List and return a list with duplicates removed. If there
 * are no duplicates the original List will be returned.
 * 
 * @author <a href="mailto:der@hplb.hpl.hp.com">Dave Reynolds</a>
 * @version $Revision: 1.1 $ on $Date: 2014/01/31 15:29:27 $
 */
public class Unique extends BaseBuiltin {

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "unique";
    }
    
    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return 2;
    }
    
    /**
     * This method is invoked when the builtin is called in a rule head.
     * Such a use is only valid in a forward rule.
     * @param args the array of argument values for the builtin, this is an array 
     * of Nodes.
     * @param context an execution context giving access to other relevant data
     */
    public boolean bodyCall(Node[] args, int length, RuleContext context) {
        checkArgs(length, context);
        Node list = getArg(0, args, context);
        java.util.List<Node> lstItems = GeUtils.getListItems(context.getGraph(), null, list);
        if (lstItems != null) {
	        Node[] lstArray = lstItems.toArray(new Node[lstItems.size()]);
	        Node[] modified = GeUtils.removeDuplicatesFromList(lstArray);
	        if (modified != null && lstArray != null && modified.length == lstArray.length) {
	        	return context.getEnv().bind(args[length - 1], list);	
	        }
	        else if (modified != null && modified.length > 0) {
		        Node ul = GeUtils.makeList(modified, context);
		    	return context.getEnv().bind(args[length - 1], ul);	     
	        }
        }
        return context.getEnv().bind(args[length - 1], RDF.Nodes.nil);
    }
}
