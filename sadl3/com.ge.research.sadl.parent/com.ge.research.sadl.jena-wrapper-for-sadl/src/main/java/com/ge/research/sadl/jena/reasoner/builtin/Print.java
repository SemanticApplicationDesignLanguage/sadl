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

import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Node_Literal;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.PrintUtil;

import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;

/**
 * Print its argument list as a side effect
 * 
 * @author <a href="mailto:der@hplb.hpl.hp.com">Dave Reynolds</a>
 * @version $Revision: 1.1 $ on $Date: 2014/01/31 15:41:06 $
 */

/**
 * Note: this version of print is a copy of the HP Labs version with the
 * output modified to go to the message handler instead of to System.out.
 */
public class Print extends TypedBaseBuiltin {

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "print";
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
        print(args, length, context);
        return true;
    }
    
    
    /**
     * This method is invoked when the builtin is called in a rule head.
     * Such a use is only valid in a forward rule.
     * @param args the array of argument values for the builtin, this is an array 
     * of Nodes.
     * @param context an execution context giving access to other relevant data
     */
    public void headAction(Node[] args, int length, RuleContext context) {
        print(args, length, context);
    }
    
    /**
     * Print a node list to stdout (stdout may have been redirected to the IDE console)
     */
    public void print(Node[] args, int length, RuleContext context) {
        String msg = "";
    	for (int i = 0 ; i < length; i++) {
    		Node arg = getArg(i, args, context);
    		if (arg instanceof Node_Literal) {
    			Object argLV = ((Node_Literal)arg).getLiteralValue();
    			msg += PrintUtil.print(argLV + " ");
    		}
    		else {
    			msg += PrintUtil.print(arg) + " ";
    		}
        }
    	System.out.println("print: " + msg);
    }

	@Override
	public String getFunctionSignatureString() {
		return "print(...)--";
	}

	@Override
	public com.ge.research.sadl.model.gp.Node validateArgumentTypes(OntModel model, List<com.ge.research.sadl.model.gp.Node> argTypes) throws UnittedQuantityHandlerException {
		// TODO Auto-generated method stub
		return null;
	}
}
