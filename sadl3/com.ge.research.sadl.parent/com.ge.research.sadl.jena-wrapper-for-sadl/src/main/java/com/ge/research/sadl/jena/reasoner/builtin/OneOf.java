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
 * $Revision: 1.2 $ Last modified on   $Date: 2015/05/21 14:10:22 $
 ***********************************************************************/

package com.ge.research.sadl.jena.reasoner.builtin;

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;

/**
 * Tests whether the first argument matches any of the subsequent arguments.
 * 
 * @author <a href="mailto:andy@naturalsemantics.com">Andy Crapo</a>
 * @version Revised 2021/07/08
 */
public class OneOf extends BaseBuiltin {

	private int argLength = 0;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "oneOf";
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
        Node val = getArg(0, args, context);
        for (int i = 1; i < args.length; i++) {
        	Node toMatch = getArg(i, args, context);
        	if (val.equals(toMatch)) {
        		return true;
        	}
        	else if (val.isLiteral() && toMatch.isLiteral() && val.getLiteralValue().equals(toMatch.getLiteralValue())) {
        		// this is needed for some string matching when the literals aren't typed the same way
        		return true;
        	}
        }
        return false;
    }
    
}
