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

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;

public class LessThan extends org.apache.jena.reasoner.rulesys.builtins.LessThan {
	
    @Override
    public int getArgLength() {
        return -1;
    }
    
	/**
	 * This extends the Jena LessThan built-in to compare string literals and URIs
	 */
    @Override
    public boolean bodyCall(Node[] args, int length, RuleContext context) {
        if (length > 3 || length < 2) {
            throw new BuiltinException(this, context, "builtin " + getName() + " requires 2 or 3 arguments but saw " + length);
        }
        Node n1 = getArg(0, args, context);
        Node n2 = getArg(1, args, context);

        boolean retVal;
        
        if (Util.comparable(n1, n2)) {
        	retVal = super.bodyCall(args, length, context);
    	}
        else {
        	String n1str;
        	String n2str;
        	if (n1.isURI()) {
        		n1str = n1.getURI();
        	}
        	else if (n1.isLiteral()) {
        		n1str = n1.getLiteralValue().toString();
        	}
        	else {
        		n1str = n1.toString();
        	}
        	if (n2.isURI()) {
        		n2str = n2.getURI();
        	}
        	else if (n2.isLiteral()) {
        		n2str = n2.getLiteralValue().toString();
        	}
        	else {
        		n2str = n2.toString();
        	}
        	retVal = (n1str.compareTo(n2str) < 0);
    	}
         
        if (length == 2) {
        	return retVal;
        }
        else {
        	 Node booleanVal =  NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(Boolean.valueOf(retVal)));
             return context.getEnv().bind(args[length - 1], booleanVal);
        }
    }

}
