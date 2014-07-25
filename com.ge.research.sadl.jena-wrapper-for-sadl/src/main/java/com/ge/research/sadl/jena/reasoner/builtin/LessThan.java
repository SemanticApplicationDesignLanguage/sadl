package com.ge.research.sadl.jena.reasoner.builtin;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.NodeFactory;
import com.hp.hpl.jena.graph.impl.LiteralLabelFactory;
import com.hp.hpl.jena.reasoner.rulesys.BuiltinException;
import com.hp.hpl.jena.reasoner.rulesys.RuleContext;
import com.hp.hpl.jena.reasoner.rulesys.Util;

public class LessThan extends com.hp.hpl.jena.reasoner.rulesys.builtins.LessThan {
	
    @Override
    public int getArgLength() {
        return -1;
    }
    
	/**
	 * This extends the Jena GreaterThan built-in to compare string literals and URIs
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
        	 Node booleanVal =  NodeFactory.createLiteral(LiteralLabelFactory.create(new Boolean(retVal)));
             return context.getEnv().bind(args[length - 1], booleanVal);
        }
    }

}
