package com.ge.research.sadl.jena.reasoner.builtin;

import org.apache.jena.graph.Node;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;

/**
 * @author alfredo
 *
 */
public class StrBefore extends BaseBuiltin {

	@Override
	public String getName() {
		return "strbefore";
	}

    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return 3;
    }
	
    public boolean bodyCall(Node[] args, int length, RuleContext context) {
        checkArgs(length, context);
        BindingEnvironment env = context.getEnv();
        Node n1 = getArg(0, args, context);
        Node n2 = getArg(1, args, context);
        if (n1.isLiteral() && n2.isLiteral()) {
            Object v1 = n1.getLiteralValue();
            Object v2 = n2.getLiteralValue();
            Node result = null;
            if (v1 instanceof String && v2 instanceof String) {
            	String s1 = (String)v1;
            	String s2 = (String)v2;
            	String sa = s1.split(s2,2)[0];
            	result = ResourceFactory.createTypedLiteral(sa).asNode();
            	return env.bind(args[2], result);
            }

        }
    	return false;
    }
    
}
