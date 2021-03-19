/************************************************************************
 * Copyright © 2021 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemantics.sadl.jena.reasoner.builtin;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.util.iterator.ExtendedIterator;

import com.ge.research.sadl.jena.reasoner.builtin.TypedBaseBuiltin;

/**
 * This class implements a Jena Built-in function in the bodyCall method that takes a SADL typed 
 * list and an element of that list as arguments and binds the next list element after the 
 * specified element, if such exists, to the 3rd argument. If successful, the method returns true
 * else it returns false.
 * @author andy@naturalsemantics.com
 */
public class ElementAfter extends TypedBaseBuiltin {

	private int argLength = 3;
	
	@Override
	public String getName() {
		return "elementAfter";
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
        Node typedList = getArg(0, args, context);
        Node matchElement = getArg(1, args, context);
        Node slmfirst = NodeFactory.createURI("http://sadl.org/sadllistmodel#first");
        Node slmrest = NodeFactory.createURI("http://sadl.org/sadllistmodel#rest");
        
        Node relevantList = typedList;
        
        boolean atMatchingElement = false;
        do {
            ClosableIterator<Triple> itr = context.find(relevantList, slmfirst, null);
            if (itr.hasNext()) {
            	Node firstElement = itr.next().getObject();
            	if (firstElement != null && firstElement.equals(matchElement)) {
            		itr.close();
            		atMatchingElement = true;
             	}
            }
	        ClosableIterator<Triple> ritr = context.find(relevantList, slmrest, null);
	        if (ritr.hasNext()) {
	        	relevantList = ritr.next().getObject();
	        }
	        else {
	        	relevantList = null;
	        }
	        ritr.close();
        } while (!atMatchingElement && relevantList != null);
        
        ClosableIterator<Triple> itr = context.find(relevantList, slmfirst, null);
        if (itr.hasNext()) {
        	Node firstElement = itr.next().getObject();
        	if (firstElement != null) {
        		itr.close();
        		return context.getEnv().bind(args[length - 1], firstElement);	     
        	}
        }
        boolean debug = true;;
		if (debug ) {
            ClosableIterator<Triple> itr2 = context.find(typedList, null, null);
            if (itr2.hasNext()) {
	            while (itr2.hasNext()) {
	            	System.out.println(itr2.next().toString());
	            }
            }
            else {
            	ExtendedIterator<Triple> itr3 = context.getGraph().find();
            	if (itr3.hasNext()) {
	            	while (itr3.hasNext()) {
	            		System.out.println(itr3.next().toString());
	            	}
            	}
            	else {
            		System.out.println(context.getGraph().toString());
            	}
            	itr3.close();
            }
            itr2.close();
        }
        return false;
    }

	@Override
	public String getFunctionSignatureString() {
		return "elementAfter(string, string)string";
	}


}
