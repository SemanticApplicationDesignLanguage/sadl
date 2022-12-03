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

import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.iterator.ClosableIterator;

import com.ge.research.sadl.jena.reasoner.builtin.TypedBaseBuiltin;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;

/**
 * This class implements a Jena Built-in function in the bodyCall method that takes a SADL typed 
 * list and checks to see if the list is the head of list, that is, it is not the object of any
 * triple with predicate "rest". If no such triple exists, the method returns true
 * else it returns false.
 * @author andy@naturalsemantics.com
 */
public class IsListHead extends TypedBaseBuiltin {

	private int argLength = 1;
	
	@Override
	public String getName() {
		return "isListHead";
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
        Node slmrest = NodeFactory.createURI("http://sadl.org/sadllistmodel#rest");
        
        ClosableIterator<Triple> itr = context.find(null, slmrest, typedList);
        if (itr.hasNext()) {
        	itr.close();
        	return false;
        }
		itr.close();
		return true;
    }

	@Override
	public String getFunctionSignatureString() {
		return "isListHead(--)";
	}

	@Override
	public com.ge.research.sadl.model.gp.Node validateArgumentTypes(OntModel model, List<com.ge.research.sadl.model.gp.Node> argTypes) throws UnittedQuantityHandlerException {
		// TODO Auto-generated method stub
		return null;
	}


}
