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
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.RDF;

import com.ge.research.sadl.jena.reasoner.builtin.TypedBaseBuiltin;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;

/**
 * This class implements a Jena Built-in function in the bodyCall method that takes a SADL typed 
 * list as input argument and binds the length of the list to the 2nd argument. 
 * If successful, the method returns true else it returns false.
 * @author andy@naturalsemantics.com
 */
public class ListLength extends TypedBaseBuiltin {

	private int argLength = 2;
	
	@Override
	public String getName() {
		return "listLength";
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
        Node slmList = NodeFactory.createURI(SadlConstants.SADL_LIST_MODEL_LIST_URI);
        Node slmfirst = NodeFactory.createURI("http://sadl.org/sadllistmodel#first");
        Node slmrest = NodeFactory.createURI("http://sadl.org/sadllistmodel#rest");
        
        Node relevantList = typedList;
        
        
        int listLength = 0;
        ClosableIterator<Triple> fitr = context.find(relevantList, slmfirst, null);
        if (fitr.hasNext()) {
        	listLength = 1;
        	fitr.close();
        }
        else {
        	// the anticipated list doesn't have a first. Could it be an rdf:List?
        	fitr = context.find(relevantList, RDF.first.asNode(), null);
        	if (fitr.hasNext()) {
        		return jenaListLengthBodyCall(args, length, context);
        	}
        }
        boolean atLastElement = false;
        do {
	        ClosableIterator<Triple> ritr = context.find(relevantList, slmrest, null);
	        if (ritr.hasNext()) {
	        	relevantList = ritr.next().getObject();
	        	listLength++;
	        }
	        else {
	        	atLastElement = true;
	        }
	        ritr.close();
        } while (!atLastElement);
        
    	Node lengthNode = Util.makeIntNode(listLength);
		return context.getEnv().bind(args[length - 1], lengthNode);	     
    }
    
	@Override
	public String getFunctionSignatureString() {
		return "listLength(--)int";
	}

	/*
	 * The two methods below are taken from the class org.apache.jena.reasoner.rulesys.builtins.ListLength
	 * and meant to handle an rdf:List argument
	 */
	public boolean jenaListLengthBodyCall(Node[] args, int length, RuleContext context) {
        BindingEnvironment env = context.getEnv();
        int len = getLength(getArg(0, args, context), context);
        if (len == -1) {
            return false;
        } else {
            env.bind(args[1], Util.makeIntNode(len));
            return true;
        }
    }
    
    /**
     * Return the length of the RDF list rooted at the given node. 
     * @param node the start of the list
     * @param context the context through which the data values can be found
     * @return the length or -1 for a malformed list.
     */
    protected static int getLength(Node node, RuleContext context ) {
         if (node.equals(RDF.Nodes.nil)) {
             return 0;
         } else {
             Node next = Util.getPropValue(node, RDF.Nodes.rest, context);
             if (next == null) {
                 return -1;
             } else {
                 int sublen = getLength(next, context);
                 if (sublen == -1) {
                     return -1;
                 } else {
                     return 1 + sublen;
                 }
             }
         }
    }

	@Override
	public com.ge.research.sadl.model.gp.Node validateArgumentTypes(OntModel model, List<com.ge.research.sadl.model.gp.Node> argTypes) throws UnittedQuantityHandlerException {
		// TODO Auto-generated method stub
		return null;
	}

}
