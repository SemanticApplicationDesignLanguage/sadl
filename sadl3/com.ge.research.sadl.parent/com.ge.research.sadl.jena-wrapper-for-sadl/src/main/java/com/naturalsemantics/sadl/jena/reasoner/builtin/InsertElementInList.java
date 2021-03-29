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

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.RDF;

import com.ge.research.sadl.jena.reasoner.builtin.TypedBaseBuiltin;
import com.ge.research.sadl.jena.reasoner.builtin.Utils;

/**
 * This class implements a Jena Built-in function in the bodyCall method that takes a SADL typed 
 * list, a new element of the same type as the list type, and an int index as arguments. The method
 * creates a new list with the new element in the index-th position and binds the new list to the
 * 4th variable. If successful, the method returns true else it returns false.
 * @author andy@naturalsemantics.com
 */
public class InsertElementInList extends TypedBaseBuiltin {

	private int argLength = 4;
	
	@Override
	public String getName() {
		return "insertElementInList";
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
        Node newElement = getArg(1, args, context);
        Node indexNode = getArg(2, args, context);
        int index = -1;
        Object v1 = indexNode.getLiteralValue();
        if (v1 instanceof Number) {
            index = ((Number)v1).intValue();
        }
        if (index >= 0) {
	        Node slmfirst = NodeFactory.createURI("http://sadl.org/sadllistmodel#first");
	        Node slmrest = NodeFactory.createURI("http://sadl.org/sadllistmodel#rest");
	        
	        List<Node> oldListElements = new ArrayList<Node>();
	        Node currentList = typedList;
	        while (currentList != null) {
	        	ClosableIterator<Triple> fitr = context.find(currentList, slmfirst, null);
	        	if (fitr.hasNext()) {
	        		oldListElements.add(fitr.next().getObject());
	        		fitr.close();
	        	}
	        	else {
	        		// this list has no element, an empty list
	        		fitr.close();
	        		break;
	        	}
		        ClosableIterator<Triple> ritr = context.find(currentList, slmrest, null);
		        if (ritr.hasNext()) {
		        	currentList = ritr.next().getObject();
		        }
		        else {
		        	currentList = null;
		        }
		        ritr.close();
	        }
	        // create new lists of the same type(s) as old list
		    // add the elements, inserting the new one in the index location
	        Node firstList = null;
	        Node lastList = null;
	        if (index > oldListElements.size()) {
	        	System.out.println("Index of insertion location is out of bounds of list (index = " + index + ", list length = " + oldListElements.size() + ")");
	        	return false;
	        }
	        if (oldListElements.size() == 0 && index == 0) {
	        	// inserting into empty list
		        Node newList = createTypedList(typedList, context);
        		Triple t = new Triple(newList, slmfirst, newElement);
        		Utils.doAddTriple(t, context, true);
        		firstList = newList;
	        }
	        else {
		        for (int i = 0; i < oldListElements.size(); i++) {
			        Node newList = createTypedList(typedList, context);
		        	if (i == index) {
		        		// the new element goes into newList
		        		// set first property of newList to new element
		        		Triple t = new Triple(newList, slmfirst, newElement);
		        		Utils.doAddTriple(t, context, true);
		        		// create a new List to contain the current element from the old list
		        		Node newList2 = createTypedList(typedList, context);
		        		if (lastList != null) {
		        			Triple t2 = new Triple(lastList, slmrest, newList);
		        			Utils.doAddTriple(t2, context, true);
		        			lastList = newList;
		        			newList = newList2;
		        		}
		        		else {
		        			// the new element is inserted into the start of the list
		        			lastList = newList;
		        			newList = newList2;
		        		}
		        	}
	        		Triple t1 = new Triple(newList, slmfirst, oldListElements.get(i));
	        		Utils.doAddTriple(t1, context, true);
	        		if (lastList == null) {
	        			firstList = newList;
	        		}
	        		else {
	        			Triple t2 = new Triple(lastList, slmrest, newList);
	        			Utils.doAddTriple(t2, context, true);
	        		}
	        		lastList = newList;
		        }
	        }
	        
       		return context.getEnv().bind(args[length - 1], firstList);	     
        }
        boolean debug = false;;
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

    /**
     * Method to create a new List instance of the same type as the List passed in.
     * @param typedList
     * @param context
     * @return
     */
	private Node createTypedList(Node typedList, RuleContext context) {
		Node newList = NodeFactory.createBlankNode();
		ClosableIterator<Triple> typitr = context.find(typedList, RDF.type.asNode(), null);
		if (typitr.hasNext()) {
			Triple typtr = new Triple(newList, RDF.type.asNode(), typitr.next().getObject());
			Utils.doAddTriple(typtr, context, true);
		}
		return newList;
	}

	@Override
	public String getFunctionSignatureString() {
		return "insertElementInList(--, --, int)--";
//		return "insertElementInList(--)--";
	}


}
