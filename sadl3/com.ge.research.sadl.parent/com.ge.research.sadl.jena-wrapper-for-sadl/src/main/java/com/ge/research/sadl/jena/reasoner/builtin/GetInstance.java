/******************************************************************
 * File:        GetInstance.java
 * Created by:  Andrew Crapo
 * 
 * (c) Copyright 2009 General Electric Company
 *
 * $Id: GetInstance.java,v 1.2 2014/02/05 21:26:02 crapo Exp $
 *****************************************************************/
package com.ge.research.sadl.jena.reasoner.builtin;

import java.io.InvalidObjectException;
import java.util.ArrayList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.CancellableBuiltin;
import com.ge.research.sadl.jena.reasoner.builtin.Utils;
import com.hp.hpl.jena.reasoner.rulesys.*;
import com.hp.hpl.jena.util.iterator.ClosableIterator;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.graph.*;

/**
 * Create a new anonymous node and bind it to the each argument
 * 
 * @author <a href="mailto:der@hplb.hpl.hp.com">Dave Reynolds</a>
 * @version $Revision: 1.2 $ on $Date: 2014/02/05 21:26:02 $
 */
public class GetInstance extends CancellableBuiltin {
    private static final Logger _logger = LoggerFactory.getLogger (GetInstance.class) ;
	private boolean doTransitiveClosure = true;

    private Node[] subj = null;
	private Node[] prop = null;
	private Node[] obj = null;
	private boolean[] getObjectValue = null;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "getInstance";
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
    	if (length < 2) { // || length == 3) {
    		throw new BuiltinException(this, context, "builtin " + getName() + " requires 2 or more arguments");
    	}
    	Node theInst = doGetInstance(args, length, context);
//    	if (!(args[length - 1] instanceof Node_RuleVariable)) {
//    		System.out.println(context.getGraph().getDeductionsGraph().toString());
//    		throw new BuiltinException(this, context, "getInstance called with last arg already bound: " + args[length - 1].toString());
//    	}
    	return context.getEnv().bind(args[length - 1], theInst);  
    }

	private Node doGetInstance(Node[] args, int length, RuleContext context) {
		checkCanceled(args, context);
		Node pclass = getArg(0, args, context);
    	if (!Utils.isOntClass(context, pclass)) {
    		throw new BuiltinException(this, context, "First argument to getInstance is not a class (" + pclass.toString() + ")");
    	}
    	Object[] inst = null;
    	Node theInst = null;
    	int numMatchSets = 0;
    	boolean forceNew = false;
		try {
			// only check for forceNew if number of args is odd (class, n pairs * 2, return var, + this flag)
			if (length % 2 == 1) {
				forceNew = Utils.getBooleanFromLiteral(getArg((length - 2), args, context));
			}
		} catch (InvalidObjectException e) {
			_logger.debug("in Rule " + context.getRule().getName() + ":", e);
			throw new BuiltinException(this, context, "Invalid argument: " + e.getMessage());
		}
    	getObjectValue = null;//true;
    	if (length > 2) {
    		numMatchSets = (length - 2) / 2;
    		subj = new Node[numMatchSets];
    		prop = new Node[numMatchSets];
    		obj = new Node[numMatchSets];
    		inst = new Object[numMatchSets];
    		getObjectValue = new boolean[numMatchSets];
    		boolean noMatchesInSomeMatchSet = false;
    		for (int i = 0; i < numMatchSets; i++) {
	    		Node n1 = getArg(1 + i * 2, args, context);	// 1, 3, 5, etc
	    		Node n2 = getArg(2 + i * 2, args, context);	// 2, 4, 6, etc
	    		if (Utils.isObjectProperty(context, n1) || Utils.isDatatypeProperty(context, n1)) {
	    			prop[i] = n1;
	    			obj[i] = n2;
	    			if (!forceNew && !noMatchesInSomeMatchSet) {
	    				ClosableIterator<Triple> citr = context.find(null, prop[i], obj[i]);
	    				java.util.List<Node> matches = null;
	    				while (citr.hasNext()) {
	    					Triple triple = (Triple) citr.next();
	    					Node sn = triple.getSubject();
	    					if (Utils.verifyNodeType(sn, pclass, context)) {
		    					if (matches == null) {
		    						matches = new ArrayList<Node>();
		    					}
		    					matches.add(sn);
	    					}
    						_logger.debug("in Rule " + context.getRule().getName() + " getInstance match failed because '" + sn.toString() + "' is not of type '" + pclass.toString() + "'");
	    				}
	    				if (matches != null) {
	    					if (matches.size() > 1) {
	    						inst[i] = matches;
	    					}
	    					else {
	    						inst[i] = matches.get(0);
	    					}
	    				}
	    			}
	    			getObjectValue[i] = false;
	    		}
	    		else if (Utils.isObjectProperty(context, n2) || Utils.isDatatypeProperty(context, n2)) {
	    			subj[i] = n1;
	    			prop[i] = n2;
	    			if (!forceNew && !noMatchesInSomeMatchSet) {
	    				ClosableIterator<Triple> citr = context.find(subj[i], prop[i], null);
	    				java.util.List<Node> matches = null;
	    				while (citr.hasNext()) {
	    					Triple triple = (Triple) citr.next();
	    					Node on = triple.getObject();
	    					if (Utils.verifyNodeType(on, pclass, context)) {
		    					if (matches == null) {
		    						matches = new ArrayList<Node>();
		    					}
		    					matches.add(on);
	    					}
    						_logger.debug("in Rule " + context.getRule().getName() + " getInstance match failed because '" + on.toString() + "' is not of type '" + pclass.toString() + "'");
	    				}
	    				if (matches != null) {
	    					if (matches.size() > 1) {
	    						inst[i] = matches;
	    					}
	    					else {
	    						inst[i] = matches.get(0);
	    					}
	    				}
	    			}
	    			getObjectValue[i] = true;
	    		}
	    		else {
	    			throw new BuiltinException(this, context, "A pair-pattern passed to getInstance must have a property as one of the pair (" + n1.toString() + ", " + n2.toString() + ")");
	    		}
	    		if (inst[i] == null) {
	    			noMatchesInSomeMatchSet = true;
	    		}
    		}
    		
    		Object tentative = null;
    		for (int i = 0; i < numMatchSets; i++) {
    			if (i == 0) {
    				tentative = inst[i];
    			}
    			else {
    				if (tentative == null || (tentative = compareMatchSets(context, tentative, inst[i])) == null) {
    					theInst = null;
    					break;
    				}
    			}
    		}
    		if (tentative != null && tentative instanceof Node) {
    			theInst = (Node) tentative;
    		}
    		
    		if (theInst != null) {
    			for (int i = 0; i < numMatchSets; i++) {
    				if (getObjectValue[i]) {
    					obj[i] = theInst;
    				}
    				else {
    					subj[i] = theInst;
    				}
    			}
    		}
    	}
    	
    	if (theInst == null) {
    		theInst = NodeFactory.createAnon();
			if (_logger.isDebugEnabled()) {
				_logger.debug("in Rule " + context.getRule().getName() + " created new bnode (" + theInst.toString() + " type will be " + pclass.toString());
			}
			Triple t = new Triple(theInst, RDF.type.asNode(), pclass);
			Utils.doAddTriple(t, context, true);		
    		if (pclass.equals(RDF.List.asNode())) {
    			Triple t3 = new Triple(theInst, RDF.first.asNode(), RDF.nil.asNode());
    			Utils.doAddTriple(t3, context, true);
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " created new rdf:List first triple (" + t3.toString() + ")");
				}
    			Triple t4 = new Triple(theInst, RDF.rest.asNode(), RDF.nil.asNode());
    			Utils.doAddTriple(t4, context, true);
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " created new rdf:List rest triple (" + t4.toString() + ")");
				}
    		}
    		else {
    			if (doTransitiveClosure ) {
    				doTransitiveClosure(theInst, pclass, context);
    			}
    		}
    		if (length > 2) {
    			for (int i = 0; i < numMatchSets; i++) {
    				Triple t2 = getObjectValue[i] ? new Triple(subj[i], prop[i], theInst) : new Triple (theInst, prop[i], obj[i]);
    				Utils.doAddTriple(t2, context, true);
    				if (_logger.isDebugEnabled()) {
    					_logger.debug("in Rule " + context.getRule().getName() + " created new triple (" + t2.toString() + ")");
    				}
    			}
    		}
    	}
    	else if (_logger.isDebugEnabled()) {
    		for (int i = 0; i < numMatchSets; i++) {
    			_logger.debug("in Rule " + context.getRule().getName() + " instance found matches (" + (getObjectValue[i] ? 
    					(subj[i].toString() + ", " + prop[i].toString() + ", " + theInst.toString()) :
    						(theInst.toString() + ", " + prop[i].toString() + ", " + obj[i].toString())));
    		}
    	}
		return theInst;
	}
    
    
    private Object compareMatchSets(RuleContext context, Object t1, Object t2) {
		if (t1 == null || t2 == null) {
			return null;
		}
    	if (t1 instanceof Node && t2 instanceof Node) {
			if (t1.equals(t2)) {
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " found potential match: " + t1.toString());
				}
				return t1;
			}
			return null;
		}
		else if (t1 instanceof Node && t2 instanceof java.util.List) {
			if ( ((java.util.List)t2).contains(t1)) {
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " found potential match: " + t1.toString());
				}
				return t1;
			}
			return null;
		}
		else if (t2 instanceof Node && t1 instanceof java.util.List) {
			if (((java.util.List)t1).contains(t2)) {
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " found potential match: " + t2.toString());
				}
				return t2;
			}
			return null;
		}
		else if (t1 instanceof java.util.List && t2 instanceof java.util.List) {
			java.util.List<Object> matches = null;
//			Object match = null;
//			int numMatch = 0;
			Object retval = null;
			for (int i = 0; i < ((java.util.List)t1).size(); i++) {
				Node n = (Node) ((java.util.List)t1).get(i);
				retval = compareMatchSets(context, n, t2);
				if (retval != null) {
					if (matches == null) {
						matches = new ArrayList<Object>();
					}
					matches.add(retval);
//					numMatch++;
				}
			}
			if (matches != null) {
				if (matches.size() == 1) {
					return matches.get(0);
				}
//				throw new BuiltinException(this, context, "More than one instance matches the getInstance criteria; unable to return unique value.");
				_logger.error("Duplicate matches (" + (matches.size()) + ") found in rule " + context.getRule().getName() + ": ");
				if (getObjectValue != null && getObjectValue.length > 0) {
					for (int i = 0; i < matches.size(); i++) {
						_logger.error("  Match " + (i + 1) + ":");
						for (int j = 0; j < getObjectValue.length; j++) {
							if (getObjectValue[j]) {
			    				ClosableIterator<Triple> citr = context.find(subj[j], prop[j], (Node) matches.get(i));
			    				while (citr.hasNext()) {
			    					Triple tr = (Triple) citr.next();
			    					_logger.error("   Triple: " + tr.toString());
			    				}
							}
							else {
			    				ClosableIterator<Triple> citr = context.find((Node) matches.get(i), prop[j], obj[j]);
			    				while (citr.hasNext()) {
			    					Triple tr = (Triple) citr.next();
			    					_logger.error("   Triple: " + tr.toString());
			    				}
							}
						}
					}
				}
				return matches.get(0);
			}
		}
		else {
			throw new BuiltinException(this, context, "Unexpected exception in getInstance comparison: please report.");
		}
		return null;
	}

	/**
     * This method is invoked when the builtin is called in a rule head.
     * Such a use is only valid in a forward rule.
     * Exected args are the instance to be annotated, the property to use and the type
     * of the resulting bNode.
     * @param args the array of argument values for the builtin, this is an array 
     * of Nodes.
     * @param context an execution context giving access to other relevant data
     */
    public void headAction(Node[] args, int length, RuleContext context) {
    	// make doGetInstance behave as if there were a return value argument
        doGetInstance(args, length + 1, context);
    }

    /**
     * Returns false if this builtin has side effects when run in a body clause,
     * other than the binding of environment variables.
     */
    public boolean isSafe() {
         return false;
    }
    
    /**
     * Returns false if this builtin is non-monotonic. This includes non-monotonic checks like noValue
     * and non-monotonic actions like remove/drop. A non-monotonic call in a head is assumed to 
     * be an action and makes the overall rule and ruleset non-monotonic. 
     * Most JenaRules are monotonic deductive closure rules in which this should be false.
     */
    public boolean isMonotonic() {
        return false;
    }
 
    /**
     * Find superclasses of input cls and make inst a type of each
     * 
     * @param inst
     * @param cls
     */
    public static synchronized void doTransitiveClosure(Node inst, Node cls, RuleContext context) {
    	ClosableIterator<Triple> citr = context.find(cls, RDFS.subClassOf.asNode(), null);
    	while (citr.hasNext()) {
    		Triple t = citr.next();
    		Node scls = t.getObject();
    		if (scls.isURI()  && !scls.equals(cls)) {
    			// only give types that are named? (this would fail for unnamed unions, intersections, etc.
    			Triple tct = new Triple(inst, RDF.type.asNode(), scls);
    			Utils.doAddTriple(tct, context, true);
    		}
    	}
    }

}
