package com.ge.research.sadl.jena.reasoner.builtin;

import java.io.InvalidObjectException;
import java.util.ArrayList;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.processing.SadlConstants;

/**
 * Check to see if there is an instance of the specified class with the specified relationships. If such and instance
 * does not exist create one and instantiate the relationships. Look for a second set of relationships to be added to 
 * the instance found or created. This built-in is necessary to support instance creation in the rule conclusion that
 * doesn't allow creation if the rule does not fire and which allows the addition of triples for an existing matching
 * instance.
 * 
 * @author 200005201
 *
 */
public class ThereExists extends CancellableBuiltin {

	@Override
	public String getName() {
		return "thereExists";
	}

    private static final Logger _logger = LoggerFactory.getLogger (ThereExists.class) ;
	private boolean doTransitiveClosure = true;

    private Node[] subj = null;	// the set of subjects to be matched
	private Node[] prop = null;	// the set of predicates to be matched
	private Node[] obj = null;	// the set of objects to be matched
	private boolean[] getObjectValue = null;	// false if the first of a pair is a property else true if the second of the pair is a property 

	/**
     * This method is invoked when the builtin is called in a rule head.
     * Such a use is only valid in a forward rule.
     * Exected args are the instance to be annotated, the property to use and the type
     * of the resulting bNode.
     * @param allArgs the array of argument values for the builtin, this is an array 
     * of Nodes.
     * @param context an execution context giving access to other relevant data
     */
    public void headAction(Node[] allArgs, int length, RuleContext context) {
		checkCanceled(allArgs, context);
		Node pclass = getArg(0, allArgs, context);
    	if (!Utils.isOntClass(context, pclass)) {
    		throw new BuiltinException(this, context, "First argument to thereExists is not a class (" + pclass.toString() + ")");
    	}
    	
    	// divide args into those for matching and those for adding to the match or creation, retaining the location of the  split
    	Node plusInstance = NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_URI + "#Plus");
    	int plusLocation = -1;
    	Node[] matchArgs = null;
    	Node[] toBeAddedArgs = null;
    	for (int i = 0; i < allArgs.length; i++) {
    		if (allArgs[i].equals(plusInstance)) {
    			plusLocation = i;
    			length = i;
    			matchArgs = new Node[i];
    			for (int j = 0; j < i; j++) {
    				matchArgs[j] = allArgs[j];
    			}
    			toBeAddedArgs = new Node[allArgs.length - (i + 1)];
    		}
    		else if (toBeAddedArgs != null) {
    			toBeAddedArgs[i - (plusLocation + 1)] = allArgs[i];
    		}
    	}
    	if (plusLocation < 0) {
    		matchArgs = allArgs;
    	}
    	
    	// now do the matching to see if one or more matches exist
    	Object[] inst = null;
    	Node theInst = null;
    	java.util.List<Node> multipleInst = null;
    	int numMatchSets = 0;
    	boolean forceNew = false;
    	getObjectValue = null;
    	if (length > 2) {
    		numMatchSets = (length - 1) / 2;
    		subj = new Node[numMatchSets];
    		prop = new Node[numMatchSets];
    		obj = new Node[numMatchSets];
    		inst = new Object[numMatchSets];
    		getObjectValue = new boolean[numMatchSets];
    		boolean noMatchesInSomeMatchSet = false;
    		for (int i = 0; i < numMatchSets; i++) {
	    		Node n1 = getArg(1 + i * 2, matchArgs, context);	// 1, 3, 5, etc
	    		Node n2 = getArg(2 + i * 2, matchArgs, context);	// 2, 4, 6, etc
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
    						_logger.debug("in Rule " + context.getRule().getName() + " thereExists match failed because '" + sn.toString() + "' is not of type '" + pclass.toString() + "'");
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
    						_logger.debug("in Rule " + context.getRule().getName() + " thereExists match failed because '" + on.toString() + "' is not of type '" + pclass.toString() + "'");
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
	    			throw new BuiltinException(this, context, "A pair-pattern passed to thereExists must have a property as one of the pair (" + n1.toString() + ", " + n2.toString() + ")");
	    		}
	    		if (inst[i] == null) {
	    			noMatchesInSomeMatchSet = true;
	    		}
    		}
    		
    		// see if there are any tentative matches
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
    		if (tentative != null) {
    			// we have a tentative match
				multipleInst = new ArrayList<Node>();
    			if (tentative instanceof Node) {
    				// we have a single match
    				theInst = (Node) tentative;
    				multipleInst.add(theInst);
    			}
    			else if (tentative instanceof ArrayList<?>){
    				// we have multiple matches
    				for (int i = 0; i < ((ArrayList<?>)tentative).size(); i++) {
    					Object tent = ((ArrayList<?>)tentative).get(i);
    					if (tent instanceof Node) {
    						multipleInst.add((Node)tent);
    					}
    				}
    				theInst = multipleInst.get(0);
    			}
    		}
    	}
    	
    	// there were no matches so we need to create one and insert the required matching triples
    	if (theInst == null) {
    		theInst = NodeFactory.createAnon();
			if (_logger.isDebugEnabled()) {
				_logger.debug("in Rule " + context.getRule().getName() + " created new bnode (" + theInst.toString() + " type will be " + pclass.toString());
			}
			multipleInst = new ArrayList<Node>();
			multipleInst.add(theInst);
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
    		// output matches to the logger
    		for (int i = 0; i < numMatchSets; i++) {
    			for (int j = 0; j < multipleInst.size(); j++) {
    				theInst = multipleInst.get(j);
    				if (getObjectValue[i]) {
    					obj[i] = theInst;
    				}
    				else {
    					subj[i] = theInst;
    				}
	    			_logger.debug("in Rule " + context.getRule().getName() + " instance found matches (" + (getObjectValue[i] ? 
	    					(subj[i].toString() + ", " + prop[i].toString() + ", " + theInst.toString()) :
	    						(theInst.toString() + ", " + prop[i].toString() + ", " + obj[i].toString())));
    			}
    		}
    	}
    	
    	// now add additional triples if there are any
    	if (plusLocation > 0 && theInst != null) {
    		for (int i = 0; i < toBeAddedArgs.length; i=i+2) {
	     		// these must be in pairs
	    		Node n1 = toBeAddedArgs[i+0];
	    		Node n2 = toBeAddedArgs[i+1];
	    		Triple t;
	    		if (Utils.isObjectProperty(context, n1) || Utils.isDatatypeProperty(context, n1)) {
	    			for (int j = 0; j < multipleInst.size(); j++) {
	    				t = new Triple(multipleInst.get(j), n1, n2);
		    			Utils.doAddTriple(t, context, true);
						if (_logger.isDebugEnabled()) {
							_logger.debug("in Rule " + context.getRule().getName() + " created new add also triple (" + t.toString() + ")");
						}
	    			}
	    		}
	    		else if (Utils.isObjectProperty(context, n2) || Utils.isDatatypeProperty(context, n2)) {
	    			for (int j = 0; j < multipleInst.size(); j++) {
	    				t = new Triple(n1, n2, multipleInst.get(j));
		    			Utils.doAddTriple(t, context, true);
						if (_logger.isDebugEnabled()) {
							_logger.debug("in Rule " + context.getRule().getName() + " created new add also triple (" + t.toString() + ")");
						}
	    			}
	    		}	    		
	    		else {
	    			throw new BuiltinException(this, context, "A pair-pattern passed to thereExists plus must have a property as one of the pair (" + n1.toString() + ", " + n2.toString() + ")");
	    		}
    		}
    	}
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
//				throw new BuiltinException(this, context, "More than one instance matches the thereExists criteria; unable to return unique value.");
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
			throw new BuiltinException(this, context, "Unexpected exception in thereExists comparison: please report.");
		}
		return null;
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
