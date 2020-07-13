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

import java.io.InvalidObjectException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.graph.Triple;
//import org.apache.jena.graph.impl.LiteralLabel;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.reasoner.rulesys.FBRuleInfGraph;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;

/**
 * This class provides useful utilities for building Jena builtin functions.
 * @author 200005201
 *
 */
public class Utils {
	private static final Logger _logger = LoggerFactory.getLogger(Utils.class);
	
    /**
     * Call this method to delete, replace, or create a triple with the given subject, datatype property, and data value. 
     * For the given subject and property, any existing matching triples will be deleted. If the object is not null, a
     * new triple will be created with that object value.
     * @param context - the RuleContext from the calling Jena built-in
     * @param s - the subject instance
     * @param p - the property
     * @param obj - the value (if any) to be set; should be a Long,Integer, Double, or Float (Date not yet supported)
     * @return - true if success
     * @throws Exception - throws an Exception if an unsupported Object is passed as obj
     */
    public static synchronized boolean deleteReplaceCreateValue(RuleContext context, Node s, Node p, Object obj) throws Exception {
    	boolean triplesRemoved = false;
		ClosableIterator<Triple> citr = context.find(s, p, null);
		if (citr.hasNext()) {
			List<Triple> toBeRemoved = new ArrayList<Triple>();
			while (citr.hasNext()) {
				Triple t = (Triple) citr.next();
				toBeRemoved.add(t);
			}
			citr.close();
			for (int i = 0; i < toBeRemoved.size(); i++) {
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " removing triple (" + toBeRemoved.get(i).toString() + ")");
				}
				doRemoveTriple(toBeRemoved.get(i), context, false);
				triplesRemoved = true;
			}
	    }
	    else {
	    	citr.close();
	    }
		if (obj != null) {
			Triple t = null;
			if (obj instanceof Double) {
				t = new Triple(s, p, Util.makeDoubleNode(((Double)obj).doubleValue()));
				doAddTriple(t, context, !triplesRemoved);
			}
			else if (obj instanceof Float){
				t = new Triple(s, p, Util.makeDoubleNode(((Float)obj).doubleValue()));
				doAddTriple(t, context, !triplesRemoved);
			}
			else if (obj instanceof Long) {
				t = new Triple(s, p, Util.makeLongNode(((Long)obj).longValue()));
				doAddTriple(t, context, !triplesRemoved);
			}
			else if (obj instanceof Integer) {
				t = new Triple(s, p, Util.makeIntNode(((Integer)obj).intValue()));
				doAddTriple(t, context, !triplesRemoved);
			}
			else if (obj instanceof Date) {
				throw new Exception("Date (" + obj.toString() + " )not yet supported in update built-in.");
			}
			else {
				throw new Exception(obj.getClass().getCanonicalName() + " (" + obj.toString() + ") not yet supported in update built-in.");
			}
			if (_logger.isDebugEnabled()) {
				_logger.debug("in Rule " + context.getRule().getName() + " added new triple (" + t.toString() + ") (notify deductions " + !triplesRemoved + ")");
			}
			return true;
		}
		return false;
	}

    /**
     * Call this method to delete, replace, or create a triple with the given subject, object property, and obj instance value. 
     * For the given subject and property, any existing matching triples will be deleted. If the object is not null, a
     * new triple will be created with that object value.
     * @param context - the RuleContext from the calling Jena built-in
     * @param s - the subject instance
     * @param p - the property
     * @param obj - the value (if any) to be set; should be an instance
     * @return - true if success
     * @throws Exception - throws an Exception if an unsupported Object is passed as obj
     */
	public static synchronized boolean deleteReplaceCreateValue(RuleContext context, Node s, Node p, Node obj) {
		boolean tripleExisted = false;
    	ClosableIterator<Triple> citr = context.find(s, p, null);
		if (citr.hasNext()) {
			List<Triple> toBeRemoved = new ArrayList<Triple>();
			while (citr.hasNext()) {
				Triple t = (Triple) citr.next();
				if (t.getObject().equals(obj)) {
					return true;
				}
				toBeRemoved.add(t);
			}
			citr.close();
			for (int i = 0; i < toBeRemoved.size(); i++) {
				if (_logger.isDebugEnabled()) {
					_logger.debug("in Rule " + context.getRule().getName() + " removing triple (" + toBeRemoved.get(i).toString() + ")");
				}
				doRemoveTriple(toBeRemoved.get(i), context, false);
				tripleExisted = true;
			}
	    }
	    else {
	    	citr.close();
	    }
		if (obj != null) {
			Triple t = new Triple(s, p, obj);
			doAddTriple(t, context, !tripleExisted);  // context.silentAdd(t); //silentAdd(t);
			if (_logger.isDebugEnabled()) {
				_logger.debug("in Rule " + context.getRule().getName() + " added new triple (" + t.toString() + ") (notify deductions " + !tripleExisted + ")");
			}
			return true;
		}
		return false;
	}

	/**
	 * Call this method to add a triple to the context.
	 * 
	 * @param context - the RuleContext from the calling built-in
	 * @param s - the subject
	 * @param p - the property
	 * @param obj - the object value
	 */
    public static synchronized void addValue(RuleContext context, Node s, Node p, Node obj) {
		if (s != null && p != null && obj != null) {
			Triple t = new Triple(s, p, obj);
			doAddTriple(t, context, true);
		}
	}

    /**
     * Call this method to get a double value from a numeric literal node
     * @param n - a numeric literal node
     * @return - the double value of the literal
     * @throws InvalidObjectException 
     */
    public static synchronized double getDoubleFromLiteral(Node n) throws InvalidObjectException {
    	if (n != null && n.isLiteral()) {
    		Object ov = n.getLiteralValue();
    		if (ov instanceof Double) {
    			return ((Double)ov).doubleValue();
    		}
    		else if (ov instanceof Float) {
    			return ((Float)ov).doubleValue();
    		}
    		else if (ov instanceof Integer) {
    			return ((Integer)ov).doubleValue();
    		}
    	}
    	throw new InvalidObjectException("Node " + (n == null ? null : n.toString()) + " is not a literal, cannot convert to double.");
    }
    
    /**
     * Call this method to get a float value from a numeric literal node
     * @param n - a numeric literal node
     * @return - the float value of the literal
     * @throws InvalidObjectException 
     */
    public static synchronized float getFloatFromLiteral(Node n) throws InvalidObjectException {
    	if (n != null && n.isLiteral()) {
    		Object ov = n.getLiteralValue();
    		if (ov instanceof Double) {
    			return ((Double)ov).floatValue();
    		}
    		else if (ov instanceof Float) {
    			return ((Float)ov).floatValue();
    		}
    		else if (ov instanceof Integer) {
    			return ((Integer)ov).floatValue();
    		}
    	}
    	throw new InvalidObjectException("Node " + (n == null ? null : n.toString()) + " is not a literal, cannot convert to float.");
    }
    
    /**
     * Call this method to get a boolean value from a literal node
     * @param n - the literal node
     * @return - the boolean value
     * @throws InvalidObjectException 
     */
    public static synchronized boolean getBooleanFromLiteral(Node n) throws InvalidObjectException {
    	if (n != null && n.isLiteral()) {
    		Object ov = n.getLiteralValue();
    		if (ov instanceof Boolean) {
    			return ((Boolean)ov).booleanValue();
    		}
     	}
    	throw new InvalidObjectException("Node " + (n == null ? null : n.toString()) + " is not a literal, cannot convert to boolean.");
    }
    
    /**
     * Call this method to create a new URI with the namespace of input node n2 and the localname newName
     * @param n2 - the existing node whose namespace is to be used
     * @param newName - the localname to be used in the new URI
     * @return
     */
    public static synchronized String transferNsToLocalName(Node n2, String newName) {
		if (n2.isURI()) {
			return n2.getNameSpace() + newName;
		}
		return null;
	}

    /**
     * Call this method to create a time-based local name
     * @param guess - the basename to which a time value is to be appended to create the name
     * @return - the time-based name
     */
	public static synchronized String createUniqueNewLocalName(String guess) {
		String trial = (guess == null ? "n" : guess) + System.currentTimeMillis();
		return trial;
	}
	
	/**
	 * Call this method to create a new instance of an existing class within the context
	 * @param context - the RuleContext from the calling built-in
	 * @param newInstUri - the URI of the new instance to be created
	 * @param existingClass - the URI of the existing class
	 * @return - a new Node with the specified URI and of the specified type
	 */
	public static synchronized Node_URI createInstanceOfClass(RuleContext context, String newInstUri, String existingClass) {
		Node_URI theClass = (Node_URI) NodeFactory.createURI(existingClass);
		return createInstanceOfClass(context, newInstUri, theClass);
	}
	
	/**
	 * Call this method to create a new instance of an existing class within the context
	 * @param context - the RuleContext from the calling built-in
	 * @param newInstUri - the URI of the new instance to be created
	 * @param theClass - the class node of which the new instance is to be a type
	 * @return - a new Node with the specified URI and of the specified type
	 */
	public static synchronized Node_URI createInstanceOfClass(RuleContext context, String newInstUri, Node_URI theClass) {
		Node_URI newInst = (Node_URI) NodeFactory.createURI(newInstUri);
		Utils.deleteReplaceCreateValue(context, newInst, RDF.Nodes.type, theClass);
		return newInst;
	}

	/**
	 * Call this method to add a triple to the context when the URI is known for subject, property, and object value.
	 * @param context - the RuleContext from the calling built-in
	 * @param subject - the subject node
	 * @param predUri - the property URI
	 * @param object - the object value node
	 */
	public static synchronized void addObjectProperty(RuleContext context, Node_URI subject, String predUri, Node_URI object) {
		if (subject == null) {
			_logger.error("addObjectProperty called with null subject");
			return;
		}
		if (predUri == null) {
			_logger.error("addObjectProperty called with null property URI");
			return;
		}
		if (object == null) {
			_logger.error("addObjectProperty called with null object");
			return;
		}
    	Node_URI prop = (Node_URI) NodeFactory.createURI(predUri);
    	Triple t = new Triple(subject, prop, object);
		doAddTriple(t, context, true); 
		if (_logger.isDebugEnabled()) {
			_logger.debug("in Rule " + context.getRule().getName() + " added new triple (" + t.toString() + ") (notify deductions true)");
		}
//    	Utils.deleteReplaceCreateValue(context, subject, prop, object);
	}
	
	/**
	 * Call this method to get the value of a triple in the context with the given subject and predicate 
	 * @param context - the RuleContext from the calling built-in
	 * @param subject - the subject to be matched
	 * @param predUri - the URI of the predicate to be matched
	 * @return - the Node which is the object of a matching triple
	 */
	public static synchronized Node getPropertyValue(RuleContext context, Node subject, String predUri) {
		Node_URI  prop = (Node_URI) NodeFactory.createURI(predUri);
		return Util.getPropValue(subject, prop, context);
	}

	/**
	 * Call this method to determine if the given Node is of type owl:ObjectProperty in the context
	 * @param context - the RuleContext from the calling built-in
	 * @param p - the Node to be tested to see if it is an ObjectProperty
	 * @return - true if an owl:ObjectProperty else false
	 */
	public static synchronized boolean isObjectProperty(RuleContext context, Node p) {
		if (p.isURI()) {
			if (find(p, RDF.type.asNode(), OWL.ObjectProperty.asNode(), context) != null) {
				return true;
			}
		}
		return false;
	}
	
	public static synchronized boolean isOntClass(RuleContext context, Node cls) {
		if (cls.isURI()) {
			Node typ = find(cls, RDF.type.asNode(), null, null, context);
			if (typ != null && (typ.equals(RDFS.Class.asNode()) ||
							    typ.equals(OWL.Class.asNode()))) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Call this method to determine if the given Node is of type owl:DatatypeProperty in the context
	 * @param context - the RuleContext from the calling built-in
	 * @param p - the Node to be tested to see if it is an DatatypeProperty
	 * @return - true if an owl:DatatypeProperty else false
	 */
	public static synchronized boolean isDatatypeProperty(RuleContext context, Node p) {
		if (p.isURI()) {
			if (find(p, RDF.type.asNode(), OWL.DatatypeProperty.asNode(), context) != null) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Call this method to determine if the given Node is of type owl:ObjectProperty or owl:DatatypeProperty in the context
	 * @param context -- the RuleContext from the calling built-in
	 * @param p -- the Node to be tested
	 * @return -- true if an OntProperty else false
	 */
	public static synchronized boolean isOntProperty(RuleContext context, Node p) {
		if (isObjectProperty(context, p) || isDatatypeProperty(context, p)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Call this method to find the subject, predicate, or object of a triple matching the input
	 * pattern in the context. One of subject, predicate, or object can be null on input.
	 * @param subj - subject Node or null
	 * @param prop - predicate Node or null
	 * @param obj - object value Node or null
	 * @param cls - if subject is null, optional specification of a class to which the matching subject must belong
	 * @param context
	 * @return
	 */
	public static synchronized Triple find(Node subj, Node prop, Node obj, RuleContext context) {
    	ClosableIterator<Triple> itr = context.find(subj, prop, obj);
    	if (itr.hasNext()) {
    		Triple t = itr.next();
    		itr.close();
    		return t;
    	}
		return null;
	}

		/**
	 * Call this method to find the subject, predicate, or object of a triple matching the input
	 * pattern in the context. One of subject, predicate, or object can be null on input.
	 * @param subj - subject Node or null
	 * @param prop - predicate Node or null
	 * @param obj - object value Node or null
	 * @param cls - if subject is null, optional specification of a class to which the matching subject must belong
	 * @param context
	 * @return
	 */
	public static synchronized Node find(Node subj, Node prop, Node obj, Node cls, RuleContext context) {
    	ClosableIterator<Triple> itr = context.find(subj, prop, obj);
    	if (itr.hasNext()) {
    		Triple t = itr.next();
    		itr.close();
    		if (subj == null) {
    			if (cls == null || verifyNodeType(((Triple)t).getMatchSubject(), cls, context)) {
    				itr.close();
    				return ((Triple)t).getMatchSubject();
    			}
    		}
    		else if (prop == null) {
    			if (cls == null || verifyNodeType(((Triple)t).getMatchPredicate(), cls, context)) {
    				itr.close();
    				return ((Triple)t).getMatchPredicate();
    			}
    		}
    		else if (obj == null) {
    			if (cls == null || verifyNodeType(((Triple)t).getMatchObject(), cls, context)) {
    				itr.close();
    				return ((Triple) t).getMatchObject();
    			}
    		}
    	}
    	return null;
	}

	/**
	 * Call this method to add a triple to the context and optionally to the FBRuleInfGraph's deductions
	 * @param t - the triple to add
	 * @param context - the RuleContext from the calling built-in
	 * @param addToDeductions - if true also add the triple to the FBRuleInfGraph's deductions
	 */
	public static synchronized void doAddTriple(Triple t, RuleContext context, boolean addToDeductions) {
		context.add( t );
		boolean deductionsNotified = false;
		if (addToDeductions && context.getGraph() instanceof FBRuleInfGraph) {
			((FBRuleInfGraph) context.getGraph()).addDeduction(t);
			deductionsNotified = true;
		}
		if (_logger.isDebugEnabled()) {
			_logger.debug("in Rule " + context.getRule().getName() + " added to context: [" + t.toString() + "], deductions notified " + deductionsNotified);
		}
	}

	/**
	 * Call this method to remove a triple from the context and optionally from the FBRuleInfGraph's deductions
	 * @param t - the triple to remove
	 * @param context - the RuleContext from the calling built-in
	 * @param addToDeductions - if true also remove the triple from the FBRuleInfGraph's deductions
	 */
    public static synchronized void doRemoveTriple(Triple t, RuleContext context, boolean removeFromDeductions) {
		context.remove(t);	
		boolean deductionsNotified = false;
		if (removeFromDeductions && context.getGraph() instanceof FBRuleInfGraph) {
			((FBRuleInfGraph) context.getGraph()).delete(t);
			deductionsNotified = true;
		}
		if (_logger.isDebugEnabled()) {
			_logger.debug("in Rule " + context.getRule().getName() + " removed from context: [" + t.toString() + "], deductions notified " + deductionsNotified);
		}
	}

	public static synchronized boolean verifyNodeType(Node matching, Node cls, RuleContext context) {
    	ClosableIterator<Triple> itr = context.find(matching, RDF.Nodes.type, cls);
    	if (itr.hasNext()) {
    		itr.close();
    		return true;
    	}
    	itr.close();
		if (_logger.isDebugEnabled()) {
	    	itr = context.find(matching, RDF.Nodes.type, null);
	    	while (itr.hasNext()) {
    			_logger.debug("Check for type '" + cls.toString() + "' failed, type is '" + itr.next().toString() + "'");
    		}
    	}
    	return false;
	}

	/**
     * Construct a new float valued node
     */
    public static synchronized Node makeFloatNode(double value) {
    	return NodeFactory.createLiteral(LiteralLabelFactory.create(new Float(value)));
//        return Node.createLiteral(new LiteralLabel(new Float(value)));
    }
    
    public static synchronized Node makeBooleanNode(boolean value) {
    	return NodeFactory.createLiteral(LiteralLabelFactory.create(new Boolean(value)));
    }
    
    public static synchronized Node makeXSDDateTimeNode(XSDDateTime value) {
    	return NodeFactory.createLiteral(LiteralLabelFactory.create(value));
    }
}
