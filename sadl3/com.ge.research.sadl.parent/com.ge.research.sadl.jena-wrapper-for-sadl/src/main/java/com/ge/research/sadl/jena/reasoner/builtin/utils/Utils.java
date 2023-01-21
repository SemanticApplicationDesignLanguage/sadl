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

package com.ge.research.sadl.jena.reasoner.builtin.utils;

import java.io.InvalidObjectException;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Node_Literal;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.graph.Triple;
//import org.apache.jena.graph.impl.LiteralLabel;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.reasoner.rulesys.FBRuleInfGraph;
import org.apache.jena.reasoner.rulesys.Node_RuleVariable;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.processing.SadlConstants;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;
import com.naturalsemanticsllc.sadl.reasoner.TypedBuiltinFunctionException;

/**
 * This class provides useful utilities for building Jena builtin functions.
 * @author 200005201
 *
 */
public class Utils {
	private static final Logger _logger = LoggerFactory.getLogger(Utils.class);
	
	private static ITypedBuiltinFunctionHelper typedBuiltinFunctionHelper = null;
	
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
	 * Call this method to create a new blank node instance of an existing class within the context
	 * @param context -- the RuleContext from the calling built-in
	 * @param existingClass -- the URI of the existing class
	 * @return -- a new blank node which is an instance of the existing class
	 */
	public static synchronized Node createInstanceOfClass(RuleContext context, String existingClass) {
		Node newInst = NodeFactory.createBlankNode();
		Node_URI theClass = (Node_URI) NodeFactory.createURI(existingClass);
		Utils.addValue(context, newInst, RDF.Nodes.type, theClass);
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
     * Convert SADL list to a java list of Nodes
     * @param root the root node of the list
     * @param context the graph containing the list assertions
     */
    public static List<Node> convertList(Node root, RuleContext context, LinkedList<Node> soFar) {
       	Node_URI fprop = (Node_URI) NodeFactory.createURI(SadlConstants.SADL_LIST_MODEL_FIRST_URI);
       	Node_URI rprop = (Node_URI) NodeFactory.createURI(SadlConstants.SADL_LIST_MODEL_REST_URI);
       	if (soFar == null) {
       		soFar = new LinkedList<Node>()    ;   	
       	}
   	 	ClosableIterator<Triple> it = context.find(root, fprop, null);
        Node result = null;
        if (it.hasNext()) {
            result = it.next().getObject();
            soFar.add(result);
        }
        it.close();
        it = context.find(root, rprop, null);
        Node rest = null;
        if (it.hasNext()) {
            rest = it.next().getObject();
            it.close();
            return convertList(rest, context, soFar);
        }
        it.close();       
        return soFar;
    }


	/**
     * Construct a new float valued node
     */
    public static synchronized Node makeFloatNode(Float value) {
    	return NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(value));
    }
    
    public static synchronized Node makeBooleanNode(Boolean value) {
    	return NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(value));
    }
    
    public static synchronized Node makeXSDDateTimeNode(XSDDateTime value) {
    	return NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(value));
    }

    /*
     * UnittedQuantity-related utilties
     */
	public static boolean listContainsUnittedQuantity(List<Node> nodes, Object context) throws TypedBuiltinFunctionException {
		if (context instanceof RuleContext) {
			return listContainsUnittedQuantity(nodes, (RuleContext)context);
		}
		throw new TypedBuiltinFunctionException("Invalid RuleContext passed to listContainsUnittedQuantity");
	}

	private static boolean listContainsUnittedQuantity(List<Node> nodes, RuleContext context) {
		if (nodes != null) {
			for (Node node : nodes) {
				if (isUnittedQuantity(node, context)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Method to determine if a Node is an instance of the SadlImplicitModel UnittedQuantity class.
	 * Since this is during inference, we can assume that transient closure over class hierarchies
	 * has made every instance of a subclass of UnittedQuantity also an instance of UnittedQuantity.
	 * @param node
	 * @param context
	 * @return
	 */
	public static boolean isUnittedQuantity(Node node, RuleContext context) {
		if (node instanceof Node_Literal) {
			return false;
		}
		if (node instanceof Node_RuleVariable) {
			return false;
		}
		Node UQCls =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
		if (context.contains(node, RDF.type.asNode(), UQCls)) {
			return true;
		}
		if (node instanceof Node_URI) {
			if (context.contains(node, RDFS.subClassOf.asNode(), UQCls)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Method to obtain the value of a Node which is an instance of the SadlImplicitModel's
	 * UnittedQuantity class
	 * @param node
	 * @param context
	 * @return
	 */
	public static Node getUnittedQuantityValue(Node node, RuleContext context) {
		Node valuePred =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI);
		ClosableIterator<Triple> citr = context.find(node, valuePred, null);
		if (citr.hasNext()) {
			Node val = citr.next().getObject();
			citr.close();
			return val;
		}
		return null;
	}
	
	/**
	 * Method to obtain the unit of a Node which is an instance of the SadlImplicitModel's
	 * UnittedQuantity class
	 * @param node
	 * @param context
	 * @return
	 */
	public static Node getUnittedQuantityUnit(Node node,  RuleContext context) {
		Node unitPred =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI);
		ClosableIterator<Triple> citr = context.find(node, unitPred, null);
		if (citr.hasNext()) {
			Node unit =  citr.next().getObject();
			citr.close();
			return unit;
		}
		return null;
	}

	/**
	 * Method to check to make sure all UnittedQuantity elements in the list have a unit and they are all the same unit.
	 * @param context
	 * @param nodeLst
	 * @throws DatatypeFormatException
	 * @throws TypedBuiltinFunctionException
	 */
	public static void checkUnittedQuantityListSameUnits(RuleContext context, List<Node> nodeLst) throws DatatypeFormatException, TypedBuiltinFunctionException {
		Node previousUnit =  null;
		for (Node node : nodeLst) {
			Node unit = getUnittedQuantityUnit(node, context);
			Node value = getUnittedQuantityValue(node, context);
			if (unit == null) {
				throw new TypedBuiltinFunctionException("Encountered a value (" + 
						value.getLiteral().getValue().toString() + " without any unit; unitted quantities must have a unit.");
			}
			if (previousUnit != null) {
				if (!previousUnit.equals(unit)) {
					throw new TypedBuiltinFunctionException("Encountered different units (" + 
								previousUnit.getLiteral().getValue().toString() + ", " + 
								unit.getLiteral().getValue().toString() + ") but units must be the same.");
				}
			}
			previousUnit = unit;
		}
	}

	/**
	 * Method to convert a list of Jena Nodes which are UnittedQuanitity instances into a list of SADL Literals.
	 * @param context
	 * @param nodeLst
	 * @param unittedQuantityProcessingConstraint
	 * @return
	 */
	public static List<UnittedQuantity> getUnittedQuantityArgumentList(RuleContext context, List<Node> nodeLst,
			UnittedQuantityBuiltinHandlingType unittedQuantityProcessingConstraint) {
		List<UnittedQuantity> uqList = new ArrayList<UnittedQuantity>();
		for (Node node : nodeLst) {
			Node unit;
			Node value;
			if (node.isLiteral()) {
				value = node;
				unit = null;
			}
			else {
				unit = getUnittedQuantityUnit(node, context);
				value = getUnittedQuantityValue(node, context);
			}
			UnittedQuantity uq = new UnittedQuantity(value, unit);
			uqList.add(uq);
		}
		return uqList;
	}

	/**
	 * Method to create a new instance of a SadlImplicitModel UnittedQuantity in a Jena model.
	 * @param context
	 * @param valNode
	 * @param unitNode
	 * @return
	 */
	public static Node createUnittedQuantity(RuleContext context, Node valNode, Node unitNode) {
		Node uQinst = Utils.createInstanceOfClass(context, SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
		Node valPred = NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI);
		Utils.addValue(context, uQinst, valPred, valNode);
		Node unitPred = NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI);
		Utils.addValue(context, uQinst, unitPred, unitNode);
		return uQinst;

	}

	/**
	 * Method to determine the unit of a UnittedQuantity output of a given operator.
	 * 
	 * @param context -- the Rule context, providing the inferred model
	 * @param n1 -- the operator as a string
	 * @param n2 -- the 1st unit of a binary operation 
	 * @param n3 -- the 2nd unit of a binary operation
	 * @return -- the unit of the binary operation output
	 * @throws UnittedQuantityHandlerException
	 */
	public static String combineUnits(RuleContext context, Node n1, Node n2, Node n3) throws TypedBuiltinFunctionException {
		String op = n1.getLiteralValue().toString();
		String u1 = n2.getLiteralValue().toString();
		String u2 = n3.getLiteralValue().toString();
		
		ITypedBuiltinFunctionHelper helper = getTypedBuiltinFunctionHelper();
		Object result = helper.combineUnits(op, u1, u2);
		if (result instanceof Node_Literal) {
			return ((Node_Literal)result).getLiteralValue().toString();
		}
		return result.toString();
	}

	public static ITypedBuiltinFunctionHelper getTypedBuiltinFunctionHelper() {
		return typedBuiltinFunctionHelper;
	}

	public static void setTypedBuiltinFunctionHelper(ITypedBuiltinFunctionHelper typedBuiltinFunctionHelper) {
		Utils.typedBuiltinFunctionHelper = typedBuiltinFunctionHelper;
	}

	public static boolean isNumber(com.ge.research.sadl.model.gp.Node argType, OntModel context) {
		String uri = argType.getURI();
		//uri is exactly a numeric type
		if (uri.equals(XSD.decimal.getURI()) || uri.equals(XSD.integer.getURI()) || uri.equals(XSD.xdouble.getURI())
				|| uri.equals(XSD.xfloat.getURI()) || uri.equals(XSD.xint.getURI()) || uri.equals(XSD.xlong.getURI())
				|| uri.equals(XSD.negativeInteger.getURI()) || uri.equals(XSD.nonNegativeInteger.getURI())
				|| uri.equals(XSD.nonPositiveInteger.getURI()) || uri.equals(XSD.positiveInteger.getURI())
				|| uri.equals(XSD.unsignedInt.getURI()) || uri.equals(XSD.unsignedLong.getURI())
				|| uri.equals(XSD.unsignedShort.getURI()) || uri.equals(XSD.xshort.getURI())) {
			return true;
		}
		if (uri.equals(XSD.duration.getURI())) {
			// xsd:duration will be considered numeric for type checking.
			return true;
		}
		//If Unitted Quantities are ignored then they are also considered a numeric type
		if(uri.equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) ||
						isNamedNodeSubclassOfNamedNode(context, uri,	SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
			return true;
		}
		return false;
	}

	private static boolean isNamedNodeSubclassOfNamedNode(OntModel context, String uri1,
			String uri2) {
		OntClass cls1 = context.getOntClass(uri1);
		if (cls1 != null) {
			OntClass cls2 = context.getOntClass(uri2);
			if (cls2 != null) {
				if (context.contains(cls1, RDFS.subClassOf, cls2)) {
					return true;
				}
			}
		}
		return false;
	}

	public static com.ge.research.sadl.model.gp.Node moreGeneralNumericType(com.ge.research.sadl.model.gp.Node type1,
			com.ge.research.sadl.model.gp.Node type2, OntModel context) {
		String uri1 = type1.getURI();
		String uri2 = type2.getURI();
		if (uri1.equals(XSD.decimal.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.decimal.getURI())) {
			return type2;
		}
		else if (uri1.equals(XSD.xdouble.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.xdouble.getURI())) {
			return type2;
		}
		else if (uri1.equals(XSD.xfloat.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.xfloat.getURI())) {
			return type2;
		}
		else if (uri1.equals(XSD.xlong.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.xlong.getURI())) {
			return type2;
		}
		else if (uri1.equals(XSD.integer.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.integer.getURI())) {
			return type2;
		}
		else if (uri1.equals(XSD.xint.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.xint.getURI())) {
			return type2;
		}
		else if (uri1.equals(XSD.integer.getURI())) {
			return type1;
		}
		else if (uri2.equals(XSD.integer.getURI())) {
			return type2;
		}
		return new NamedNode(XSD.decimal.getURI(), NodeType.DataTypeNode);
	}

}
