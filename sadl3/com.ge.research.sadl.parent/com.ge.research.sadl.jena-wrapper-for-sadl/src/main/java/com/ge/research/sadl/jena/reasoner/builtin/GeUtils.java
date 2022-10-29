/************************************************************************
 * Copyright © 2007-2022 - General Electric Company, All Rights Reserved
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.graph.Triple;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.reasoner.InfGraph;
import org.apache.jena.reasoner.rulesys.Builtin;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.Node_RuleVariable;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;

/**
 * This class provides useful utilities for building Jena builtin functions.
 * @author 200005201
 *
 */
public class GeUtils {
    protected static final Logger logger = LoggerFactory.getLogger(GeUtils.class);
	
	public static synchronized List<Node> getListItems(InfGraph grph, List<Node> list, Node n) {
    	ExtendedIterator<Triple> bigItr = grph.find(n, RDF.Nodes.first, null);
    	while (bigItr.hasNext()) {
    		Object t = bigItr.next();
    		Node o = ((Triple) t).getMatchObject();
    		if (list == null) {
    			list = new ArrayList<Node>();
    		}
			list.add(o);
    	}
    	bigItr = grph.find(n, RDF.Nodes.rest, null);
    	while (bigItr.hasNext()) {
    		Object t = bigItr.next();
    		Node o = ((Triple)t).getMatchObject();
    		list = getListItems(grph, list, o);
    	}
    	return list;
    }
	
    public static synchronized String listToString(java.util.List<Object> list) {
		StringBuffer sb = new StringBuffer();
		sb.append("[");
		for (int i = 0; i < list.size(); i++) {
			if (i > 0) {
				sb.append(", ");
			}
			sb.append(list.get(i));
		}
		sb.append("]");
		return sb.toString();
	}

    /**
     * Construct an RDF list from the given array of nodes and assert it
     * in the graph returning the head of the list.
     */
    public static synchronized Node makeList(Node[] nodes, RuleContext context) {
        return doMakeList(nodes, 0, context);
    }
    
    /**
     * Internals of makeList.
     */
    private static Node doMakeList(Node[] nodes, int next, RuleContext context) {
        if (next < nodes.length) {
            Node listNode = NodeFactory.createBlankNode();
            context.add(new Triple(listNode, RDF.Nodes.first, nodes[next]));
            context.add(new Triple(listNode, RDF.Nodes.rest, doMakeList(nodes, next+1, context)));
            return listNode;
        } else {
            return RDF.Nodes.nil;
        }
    }
    
    public static synchronized Node addToList(Node l, Node e, RuleContext context) {
    	if (e == null) {
    		return null;
    	}
    	if (l == null) {
        	Node listNode = NodeFactory.createBlankNode();
        	context.add(new Triple(listNode, RDF.Nodes.type, RDF.List.asNode()));
            context.add(new Triple(listNode, RDF.Nodes.first, e));
            context.add(new Triple(listNode, RDF.Nodes.rest, RDF.Nodes.nil));
            return listNode;
    	}
    	else {
    		ExtendedIterator<Triple> itr = context.getGraph().find(l, RDF.Nodes.first, null);
    		if (itr.hasNext()) {
    			Triple t = itr.next();
    			Node first = t.getMatchObject();
        		if (first.equals(RDF.nil.asNode())) {
        			Utils.deleteReplaceCreateValue(context, l, RDF.Nodes.first, e);
        		}
        		else {
            		Node rest = Util.getPropValue(l, RDF.Nodes.rest, context);
            		if (rest.equals(RDF.Nodes.nil)) {
            			Utils.deleteReplaceCreateValue(context, l, RDF.Nodes.rest, e);
            		}
            		else {
            			return addToList(rest, e, context);
            		}
        		}
    		}
    		else {
    			return null;
    		}
    		return l;
    	}
    }

    public static synchronized Node getArg(int n, Node[] args, RuleContext context) {
        return context.getEnv().getGroundVersion(args[n]);
    }
    
    public static synchronized boolean isGraphPatternInput(Builtin bi, Node[] args, int length, RuleContext context) {
    	for (Node n : args) {
    		if (!(n instanceof Node_URI) && !(n instanceof Node_RuleVariable)) {
    			return false;
    		}
    	}
    	// verify--the 2nd and then every 3rd arg is a property
    	for (int i = 1; i < args.length; i = i + 3) {
    		Node n = args[i];
    		ClosableIterator<Triple> itr = context.find(n, RDF.type.asNode(), null);
    		boolean isProp = false;
    		while (itr.hasNext()) {
    			Node on = itr.next().getObject();
    			if (!on.isURI()) {
    				return false;
    			}
    			if (on.equals(OWL.ObjectProperty.asNode()) ||
    					on.equals(OWL.DatatypeProperty.asNode()) ||
    					on.equals(OWL.AnnotationProperty.asNode()) ||
    					on.equals(RDF.Property.asNode())) {
    				itr.close();
    				isProp = true;
    				break;
    			}
    		}
    		if (!isProp) {
    			itr.close();
    			return false;
    		}
    	}
    	return true;
    }

    public static synchronized Node[] matchNonSparqlPattern(Builtin bi, Node[] args, int length, boolean retValIsVariable, RuleContext context) {
		int numTriples = length / 3;
    	int retvarcnt = length % 3;
    	// Here we are looking for evidence that the arguments are not correct. The correct arguments will be one of:
    	//	1. some multiple of three arguments (each set of 3 a triple pattern) followed by the return variable argument
    	//	2. as above except that the last triple pattern does not have an object as the list is those values (1 less arg than above)
    	// If it isn't one of these throw an exception so that the user is notified of the problem.
    	if ((retvarcnt == 0 && numTriples > 0 && args[length - 2].isVariable()) || (retValIsVariable ? retvarcnt > 1 : retvarcnt > 2)) {
    		// it's ok to have a missing object in the last triple pattern, in which case the last one in the pattern 
    		//   will not be a variable (Note: this is ignoring the actual last argument which is the return variable)
    		throw new BuiltinException(bi, context, "Invalid number of arguments; complete set of triple patterns required.");
    	}
    	if (numTriples == 0 && args.length == 2) {
    		// special case
    		numTriples = 1;
    	}
		Node[][] pattern = new Node[numTriples][3];
		Map<Node, Integer> variableCount = new HashMap<Node,Integer>();
		Node listVar = null;
		
		for (int tripleIdx = 0; tripleIdx < numTriples; tripleIdx++) {
			for (int inTripleIdx = 0; inTripleIdx < 3; inTripleIdx++) {
				if (tripleIdx * 3 + inTripleIdx < args.length) {
					Node n = getArg((tripleIdx * 3 + inTripleIdx), args, context);
					if (n.isVariable()) {
						int cnt = 1;
						if (variableCount.containsKey(n)) {
							Integer count = variableCount.get(n);
							cnt = count.intValue() + 1;
						}
						else {
			   				if (numTriples > 1 && tripleIdx == (numTriples - 1) && inTripleIdx < 2) {
			   					throw new BuiltinException(bi, context, "For complex patterns (more than one edge), the list variable must be in the object value position.");
			   				}
						}
						variableCount.put(n, Integer.valueOf(cnt));
						listVar = n;
					}
					pattern[tripleIdx][inTripleIdx] = n;
				}
			}
		}
		
		// if numTriples is > 1 then the lastVar count must be 1 and that will be the values in the list
		if (numTriples == 1) {
			// only one edge
			if (variableCount.size() > 1) {
				throw new BuiltinException(bi, context, "Simple patterns (only one edge) can only have a single variable.");
			}
		}
		
		Map<Node, Node[]> varValues = new HashMap<Node, Node[]>();
		
		for (int tripleIdx = 0; tripleIdx < numTriples; tripleIdx++) {
		    Node subj = pattern[tripleIdx][0];
		    Node pred = pattern[tripleIdx][1];
		    Node obj = pattern[tripleIdx][2];
		    
		    varValues = matchPattern(subj, pred, obj, varValues, context);
		    if (varValues == null) {
		    	break;
		    }
		}
		Node[] nodes = (varValues != null) ? varValues.get(listVar) : null;
		return nodes;
	}

	private static Map<Node, Node[]> matchPattern(Node subj, Node pred, Node obj, Map<Node, Node[]> varValues, RuleContext context) {
		Node[] subjects = null;
		Node[] predicates = null;
		Node[] objects = null;
		
		subjects = createNodeArray(subj, varValues);
		predicates = createNodeArray(pred, varValues);
		objects = obj != null ? createNodeArray(obj, varValues) : null;
		
		// if there are variables in the match pattern, one and only one of these should be null; if there are no variables none will be null
		int numSubjects = (subjects == null) ? 1 : subjects.length;
		int numPredicates = (predicates == null) ? 1 : predicates.length;
		int numObjects = (objects == null) ? 1 : objects.length;
		
		Node s;
		Node p;
		Node ov;
		
		boolean entirePatternExactMatch = false;
		
		List<Triple> successfulTriples = null;
		for (int is = 0; is < numSubjects; is++) {
			s = (subjects != null) ? subjects[is] : null;
			for (int ip = 0; ip < numPredicates; ip++) {
				p = (predicates != null) ? predicates[ip] : null;
				for (int io = 0; io < numObjects; io++) {
					ov = (objects != null) ? objects[io] : null;

			        // do query for single triple to match
					logger.debug("  matchPattern finding pattern (" + s + ", " + p + ", " + ov + ")");
			        ClosableIterator<Triple> citr = context.find(s, p, ov);
			        if (citr.hasNext()) {
			        	boolean isExactMatch = false;
			        	java.util.List<Node> listNodes = new ArrayList<Node>();
			            while (citr.hasNext()) {
			            	// For some reason, this iterator returns multiple identical triples. Arguably this should
			            	//	not be the case as a model cannot be thought to have more than one graph edge (identical
			            	//	subject, predicate, and object). Therefore we will test to make sure that it isn't a duplicate
			            	//	before adding to the list. AWC 1/23/2013 (This problem manifested itself when we stopped removing
			            	//	duplicates from the final return of the list built-in.)
			            	Triple o = citr.next();
			            	boolean isDuplicate = true;
			            	if (successfulTriples == null) {
			            		successfulTriples = new ArrayList<Triple>();
			            	}
			            	if (ov == null || obj.isVariable()) {		// 10/08/2015: changing from && to || for testing purposes--trying to understand Abha's different result on countMatches(s1,p,x1,s2,p,x1)
			            		Node n = o.getObject();
			            		if (!listNodes.contains(n)) {
					            	successfulTriples.add(o);
			            			listNodes.add(n);	
			            			isDuplicate = false;
			            		}
			            	}
			            	else if (p == null || pred.isVariable()) {
			            		Node n = o.getPredicate();
			            		if (!listNodes.contains(n)) {
					            	successfulTriples.add(o);
			            			listNodes.add(n);
			            			isDuplicate = false;
			            		}
			            	}
			            	else if (s == null || subj.isVariable()) {
			            		Node n = o.getSubject();
			            		if (!listNodes.contains(n)) {
					            	successfulTriples.add(o);
			            			listNodes.add(n);
			            			isDuplicate = false;
			            		}
			            	}
			            	else {
			            		// this is a complete pattern that we are trying to match
			            		successfulTriples.add(o);
			            		isExactMatch = true;
			            		entirePatternExactMatch = true;
			            		isDuplicate = false;
			            	}
			            	logger.debug("    matchPattern " + (isDuplicate ? "(duplicate) " : "") + "found triple: " + ((Triple)o).toString());
			            }  // end while
			            logger.debug("  matchPattern found " + listNodes.size() + " matching nodes");
			            
			            if (!isExactMatch) {
				            // add new variables to varValues
				    		Node[] nodes = new Node[listNodes.size()];
				    		for (int i = 0; i < listNodes.size(); i++) {
				    			nodes[i] = listNodes.get(i);
				    		}
				    		if (subj.isVariable() && subjects == null) {
				    			varValues = addToArray(varValues, subj, nodes);
				    		}
				    		else if (pred.isVariable() && predicates == null) {
				    			varValues = addToArray(varValues, pred, nodes);
				    		}
				    		else if (obj == null || (obj.isVariable() && objects == null)) {
				    			varValues = addToArray(varValues, obj, nodes);
				    		}
			            }
			        }
			        else if ((s != null && s.isVariable()) || 
			        		(p != null && p.isVariable()) || (ov != null && ov.isVariable())) { // ?? not sure about this...
			        	// there are no values for this triple pattern
			        	// cause for failure?
			        	citr.close();
				        	logger.debug("  matchPattern found nothing, returning null");
			        	return null;
		        	
			        }
			        citr.close();
				}
			}
		}
		
		if (successfulTriples == null) {
			return null;
		}
		else {
			// remove any unsuccessful values
			if (subj.isVariable() && subjects != null) {
				for (int i = subjects.length - 1; i >= 0 ; i--) {
					boolean matches = false;
					for (int j = 0; j < successfulTriples.size(); j++) {
						Triple t = successfulTriples.get(j);
						if (t.getSubject().equals(subjects[i])) {
							matches = true;
							entirePatternExactMatch = false;
							continue;
						}
					}
					if (!matches) {
						varValues = removeFromArray(varValues, subj, subjects[i]);
					}
				}
			}
			if (pred.isVariable() && predicates != null) {
				for (int i = predicates.length - 1; i >= 0 ; i--) {
					boolean matches = false;
					for (int j = 0; j < successfulTriples.size(); j++) {
						Triple t = successfulTriples.get(j);
						if (t.getPredicate().equals(predicates[i])) {
							matches = true;
							entirePatternExactMatch = false;
							continue;
						}
					}
					if (!matches) {
						varValues = removeFromArray(varValues, pred, predicates[i]);
					}
				}
			}
			if (obj != null && obj.isVariable() && objects != null) {
				for (int i = objects.length - 1; i >= 0; i--) {
					boolean matches = false;
					for (int j = 0; j < successfulTriples.size(); j++) {
						Triple t = successfulTriples.get(j);
						if (t.getObject().equals(objects[i])) {
							matches = true;
							entirePatternExactMatch = false;
							continue;
						}
					}
					if (!matches) {
						varValues = removeFromArray(varValues, obj, objects[i]);
					}
				}
			}
			if (entirePatternExactMatch && varValues.size() == 0) {
//				Node trueNode = Utils.makeBooleanNode(true);	// 10/20/2014, awc: don't know why this won't resolve....
				Node[] result = new Node[1];
				result[0] = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(Boolean.valueOf(true)));
				varValues.put(null, result);
			}
		}
        return varValues;
	}

	private static Map<Node, Node[]> removeFromArray(Map<Node, Node[]> varValues, Node n, Node node) {
		if (varValues.containsKey(n)) {
			Node[] curVals = varValues.get(n);
			int remove = -1;
			for (int i = 0; i < curVals.length; i++) {
				if (curVals[i].equals(node)) {
					remove = i;
					break;
				}
			}
			if (remove >= 0) {
				logger.debug("removeFromArray removing " + remove + "th value: " + curVals[remove]);
				Node[] newVals = new Node[curVals.length - 1];
				for (int i = 0; i < newVals.length; i++) {
					if (i < remove) {
						newVals[i] = curVals[i];
					}
					else if (i >= remove){
						newVals[i] = curVals[i + 1];
					}
				}
				varValues.put(n,  newVals);
			}
		}
		return varValues;
	}

	private static Map<Node, Node[]> addToArray(Map<Node, Node[]> varValues, Node n, Node[] nodes) {
		if (varValues.containsKey(n)) {
			Node[] curVals = varValues.get(n);
			int newSize = curVals.length + nodes.length;
			Node[] newVals = new Node[newSize];
			for (int i = 0; i < curVals.length; i++) {
				newVals[i] = curVals[i];
			}
			for (int j = 0; j < nodes.length; j++) {
				newVals[curVals.length + j] = nodes[j];
			}
			varValues.put(n, newVals);
		}
		else {
			varValues.put(n, nodes);
		}
		return varValues;
	}

	private static Node[] createNodeArray(Node node, Map<Node, Node[]> varValues) {
		Node[] nodes = null;
        if (node.isVariable()) {
        	if (varValues.containsKey(node)) {
        		nodes = varValues.get(node);
        	}
        	// else this is a variable without any values--leave subjects null
        }
        else {
        	nodes = new Node[1];
        	nodes[0] = node;
        }
        return nodes;
	}

	public static synchronized Node[] removeDuplicatesFromList(Node[] nodes) {
		List<Integer>dups = null; 
		for (int i = (nodes.length - 1); i >= 0; i--) {
			Node n = nodes[i];
			for (int j = 0; j < i; j++) {
				if (n.equals(nodes[j])) {
					if (dups == null) {
						dups = new ArrayList<Integer>();
					}
					if (!dups.contains(i)) {
						dups.add(i);
					}
				}
			}
		}
		if (dups != null) {
			Node[] newNodes = new Node[nodes.length - dups.size()];
			int newIdx = 0;
			for (int i = 0; i < nodes.length; i++) {
				if (!dups.contains(i)) {
					newNodes[newIdx++] = nodes[i];
				}
			}
			return newNodes;
		}
		return nodes;
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
		Node UQCls =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
		ClosableIterator<Triple> citr = context.find(node, RDF.type.asNode(), UQCls);
		if (citr.hasNext()) {
			citr.close();
			return true;
		}
		return false;
	}

	/**
	 * Method to determine if any of the elements of a list of Nodes is an instance of the SadlImplicitModel's
	 * UnittedQuantity class.
	 * @param nodes
	 * @param context
	 * @return
	 */
	public static boolean listContainsUnittedQuantity(List<Node> nodes, RuleContext context) {
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
	 * Method to get a list of Nodes which are the values of a list of instances of the SadlImplicitModel's
	 * UnittedQuantity class
	 * @param bi
	 * @param nodes
	 * @param context
	 * @return
	 */
	public static List<Node> getUnittedQuantityValues(Builtin bi, List<Node> nodes, BuiltinUnittedQuantityStatus builtinUqStatus, RuleContext context) {
		List<Node> values = new ArrayList<Node>();
		boolean firstNode = true;
		for (Node node : nodes) {
			Node value = getUnittedQuantityValue(node, context);
			if (!firstNode && value == null) {
				if (builtinUqStatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly) || 
						builtinUqStatus.equals(BuiltinUnittedQuantityStatus.LeftUnitsOnly)) {
					value = node;
				}
				else {
					throw new BuiltinException(bi, context, "Encountered a non-UnittedQuantity or invalid UnittedQuantity while processing UnittedQuantities");
				}
			}
			values.add(value);
			firstNode = false;
		}
		return values;
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
	 * Method to get a list of Nodes which are the units of a list of instances of the SadlImplicitModel's
	 * UnittedQuantity class
	 * @param bi
	 * @param nodes
	 * @param context
	 * @return
	 */
	public static List<Node> getUnittedQuantityUnits(Builtin bi, List<Node> nodes,  BuiltinUnittedQuantityStatus builtinUqStatus, RuleContext context) {
		List<Node> units = new ArrayList<Node>();
		boolean firstNode = true;
		for (Node node : nodes) {
			Node unit = getUnittedQuantityUnit(node, context);
			if (!firstNode && unit == null) {
				if (!builtinUqStatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly) && 
						!builtinUqStatus.equals(BuiltinUnittedQuantityStatus.LeftUnitsOnly)) {
					throw new BuiltinException(bi, context, "Encountered a non-UnittedQuantity or invalid UnittedQuantity while processing UnittedQuantities");
				}
			}
			units.add(unit);
			firstNode = false;
		}
		return units;
	}

}
