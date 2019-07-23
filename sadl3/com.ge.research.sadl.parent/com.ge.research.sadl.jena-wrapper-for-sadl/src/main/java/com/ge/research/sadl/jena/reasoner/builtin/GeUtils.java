package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.Utils;
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.NodeFactory;
import com.hp.hpl.jena.graph.Node_URI;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.graph.impl.LiteralLabelFactory;
import com.hp.hpl.jena.reasoner.InfGraph;
import com.hp.hpl.jena.reasoner.rulesys.Builtin;
import com.hp.hpl.jena.reasoner.rulesys.BuiltinException;
import com.hp.hpl.jena.reasoner.rulesys.Node_RuleVariable;
import com.hp.hpl.jena.reasoner.rulesys.RuleContext;
import com.hp.hpl.jena.reasoner.rulesys.Util;
import com.hp.hpl.jena.util.iterator.ClosableIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDF;

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
            Node listNode = NodeFactory.createAnon();
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
        	Node listNode = NodeFactory.createAnon();
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
		Node[][] pattern = new Node[numTriples][3];
		Map<Node, Integer> variableCount = new HashMap<Node,Integer>();
		Node listVar = null;
		
		for (int tripleIdx = 0; tripleIdx < numTriples; tripleIdx++) {
			for (int inTripleIdx = 0; inTripleIdx < 3; inTripleIdx++) {
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
					variableCount.put(n, new Integer(cnt));
					listVar = n;
				}
				pattern[tripleIdx][inTripleIdx] = n;
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
		objects = createNodeArray(obj, varValues);
		
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
				    		else if (obj.isVariable() && objects == null) {
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
			if (obj.isVariable() && objects != null) {
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
				result[0] = NodeFactory.createLiteral(LiteralLabelFactory.create(new Boolean(true)));
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


}
