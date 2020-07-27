package com.ge.research.sadl.jena.missingpatterns;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.AllValuesFromRestriction;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Restriction;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDFS;

import com.ge.research.sadl.jena.IntermediateFormTranslator;
import com.ge.research.sadl.jena.missingpatterns.DirectedPath.DirectedPathSource;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;

/*
See Path Finding Methodology in SADL documentation.

A rule, query, or other construct using triple patterns can reference a number of concepts which are nodes in the domain model graph. 
It may also reference local variables which will have, at a minimum, a type which is a node in the domain model graph, and may have a definition 
of the variable expressed in terms of nodes and edges in the domain model graph. The purpose of path finding is to connect all of these concepts (nodes) 
together through directed edges (triples) consistent with the domain model graph. This is necessary in order to properly ground the construct. It is 
needed because a construct can leave out some or virtually all of the graph patterns that relate 
concepts to each other within the construct. These missing graph patterns are of two types: replacement triples for a property node not in a property 
chain, and missing triples for incomplete property chains. These missing triples make explicit the construct's unambiguous meaning. 
See also Incomplete Property Chains in the documentation.

Sometimes instances are referenced in a construct. An instance is an anchor for the graph patterns of a construct; it is itself a grounding. 
Other concepts in the construct can be related to the instance. This can happen explicitly in the construct, or because a class 
to which  the instance belongs is in the domain of a property, is in the range of a property, or is the restriction class of an all-values-from property 
restriction.  (The role of instances needs to be verified.) However, unless explicitly expressed in the construct or unless there is a 
has-values property restriction in the domain ontology model, one can never know that a particular instance is the subject or object of a missing triple.

The information necessary to accomplish path finding comes from the following sources.
	1) The construct itself which contains the following information:
		  a) the triples and path-defining built-ins in the construct's statements (see footnote 1)
		  b) any construct-specific variable names and their definitions
	2) Construct variables names and their definitions
	3) Ontology restrictions in the domain model ontology
	4) The class hierarchy in the domain model ontology
	5) The specified domain and range of a given property in the domain model ontology.
	
These sources have a precedence. The construct (1) has the highest precedence, followed by construct 
variables (2). These are followed by ontology restrictions (3). Class hierarchy (4) and 
property domain and range (5) are of lowest precedence and are not in contention so no order between them is necessary.

The pre-processed inputs for path finding are the following.
	1) A list of ontology classes and individuals that are referenced in the construct, either directly or via definitions of variables referenced in the construct
	2) The triples and built-in relationships that are explicit in the construct, including those that are nested
	3) The definition of  variables referenced in the construct, including nested graph patterns
	4) A list of triples that are provided as local restrictions, AKA guidance, specified in the construct
	5) The domain model ontology, which will contain class hierarchies, ontology restrictions, and property domain and range information.
	
The first and second of these inputs allows the identification of a set of root nodes. This is the starting point of the path finding process. The process is 
carried out by walking "up" the graph, meaning from triple object to triple subject, or from a built-in function's output to input, from each root node. 
If this walk up the graph from a root node reaches another root node, the set of triples leading from the first root node to the second root node is a direct 
path partial solution. It is also possible that there is no direct path partial solution linking two root nodes but there is a node at which the upward paths 
from the two root nodes intersect. This is an intersection solution and is characterized by the two paths leading up to the intersection. 

The solution is complete when a path has been identified from every root node to every other root node. Note that it is possible that every root node has a 
path to at least one other root node without a solution having been reached because the paths found may not connect one group of root nodes with another. 
If an agent is specified, the solution can be assumed to be complete when every root node has an identified path to the agent node.

If there is more than one possible path between two root nodes, then the specification is ambiguous and an error is generated and the modeler should be directed 
to disambiguate the multiple paths by local restriction in the specification or in the context.

Path finding proceeds as follows. Each time a potential directed path segment is identified it is indexed by subject and by object, making it easy to find 
potential path segments in either direction. Also, as paths up are found their subjects (upper ends) are added to two maps.
	1) A map of nodes reached and the path(s) leading to them from the root node(s). More specifically, this map is keyed by root node and the value associated 
	   with each root node key is itself a secondary map whose key (secondary key) is the node reached and whose value is a list of directed paths that lead from 
	   the primary key root node to the secondary key. This map provides a means of easily identifying a newly discovered intersection as it allows the path 
	   from any root node to any other node reached to be easily identified.
	2) A list of nodes reached that have not been further explored, called effective roots. These serve as a launch point for further upward search in the next 
	   iteration (see below). For the first iteration, the effective roots list is set to the root nodes.
	   
A list is also kept of ceiling nodes, nodes from which no further upward paths can be found. This eliminates redundant searches for upward paths from these nodes.

The path-finding process proceeds in this manner.
	1) For each node in the list of effective roots, find a set of paths leading upward by doing the following. Note that for a given lower node and edge 
	   (property or built-in),  the precedence identified above must be respected.
		a) Check paths identified in the specification, in variables definitions for variables actually used in the specification, or in local restrictions (guidance), 
		   in either the Context or the specification, for a path up from the Resource
		b) Check ontology restrictions for a path up from the Resource
		c) Check class hierarchy for a path up from the Resource (Note that when two members of a class hierarchy are both roots, references to the more general in 
		   replacement triples are replaced with references to the more specific.)
		d) Check property ranges for a path up from the Resource. 
	2) Each time an upward path segment is found its subject is added to the list of effective roots for the next iteration and a check is made to see if the subject of the new path is 
		a) a root, in which case this establishes a direct path partial solution, or 
		b) in the list of nodes reached from a different root, in which case this identifies an intersection with another path upward.
	3) When steps 1 and 2 have been completed for the current list of effective roots, the process is repeated for the new effective roots identified during this iteration.
	4) Iteration continues until all roots are connected or until no more paths up can be found. When a path up from an effective root is not found, the node is added to the 
	   list of ceiling nodes and will not be used as an effective root again.

Checking that a solution is complete is relatively easy in the presence of an agent node. The solution is complete when a path has been established between the 
agent node and every root node. Otherwise a test is made to see if a random root node is connected to every other root node. If it is then there can be but one 
group and the solution is complete.

Once the solution has been generated as described above the intermediate form of the specification is processed to add missing triples. Each root node in the 
specification and variable definitions that has an upward path to another root node has the triples in this upward path added to the node as missing triples. 
Note that when property nodes that are not part of a property chain are encountered in the pre-processing for path finding, a replacement triple is identified 
and associated with the property node. The subject of this replacement triple is used as a member of the list of root nodes. Note also that only triples can be 
missing. Any built-in will be explicit in the specification or variable definition and so cannot be missing (based on current functionality--it would be possible 
in the future to allow implied functions where as now only implied property chains are supported).

Footnotes
	(1) Only certain built-in functions provide valuable path-specifying information. Among these are the list functions that take a typed list as input and return an element of the list. Therefore they provide a means of getting from the List class (upper node) to the list element (lower node).

 */
/**
 * Basic approach to missing direct paths borrowed from one of the answers to this stackoverflow question:
 * 		https://stackoverflow.com/questions/37967465/query-to-find-all-the-path-between-two-nodes-in-a-owl-ontology
 * In addition, this class looks for common ancestors to find missing paths that are not direct (where direct means
 * in the direction of the directed paths only).
 * 
 * The implementation does the following:
 *  1) Record the OntModel Resources between which a path is to be found in rsrcRoots.
 *  2) Take the connections in the Context and in the Requirement as a source of potential connections in path finding;
 *     place them in the field existingDirectedPaths
 *  3) Iterating over rsrcRoots, explore paths down from a root, going one step deeper on each iteration.
 *     a) If the newly added DirectedPath ends with a Resource also in rsrcRoots, this is a solution. Record it.
 *     b) If there is already a solution path from one root to another root, and it isn't equivalent, throw a
 *        MultiplePathsFoundException.
 *     
 * (both triples and some builtins, e.g., element in list operations that connect a class to a list class.
 * The concepts in the upper node of requirement GraphPatternElements, with bare properties providing their domain, 
 * become the roots passed to this algorithm. When a search down from any root matches the upper node of any requirement 
 * connection, the path up from that upper node becomes a list of missing patterns for that upper node.
 * 
 * @author 200005201
 *
 */
public class PathFinder {
	protected static final org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger(PathFinder.class);
	public enum PatternType {Exploratory, Missing, Replacement}
	public enum PartOfSpeech {Subject, Predicate, Object, Argument}
	
	/* A DirectedPath represents metadata from an ontology. The subject will be a Resource, the property will be a Property which can have an instance
	 * of the subject Resource as its subject (the subject Resource is in the domain of the property), and the object Resource is the effective
	 * range of the property, which may be narrower than the range of the property.
	 */
	protected OntModel theJenaModel;
//	private List<Resource> rsrcRoots;
	
	protected Map<NamedNode, List<GraphPatternElement>> nodesToInvestigateWithContext = null;

	/* Solutions are a set of paths recorded in linked lists of Triples.
	 * The paths are placed in a map keyed by the starting Resource, and are themselves in a map
	 * keyed by ending Resource. This allows a solution to be found by its starting node and ending node.
	 */
	protected Map<Resource, Map<Resource, DirectedPath>> solutions = null;	// the outer map is keyed by path start Resource, the inner map by path end Resource
	protected Map<Resource, List<DirectedPath>> existingDirectedPathsBySubject;	// paths specified by guidance mapped by subject
	protected Map<Resource, List<DirectedPath>> existingDirectedPathsByObject;	// paths specified by guidance mapped by object
	protected IntermediateFormTranslator iftranslator;
	protected Map<Node, List<Node>> replacements;	// a map of replacement Nodes and the Nodes that each replaces
	protected Map<NamedNode, DirectedPath> missingTripleReplacements = null;	// a map of NamedNodes with a replacement TripleElement (as a DirectedPath)
	protected NamedNode anchoringNode = null;	// the anchoring node, if any, specified in the semantic construct
	
	protected boolean nodeAddedToNodesReached = true;	// flag to allow failure if not nodes are being added over entire iteration

	/**
	 * Constructor for PathFinder class
	 * @param iftranslator
	 * @param vSet
	 * @param m
	 */
	public PathFinder(IntermediateFormTranslator iftranslator, OntModel m) {
		setIftranslator(iftranslator);
		setTheJenaModel(m);
	}
	
	/**
	 * Method to find the missing graph patterns in a CtxRequirement
	 * @param rule
	 * @return
	 * @throws PathFindingException
	 * @throws TranslationException
	 * @throws MultiplePathsFoundException
	 * @throws InvalidTypeException
	 * @throws InvalidNameException
	 */
	public Map<Resource, Map<Resource, DirectedPath>> findMissingGraphPatterns(Rule rule) throws PathFindingException, TranslationException, MultiplePathsFoundException, InvalidTypeException, InvalidNameException {
		List<VariableNode> variablesUsed = new ArrayList<VariableNode>();
		List<DirectedPath> knownPaths = new ArrayList<DirectedPath>();
		List<Resource> roots = new ArrayList<Resource>();
		
		List<GraphPatternElement> ctxGpes = rule.getGivens();
		if (ctxGpes != null) {
			for (GraphPatternElement gpe : ctxGpes) {
				preProcess(variablesUsed, knownPaths, roots, gpe, true);
			}
		}
		
		List<GraphPatternElement> whns = rule.getIfs();
		if (whns != null) {
			for (GraphPatternElement gpe : whns) {
				preProcess(variablesUsed, knownPaths, roots, gpe, false);
			}
		}
		List<GraphPatternElement> thns = rule.getThens();
		if (thns != null) {
			for (GraphPatternElement gpe : thns) {
				preProcess(variablesUsed, knownPaths, roots, gpe, false);
			}
		}
		
		return findMissingGraphPatterns(getIftranslator().getAnchoringNode(rule), knownPaths, roots);
	}

	/**
	 * Method to find the missing graph patterns in a CtxRequirement after extracting agent, variables, known paths, and roots
	 * @param anchoringNode
	 * @param rule
	 * @param variablesUsed
	 * @param knownPaths
	 * @param roots
	 * @return
	 * @throws TranslationException
	 * @throws MultiplePathsFoundException
	 * @throws PathFindingException
	 */
	protected Map<Resource, Map<Resource, DirectedPath>> findMissingGraphPatterns(NamedNode anchoringNode, List<DirectedPath> knownPaths, List<Resource> roots) throws TranslationException, MultiplePathsFoundException, PathFindingException {
		setAnchoringNode(anchoringNode);
		if (anchoringNode != null && roots != null) {
			Resource agentRsrc = getIftranslator().getResourceFromNamedNode(anchoringNode);
			if (!roots.contains(agentRsrc)) {
				roots.add(agentRsrc);
			}
		}
		if (knownPaths != null) {
			for (DirectedPath dp : knownPaths) {
				addExistingDirectedPath(dp);
			}
		}
		return getPaths(roots);
	}

	/**
	 * Method to get paths given a set of roots. Additional information from a specification 
	 * may or may not be present (can be called directly or thru findMissingGraphPatterns)
	 * @param roots
	 * @throws MultiplePathsFoundException 
	 * @throws TranslationException 
	 * @throws PathFindingException 
	 */
	public Map<Resource, Map<Resource, DirectedPath>>  getPaths(List<Resource> roots) throws TranslationException, MultiplePathsFoundException, PathFindingException {
		if (roots != null && roots.size() > 0) {
			List<Resource> solved = new ArrayList<Resource>();
			List<Resource> effectiveRoots = new ArrayList<Resource>();
			List<Resource> ceilingNodes = new ArrayList<Resource>();
			// nodesReached is a map of maps. The key to the upper map is root at which the search began. The key to the lower (inner) map is the highest node, the "node reached".
			Map<Resource, Map<Resource, List<DirectedPath>>> nodesReached = new HashMap<Resource, Map<Resource, List<DirectedPath>>>();
			Map<Resource, List<DirectedPath>> indexedBySubject = new HashMap<Resource, List<DirectedPath>>();
			Map<Resource, List<DirectedPath>> indexedByObject = new HashMap<Resource, List<DirectedPath>>();
			addAllUnique(effectiveRoots, roots);
			logger.debug("Entering getPaths:");
			logger.debug("Guidance (existing paths):\n{}", existingDirectedPathsAsString());
			int itercntr = 0;
			while (effectiveRoots != null && effectiveRoots.size() > 0 && isNodeAddedToNodesReached()) {
				logger.debug("Beginning while loop, iteration {}, effective roots:\n{}", itercntr++, resourceListAsString(effectiveRoots));
				setNodeAddedToNodesReached(false);
				boolean thisRootSolutionComplete = false;
				List<Resource> newEffectiveRoots = new ArrayList<Resource>();
				List<Resource> rootsRetired = new ArrayList<Resource>();
			
				if(!thisRootSolutionComplete) {
					for (Resource root : effectiveRoots) {
						List<DirectedPath> gFound = findPathsUpFromGuidance(root);
						logger.debug("  Paths found from guidance for root '{}':\n{}", root, pathListAsString(gFound));
						List<Resource> newHeights = indexAndFindNewEffectiveRoots(roots, indexedBySubject, indexedByObject, gFound, nodesReached);
						if (newHeights == null) {
							if(!ceilingNodes.contains(root)) {
								ceilingNodes.add(root);
							}
						}
						else {
							rootsRetired.add(root);
							ceilingNodes.remove(root);
							List<Resource> notSolved = checkForSolutions(root, roots, newHeights, indexedBySubject, indexedByObject, ceilingNodes, solved);
							if (notSolved.size() > 0) {
								addAllUnique(newEffectiveRoots, notSolved);
							}
						}
					}
				}
				
				if (!thisRootSolutionComplete) {
					effectiveRoots.removeAll(rootsRetired);
					for (Resource root : effectiveRoots) {
						List<DirectedPath> orFound = findPathsUpFromOntologyRestrictions(root);
						logger.debug("  Paths found from ontology restrictions for root '{}':\n{}", root, pathListAsString(orFound));
						List<Resource> newHeights = indexAndFindNewEffectiveRoots(roots, indexedBySubject, indexedByObject, orFound, nodesReached);
						if (newHeights == null) {
							if(!ceilingNodes.contains(root)) {
								ceilingNodes.add(root);
							}
						}
						else {
							rootsRetired.add(root);
							ceilingNodes.remove(root);
							List<Resource> notSolved = checkForSolutions(root, roots, newHeights, indexedBySubject, indexedByObject, ceilingNodes, solved);
							if (notSolved.size() > 0) {
								addAllUnique(newEffectiveRoots, notSolved);
							}
						}
					}
				}
	
				if (!thisRootSolutionComplete) {
					effectiveRoots.removeAll(rootsRetired);
					for (Resource root : effectiveRoots) {
						List<DirectedPath> chFound = findPathsUpFromClassHierarchy(roots, root);
						logger.debug("  Paths found from class hierarchy for root '{}':\n{}", root, pathListAsString(chFound));
						List<Resource> newHeights = indexAndFindNewEffectiveRoots(roots, indexedBySubject, indexedByObject, chFound, nodesReached);
						if (newHeights == null) {
							if(!ceilingNodes.contains(root)) {
								ceilingNodes.add(root);
							}
						}
						else {
							rootsRetired.add(root);
							ceilingNodes.remove(root);
							List<Resource> notSolved = checkForSolutions(root, roots, newHeights, indexedBySubject, indexedByObject, ceilingNodes, solved);
							if (notSolved.size() > 0) {
								addAllUnique(newEffectiveRoots, notSolved);
							}
						}
					}
				}
	
				if (!thisRootSolutionComplete) {
//					effectiveRoots.removeAll(rootsRetired);		// don't remove--class hierarchy and ranges should both be checked for each root
					for (Resource root : effectiveRoots) {
						List<DirectedPath> rngFound = findPathsUpFromOntologyRanges(root);
						logger.debug("  Paths found from ontology ranges for root '{}':\n{}", root, pathListAsString(rngFound));
						List<Resource> newHeights = indexAndFindNewEffectiveRoots(roots, indexedBySubject, indexedByObject, rngFound, nodesReached);
						if (newHeights == null) {
							if(!ceilingNodes.contains(root)) {
								ceilingNodes.add(root);
							}
						}
						else {
							rootsRetired.add(root);
							ceilingNodes.remove(root);
							List<Resource> notSolved = checkForSolutions(root, roots, newHeights, indexedBySubject, indexedByObject, ceilingNodes, solved);
							if (notSolved.size() > 0) {
								addAllUnique(newEffectiveRoots, notSolved);
							}
						}
					}
				}
				newEffectiveRoots = getNewEffectiveRoots(roots, newEffectiveRoots, ceilingNodes, solved, nodesReached);
				if (newEffectiveRoots != null && newEffectiveRoots.size() > 0 && newEffectiveRoots.equals(effectiveRoots)) {
					throw new TranslationException("Loop detected, unable to find paths.");
				}
				effectiveRoots = newEffectiveRoots;
			}
			generateSolutionsFromNodesReached(roots, nodesReached);
			logger.debug("Leaving getPaths, solution is:\n{}", getSolutionsAsString());
		}
		return getSolutions();
	}

	/**
	 * Method to add a known DirectedPath to the class fields for tracking existing paths by subject and object
	 * @param dp
	 */
	private void addExistingDirectedPath(DirectedPath dp) {
		if (existingDirectedPathsByObject == null) {
			existingDirectedPathsByObject = new HashMap<Resource, List<DirectedPath>>();
			List<DirectedPath> paths = new ArrayList<DirectedPath>();
			paths.add(dp);
			existingDirectedPathsByObject.put(dp.getObject(), paths);
		}
		else {
			if (existingDirectedPathsByObject.containsKey(dp.getObject())) {
				List<DirectedPath> paths = existingDirectedPathsByObject.get(dp.getObject());
				if (!paths.contains(dp)) {
					existingDirectedPathsByObject.get(dp.getObject()).add(dp);
				}
			}
			else {
				List<DirectedPath> paths = new ArrayList<DirectedPath>();
				paths.add(dp);
				existingDirectedPathsByObject.put(dp.getObject(), paths);
			}
		}
	}

	/**
	 * Method to pre-process a GraphPatternElement
	 * @param variablesUsed
	 * @param knownPaths
	 * @param roots
	 * @param gpe
	 * @param inContext
	 * @return
	 * @throws TranslationException
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws PathFindingException
	 */
	protected DirectedPath preProcess(List<VariableNode> variablesUsed, List<DirectedPath> knownPaths, List<Resource> roots, GraphPatternElement gpe, boolean inContext) throws TranslationException, InvalidNameException, InvalidTypeException, PathFindingException {
		DirectedPath result = null;
		if (gpe instanceof TripleElement) {
			if (!(((TripleElement)gpe).getPredicate() instanceof NamedNode)) {
				Resource subj = preProcess(variablesUsed, knownPaths, roots, ((TripleElement)gpe).getSubject(), inContext, PartOfSpeech.Subject);
				Property prop;
				if (((TripleElement)gpe).getPredicate() instanceof NamedNode) {
					prop = getIftranslator().getPropertyFromNamedNode((NamedNode) ((TripleElement)gpe).getPredicate());
				}
				else if (((TripleElement)gpe).getPredicate() instanceof ProxyNode && 
						((ProxyNode)((TripleElement)gpe).getPredicate()).getProxyFor() instanceof BuiltinElement &&
						((BuiltinElement)((ProxyNode)((TripleElement)gpe).getPredicate()).getProxyFor()).getFuncName().equals("previous") &&
						((BuiltinElement)((ProxyNode)((TripleElement)gpe).getPredicate()).getProxyFor()).getArguments().get(0) instanceof NamedNode) {
					prop = getIftranslator().getPropertyFromNamedNode((NamedNode) ((BuiltinElement)((ProxyNode)((TripleElement)gpe).getPredicate()).getProxyFor()).getArguments().get(0));	
				}
				else {
					throw new PathFindingException("Triple predicate is not a NamedNode or previous of a NamedNode; condition not handled.");
				}
				Resource obj = preProcess(variablesUsed, knownPaths, roots, ((TripleElement)gpe).getObject(), inContext, PartOfSpeech.Object);
				
				//Subject is property
				if (((TripleElement)gpe).getSubject() instanceof NamedNode && isProperty(((TripleElement)gpe).getSubject())) {
					DirectedPath replacement = addNodeAndContainerToBeInvestigated(subj, gpe, PatternType.Replacement);
					if (replacement != null) {
						fillInUpper(replacement);
						if (!inContext && replacement.getSubject() != null && !roots.contains(replacement.getSubject())) {
							roots.add(replacement.getSubject());
						}
						if (!knownPaths.contains(replacement)) {
							knownPaths.add(replacement);
						}
						((NamedNode) ((TripleElement)gpe).getSubject()).setMissingTripleReplacement(createReplacementTriple(replacement));
						addMissingTripleReplacement((NamedNode) ((TripleElement)gpe).getSubject(), replacement);
					}
				}else {
					DirectedPath dp = new DirectedPath(subj, prop, obj);
					dp.setPathSource(DirectedPathSource.LocalRestriction);
					validateDirectedPath(dp);
					if (!knownPaths.contains(dp)) {
						knownPaths.add(dp);
					}
				
					if (!inContext && ((TripleElement)gpe).getSubject() instanceof NamedNode && !roots.contains(dp.getSubject())) {
						// only add to roots if this isn't a nested triple (subject is a NamedNode), isn't in the Context, isn't a decomposition, and hasn't already been added.
						roots.add(dp.getSubject());
					}
					
					result = dp;
				}
			}
		}
		else if (gpe instanceof BuiltinElement) {
			boolean isPathGenerating = getIftranslator().isPathGeneratingBuiltinElement((BuiltinElement)gpe);
			if (isPathGenerating) {
				List<DirectedPath> dps = getDirectedPathFromGPE(gpe);
				for (DirectedPath dp : dps) {
					dp.setPathSource(DirectedPathSource.LocalRestriction);
					if (!knownPaths.contains(dp)) {
						knownPaths.add(dp);
					}
				}
			}
			
			List<Node> args = ((BuiltinElement)gpe).getArguments();
			if (args != null) {
				// check to see if this built-in contains a set of graph patterns; if so do not process its arguments.
				if (!isGraphPatternArguments(args)) {			
					for (int i = 0; i < args.size(); i++) {
						Node arg = args.get(i);
						Resource rsrc = preProcess(variablesUsed, knownPaths, roots, arg, inContext, PartOfSpeech.Argument);
						if (arg instanceof NamedNode && isProperty(arg)) {
							DirectedPath replacement = addNodeAndContainerToBeInvestigated(rsrc, gpe, PatternType.Replacement);
							if (replacement != null) {
								fillInUpper(replacement);
								if (!inContext && replacement.getSubject() != null && !roots.contains(replacement.getSubject())) {
									roots.add(replacement.getSubject());
								}
								if (!knownPaths.contains(replacement)) {
									knownPaths.add(replacement);
								}
								((NamedNode) arg).setMissingTripleReplacement(createReplacementTriple(replacement));
								addMissingTripleReplacement((NamedNode) arg, replacement);
							}
						}
					}
				}
			}
		}
		else if (gpe instanceof Junction) {
			preProcess(variablesUsed, knownPaths, roots, (Node)((Junction)gpe).getLhs(), inContext, PartOfSpeech.Argument);
			preProcess(variablesUsed, knownPaths, roots, (Node)((Junction)gpe).getRhs(), inContext, PartOfSpeech.Argument);
		}
		return result;
	}
	
	/**
	 * Method to examine the arguments of a built-in and determine if they match a graph pattern type built-in
	 * @param args
	 * @return
	 */
    public boolean isGraphPatternArguments(List<Node> args) {
    	for (Node n : args) {
    		// See GeUtils.isGraphPatternInput for how this kind of built-in is tested during Jena Rule Engine processing.
    		// In that method this test is for an argument that is neither a URI node nor a rule variable node
    		if (!(n instanceof NamedNode) && !(n instanceof VariableNode)) {
    			return false;
    		}
    	}
    	// verify--the 2nd and then every 3rd arg is a property
    	for (int i = 1; i < args.size(); i = i + 3) {
    		Node n = args.get(i);
    		boolean isProp = (n instanceof NamedNode) && isProperty(((NamedNode)n).getNodeType());
    		if (!isProp) {
     			return false;
    		}
    	}
    	return true;
    }


	protected static boolean isProperty(NodeType oct) {
		if (oct.equals(NodeType.ObjectProperty) || oct.equals(NodeType.DataTypeProperty)
				|| oct.equals(NodeType.PropertyNode)) {
			return true;
		}
		return false;
	}

	/**
	 * Method to validate a DirectedPath by trying to fill in blank subject and/or object
	 * @param dp
	 * @throws TranslationException
	 * @throws PathFindingException
	 */
	protected void validateDirectedPath(DirectedPath dp) throws TranslationException, PathFindingException {
		if (dp.getSubject() == null) {
			fillInUpper(dp);
		}
		if (dp.getObject() == null) {
			fillInLower(dp);
		}
	}

	/**
	 * Method to pre-process a Node
	 * @param variablesUsed
	 * @param knownPaths
	 * @param roots
	 * @param node
	 * @param inContext
	 * @param pos
	 * @return
	 * @throws TranslationException
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws PathFindingException
	 */
	protected Resource preProcess(List<VariableNode> variablesUsed, List<DirectedPath> knownPaths, List<Resource> roots, Node node, boolean inContext, PartOfSpeech pos) throws TranslationException, InvalidNameException, InvalidTypeException, PathFindingException {
		Resource result = null;
		if (node instanceof VariableNode) {
			if (!variablesUsed.contains(node)) {
				variablesUsed.add((VariableNode) node);
				if (((VariableNode)node).getType() != null && ((VariableNode)node).getType() instanceof NamedNode) {
					result = getIftranslator().getResourceFromNamedNode((NamedNode) ((VariableNode)node).getType());
				}
				if (!inContext &&((VariableNode)node).getDefinitions() != null) {
					// don't look at variable definitions unless they are being used in the specification itself, not in the Context
					for (Node defn : ((VariableNode)node).getDefinitions()) {
						if (defn instanceof ProxyNode) {
							DirectedPath vddp = preProcess(variablesUsed, knownPaths, roots, ((ProxyNode)defn).getProxyFor(), inContext);
							if (vddp != null) {
								if (pos.equals(PartOfSpeech.Subject)) {
									result = vddp.getObject();
								}
								else if (pos.equals(PartOfSpeech.Object)) {
//									result = vddp.getSubject();
									result = vddp.getObject();	// I think we still want to return the object, even if the embedding is an object. awc 6/19/2018
								}
							}
						}
					}
				}
			}
		}
		else if (node instanceof NamedNode) {
			result = getIftranslator().getResourceFromNamedNode((NamedNode) node);
		}
		else if (node instanceof ProxyNode) {
			DirectedPath pndp = preProcess(variablesUsed, knownPaths, roots, ((ProxyNode)node).getProxyFor(), inContext);
			if (pndp != null) {
				if (pos.equals(PartOfSpeech.Subject)) {
					result = pndp.getObject();
				}
				else if (pos.equals(PartOfSpeech.Object)) {
					result = pndp.getSubject();
				}
			}
		}
		return result;
	}
	
	/**
	 * Method to add members to an existing list only those members of a second list that are not in the first list
	 * @param existingList
	 * @param toBeAdded
	 */
	protected void addAllUnique(List<Resource> existingList, List<Resource> toBeAdded) {
		for (Resource rsrc : toBeAdded) {
			if (!existingList.contains(rsrc)) {
				existingList.add(rsrc);
			}
		}
	}

	/**
	 * Method to serialize existing directed paths as a String
	 * @return
	 */
	protected String existingDirectedPathsAsString() {
		List<DirectedPath> dps = new ArrayList<DirectedPath>();
		if (existingDirectedPathsBySubject != null) {
			Iterator<Resource> edpsitr = existingDirectedPathsBySubject.keySet().iterator();
			while (edpsitr.hasNext()) {
				List<DirectedPath> edps = existingDirectedPathsBySubject.get(edpsitr.next());
				for (DirectedPath dp : edps) {
					if (!dps.contains(dp)) {
						dps.add(dp);
					}
				}
			}
		}
		if (existingDirectedPathsByObject != null) {
			Iterator<Resource> edpoitr = existingDirectedPathsByObject.keySet().iterator();
			while (edpoitr.hasNext()) {
				List<DirectedPath> edpo = existingDirectedPathsByObject.get(edpoitr.next());
				for (DirectedPath dp : edpo) {
					if (!dps.contains(dp)) {
						dps.add(dp);
					}
				}
			}
		}
		StringBuffer sb = new StringBuffer();
		if (dps.size() > 0) {
			for (DirectedPath dp : dps) {
				sb.append("   ");
				sb.append(dp.toString());
				sb.append("\n");
			}
		}
		return sb.toString();
	}

	protected Object pathListAsString(List<DirectedPath> paths) {
		StringBuilder sb = new StringBuilder("Path list:\n");
		if (paths != null) {
			for (DirectedPath path : paths) {
				sb.append("  ");
				sb.append(path.toString());
				sb.append("\n");
			}
		}
		return sb.toString();
	}

	/**
	 * Method to provide serialization of roots
	 * @param roots
	 * @return
	 */
	public String resourceListAsString(List<Resource> roots) {
		StringBuilder sb = new StringBuilder("Roots:\n");
		if (roots != null) {
			for (Resource root : roots) {
				sb.append("  ");
				sb.append(root.toString());
				sb.append("\n");
			}
		}
		return sb.toString();
	}

	/**
	 * Method to generate a solution map from the nodesReached map
	 * @param roots
	 * @param nodesReached
	 * @return
	 */
	private Map<Resource, Map<Resource, DirectedPath>> generateSolutionsFromNodesReached(List<Resource> roots,
			Map<Resource, Map<Resource, List<DirectedPath>>> nodesReached) {
		if (nodesReached != null) {
			if (solutions == null) {
				solutions = new HashMap<Resource, Map<Resource, DirectedPath>>();
			}
			else {
				solutions.clear();
			}
			for (Resource root : roots) {
				Map<Resource, List<DirectedPath>> innerMap = nodesReached.get(root);
				if (innerMap != null) {
					for (Resource key : roots) {
						if (!key.equals(root)) {
							if (innerMap.containsKey(key)) {
								List<DirectedPath> solnPaths = innerMap.get(key);
								DirectedPath startingDp = null;
								DirectedPath lastDp = null;
								for (DirectedPath dp : solnPaths) {
									DirectedPath newDp = newCopyOfDirectedPath(dp);
									if (lastDp != null) {
										newDp.setPrevious(lastDp);
										lastDp.addNext(newDp);
									}
									else {
										startingDp = newDp;
									}
									lastDp = newDp;
								}
								if (startingDp != null) {
									Map<Resource, DirectedPath> inside = new HashMap<Resource, DirectedPath>();
									inside.put(key, startingDp);
									solutions.put(root, inside);
								}
							}
						}
					}
				}
			}
		}
		return solutions;
	}

	/**
	 * Method to compute the effective roots for the next iteration
	 * @param newEffectiveRoots
	 * @param ceilingNodes
	 * @param solved
	 * @param solved2 
	 * @param nodesReached 
	 * @param notSolved
	 * @return
	 */
	private List<Resource> getNewEffectiveRoots(List<Resource> roots, List<Resource> newEffectiveRoots, List<Resource> ceilingNodes,
			List<Resource> solved, Map<Resource, Map<Resource, List<DirectedPath>>> nodesReached) {
		boolean allRootsConnected = true;
		
		// Identify a target to serve as a test node against which all other roots are checked for paths to determine if solution is complete
		Resource target = null;
		if (getAnchoringNode() != null) {
			target = getIftranslator().getResourceFromNamedNode(getAnchoringNode());
		}
		else if (ceilingNodes != null && ceilingNodes.size() > 0) {
			for (Resource ceiling : ceilingNodes) {
				if (roots.contains(ceiling)) {
					target = ceiling;
				}
			}
		}
		if (target == null) {
			Double idx = new Double(Math.random() * roots.size());
			target = roots.get(idx.intValue());
		}
		
		// Now iterate through roots and for all but target look for completed paths to target
		List<Resource> intersectionNodes = null;
		for (Resource root : roots) {
			boolean thisRootConnected = false;
			if (!root.equals(target)) {
				if (nodesReached.containsKey(root)) {
					// there are nodes reached from this root
					Map<Resource, List<DirectedPath>> innerMap = nodesReached.get(root);
					if (innerMap.containsKey(target)) {
						// there is a direct path from this root to the target
						thisRootConnected = true;
					}
					else if (innerMap != null) {
						// check for intersections
						Map<Resource, List<DirectedPath>> otherInnerMap = nodesReached.get(target);
						if (otherInnerMap != null) {
							for (Resource heightReached : newEffectiveRoots) {
								if (innerMap.containsKey(heightReached) && otherInnerMap.containsKey(heightReached)) {
									// the node heightReached has been reached from two different roots so this is an intersection node
									logger.debug("'{}' is an intersection node from '{}' and from '{}'", 
											heightReached.toString(), target.toString(), root.toString());
									logger.debug("Nodes reached:\n{}", getNodesReachedAsString(nodesReached));
									if (intersectionNodes == null) intersectionNodes = new ArrayList<Resource>();
									intersectionNodes.add(heightReached);
									thisRootConnected = true;
								}
							}
						}
					}
				}
				if (!thisRootConnected) {
					allRootsConnected = false;
					break;
				}
			}
		}
		if (intersectionNodes != null) {
			roots.addAll(intersectionNodes);
		}
		List<Resource> results = null;
		if (!allRootsConnected) {
			results = new ArrayList<Resource>();
			for (Resource r : newEffectiveRoots) {
				if (!solved.contains(r) && !results.contains(r) && !ceilingNodes.contains(r)) {
					results.add(r);
				}
			}
		}
		return results;
	}

	/**
	 * Method to check for solutions and remove solved nodes from effective roots
	 * @param root
	 * @param roots
	 * @param newHeights
	 * @param indexedBySubject
	 * @param indexedByObject
	 * @param ceilingNodes
	 * @param solved
	 * @return
	 * @throws TranslationException
	 * @throws MultiplePathsFoundException
	 */
	private List<Resource> checkForSolutions(Resource root, List<Resource> roots, List<Resource> newHeights,
			Map<Resource, List<DirectedPath>> indexedBySubject, Map<Resource, List<DirectedPath>> indexedByObject, List<Resource> ceilingNodes, List<Resource> solved) throws TranslationException, MultiplePathsFoundException {
		for (Resource height : newHeights) {
			if (!height.equals(root) && roots.contains(height) && roots.contains(root)) {
				// this must be a direct path solution
				solved.add(height);
				logger.debug("Direct solution path found from '{}' (object) up to '{}' (subject)", root.toString(), height.toString());
				DirectedPath dp = constructSolutionPath(height, root, indexedBySubject, indexedByObject);
				if (dp != null && recordSolution(dp.getSubject(), dp)) {
					solved.add(dp.getSubject());
					solved.add(dp.getObject());
				}
			}
		}
		newHeights.removeAll(solved);
		return newHeights;
	}

	/**
	 * Method to construct solution DirectedPath
	 * @param root
	 * @param height
	 * @param indexedBySubject
	 * @param indexedByObject
	 * @return
	 */
	private DirectedPath constructSolutionPath(Resource subject, Resource object,
			Map<Resource, List<DirectedPath>> indexedBySubject, Map<Resource, List<DirectedPath>> indexedByObject) {
		DirectedPath solnPath = null;
		if (indexedBySubject.containsKey(subject)) {
			List<DirectedPath> index = indexedBySubject.get(subject);
			for (DirectedPath dp : index) {
				if (dp.getObject() != null && dp.getObject().equals(object)) {
					solnPath = dp;
					break;
				}
			}
		}
		return solnPath;
	}

	/**
	 * Method to add new DirectedPaths to indices and return new effective roots
	 * @param roots -- the list of original (not effective) roots for the path finding problem
	 * @param nodeReached 
	 * @param ceilingNodes -- nodes from which no upward path can be found
	 * @param indexedBySubject -- path elements indexed by subject
	 * @param indexedByObject -- path elements indexed by object
	 * @param found -- a list of new directed path segments that extend upward
	 * @return -- a list of new upper nodes to become future effective roots
	 * @throws TranslationException
	 * @throws MultiplePathsFoundException
	 * @throws PathFindingException 
	 */
	private List<Resource> indexAndFindNewEffectiveRoots(List<Resource> roots, Map<Resource, List<DirectedPath>> indexedBySubject, Map<Resource, 
			List<DirectedPath>> indexedByObject, List<DirectedPath> found,
			Map<Resource, Map<Resource, List<DirectedPath>>> nodesReached) throws TranslationException, MultiplePathsFoundException {
		ArrayList<Resource> newHeights =null;
		if (found != null) {
			logger.debug("Indexing new paths:\n{}", pathListAsString(found));
			newHeights = new ArrayList<Resource>();
			for (DirectedPath dp : found) {
				Resource nodeReached = dp.getObject();
				checkForMultiplePaths(dp, indexedBySubject, indexedByObject);
				addDirectedPathToIndex(dp, indexedBySubject, dp.getSubject());
				addDirectedPathToIndex(dp, indexedByObject, dp.getObject());
				newHeights.add(dp.getSubject());
				// update nodesReached
				Map<Resource, List<DirectedPath>> innerMap = null;
				if (roots.contains(nodeReached)) {
					// this is an original root; only original roots are keys in the outer map
					innerMap = nodesReached.get(nodeReached);
					if (innerMap == null) {
						innerMap = new HashMap<Resource, List<DirectedPath>>();
						nodesReached.put(nodeReached, innerMap);
					}
					List<DirectedPath> theNewList = new ArrayList<DirectedPath>();
					if (innerMap.containsKey(nodeReached)) {
						// we have reached this node before
						List<DirectedPath> theList = innerMap.get(nodeReached);
						theNewList.addAll(theList);
						// add the new path
						theNewList.add(0, dp);
					}
					else {
						// add the new path
						theNewList.add(dp);
					}
					// now put the new inner map into the outer map
					innerMap.put(dp.getSubject(), theNewList);
					setNodeAddedToNodesReached(true);
				}
				else {
					// this is not an original root--find all paths up to this node and extend them
					Iterator<Resource> rootItr = nodesReached.keySet().iterator();
					while (rootItr.hasNext()) {
						Resource origRoot = rootItr.next();
						innerMap = nodesReached.get(origRoot);
						if (innerMap != null && innerMap.containsKey(nodeReached)) {
							// we have a path from this original root up to the node reached
							List<DirectedPath> theNewList = new ArrayList<DirectedPath>();	
							theNewList.addAll(innerMap.get(nodeReached));
							theNewList.add(0, dp);
							innerMap.put(dp.getSubject(), theNewList);
							setNodeAddedToNodesReached(true);
						}
					}
				}
			}
			logger.debug("At end of indexing, nodes reached are:\n{}", getNodesReachedAsString(nodesReached));
		}
		return newHeights;
	}

	private void checkForMultiplePaths(DirectedPath dp, Map<Resource, List<DirectedPath>> indexedBySubject,
			Map<Resource, List<DirectedPath>> indexedByObject) throws MultiplePathsFoundException {
		if (indexedBySubject.containsKey(dp.getSubject()) && indexedByObject.containsKey(dp.getObject())) {
			// there already exists a path with the same subject and with the same object
			List<DirectedPath> sval = indexedBySubject.get(dp.getSubject());
//			List<DirectedPath> oval = indexedByObject.get(dp.getObject());
			for (DirectedPath dpexisting : sval) {
				if (dp.getSubject().equals(dpexisting.getSubject()) && dp.getObject().equals(dpexisting.getObject())) {
					if (!dp.getConnection().equals(dpexisting.getConnection())) {
						throw new MultiplePathsFoundException(dp.getSubject(), dp.getObject(), dp, dpexisting);
					}
				}
			}
		}
	}

	/**
	 * Method to add a new DirectedPath to the specified index with specified key
	 * @param dp
	 * @param index
	 * @param key
	 */
	private void addDirectedPathToIndex(DirectedPath dp, Map<Resource, List<DirectedPath>> index, Resource key) {
		if (index.containsKey(key)) {
			List<DirectedPath> content = index.get(key);
			if (!content.contains(dp)) {
				content.add(dp);
			}
		}
		else {
			List<DirectedPath> newContent = new ArrayList<DirectedPath>();
			newContent.add(dp);
			index.put(key, newContent);
		}
	}

	/**
	 * Method to find DirectedPaths up from a given node using ontology range information
	 * @param root
	 * @return
	 * @throws TranslationException
	 * @throws PathFindingException
	 */
	private List<DirectedPath> findPathsUpFromOntologyRanges(Resource root) throws TranslationException, PathFindingException {
		List<DirectedPath> results = null;
		StmtIterator stmtitr = getTheJenaModel().listStatements(null, RDFS.range, root);
		while (stmtitr.hasNext()) {
			Resource candidate = stmtitr.nextStatement().getSubject();
			if (candidate.canAs(Property.class)) {
				Property prop = candidate.as(Property.class);
				DirectedPath itps = new DirectedPath(null, prop, root);
				itps.setPathSource(DirectedPathSource.OntologyRange);
				validateDirectedPath(itps);
				if (results == null) results = new ArrayList<DirectedPath>();
				results.add(itps);
			}
		}
		return results;
	}

	/**
	 * Method to find DirectedPaths up from a given node using ontology class hierarchy information
	 * @param roots
	 * @param root
	 * @return
	 * @throws TranslationException
	 */
	private List<DirectedPath> findPathsUpFromClassHierarchy(List<Resource> roots, Resource root) throws TranslationException {
		List<DirectedPath> results = null;
		if (root.canAs(OntResource.class)) {
			if (root.as(OntResource.class).isClass()) {
				ExtendedIterator<OntClass> scitr = root.as(OntResource.class).asClass().listSubClasses(true);
				while (scitr.hasNext()) {
					OntClass sprcls = scitr.next();
					if (roots.contains(sprcls)) {
						// both classes are in roots so we want to substitute references to the more general with references to the more specific
						Map<NamedNode, DirectedPath> mtrs = getMissingTripleReplacements();
						if (mtrs != null) {
							Iterator<NamedNode> nnitr = mtrs.keySet().iterator();
							while (nnitr.hasNext()) {
								NamedNode nn = nnitr.next();
								if (nn.getMissingTripleReplacement() != null) {
									GraphPatternElement gpe = nn.getMissingTripleReplacement().getProxyFor();
									if (gpe instanceof TripleElement && ((TripleElement)gpe).getSubject() != null && 
											((TripleElement)gpe).getSubject().equals(getIftranslator().getNamedNodeFromResourceMap(root))) {
										((TripleElement)gpe).setSubject(getIftranslator().getNamedNodeFromResourceMap(sprcls));
									}
								}
							}
						}
					}
					DirectedPath dp = new DirectedPath(sprcls, RDFS.subClassOf, root);
					dp.setPathSource(DirectedPathSource.ClassHierarchy);
					if (results == null) results = new ArrayList<DirectedPath>();
					results.add(dp);
				}
			}
		}
		return results;
	}

	/**
	 * Method to find DirectedPaths (to be created) from a Resource to higher ones using 
	 * ontology restriction.
	 * @param key -- the Jena Resource that is key to the DirectedPath
	 * @param itps -- the DirectedPath to be filled in
	 * @param lowerNode -- the Jena Resource to be used in the climb
	 * @return -- true if the climb succeeds else false
	 * @throws PathFindingException
	 * @throws TranslationException 
	 */
	private List<DirectedPath> findPathsUpFromOntologyRestrictions(Resource lowerNode) throws TranslationException {
		logger.debug("Entering findDirectedPathsByRestriction, upperNode = '{}'", lowerNode.toString());
		List<DirectedPath> results = null;
		Resource effectiveLowerNode = lowerNode;

		// TODO add hasValuesFrom for instances?		
		
		// allValuesFrom
		StmtIterator stmtitr = getTheJenaModel().listStatements(null, OWL.allValuesFrom, lowerNode);
		while (stmtitr.hasNext()) {
			Property prop = null;
			Restriction rest = null;
			Statement stmt = stmtitr.nextStatement();
			Statement opstmt = stmt.getSubject().getProperty(OWL.onProperty);
			if (opstmt != null) {
				RDFNode opobj = opstmt.getObject();
				if (opobj.canAs(Property.class)) {
					prop = opobj.as(Property.class);
					if (prop.toString().equals(SadlConstants.SADL_LIST_MODEL_FIRST_URI)) {
						Resource firstRest = opstmt.getSubject();
						if (firstRest.canAs(OntClass.class)) {
							ExtendedIterator<OntClass> scitr = firstRest.as(OntClass.class).listSubClasses(true);
							while (scitr.hasNext()) {
								OntClass sc = scitr.next();
								if (!sc.toString().equals(SadlConstants.SADL_LIST_MODEL_LIST_URI)) {
									effectiveLowerNode = sc;
									prop = null;
								}
							}
						}
					}
					else {
						rest = opstmt.getSubject().as(Restriction.class);
					}
				}
			}
			if (!effectiveLowerNode.equals(lowerNode)) {
				// have to do the allValuesFrom for the List class
				StmtIterator stmtitr2 = getTheJenaModel().listStatements(null, OWL.allValuesFrom, effectiveLowerNode);
				if (stmtitr2.hasNext()) {
					Statement stmt2 = stmtitr2.nextStatement();
					Statement opstmt2 = stmt2.getSubject().getProperty(OWL.onProperty);
					if (opstmt2 != null) {
						RDFNode opobj = opstmt2.getObject();
						if (opobj.canAs(Property.class)) {
							prop = opobj.as(Property.class);
							rest = opstmt.getSubject().as(Restriction.class);
						}
					}
				}			
			}
			if (prop != null && !effectiveLowerNode.equals(lowerNode)) {
				if (results == null) results = new ArrayList<DirectedPath>();
				DirectedPath result = new DirectedPath(null, prop, effectiveLowerNode);
				fillInUpperFromRestriction(effectiveLowerNode, result, rest);
				result.setPathSource(DirectedPathSource.OntologyRestriction);
				results.add(result);
			}
		}	// end while iterator on OWL.allValuesFrom

		// someValuesFrom
		stmtitr = getTheJenaModel().listStatements(null, OWL.someValuesFrom, effectiveLowerNode);
		while (stmtitr.hasNext()) {
			Property prop = null;
			Restriction rest = null;
			Statement stmt = stmtitr.nextStatement();
			Statement opstmt = stmt.getSubject().getProperty(OWL.onProperty);
			if (opstmt != null) {
				RDFNode opobj = opstmt.getObject();
				if (opobj.canAs(Property.class)) {
					prop = opobj.as(Property.class);
					rest = opstmt.getSubject().as(Restriction.class);
				}
			}
			if (prop != null) {
				DirectedPath result;
				if (results == null) results = new ArrayList<DirectedPath>();
				if (!effectiveLowerNode.equals(lowerNode)) {
					result  = new DirectedPath(null, prop, effectiveLowerNode);
				}
				else {
					result = new DirectedPath(null, prop, effectiveLowerNode);
				}
				result.setPathSource(DirectedPathSource.OntologyRestriction);
				fillInUpperFromRestriction(effectiveLowerNode, result, rest);
				results.add(result);
			}
		}

		// Qualified Cardinality
		stmtitr = getTheJenaModel().listStatements(null, OWL2.onClass, effectiveLowerNode);
		while (stmtitr.hasNext()) {
			Property prop = null;
			Restriction rest = null;
			Statement stmt = stmtitr.nextStatement();
			Statement opstmt = stmt.getSubject().getProperty(OWL.onProperty);
			if (opstmt != null) {
				RDFNode opobj = opstmt.getObject();
				if (opobj.canAs(Property.class)) {
					prop = opobj.as(Property.class);
					rest = opstmt.getSubject().as(Restriction.class);
				}
			}
			if (prop != null) {
				DirectedPath result;
				if (results == null) results = new ArrayList<DirectedPath>();
				if (!effectiveLowerNode.equals(lowerNode)) {
					result  = new DirectedPath(null, prop, effectiveLowerNode);
				}
				else {
					result = new DirectedPath(null, prop, effectiveLowerNode);
				}
				result.setPathSource(DirectedPathSource.OntologyRestriction);
				fillInUpperFromRestriction(effectiveLowerNode, result, rest);
				results.add(result);
			}
		}
		return results;
	}

	/**
	 * Method to find the a upward from guidance
	 * @param root
	 * @return
	 */
	private List<DirectedPath> findPathsUpFromGuidance(Resource root) {
		if (existingDirectedPathsByObject != null) {
			if (existingDirectedPathsByObject.containsKey(root)) {
				return existingDirectedPathsByObject.get(root);
			}
		}
		return null;
	}

	/**
	 * Method to fill in the subject of a TripleElement from the domain of the property
	 * @param tr
	 * @return
	 * @throws PathFindingException
	 * @throws TranslationException
	 */
	private Resource fillSubjectInTripleElement(TripleElement tr) throws PathFindingException, TranslationException {
		if (tr.getSubject() == null && tr.getPredicate() != null && tr.getPredicate() instanceof NamedNode) {
			StmtIterator stmtitr = getTheJenaModel().listStatements(getIftranslator().getResourceFromNamedNode((NamedNode) tr.getPredicate()), RDFS.domain, (RDFNode)null);
			if (stmtitr.hasNext()) {
				RDFNode objNode = stmtitr.nextStatement().getObject();
				if (objNode.isResource()) {
					tr.setSubject(getIftranslator().getNamedNodeFromResourceMap(objNode.asResource()));
					registerReplacement(tr.getSubject(), tr.getPredicate());
					return objNode.asResource();
				}
				else {
					throw new PathFindingException("Domain (" + objNode.toString() + ") isn't a Resource--how could this be?");
				}
			}
			if (stmtitr.hasNext()) {
				logger.debug("Property '{}' has more than one domain.", tr.getPredicate().toString());
			}
		}
		return null;
	}

	/**
	 * Method to register a replacement so that we can look it up later
	 * @param subject
	 * @param predicate
	 */
	private void registerReplacement(Node subject, Node predicate) {
		if (replacements == null) {
			replacements = new HashMap<Node, List<Node>>();
		}
		if (replacements.containsKey(subject)) {
			replacements.get(subject).add(predicate);
		}
		else {
			List<Node> predicates = new ArrayList<Node>();
			predicates.add(predicate);
			replacements.put(subject, predicates);
		}
	}
	
	/**
	 * Method to retrieve a replacement.
	 * @param node
	 * @return
	 */
	public List<Node> getReplacements(Node node) {
		if (replacements != null) {
			return replacements.get(node);
		}
		return null;
	}

	/**
	 * Method to duplicate a DirectedPath
	 * @param dp
	 * @return
	 */
	private DirectedPath newCopyOfDirectedPath(DirectedPath dp) {
		return new DirectedPath(dp.getSubject(), dp.getConnection(), dp.getObject());
	}

	/**
	 * Method to fill in upper nodes of an InvertedBuiltinTreePathStep which has a known BuiltinElement
	 * @param itps -- the InvertedBuiltinTreePathStep to be filled in
	 * @throws TranslationException 
	 * @throws PathFindingException 
	 */
	protected boolean fillInUpper(DirectedPath itps) throws TranslationException, PathFindingException {
		logger.debug("Call to fillInUpper for DirectedPath '{}'", itps.toString());
		if (itps.getConnection() != null && itps.getConnection() instanceof BuiltinElement) {
			List<Node> args = ((BuiltinElement) itps.getConnection()).getArguments();
			for (int i = 0; i < args.size(); i++) {
				Node arg = args.get(i);
				if (arg instanceof VariableNode) {
//					itps.setSubject((NamedNode) arg);
					throw new TranslationException("Variable subjects not handled");
				}
				else if (arg instanceof NamedNode) {
					Resource rsrc = getIftranslator().getResourceFromNamedNode((NamedNode) arg); 
					if (rsrc != null) {
						itps.setSubject(rsrc);
						logger.debug("    upper filled: {}", itps.toString());
						return true;
					}
				}
				else if (arg instanceof ProxyNode && ((ProxyNode)arg).getProxyFor() instanceof GraphPatternElement) {
					GraphPatternElement arggpe = (GraphPatternElement) ((ProxyNode)arg).getProxyFor();
					if (arggpe instanceof TripleElement) {
						if (((TripleElement)arggpe).getPredicate() instanceof NamedNode) {
							Property prop = getIftranslator().getPropertyFromNamedNode((NamedNode) ((TripleElement)arggpe).getPredicate()); 
							if (prop != null) {
								itps.setConnection(prop);
							}
						}
						else {
							throw new TranslationException("Unexpected non-NamedNode in triple predicate");
						}
					}
				}
			}
		}
		if (itps.getSubject() == null && itps.getConnection() instanceof Property) {
			StmtIterator stmtitr = getTheJenaModel().listStatements((Property) itps.getConnection(), RDFS.domain, (RDFNode)null);
			if (stmtitr.hasNext()) {
				RDFNode objNode = stmtitr.nextStatement().getObject();
				if (objNode.isResource()) {
					itps.setSubject(objNode.asResource());
					logger.debug("    upper filled: {}", itps.toString());
					return true;
				}
				else {
					throw new PathFindingException("Domain (" + objNode.toString() + ") isn't a Resource--how could this be?");
				}
			}
			if (stmtitr.hasNext()) {
				throw new PathFindingException("Property '" + itps.getConnection().toString() + "' has more than one domain.");
			}
			return false;
		}
		logger.debug("    upper NOT filled: {}", itps.toString());
		return true;
	}

	/**
	 * Method to fill in the "upperNode" of an DirectedPath which has a known property but no upperNode
	 * using restrictions in the ontology
	 * @param key -- the Jena Resource that is key to the DirectedPath
	 * @param itps -- the DirectedPath to be filled in
	 * @param op -- the property to be used
	 * @param rest -- the Restriction to be used
	 * @return -- true if the upperNode of the DirectedPath was successfully filled in else false
	 */
	private boolean fillInUpperFromRestriction(Resource key, DirectedPath itps, Restriction rest) {
		logger.debug("Call to fillInUpperFromRestriction for DirectedPath '{}' with key '{}'", itps.toString(), key.toString());
		boolean result = false;
		if (itps.getSubject() != null) {
			result = true;
		}
		else {
			ExtendedIterator<OntClass> scitr = rest.listSubClasses(true);
			while (scitr.hasNext()) {
				OntClass cls = scitr.next();
				itps.setSubject(cls);
				result = true;
				logger.debug("    upper filled: {}", itps.toString());
				// TODO what if there's more than one? Can there be?			
			}
		}
		return result;
	}

	/**
	 * Method to try to fill in the subject (upper end) of a DirectedPath generated from a TripleElement
	 * @param dp
	 * @throws PathFindingException
	 */
	private void fillInLower(DirectedPath dp) throws PathFindingException {
		if (dp.getConnection() instanceof Property) {
			logger.debug("Call to fillInLower for DirectedPath '{}'", dp.toString());
			boolean done = false;
			// check existing, then ontology restrictions, then range
			if (existingDirectedPathsBySubject != null) {
				if (existingDirectedPathsBySubject.containsKey(dp.getSubject())) {
					List<DirectedPath> dps = existingDirectedPathsBySubject.get(dp.getSubject());
					for (DirectedPath dpf : dps) {
						if (dpf.getConnection().equals(dp) && dp.getObject() != null) {
							dp.setObject(dpf.getObject());
							done = true;
						}
					}
				}
			}
			if (!done) {
				StmtIterator stmtitr = getTheJenaModel().listStatements(null, OWL.onProperty, (Property)dp.getConnection());
				while (stmtitr.hasNext() && !done) {
					Resource restcls = stmtitr.nextStatement().getSubject();
					if (restcls.canAs(AllValuesFromRestriction.class)){
						OntResource subjectResource = dp.getSubject().as(OntResource.class);
						OntClass subjectClass = null;
						if(subjectResource.canAs(OntClass.class))	{
							subjectClass = subjectResource.as(OntClass.class);
						}else if(subjectResource.canAs(Individual.class) && !subjectResource.asIndividual().isProperty()) {
							subjectClass = subjectResource.as(Individual.class).getOntClass();
						}
						if (subjectClass != null) {						
							if(restcls.as(AllValuesFromRestriction.class).hasSubClass(subjectClass)) {
								Resource avf = restcls.as(AllValuesFromRestriction.class).getAllValuesFrom();
								dp.setObject(avf);
								done = true;
							}else {
								for(OntClass superClass : subjectClass.listSuperClasses().toList()) {
									if(restcls.as(AllValuesFromRestriction.class).hasSubClass(superClass)) {
										Resource avf = restcls.as(AllValuesFromRestriction.class).getAllValuesFrom();
										dp.setObject(avf);
										done = true;
										break;
									}
								}
							}
						}
					}
				}	
			}
			if (!done) {
				StmtIterator stmtitr = getTheJenaModel().listStatements((Property)dp.getConnection(), RDFS.range, (RDFNode)null);
				while (stmtitr.hasNext()) {
					RDFNode rng = stmtitr.nextStatement().getObject();
					if (rng.isResource()) {
						dp.setObject(rng.asResource());
						done = true;
					}
					if (stmtitr.hasNext()) {
						throw new PathFindingException("Trying to use range of '" + dp.getConnection().toString() + "' to get lower node of '" + dp.toString() + "'. Range has multiple classes.");
					}
				}
			}
			if (done) {
				logger.debug("   successfully filled: {}", dp.toString());
			}
		}
		else {
			throw new PathFindingException("Should only need to fill in lower for triple DirectedPath (" + dp.toString() + ")");
		}
	}

	/**
	 * Method to create a TripleELement as a replacement triple from a DirectedPath.
	 * @param replacement
	 * @return
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws TranslationException
	 */
	protected ProxyNode createReplacementTriple(DirectedPath replacement) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (replacement != null) {
			Node subject = null;
			Node predicate = null;
			Node object = null;
			if (replacement.getSubject() != null) {
				subject = getIftranslator().getNamedNodeFromResourceMap(replacement.getSubject());
			}
			if (replacement.getConnection() instanceof Property) {
				predicate = getIftranslator().getNamedNodeFromResourceMap((Resource) replacement.getConnection());
			}
			if (replacement.getObject() != null) {
				object = getIftranslator().getNamedNodeFromResourceMap(replacement.getObject());
			}
			TripleElement replacementTriple = new TripleElement(subject, predicate, object);
			getIftranslator().getModelProcessor();
			return (ProxyNode) SadlModelProcessor.nodeCheck(replacementTriple);
		}
		return null;
	}

	/**
	 * Method to add a Node and its GrapthPatternElement container to the map of nodes to be investigated. If the patternType is
	 * PatternType.Replacement (which happens when the node is a property not in a property chain) then a replacement DirectedPath
	 * is generated and returned with the property Resource as predicate. The subject and object will be filled in later.
	 * @param subject
	 * @param container
	 * @param patternType
	 * @return
	 * @throws TranslationException
	 */
	protected DirectedPath addNodeAndContainerToBeInvestigated(Resource subject, GraphPatternElement container, PatternType patternType) throws TranslationException {
		NamedNode node = getIftranslator().getNamedNodeFromResourceMap(subject);
		if (nodesToInvestigateWithContext == null) {
			nodesToInvestigateWithContext = new HashMap<NamedNode, List<GraphPatternElement>>();
			List<GraphPatternElement> newList = new ArrayList<GraphPatternElement>();
			newList.add(container);
			nodesToInvestigateWithContext.put(node, newList);
			logger.debug("Adding node {} to be investigated with container: \n {}", node.toString(), container.toDescriptiveString());
		}
		else {
			if (nodesToInvestigateWithContext.containsKey(node)) {
				if (!nodesToInvestigateWithContext.get(node).contains(container)) {
					nodesToInvestigateWithContext.get(node).add(container);
					logger.debug("Adding node {} to be investigated with container: \n {}", node.toString(), container.toDescriptiveString());
				}
			}
			else {
				List<GraphPatternElement> newList = new ArrayList<GraphPatternElement>();
				newList.add(container);
				nodesToInvestigateWithContext.put(node, newList);
				logger.debug("Adding node {} to be investigated with container: \n {}", node.toString(), container.toDescriptiveString());
			}
		}
		DirectedPath newTr = null;
		if (patternType != null && patternType.equals(PatternType.Replacement)) {
			newTr = new DirectedPath(null, subject, null);
		}
		return newTr;
	}

	/**
	 * Method to determine if a Node is a property in the ontology
	 * @param nodeType
	 * @return
	 */
	protected boolean isProperty(Node nodeType) {
		return getIftranslator().getModelProcessor().isProperty(nodeType);
	}

	/**
	 * Method to record a solution. Solutions are a set of paths recorded in linked lists of Triples.
	 * The paths are placed in a map keyed by the starting Resource, and are themselves in a map
	 * keyed by ending Resource. This allows a solution to be found by its starting node and ending node.
	 * @param topRoot -- the root Resource of this path
	 * @param lastTr -- the last triple in this path
	 * @return
	 * @throws TranslationException
	 * @throws MultiplePathsFoundException 
	 */
	protected boolean recordSolution(Resource topRoot, DirectedPath lastTr) throws TranslationException, MultiplePathsFoundException {
		boolean retval = false;
		Resource pathEndKey = lastTr.getObject();
		DirectedPath current = lastTr;
		DirectedPath next = null;
		do {
			// duplicate the current triple
			DirectedPath dupTr = new DirectedPath(current.getSubject(), current.getConnection(), current.getObject());
			if (next != null) {
				dupTr.addNext(next);
				next.setPrevious(dupTr);
			}
			next = dupTr;
			current = current.getPrevious();
		} while (current != null);	
		
		if (getSolutions() == null) {
			setSolutions(new HashMap<Resource, Map<Resource, DirectedPath>>());
			Map<Resource, DirectedPath> paths = new HashMap<Resource, DirectedPath>();
			paths.put(pathEndKey, next);
			getSolutions().put(topRoot, paths);
			retval = true;
		}
		else if (getSolutions().containsKey(topRoot)) {
			Map<Resource, DirectedPath> paths = getSolutions().get(topRoot);
			if (!paths.containsKey(pathEndKey)) {
				paths.put(pathEndKey, next);
				retval = true;
			}
			else {
				DirectedPath path = paths.get(pathEndKey);
				if (!equivalentPaths(path, next)) {
					throw new MultiplePathsFoundException(topRoot, pathEndKey, path, next);
				}
				// retval remains false
			}
		}
		else {
			Map<Resource, DirectedPath> paths = new HashMap<Resource, DirectedPath>();
			paths.put(pathEndKey, next);
			getSolutions().put(topRoot, paths);
			retval = true;
		}
		return retval;
	}
	
	/**
	 * Method to determine if a solution has already been found for a start and an end Resource
	 * @param start
	 * @param end
	 * @return
	 */
	public boolean solutionFound(Resource start, Resource end) {
		if (getSolutions() != null && getSolutions().containsKey(start)) {
			if (getSolutions().get(start).containsKey(end)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Method to determine if two DirectedPaths are equivalent. Note
	 * that this is recursive if the DirectedPaths each have a single next path
	 * and will return false if either has more than one next path.
	 * @param dp1
	 * @param dp2
	 * @return
	 */
	private boolean equivalentPaths(DirectedPath dp1, DirectedPath dp2) {
		if (dp1.getNext() == null && dp2.getNext() == null) {
			if (dp1.equals(dp2)) {
				return true;
			}
		}
		else if (dp1.getSubject().equals(dp2.getSubject()) && dp1.getObject().equals(dp2.getObject()) && dp1.getConnection().equals(dp2.getConnection())) {
			if (dp1.getNext() != null && dp1.getNext().size() == 1 && dp2.getNext() != null && dp2.getNext().size() == 1) {
				return equivalentPaths(dp1.getNext().get(0), dp2.getNext().get(0));
			}
		}
		return false;
	}

	/**
	 * Method to find an OntClass which is a list with elements of the given type
	 * @param listElementType
	 * @return
	 */
	protected OntClass findListOfType(Resource listElementType) {
		OntClass typedList = null;
		StmtIterator stmtitr = getTheJenaModel().listStatements(null, OWL.allValuesFrom, listElementType);
		if (stmtitr.hasNext()) {
			Statement stmt = stmtitr.nextStatement();
			Statement opstmt = stmt.getSubject().getProperty(OWL.onProperty);
			if (opstmt != null) {
				RDFNode opobj = opstmt.getObject();
				if (opobj.canAs(Property.class)) {
					Property prop = opobj.as(Property.class);
					if (prop.toString().equals("http://sadl.org/sadllistmodel#first")) {
						Resource firstRest = opstmt.getSubject();
						if (firstRest.canAs(OntClass.class)) {
							ExtendedIterator<OntClass> scitr = firstRest.as(OntClass.class).listSubClasses(true);
							while (scitr.hasNext()) {
								OntClass sc = scitr.next();
								if (!sc.toString().equals("http://sadl.org/sadllistmodel#List")) {
									typedList = sc;
								}
							}
						}
					}
				}
			}
		}
		return typedList;
	}

	/**
	 * Getter for the OntModel theJenaModel
	 */
	protected OntModel getTheJenaModel() {
		return theJenaModel;
	}

	/**
	 * Setter for the OntModel theJenaModel
	 * @param theJenaModel
	 */
	protected void setTheJenaModel(OntModel theJenaModel) {
		this.theJenaModel = theJenaModel;
	}

	/**
	 * Method to get one or more DirectedPaths from a GraphPatternElement. Whether multiple
	 * DirectedPaths are returned depends upon the GraphPatternElement subclass--Junctions, for
	 * example, return multiple DirectedPaths
	 * @param gpe
	 * @return
	 * @throws TranslationException
	 * @throws PathFindingException 
	 */
	protected List<DirectedPath> getDirectedPathFromGPE(GraphPatternElement gpe) throws TranslationException, PathFindingException {
		List<DirectedPath> results = new ArrayList<DirectedPath>();
		if (gpe instanceof TripleElement) {
			Node subj = ((TripleElement)gpe).getSubject();
			if (subj instanceof NamedNode) {
				Resource s = getIftranslator().getResourceFromNamedNode((NamedNode) subj); // getTheJenaModel().getResource(subj.toFullyQualifiedString());
				Node pred = ((TripleElement)gpe).getPredicate();
				if (pred instanceof NamedNode) {
					Property p = getIftranslator().getPropertyFromNamedNode((NamedNode) pred); // getTheJenaModel().getProperty(pred.toFullyQualifiedString());
					boolean isDatatypeProperty = p.canAs(DatatypeProperty.class);
					if(!isDatatypeProperty) {
						Node obj = ((TripleElement)gpe).getObject();
						if (obj instanceof NamedNode) {
							Resource o = getIftranslator().getResourceFromNamedNode((NamedNode) obj); // getTheJenaModel().getResource(obj.toFullyQualifiedString());
							results.add(new DirectedPath(s, p, o));
						}
						else if (obj == null) {
							DirectedPath dp = new DirectedPath(s,p,null);
							validateDirectedPath(dp);
							if(dp.getObject() != null) {
								results.add(dp);
							}
						}
					}
				}
			}
		}
		else if (gpe instanceof BuiltinElement) {
			boolean isPathGenerating = getIftranslator().isPathGeneratingBuiltinElement((BuiltinElement)gpe);
			List<Node> args = ((BuiltinElement)gpe).getArguments();
			if (args != null) {
				for (int i = 0; i < args.size(); i++) {
					Node arg = args.get(i);
					if (arg instanceof ProxyNode) {
						List<DirectedPath> argPaths = getDirectedPathFromGPE((GraphPatternElement) ((ProxyNode)arg).getProxyFor());
						if (argPaths.size() > 0) {
							results.addAll(argPaths);
							if (isPathGenerating && i == 0) {
								// this is the first arg so (at least for list element built-ins) it is the list
								//	create a DirectedPath from Typed List (upper) to list element type (lower)
								validateDirectedPath(argPaths.get(0));
								Resource listType = argPaths.get(0).getObject();
								Resource elementType = getIftranslator().getResourceFromNamedNode(getIftranslator().getModelProcessor().getTypedListType(listType));
								if (elementType != null && listType != null) {
									results.add(new DirectedPath(listType, gpe, elementType));
								}
							}
						}
					}
				}
			}
		}
		else if (gpe instanceof Junction) {
			Object lhs = ((Junction)gpe).getLhs();
			Object rhs = ((Junction)gpe).getRhs();
			if (lhs instanceof ProxyNode) {
				List<DirectedPath> lhsDPs = getDirectedPathFromGPE((GraphPatternElement) ((ProxyNode)lhs).getProxyFor());
				if (rhs instanceof ProxyNode) {
					List<DirectedPath> rhsDPs = getDirectedPathFromGPE((GraphPatternElement) ((ProxyNode)rhs).getProxyFor()); 
					if (lhsDPs != null) {
						results.addAll(lhsDPs);
					}
					if (rhsDPs != null) {
						results.addAll(rhsDPs);
					}
				}
			}
		}
		return results;
	}

	/**
	 * Getter for intermediate form translator
	 * @return
	 */
	protected IntermediateFormTranslator getIftranslator() {
		return iftranslator;
	}

	/**
	 * Setter for intermediate form translator
	 * @param iftranslator
	 */
	protected void setIftranslator(IntermediateFormTranslator iftranslator) {
		this.iftranslator = iftranslator;
	}

	/** Method to register a missing triple replacement DirectedPath
	 * (This occurs when there is a property node which is not part of a triple pattern.)
	 * @param node
	 * @param replacement
	 */
	protected void addMissingTripleReplacement(NamedNode node, DirectedPath replacement) {
		if (missingTripleReplacements == null) {
			missingTripleReplacements = new HashMap<NamedNode, DirectedPath>();
		}
		getMissingTripleReplacements().put(node, replacement);
	}

	/**
	 * Getter for missing triple replacements, which occur when there is a property node that is not part of a triple pattern
	 * @return
	 */
	public Map<NamedNode, DirectedPath> getMissingTripleReplacements() {
		return missingTripleReplacements;
	}

	/**
	 * Getter for the solutions generated by this PathFinder
	 * @return
	 */
	public Map<Resource, Map<Resource, DirectedPath>> getSolutions() {
		return solutions;
	}

	/**
	 * Setter for the solution field, which hold missing paths found by this PathFinder
	 * @param solutions
	 */
	private void setSolutions(Map<Resource, Map<Resource, DirectedPath>> solutions) {
		this.solutions = solutions;
	}
	
	/**
	 * Method to serialize solution set as a String
	 * @return
	 */
	public String getSolutionsAsString() {
		Map<Resource, Map<Resource, DirectedPath>> solns = getSolutions();
		if (solns != null) {
			StringBuffer sb = new StringBuffer();
			Iterator<Resource> solnkeyitr = solns.keySet().iterator();
			while (solnkeyitr.hasNext()) {
				Resource solnkey = solnkeyitr.next();
				sb.append("Solutions for key: ");
				sb.append(solnkey.toString());
				Map<Resource, DirectedPath> soln = solns.get(solnkey);
				Iterator<Resource> inneritr = soln.keySet().iterator();
				while (inneritr.hasNext()) {
					DirectedPath itps = soln.get(inneritr.next());
					sb.append("\n  ");
					sb.append(itps.toString());
					List<DirectedPath> below = itps.getNext();
					int indentCntr = 1;
					while (below != null) {
						indentCntr++;
						for ( DirectedPath b : below) {
							sb.append("\n");
							for (int i = 0; i < indentCntr; i++) {
								sb.append("  ");
							}
							sb.append(b.toString());
							below = b.getNext();
						}
					}
				}
				sb.append("\n\n");
			}
			return sb.toString();
		}
		return "";
	}

	/**
	 * Method to serialize nodesReached for visual inspection
	 * @param nodesReached
	 * @return
	 */
	private String getNodesReachedAsString(Map<Resource, Map<Resource, List<DirectedPath>>> nodesReached) {
		if (nodesReached != null) {
			StringBuffer sb = new StringBuffer("Nodes reached:\n");
			Iterator<Resource> lowerItr = nodesReached.keySet().iterator();
			while (lowerItr.hasNext()) {
				Resource lower = lowerItr.next();
				Map<Resource, List<DirectedPath>> innerMap = nodesReached.get(lower);
				Iterator<Resource> upperItr = innerMap.keySet().iterator();
				while (upperItr.hasNext()) {
					Resource upper = upperItr.next();
					List<DirectedPath> dps = innerMap.get(upper);
					sb.append("  up from ");
					sb.append(lower.toString());
					sb.append(" to ");
					sb.append(upper.toString());
					sb.append(":\n");
					for (DirectedPath dp : dps) {
						sb.append("    ");
						sb.append(dp.toString());
						sb.append("\n");
					}
				}
			}
			return sb.toString();
		}
		return "";
	}

	/**
	 * Getter for the agent, if any, that is the actor
	 * @return
	 */
	public NamedNode getAnchoringNode() {
		return anchoringNode;
	}

	/**
	 * Setter for the agent, if any, that is the actor
	 * @param agent
	 */
	private void setAnchoringNode(NamedNode anchoringNode) {
		this.anchoringNode = anchoringNode;
	}

	private boolean isNodeAddedToNodesReached() {
		return nodeAddedToNodesReached;
	}

	private void setNodeAddedToNodesReached(boolean nodeAddedToNodesReached) {
		this.nodeAddedToNodesReached = nodeAddedToNodesReached;
	}

}
