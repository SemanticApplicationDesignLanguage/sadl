/************************************************************************
 * Copyright (c) 2007-2010 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.visualize;


import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.ui.visualize.GraphGenerator.UriStrategy;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

/*
 * This class is the root class for generating graphs ins SADL. 
 * 
 * Notes
 *   1. Graph file naming conventions: It is important that file names used to save graphs to file and file names used in hyperlinking be the same. Therefore a common graph 
 *      file naming method is created for the purpose of generating a name. This method is found in ??? and implements this naming strategy:
 *   	a. When a graph file coincides one-to-one with a model file, e.g., foo.sadl, the graph file name will be created by adding the graph file extension to that
 *   	   name, e.g., foo.sadl.svg. Because it is possible that files being graphed may be the same, except for the extension, the extension is retained as part of the
 *    	   base filename.
 *      b. When the graph file name is not directly associates with a specific model file, the name is derived from the model file and the concept anchor in that file.
 *   2. Node labels and and anchor names must also coincide so consistent methods exists in the GraphSegment class to convert to string. They are:
 *   	a. String resourceToString(Resource rsrc) 
 *   	b. String conceptNameToString(ConceptName cn)
 *      They implements the strategy specified in the enum UriStrategy. The node label can be URI, QName, or LocalName.
 *   	 
 * */
public class GraphGenerator {
	private static final Logger logger = LoggerFactory.getLogger(GraphGenerator.class);
	protected static final String COLOR = "color";
	protected static final String BLUE = "blue";
	protected static final String INSTANCE_BLUE = "blue";
	protected static final String CLASS_BLUE = "blue4";
	protected static final String RED = "red";
	protected static final String WHITE = "white";
	protected static final String PROPERTY_GREEN = "green3";
	protected static final String BLACK = "black";
	
	protected static final String SHAPE = "shape";
	protected static final String OCTAGON = "octagon";
	protected static final String DIAMOND = "diamond";
	
	protected static final String STYLE = "style";
	protected static final String FILLED = "filled";
	protected static final String BOLD = "bold";
	protected static final String FONTCOLOR = "fontcolor";
	protected static final String FILL_COLOR = "fillcolor";
	
	protected static final String IS_IMPORT = "isImport";
	protected static final String LINK_URL = "href";

	
	public enum UriStrategy {URI_ONLY, QNAME_ONLY, LOCALNAME_ONLY, QNAME_WITH_URI_TOOLTIP, LOCALNAME_WITH_QNAME_TOOLTIP, LOCALNAME_WITH_URI_TOOLTIP}
	
	private UriStrategy uriStrategy = UriStrategy.QNAME_ONLY;

	private IProject project = null;
	private OntModel theJenaModel = null;
	private ConceptName anchor = null;
	protected IConfigurationManagerForIDE configMgr;
	private boolean includeDuplicates = false;
	private long lastSequenceNumber = 0;		// When includeDuplicates is true, nodes with the same URI must be distinguished from one another. This is done with a sequenceNumber set in the GraphSegment class.
	private Property impliedProperty;
	private IGraphVisualizer visualizer = null;
	private IProgressMonitor monitor = null;
	private HashMap<String, List<String>> classToPropertyMap = null;
	
	public enum Orientation {TD, LR}

	public GraphGenerator(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, IProject project, String publicUri, ConceptName startNode) throws ConfigurationException, IOException {
		setConfigMgr(configMgr);
		setVisualizer(visualizer);
		setProject(project);
		if (publicUri != null) {
			setTheJenaModel(configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS));
		}
		setAnchor(startNode);
	}
	
	public GraphGenerator(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, IProject project, String publicUri, ConceptName startNode, IProgressMonitor monitor) throws ConfigurationException, IOException {
		setConfigMgr(configMgr);
		setVisualizer(visualizer);
		setProject(project);
		if (publicUri != null) {
			setTheJenaModel(configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS));
		}
		setAnchor(startNode);
		setProgressMonitor(monitor);
	}

	public ResultSet generateClassHierarchy(int size) throws ConfigurationException, InvalidNameException {
		isCanceled();
		OntClass cls = getTheJenaModel().getOntClass(getAnchor().toFQString());
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		data = generateClassHierarchy(cls, size, data);
		ResultSet rs = convertDataToResultSet(data);
		return rs;
	}

	private List<GraphSegment> generateClassHierarchy(OntClass cls, int size, List<GraphSegment> data) {
		data = generateClassSubclasses(cls, size, data);
		data = generateClassSuperclasses(cls, size, data);
		data = generateClassInstances(cls, size, data);
		return data;
	}

	private List<GraphSegment> generateClassInstances(OntClass cls, int size, List<GraphSegment> data) {
		if (size <= 0) return data;
		List<GraphSegment> instData = new ArrayList<GraphSegment>();
		instData = classInstances(cls, instData);
		for (int i = 0; i < data.size(); i++) {
			GraphSegment gs = data.get(i);
			if (gs.getSubject() instanceof OntClass) {
				instData = classInstances((OntClass)gs.getSubject(), instData);
			}
			if (gs.getObject() instanceof OntClass) {
				instData = classInstances((OntClass)gs.getObject(), instData);
			}
		}
		data.addAll(instData);
		return data;
	}

	public ResultSet generateClassNeighborhood(int size) throws ConfigurationException, InvalidNameException {
		isCanceled();
		ExtendedIterator<OntClass> classIter = getTheJenaModel().listClasses();
		try{
			//Add all of the class triples
			while(classIter.hasNext()){
				Object obj = classIter.next();	
				String uri = obj.toString();
				int i = 0;
			}
		}
		catch (Throwable t) {
			
		}
		OntClass cls = getTheJenaModel().getOntClass(getAnchor().toFQString());
		if (cls != null) {
			List<GraphSegment> data = new ArrayList<GraphSegment>();
			data = generateClassPropertiesWithDomain(cls, -1, size, false, data);
			data = generateClassPropertiesWithRange(cls, size, data);
			return convertDataToResultSet(data);
		}
		return null;
	}

	public ResultSet generatePropertyNeighborhood(int size) throws ConfigurationException, InvalidNameException {
		OntProperty ontprop = getTheJenaModel().getOntProperty(getAnchor().toFQString());

		List<GraphSegment> data = new ArrayList<GraphSegment>();
		ExtendedIterator<? extends OntResource> eitr = ontprop.listDomain();
		while (eitr.hasNext()) {
			isCanceled();
			OntResource dmn = eitr.next();
			if (dmn.canAs(OntClass.class)){
				data = generatePropertyRange(dmn.as(OntClass.class), -1, ontprop, size, false, data);
			}
			data = generateClassPropertiesWithRange(dmn.as(OntClass.class), size - 1, data);
		}
//		ExtendedIterator<? extends OntResource> eitr2 = ontprop.listRange();
//		while (eitr2.hasNext()) {
//			OntResource rng = eitr2.next();
//			if (ontprop.as(OntProperty.class).isObjectProperty()) {
//				data = generateClassPropertiesWithDomain(rng.as(OntClass.class), data);
//			}
//		}
		return convertDataToResultSet(data);
	}

	public ResultSet generateIndividualNeighborhood(int size) throws ConfigurationException, InvalidNameException {
		isCanceled();
		Individual inst = getTheJenaModel().getIndividual(getAnchor().toFQString());

		List<GraphSegment> data = new ArrayList<GraphSegment>();
		data = generateIndividualNeighborhood(inst, size, data);
		return convertDataToResultSet(data);
	}

	private List<GraphSegment> classInstances(OntClass cls, List<GraphSegment> instData) {
		ExtendedIterator<? extends OntResource> itr = cls.listInstances(true);
		while (itr.hasNext()) {
			OntResource or = itr.next();
			if (or.canAs(Individual.class)){
				GraphSegment gs = new GraphSegment(cls, "instance", or.as(Individual.class), configMgr);
				gs.addHeadAttribute(COLOR, CLASS_BLUE);
				gs.addTailAttribute(COLOR, INSTANCE_BLUE);
				if (!instData.contains(gs)) {
					instData.add(gs);
				}
			}
		}
		return instData;
	}

	private List<GraphSegment> generateIndividualNeighborhood(Individual inst, int graphRadius,
			List<GraphSegment> data) {
		if (graphRadius <= 0) return data;
		// as object
		StmtIterator sitr = getTheJenaModel().listStatements(null, null, inst);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Property prop = stmt.getPredicate();
			if (prop.equals(OWL.hasValue)) {
				continue;
			}
			Resource subj = stmt.getSubject();
			GraphSegment sg = new GraphSegment(subj, prop, stmt.getObject(), configMgr);
			if (!stmt.getPredicate().getNameSpace().equals(RDF.getURI()) && !data.contains(sg)) {
				if (stmt.getObject().isURIResource()) {
					if (stmt.getObject().canAs(OntClass.class)){
						sg.addTailAttribute(COLOR, CLASS_BLUE);
					}
					else {
						sg.addTailAttribute(COLOR, INSTANCE_BLUE);
					}
				}
				sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
				data.add(sg);
				if (subj.canAs(Individual.class)) {
					sg.addHeadAttribute(COLOR, INSTANCE_BLUE);
					data = generateIndividualNeighborhood(subj.as(Individual.class), graphRadius - 1, data);
				}
				else {
					sg.addHeadAttribute(COLOR, CLASS_BLUE);
				}
			}
		}
		// as subject
		sitr = inst.listProperties();
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Property prop = stmt.getPredicate();
			RDFNode obj = stmt.getObject();
			GraphSegment sg = new GraphSegment(stmt.getSubject(), prop, obj, configMgr);
			if (!data.contains(sg)) {
				sg.addHeadAttribute(COLOR, INSTANCE_BLUE);
				if (obj.canAs(OntClass.class)){
					sg.addTailAttribute(COLOR, CLASS_BLUE);
				}
				else if (obj.canAs(Individual.class)){
					sg.addTailAttribute(COLOR, INSTANCE_BLUE);
				}
				sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
				data.add(sg);
				if (!stmt.getPredicate().equals(RDF.type) && obj.canAs(Individual.class)) {
					data = generateIndividualNeighborhood(obj.as(Individual.class), graphRadius - 1, data);
				}
				else if (obj.canAs(OntClass.class)) {
					data = generateClassHierarchy(obj.as(OntClass.class), graphRadius - 1, data);
				}
			}
		}
		return data;
	}

	private List<GraphSegment> generateClassPropertiesWithRange(OntClass cls, int graphRadius,
			List<GraphSegment> data) {
		if (graphRadius <= 0) return data;
		StmtIterator sitr = getTheJenaModel().listStatements(null, RDFS.range, cls);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource prop = stmt.getSubject();
			if (prop.canAs(OntProperty.class)) {
				OntProperty ontprop = prop.as(OntProperty.class);
				ExtendedIterator<? extends OntResource> eitr = ontprop.listDomain();
				while (eitr.hasNext()) {
					OntResource dmn = eitr.next();
					GraphSegment sg = new GraphSegment(dmn, prop, cls, configMgr);
					if (dmn.isClass()) {
						sg.addHeadAttribute(COLOR, CLASS_BLUE);
					}
					else if (dmn.isIndividual()) {
						sg.addHeadAttribute(COLOR, INSTANCE_BLUE);
					}
					sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
					sg.addTailAttribute(COLOR, CLASS_BLUE);
					if (!data.contains(sg)) {
						data.add(sg);
						data = generateClassPropertiesWithRange(dmn.as(OntClass.class), graphRadius - 1, data);
					}
				}
			}
		}
		return data;
	}

	protected List<GraphSegment> generateClassPropertiesWithDomain(OntClass cls, long subjSeqNumber, int graphRadius, boolean fillNodes,
			List<GraphSegment> data) {
		if (graphRadius <= 0) return data;
		if (isIncludeDuplicates() && subjSeqNumber < 0) {
			subjSeqNumber = getNewSequenceNumber();
		}
		List<Resource> handledProperties = new ArrayList<Resource>();
		ExtendedIterator<OntClass> superitr = cls.listSuperClasses(true);
		while (superitr.hasNext()) {
			OntClass superCls = superitr.next();
			StmtIterator sitr = getTheJenaModel().listStatements(null, RDFS.domain, superCls);
			while (sitr.hasNext()) {
				Statement stmt = sitr.nextStatement();
				Resource prop = stmt.getSubject();
				if (displayPropertyOfClass(cls, prop)) {
					data = generatePropertyRange(cls, subjSeqNumber, prop, graphRadius - 1, fillNodes, data);
					handledProperties.add(prop);
				}
			}
//			data = generateClassPropertiesWithDomain(superCls, subjSeqNumber, graphRadius - 1, data);
		}
		// now look for unions? or intersections containing the cls
		String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
		qstr += "select ?prop where {?prop rdfs:domain/(owl:unionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
		QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getTheJenaModel());;		
		com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
		while (results.hasNext()) {
			QuerySolution soln = results.next();
			RDFNode prop = soln.get("?prop");
			if (!handledProperties.contains(prop.asResource()) && displayPropertyOfClass(cls, prop.asResource())) {
				data = generatePropertyRange(cls, subjSeqNumber, prop.as(OntProperty.class), graphRadius, fillNodes, data);
				handledProperties.add(prop.asResource());
			}
		}
		return data;
	}
	
	protected boolean displayPropertyOfClass(OntClass cls, Resource prop) {
		if (prop.canAs(OntProperty.class) && 
		    !isImpliedPropertyOfClass(cls, prop) &&
		    !classToPropertyMapContains(cls.getURI(), prop.getURI())) {
			return true;
		}
		return false;
	}

	private boolean isImpliedPropertyOfClass(Resource cls, Resource prop) {
		return getTheJenaModel().contains(cls, getImpliedProperty(), prop);
	}

	private List<GraphSegment> generatePropertyRange(OntClass cls, long subjSeqNumber, Resource prop, int graphRadius, boolean fillNodes, List<GraphSegment> data) {
		isCanceled();
		if (graphRadius <= 0) return data;
		boolean isList = false;
		Statement stmt = prop.getProperty(getTheJenaModel().getAnnotationProperty(SadlConstants.LIST_RANGE_ANNOTATION_PROPERTY));
		if (stmt != null) {
			RDFNode obj = stmt.getObject();
			if (obj.isLiteral()) {
				Object lit = obj.asLiteral().getValue();
				if (lit != null && lit.toString().equals("LIST")) {
					isList = true;
				}
			}
		}
		ExtendedIterator<? extends OntResource> eitr = prop.as(OntProperty.class).listRange();
		while (eitr.hasNext()) {
			//get range of prop
			OntResource rng = eitr.next();
			//if it's a list and range class
			if (isList && rng.canAs(OntClass.class)) {
				//get list class
				//Check for an all values from restriction
				Resource listClass = getTheJenaModel().getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
				//if list class exists or the range has a superclass that is a list
				if (listClass == null || rng.as(OntClass.class).hasSuperClass(listClass)) {
					//iterate across all of the statements that are subclasses of range?
					StmtIterator stmtitr = getTheJenaModel().listStatements(rng, RDFS.subClassOf, (RDFNode)null);
					while (stmtitr.hasNext()) {
//					ExtendedIterator<OntClass> scitr = rng.as(OntClass.class).listSuperClasses(true);
//					while (scitr.hasNext()) {
						Statement supclsstmt = stmtitr.nextStatement();
						RDFNode supclsnode = supclsstmt.getObject();
						//if the superclass is a class
						if (supclsnode.canAs(OntClass.class)){
							//get the subclass
							OntClass subcls = supclsnode.as(OntClass.class);  // scitr.next();
							if (subcls.hasProperty(OWL.onProperty, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
								Statement avf = subcls.getProperty(OWL.allValuesFrom);
								if (avf != null) {
									RDFNode listtype = avf.getObject();
									if (listtype.canAs(OntResource.class)){
										rng = listtype.as(OntResource.class);
									}
								}
								stmtitr.close();
								break;
							}
						}
					}
				}
			}
			GraphSegment sg = isList ? new GraphSegment(cls, prop, rng, isList, configMgr) : new GraphSegment(cls, prop, rng, configMgr);
			long objSeqNumber = -1L;
			if (isIncludeDuplicates()) {
				objSeqNumber = getNewSequenceNumber();
				sg.setSubjectNodeDuplicateSequenceNumber(subjSeqNumber);
				sg.setObjectNodeDuplicateSequenceNumber(objSeqNumber);
			}
			if (isIncludeDuplicates() || !data.contains(sg)) {
				if (fillNodes) {
					sg.addHeadAttribute(STYLE, FILLED);
					sg.addHeadAttribute(FILL_COLOR,CLASS_BLUE);
					sg.addHeadAttribute(FONTCOLOR, WHITE);
				}
				else {
					sg.addHeadAttribute(COLOR, CLASS_BLUE);
				}
				sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
				if (prop.canAs(ObjectProperty.class) && !(rng.isURIResource() && rng.getNameSpace().equals(XSD.getURI()))) {
					if (fillNodes) {
						sg.addTailAttribute(STYLE, FILLED);
						sg.addTailAttribute(FILL_COLOR, CLASS_BLUE);
						sg.addTailAttribute(FONTCOLOR, WHITE);
					}
					else {
						sg.addTailAttribute(COLOR, CLASS_BLUE);
					}
				}
				else {
					// what for XSD and user-defined types?
				}
				data.add(sg);
				if (prop.as(OntProperty.class).isObjectProperty() && !cls.equals(rng)) {
					data = generateClassPropertiesWithDomain(rng.as(OntClass.class), objSeqNumber, graphRadius - 1, fillNodes, data);
				}
			}
			else {
				logger.debug("Ignoring duplicate graph segment '" + sg.toString());
			}
		}
		return data;
	}

	private List<GraphSegment> generateClassSuperclasses(OntClass cls, int size, List<GraphSegment> data) {
		if (size <= 0) return data;
		try {
			ExtendedIterator<OntClass> eitr = cls.listSuperClasses(true);
			while (eitr.hasNext()) {
				Resource scr = eitr.next();
	
	//		StmtIterator sitr = getJenaModel().listStatements(cls, RDFS.subClassOf, (Resource)null);
	//		while (sitr.hasNext()) {
	//			Resource scr = sitr.next().getSubject();
				
				if (scr != null && !scr.equals(cls) && scr.canAs(OntClass.class)) {
					OntClass supercls = scr.as(OntClass.class);
					GraphSegment sg;
					if (supercls.isRestriction()) {
						sg = new GraphSegment(supercls, "restricts", cls, configMgr);
						sg.addHeadAttribute(COLOR, RED);
						sg.addEdgeAttribute(COLOR, RED);
						sg.addTailAttribute(COLOR, CLASS_BLUE);
					}
					else {
						sg = new GraphSegment(supercls, "subClass", cls, configMgr);
					}
					if (!data.contains(sg)) {
						data.add(sg);
					}
					data = generateClassSuperclasses(supercls, size - 1, data);
				}
			}
		}
		catch (Throwable t) {
			
		}
		if (cls.isUnionClass()) {
			// if all of the members of a Union class are subclasses of a common super class, then that common super class is a super class of the Union class
			ExtendedIterator<? extends OntClass> eunitr = cls.asUnionClass().listOperands();
			List<OntClass> commonSupers = new ArrayList<OntClass>();
			int idx = 0;
			while (eunitr.hasNext()) {
				OntClass member = eunitr.next();
				ExtendedIterator<OntClass> sclsitr = member.listSuperClasses(false);
				List<OntClass> otherSupers = new ArrayList<OntClass>();
				while (sclsitr.hasNext()) {
					if (idx == 0) {
						// first pass add all members to list
						commonSupers.add(sclsitr.next());
					}
					else {
						// subsequent pass 
						otherSupers.add(sclsitr.next());
					}
				}
				if (idx > 0) {
					// remove any members from commonSupers that are not in otherSupers
					for (int i = commonSupers.size() - 1; i >= 0; i--) {
						OntClass ccls = commonSupers.get(i);
						if (!otherSupers.contains(ccls)) {
							commonSupers.remove(ccls);
						}
					}
				}
				idx++;
			}
			if (commonSupers.size() > 0) {
				for (int i = 0; i < commonSupers.size(); i++) {
					OntClass cscls = commonSupers.get(i);
					GraphSegment sg = new GraphSegment(cscls, "subClass", cls, configMgr);
					sg.addHeadAttribute(COLOR, CLASS_BLUE);
					sg.addTailAttribute(COLOR, CLASS_BLUE);
					if (!data.contains(sg)) {
						data.add(sg);
					}
					data = generateClassSuperclasses(cscls, size - 1, data);
				}
			}
		}
		else if (cls.isIntersectionClass()) {
			// each member class of an Intersection class is a super class of the Intersection class
			ExtendedIterator<? extends OntClass> einteritr = cls.asIntersectionClass().listOperands();
			while (einteritr.hasNext()) {
				OntClass member  = einteritr.next();
				GraphSegment sg = new GraphSegment(member, "subClass", cls, configMgr);
				sg.addHeadAttribute(COLOR, CLASS_BLUE);
				sg.addTailAttribute(COLOR, CLASS_BLUE);
				if (!data.contains(sg)) {
					data.add(sg);
					data = generateClassSuperclasses(member, size - 1, data);
				}
			}
		}
		return data;
	}
	
	private List<GraphSegment> generateClassSubclasses(OntClass cls, int size, List<GraphSegment> data) {
		if (size <= 0) return data;
		ExtendedIterator<OntClass> eitr = cls.listSubClasses(true);
		while (eitr.hasNext()) {
			OntClass subcls = eitr.next();
			GraphSegment sg;
			if (subcls.isRestriction()) {
				sg = new GraphSegment(cls, "restricts", subcls, configMgr);
				sg.addHeadAttribute(COLOR, RED);
				sg.addEdgeAttribute(COLOR, RED);
			}
			else {
				sg = new GraphSegment(cls, "subClass", subcls, configMgr);
				sg.addHeadAttribute(COLOR, CLASS_BLUE);
				sg.addTailAttribute(COLOR, CLASS_BLUE);
			}
			if (!data.contains(sg)) {
				data.add(sg);
				data = generateClassSubclasses(subcls, size - 1, data);
			}
		}
		
		// if this class is part of an intersection, then the superclass of the intersection is a subclass of this class
		if (cls.isURIResource()) {
			String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
	//		qstr += "select ?subclass ?superclass where {?subclass rdfs:subClassOf/(owl:intersectionOf/rdf:rest*/rdf:first)? ?superclass}";
			qstr += "select ?subclass ?superclass where {?subclass rdfs:subClassOf/(owl:intersectionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
			QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getTheJenaModel());;		
			com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
			while (results.hasNext()) {
				QuerySolution soln = results.next();
				RDFNode sub = soln.get("?subclass");
				if (sub.canAs(OntClass.class)) {
					OntClass subcls = sub.as(OntClass.class);
					GraphSegment sg = new GraphSegment(cls, "subClass", subcls, configMgr);
					if (!data.contains(sg)) {
						sg.addHeadAttribute(COLOR, CLASS_BLUE);
						sg.addTailAttribute(COLOR, CLASS_BLUE);
						data.add(sg);
						data = generateClassSubclasses(subcls, size - 1, data);
					}	
				}
			}
		}
//		StmtIterator sitr = model.listStatements(null, OWL.intersectionOf, cls);
//		while (sitr.hasNext()) {
//			OntClass intersectCls = sitr.nextStatement().getSubject().as(OntClass.class);
//			StmtIterator sitr2 = model.listStatements(null, RDFS.subClassOf, intersectCls);
//			while (sitr2.hasNext()) {
//				OntClass subcls = sitr2.nextStatement().getSubject().as(OntClass.class);
//				GraphSegment sg = new GraphSegment(cls, "subClass", subcls);
//				if (!data.contains(sg)) {
//					data.add(sg);
//					data = generateClassSubclasses(subcls, data);
//				}	
//			}
//		}
		return data;
	}

	public ResultSet convertDataToResultSet(List<GraphSegment> data) throws InvalidNameException {
		List<String> columnList = new ArrayList<String>();
		columnList.add("head");
		columnList.add("edge");
		columnList.add("tail");
		List<String> headKeyList = null;
		List<String> edgeKeyList = null;
		List<String> tailKeyList = null;
		
		for (int i = 0; i < data.size(); i++) {
			isCanceled();
			GraphSegment gs = data.get(i);
			if (gs.getHeadAttributes() != null) {
				if (headKeyList == null) headKeyList = new ArrayList<String>();
				Iterator<String> keyitr = gs.getHeadAttributes().keySet().iterator();
				while (keyitr.hasNext()) {
					String key = keyitr.next();
					if (!headKeyList.contains(key)) {
						headKeyList.add(key);
						columnList.add("head_" + key);
					}
				}
			}
			if (gs.getEdgeAttributes() != null) {
				if (edgeKeyList == null) edgeKeyList = new ArrayList<String>();
				Iterator<String> keyitr = gs.getEdgeAttributes().keySet().iterator();
				while (keyitr.hasNext()) {
					String key = keyitr.next();
					if (!edgeKeyList.contains(key)) {
						edgeKeyList.add(key);
						columnList.add("edge_" + key);
					}
				}
			}
			if (gs.getTailAttributes() != null) {
				if (tailKeyList == null) tailKeyList = new ArrayList<String>();
				Iterator<String> keyitr = gs.getTailAttributes().keySet().iterator();
				while (keyitr.hasNext()) {
					String key = keyitr.next();
					if (!tailKeyList.contains(key)) {
						tailKeyList.add(key);
						columnList.add("tail_" + key);
					}
				}
			}
		}
		int maxColumns = 3 + 
				(headKeyList != null ? headKeyList.size() : 0) + 
				(edgeKeyList != null ? edgeKeyList.size() : 0) +
				(tailKeyList != null ? tailKeyList.size() : 0);
		if (isIncludeDuplicates()) {
			maxColumns = maxColumns + 2;
		}
		Object array[][] = new Object[data.size()][maxColumns];
		
		boolean dataFound = false;
		for(int i = 0; i < data.size(); i++) {
			isCanceled();
			GraphSegment gs = data.get(i);
			gs.setUriStrategy(getUriStrategy());
			String s = gs.subjectToString();
			String p = gs.predicateToString();
			String o = gs.objectToString();
			array[i][0] = s;
			array[i][1] = p;
			array[i][2] = o;
			dataFound = true;
			array = attributeToDataArray("head", gs.getHeadAttributes(), columnList, array, i, gs);
			array = attributeToDataArray("edge", gs.getEdgeAttributes(), columnList, array, i, gs);
			array = attributeToDataArray("tail", gs.getTailAttributes(), columnList, array, i, gs);
			if (isIncludeDuplicates()) {
				array[i][maxColumns - 2] = gs.getSubjectNodeDuplicateSequenceNumber();
				array[i][maxColumns - 1] = gs.getObjectNodeDuplicateSequenceNumber();
			}
		}
		if (dataFound) {
			if (isIncludeDuplicates()) {
				columnList.add("head_sequence_number");
				columnList.add("tail_sequence_number");
			}
			String[] headers = columnList.toArray(new String[0]);
			ResultSet rs = new ResultSet(headers, array);
			return rs;
		}
		return null;
	}

	private Object[][] attributeToDataArray(String graphPart, Map<String,String> map, List<String> columnList, Object[][] array, int i, GraphSegment gs) {
		if (map != null) {
			Iterator<String> attritr = map.keySet().iterator();
			while (attritr.hasNext()) {
				String key = attritr.next();
				int cidx = columnList.indexOf(graphPart + "_" + key);
				if (cidx > 0) {
					array[i][cidx] = map.get(key);
				}
			}
		}
		return array;
	}

	public String uriStringToString(String uri) {
		int sep = uri.lastIndexOf('#');
		if (sep > 0) {
			String ns = uri.substring(0, sep);
			String ln = uri.substring(sep + 1);
			// get the prefix and if there is one generate qname
			String prefix = configMgr.getGlobalPrefix(ns);
			if (prefix != null && prefix.length() > 0) {
				return prefix + ":" + ln;
			}
			return ln;
		}
		return uri;
	}

	protected OntModel getTheJenaModel() {
		return theJenaModel;
	}

	protected void setTheJenaModel(OntModel model) {
		this.theJenaModel = model;
	}
	
	protected void isCanceled(){
		if(monitor != null && monitor.isCanceled()){
			throw new OperationCanceledException();
		}
	}
	
	protected void setProgressMonitor(IProgressMonitor monitor){
		this.monitor = monitor;
	}
	
	protected ConceptName getAnchor() {
		return anchor;
	}

	protected void setAnchor(ConceptName anchor) {
		this.anchor = anchor;
	}
	
	protected boolean classToPropertyMapContains(String c, String p){
		if(this.classToPropertyMap == null){
			this.classToPropertyMap = new HashMap<String,List<String>>();
			addClassToPropertyMap(c,p);
			return false;
		}
		
		if(this.classToPropertyMap.containsKey(c) &&
		   this.classToPropertyMap.get(c).contains(p)){
			return true;
		}
		
		addClassToPropertyMap(c,p);
		
		return false;
	}
	
	protected void addClassToPropertyMap(String c, String p){
		if(this.classToPropertyMap.containsKey(c)){
			this.classToPropertyMap.get(c).add(p);
		}else{
			this.classToPropertyMap.put(c, new ArrayList<String>());
			addClassToPropertyMap(c,p);
		}
	}

	public UriStrategy getUriStrategy() {
		return uriStrategy;
	}

	public void setUriStrategy(UriStrategy uriStrategy) {
		this.uriStrategy = uriStrategy;
	}

	public boolean isIncludeDuplicates() {
		return includeDuplicates;
	}

	public void setIncludeDuplicates(boolean includeDuplicates) {
		this.includeDuplicates = includeDuplicates;
	}

	private Property getImpliedProperty() {
		if (impliedProperty == null && getTheJenaModel() != null) {
			setImpliedProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI));
		}
		return impliedProperty;
	}

	private void setImpliedProperty(Property impliedProperty) {
		this.impliedProperty = impliedProperty;
	}

	protected long getLastSequenceNumber() {
		return lastSequenceNumber;
	}

	protected long getNewSequenceNumber() {
		return ++lastSequenceNumber;
	}

	public static boolean isProperty(OntConceptType srType) {
		if (srType.equals(OntConceptType.ANNOTATION_PROPERTY) || 
				srType.equals(OntConceptType.CLASS_PROPERTY) ||
				srType.equals(OntConceptType.DATATYPE_PROPERTY) ||
				srType.equals(OntConceptType.RDF_PROPERTY)) {
			return true;
		}
		return false;
	}

	public String uriToString(String uri) {
		if (getUriStrategy().equals(UriStrategy.LOCALNAME_ONLY)) {
			if (uri.contains("#")) {
				return uri.substring(uri.indexOf("#") + 1);
			}
			return uri;
		}
		else if (getUriStrategy().equals(UriStrategy.LOCALNAME_WITH_QNAME_TOOLTIP)) {
			if (uri.contains("#")) {
				return uri.substring(uri.indexOf("#") + 1);
			}
			return uri;
		}
		else if (getUriStrategy().equals(UriStrategy.LOCALNAME_WITH_URI_TOOLTIP)) {
			if (uri.contains("#")) {
				return uri.substring(uri.indexOf("#") + 1);
			}
			return uri;
		}
		else if (getUriStrategy().equals(UriStrategy.QNAME_ONLY)) {
			if (uri.contains("#")) {
				String ns = uri.substring(0, uri.indexOf("#"));
				String prefix = getPrefix(ns);
				if (prefix != null) {
					return  prefix + ":" + uri.substring(uri.indexOf("#") + 1);
				}
				else {
					return uri.substring(uri.indexOf("#") + 1);
				}
			}
			return uri;
		}
		else if (getUriStrategy().equals(UriStrategy.QNAME_WITH_URI_TOOLTIP)) {
			if (uri.contains("#")) {
				String ns = uri.substring(0, uri.indexOf("#"));
				String prefix = getPrefix(ns);
				if (prefix != null) {
					return  prefix + ":" + uri.substring(uri.indexOf("#") + 1);
				}
				else {
					return uri.substring(uri.indexOf("#") + 1);
				}
			}
			return uri;
		}
		else if (getUriStrategy().equals(UriStrategy.URI_ONLY)) {
			return uri;
		}
		return uri;
	}

	private String getPrefix(String ns) {
		String prefix;
		if (ns.equals(XSD.getURI())) prefix = "xsd";
		else if (ns.equals(RDF.getURI())) prefix = "rdf:";
		else if (ns.equals(RDFS.getURI())) prefix = "rdfs:";
		else if (ns.equals(OWL.getURI())) prefix = "owl:";
		else {
			if (ns.endsWith("#")) ns = ns.substring(0, ns.length() - 1);
			prefix = configMgr.getGlobalPrefix(ns); 
		}
		return prefix;
	}

	protected IConfigurationManagerForIDE getConfigMgr() {
		return configMgr;
	}

	protected void setConfigMgr(IConfigurationManagerForIDE configMgr) {
		this.configMgr = configMgr;
	}

	protected IProject getProject() {
		return project;
	}

	protected void setProject(IProject project) {
		this.project = project;
	}
	
	public String getGraphFilenameExtension() throws Exception {
		if (getVisualizer() != null) {
			return getVisualizer().getGraphFilenameExtension();
		}
		throw new Exception("No visualizer has been established; can't determine graph filename extension.");
	}

	protected IGraphVisualizer getVisualizer() {
		return visualizer;
	}

	protected void setVisualizer(IGraphVisualizer visualizer) {
		this.visualizer = visualizer;
	}

	public String getBaseFilenameFromPublicUri(String publicUri) throws Exception {
		String altUrl = getConfigMgr().getAltUrlFromPublicUri(publicUri);
		if (altUrl != null) {
			int lastSlash = altUrl.lastIndexOf('/');
			if (lastSlash > 0) {
				return altUrl.substring(lastSlash + 1); 
			}
		}
		throw new Exception("Unable to find actual URL for public URI '" + publicUri + "'");
	}
}
