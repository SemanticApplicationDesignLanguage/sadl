package com.ge.research.sadl.ui.visualize;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.MaxCardinalityRestriction;
import com.hp.hpl.jena.ontology.MinCardinalityRestriction;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

public class GraphGenerator {
	private static final Logger logger = LoggerFactory.getLogger(GraphGenerator.class);
	protected static final String BLUE = "blue";
	protected static final String COLOR = "color";
	protected static final String SHAPE = "shape";
	protected static final String OCTAGON = "octagon";
	protected static final String RED = "red";

	private OntModel model = null;
	private ConceptName anchor = null;
	protected IConfigurationManagerForIDE configMgr;
	
	public enum Orientation {TD, LR}

	public GraphGenerator(IConfigurationManagerForIDE configMgr, String publicUri, ConceptName startNode) throws ConfigurationException, IOException {
		this.configMgr = configMgr;
		setModel(configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS));
		setAnchor(startNode);
	}

	public ResultSet generateClassHierarchy(int size) throws ConfigurationException {
		OntClass cls = getModel().getOntClass(getAnchor().toFQString());
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
			if (gs.subject instanceof OntClass) {
				instData = classInstances((OntClass)gs.subject, instData);
			}
			if (gs.object instanceof OntClass) {
				instData = classInstances((OntClass)gs.object, instData);
			}
		}
		data.addAll(instData);
		return data;
	}

	public ResultSet generateClassNeighborhood(int size) throws ConfigurationException {
		OntClass cls = getModel().getOntClass(getAnchor().toFQString());
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		data = generateClassPropertiesWithDomain(cls, size, data);
		data = generateClassPropertiesWithRange(cls, size, data);
		return convertDataToResultSet(data);
	}

	public ResultSet generatePropertyNeighborhood(int size) throws ConfigurationException {
		OntProperty ontprop = getModel().getOntProperty(getAnchor().toFQString());

		List<GraphSegment> data = new ArrayList<GraphSegment>();
		ExtendedIterator<? extends OntResource> eitr = ontprop.listDomain();
		while (eitr.hasNext()) {
			OntResource dmn = eitr.next();
			if (dmn.canAs(OntClass.class)){
				data = generatePropertyRange(dmn.as(OntClass.class), ontprop, size - 1, data);
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

	public ResultSet generateIndividualNeighborhood(int size) throws ConfigurationException {
		Individual inst = getModel().getIndividual(getAnchor().toFQString());

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
				if (!instData.contains(gs)) {
					instData.add(gs);
				}
				gs.addTailAttribute(COLOR, BLUE);
			}
		}
		return instData;
	}

	private List<GraphSegment> generateIndividualNeighborhood(Individual inst, int graphRadius,
			List<GraphSegment> data) {
		if (graphRadius <= 0) return data;
		// as object
		StmtIterator sitr = getModel().listStatements(null, null, inst);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Property prop = stmt.getPredicate();
			if (prop.equals(OWL.hasValue)) {
				continue;
			}
			Resource subj = stmt.getSubject();
			GraphSegment sg = new GraphSegment(subj, prop, stmt.getObject(), configMgr);
			if (!stmt.getPredicate().getNameSpace().equals(RDF.getURI()) && !data.contains(sg)) {
				data.add(sg);
				if (subj.canAs(Individual.class)) {
					data = generateIndividualNeighborhood(subj.as(Individual.class), graphRadius - 1, data);
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
				sg.addHeadAttribute(COLOR, BLUE);
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
		StmtIterator sitr = getModel().listStatements(null, RDFS.range, cls);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource prop = stmt.getSubject();
			if (prop.canAs(OntProperty.class)) {
				OntProperty ontprop = prop.as(OntProperty.class);
				ExtendedIterator<? extends OntResource> eitr = ontprop.listDomain();
				while (eitr.hasNext()) {
					OntResource dmn = eitr.next();
					GraphSegment sg = new GraphSegment(dmn, prop, cls, configMgr);
					if (!data.contains(sg)) {
						data.add(sg);
						data = generateClassPropertiesWithRange(dmn.as(OntClass.class), graphRadius - 1, data);
					}
				}
			}
		}
		return data;
	}

	private List<GraphSegment> generateClassPropertiesWithDomain(OntClass cls, int graphRadius,
			List<GraphSegment> data) {
		if (graphRadius <= 0) return data;
		StmtIterator sitr = getModel().listStatements(null, RDFS.domain, cls);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource prop = stmt.getSubject();
			if (prop.canAs(OntProperty.class)) {
				data = generatePropertyRange(cls, prop, graphRadius - 1, data);
			}
		}
		// now look for unions? or intersections containing the cls
		String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
		qstr += "select ?prop where {?prop rdfs:domain/(owl:unionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
		QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getModel());;		
		com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
		while (results.hasNext()) {
			QuerySolution soln = results.next();
			RDFNode prop = soln.get("?prop");
			if (prop.canAs(OntProperty.class)) {
				data = generatePropertyRange(cls, prop.as(OntProperty.class), graphRadius - 1, data);
			}
		}
		return data;
	}

	private List<GraphSegment> generatePropertyRange(OntClass cls,
			Resource prop, int graphRadius, List<GraphSegment> data) {
		if (graphRadius <= 0) return data;
		boolean isList = false;
		Statement stmt = prop.getProperty(getModel().getAnnotationProperty(SadlConstants.LIST_RANGE_ANNOTATION_PROPERTY));
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
				Resource listClass = getModel().getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
				//if list class exists or the range has a superclass that is a list
				if (listClass == null || rng.as(OntClass.class).hasSuperClass(listClass)) {
					//iterate across all of the statements that are subclasses of range?
					StmtIterator stmtitr = getModel().listStatements(rng, RDFS.subClassOf, (RDFNode)null);
					while (stmtitr.hasNext()) {
//					ExtendedIterator<OntClass> scitr = rng.as(OntClass.class).listSuperClasses(true);
//					while (scitr.hasNext()) {
						Statement supclsstmt = stmtitr.nextStatement();
						RDFNode supclsnode = supclsstmt.getObject();
						//if the superclass is a class
						if (supclsnode.canAs(OntClass.class)){
							//get the subclass
							OntClass subcls = supclsnode.as(OntClass.class);  // scitr.next();
							if (subcls.hasProperty(OWL.onProperty, getModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
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
			GraphSegment sg = isList ? new GraphSegment(cls, prop, rng, isList) : new GraphSegment(cls, prop, rng, configMgr);
			if (!data.contains(sg)) {
				data.add(sg);
				if (prop.as(OntProperty.class).isObjectProperty()) {
					data = generateClassPropertiesWithDomain(rng.as(OntClass.class), graphRadius - 1, data);
				}
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
			QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getModel());;		
			com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
			while (results.hasNext()) {
				QuerySolution soln = results.next();
				RDFNode sub = soln.get("?subclass");
				if (sub.canAs(OntClass.class)) {
					OntClass subcls = sub.as(OntClass.class);
					GraphSegment sg = new GraphSegment(cls, "subClass", subcls, configMgr);
					if (!data.contains(sg)) {
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

	public ResultSet convertDataToResultSet(List<GraphSegment> data) {
		List<String> columnList = new ArrayList<String>();
		columnList.add("head");
		columnList.add("edge");
		columnList.add("tail");
		List<String> headKeyList = null;
		List<String> edgeKeyList = null;
		List<String> tailKeyList = null;
		
		for (int i = 0; i < data.size(); i++) {
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
		Object array[][] = new Object[data.size()][maxColumns];
		
		boolean dataFound = false;
		for(int i = 0; i < data.size(); i++) {
			GraphSegment gs = data.get(i);
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
		}
		if (dataFound) {
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
			String ns = uri.substring(0, sep - 1);
			String ln = uri.substring(sep);
			// get the prefix and if there is one generate qname
			String prefix = configMgr.getGlobalPrefix(ns);
			if (prefix != null) {
				return prefix + ":" + ln;
			}
			return ln;
		}
		return uri;
	}

	protected OntModel getModel() {
		return model;
	}

	protected void setModel(OntModel model) {
		this.model = model;
	}

	protected ConceptName getAnchor() {
		return anchor;
	}

	protected void setAnchor(ConceptName anchor) {
		this.anchor = anchor;
	}

}
