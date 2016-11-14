package com.ge.research.sadl.ui.visualize;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.ui.visualize.GraphGenerator;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.ResultSet;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDFS;

public class OntologyGraphGenerator {

	private static final Logger logger = LoggerFactory.getLogger(OntologyGraphGenerator.class);
	protected static final String COLOR = "color";
	protected static final String SHAPE = "shape";
	protected static final String STYLE = "style";
	protected static final String FILLED = "filled";
	protected static final String FILL_COLOR = "fillcolor";
	protected static final String BLUE = "blue";
	protected static final String LIGHT_BLUE = "cadetblue1";
	protected static final String YELLOW = "yellow";
	protected static final String BLACK = "black";
	protected static final String OCTAGON = "octagon";
	protected static final String DIAMOND = "diamond";
	protected static final String RED = "red";
	protected static final String GREEN = "green";
	

	private OntModel withImportModel = null; //model with imports
	private OntModel localModel = null;   //model without imports
	private ConceptName anchor = null;
	private IConfigurationManagerForIDE configMgr;
	
	public enum Orientation {TD, LR}
	
	public OntologyGraphGenerator(IConfigurationManagerForIDE configMgr, String publicUri) throws ConfigurationException, IOException {
		this.configMgr = configMgr;
		setModels(configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS), configMgr.getOntModel(publicUri, Scope.LOCALONLY));
	}
	
	public ResultSet generateOntologyResultSet(List<String[]> ontologyResults, String publicUri){
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		boolean isImport = false;
		
		ExtendedIterator<OntClass> classIter = getModelWithImports().listClasses();
		try{
			while(classIter.hasNext()){
				OntClass classInst = classIter.next();			
				//check to see if this is an imported class
				if(getLocalModel().containsResource(classInst)){
					isImport = false;
	//				GraphSegment gs = new GraphSegment(classInst, "", classInst, configMgr);
	//				if(!data.contains(gs)){
	//					data.add(gs);
	//				}
				}else{
					isImport = true;
				}
				//if not an import do the next three steps
				if(!isImport){
					//add parents to this node
					data = addParentClasses(classInst, data, ontologyResults, publicUri);
					//add instances of this node
					data = addClassInstances(classInst, data, ontologyResults, publicUri);
					//add properties of this node
					data = addClassProperties(classInst, data, publicUri);
				}
				
				//add children of this node
				
			}
		}catch(Exception e){
			
		}
		
		ResultSet rs = convertDataToResultSet(data);

		return rs;
	}
	
	
	

	

	private List<GraphSegment> addClassProperties(OntClass cls,
			List<GraphSegment> data, String parentPublicUri) throws ConfigurationException, IOException {
		StmtIterator sitr = getLocalModel().listStatements(null, RDFS.domain, cls);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource prop = stmt.getSubject();
			if (prop.canAs(OntProperty.class)) {
				data = generatePropertyRange(cls, prop, data, parentPublicUri);
			}
		}
		// now look for unions? or intersections containing the cls
		String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
		qstr += "select ?prop where {?prop rdfs:domain/(owl:unionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
		QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getLocalModel());;		
		com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
		while (results.hasNext()) {
			QuerySolution soln = results.next();
			RDFNode prop = soln.get("?prop");
			if (prop.canAs(OntProperty.class)) {
				data = generatePropertyRange(cls, prop.as(OntProperty.class), data, parentPublicUri);
			}
		}
		return data;
	}

	private List<GraphSegment> generatePropertyRange(OntClass cls,
			Resource prop, List<GraphSegment> data, String parentPublicUri) throws ConfigurationException, IOException {
		boolean isList = false;
		Statement stmt = prop.getProperty(getLocalModel().getAnnotationProperty(SadlConstants.LIST_RANGE_ANNOTATION_PROPERTY));
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
				Resource listClass = getLocalModel().getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
				//if list class exists or the range has a superclass that is a list
				if (listClass == null || rng.as(OntClass.class).hasSuperClass(listClass)) {
					//iterate across all of the statements that are subclasses of range?
					StmtIterator stmtitr = getLocalModel().listStatements(rng, RDFS.subClassOf, (RDFNode)null);
					while (stmtitr.hasNext()) {
//					ExtendedIterator<OntClass> scitr = rng.as(OntClass.class).listSuperClasses(true);
//					while (scitr.hasNext()) {
						Statement supclsstmt = stmtitr.nextStatement();
						RDFNode supclsnode = supclsstmt.getObject();
						//if the superclass is a class
						if (supclsnode.canAs(OntClass.class)){
							//get the subclass
							OntClass subcls = supclsnode.as(OntClass.class);  // scitr.next();
							if (subcls.hasProperty(OWL.onProperty, getLocalModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
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
				sg.addEdgeAttribute(COLOR, GREEN);
				if(!isInImports((OntClass)rng, parentPublicUri)){
					sg.addTailAttribute(STYLE, FILLED);
					sg.addTailAttribute(FILL_COLOR, LIGHT_BLUE);
				}else{
					sg.addTailAttribute("isImport", "true");
				}
				if (prop.as(OntProperty.class).isObjectProperty()) {
					data = addClassProperties(rng.as(OntClass.class), data, parentPublicUri);
				}
			}
		}
		return data;
	}
	
	
	
	
	private List<GraphSegment> addClassInstances(OntClass classInst, List<GraphSegment> data, List<String[]> ontologyResults, String publicUri){
		ExtendedIterator<? extends OntResource> itr = classInst.listInstances(true);
		try{
		while (itr.hasNext()) {
			OntResource or = itr.next();
			if (or.canAs(Individual.class)){
				GraphSegment gs = new GraphSegment(or.as(Individual.class), "is a", classInst, configMgr);
				if (!data.contains(gs)) {
					data.add(gs);
				}
				gs.addHeadAttribute(SHAPE, DIAMOND);
				gs.addHeadAttribute(FILL_COLOR, YELLOW);
				gs.addHeadAttribute(STYLE, FILLED);
				if(!isInImports(classInst, publicUri)){
					gs.addTailAttribute(FILL_COLOR, LIGHT_BLUE);
					gs.addTailAttribute(STYLE, FILLED);
				}else{
					gs.addTailAttribute("isImport", "true");
				}
			}
		}
		}catch(Exception e){
			//TODO handle error
		}
		
		return data;
	}

	private List<GraphSegment> addParentClasses(OntClass classInst, List<GraphSegment> data, List<String[]> ontologyResults, String publicUri) {
		try {
			ExtendedIterator<OntClass> eitr = classInst.listSuperClasses(true);
			while (eitr.hasNext()) {
				Resource scr = eitr.next();	
				if (scr != null && !scr.equals(classInst) && scr.canAs(OntClass.class)) {
					OntClass supercls = scr.as(OntClass.class);					
					GraphSegment sg;
					if (supercls.isRestriction()) {
						//sg = new GraphSegment(supercls, "restricts", classInst, configMgr);
						//sg.addHeadAttribute(COLOR, RED);
						//sg.addEdgeAttribute(COLOR, RED);
					}
					else {
						sg = new GraphSegment(supercls, "subClass", classInst, configMgr);

						if (!data.contains(sg)) {
							data.add(sg);
							
						}
						
						if(!isInImports(supercls, publicUri)){
							sg.addHeadAttribute(STYLE, FILLED);
							sg.addHeadAttribute(FILL_COLOR,LIGHT_BLUE);
						}else{
							sg.addHeadAttribute("isImport", "true");
						}
						if(!isInImports(classInst, publicUri)){
							sg.addTailAttribute(STYLE, FILLED);
							sg.addTailAttribute(FILL_COLOR,LIGHT_BLUE);
						}else{
							sg.addTailAttribute("isImport", "true");
						}
					}
					
					//data = addParentClasses(supercls,data);
				}
			}
		}
		catch (Throwable t) {
			
		}
		if (classInst.isUnionClass()) {
			data = addUnionClassData(classInst, data);
		}
		else if (classInst.isIntersectionClass()) {
			data = addIntersectionClassData(classInst, data);
		}
		return data;
	}
	
	private boolean isInImports(OntClass classInst, String parentPublicUri) throws ConfigurationException, IOException{
		Map<String,String> map = configMgr.getImports(parentPublicUri, Scope.LOCALONLY);
		if (map != null) {
			Set<String> keys = map.keySet();
			String[] uri = classInst.getURI().split("#");
			if(keys.contains(uri[0])){
				return true;
			}else{
				return false;
			}
		}else{
			return false;
		}
	}

	private List<GraphSegment> addIntersectionClassData(OntClass classInst, List<GraphSegment> data) {
		// each member class of an Intersection class is a super class of the Intersection class
		ExtendedIterator<? extends OntClass> einteritr = classInst.asIntersectionClass().listOperands();
		while (einteritr.hasNext()) {
			OntClass member  = einteritr.next();
			GraphSegment sg = new GraphSegment(member, "subClass", classInst, configMgr);
			if (!data.contains(sg)) {
				data.add(sg);
				//data = addParentClasses(member, data);
			}
		}
		return data;
	}

	private List<GraphSegment> addUnionClassData(OntClass classInst, List<GraphSegment> data) {
		// if all of the members of a Union class are subclasses of a common super class, then that common super class is a super class of the Union class
		ExtendedIterator<? extends OntClass> eunitr = classInst.asUnionClass().listOperands();
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
				GraphSegment sg = new GraphSegment(cscls, "subClass", classInst, configMgr);
				if (!data.contains(sg)) {
					data.add(sg);
				}
				//data = addParentClasses(cscls, data);
			}
		}
		return data;
	}

	protected OntModel getLocalModel() {
		return localModel;
	}
	
	protected OntModel getModelWithImports() {
		return withImportModel;
	}

	protected void setModels(OntModel imports, OntModel local) {
		this.withImportModel = imports;
		this.localModel = local;
	}
	
	public ResultSet convertDataToResultSet(List<GraphSegment> data) {
		boolean headImport = false;
		boolean tailImport = false;
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
					if(key=="isImport"){
						headImport = true;
					}else if (!headKeyList.contains(key)) {
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
					if(key=="isImport"){
						tailImport = true;
					}else if (!tailKeyList.contains(key)) {
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
			String s = headImport ? gs.subjectToString() : gs.subjectToStringNoPrefix();
			String p = gs.predicateToStringNoPrefix();
			String o = tailImport ? gs.objectToString() : gs.objectToStringNoPrefix();
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
	
	
}
