package com.ge.research.sadl.ui.visualize;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;

import com.ge.research.sadl.ui.handlers.SadlActionHandler;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.ResultSet;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.impl.OntClassImpl;
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
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDFS;

/**
 * @author Tyler Dicks
 *
 */
public class OntologyGraphGenerator {

	protected static String COLOR = "color";
	protected static String SHAPE = "shape";
	protected static String STYLE = "style";
	protected static String FILLED = "filled";
	protected static String FONTCOLOR = "fontcolor";
	protected static String FILL_COLOR = "fillcolor";
	protected static String BLUE = "blue";
	protected static String CLASS_BLUE = "blue4";
	protected static String INSTANCE_BLUE = "blue";
	protected static String WHITE = "white";
	protected static String BLACK = "black";
	protected static String OCTAGON = "octagon";
	protected static String DIAMOND = "diamond";
	protected static String RED = "red";
	protected static String PROPERTY_GREEN = "green3";
	protected static String IS_IMPORT = "isImport";
	protected static String LINK_URL = "href";
	

	private OntModel withImportModel = null; //model with imports
	private OntModel localModel = null;   //model without imports
	private IConfigurationManagerForIDE configMgr;
	private IProject project = null;
	
	public enum Orientation {TD, LR}
	
	/**
	 * Contructor for the OntologyGraphGenerator class
	 * 
	 * @param configMgr
	 * @param publicUri
	 * @param project
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	public OntologyGraphGenerator(IConfigurationManagerForIDE configMgr, String publicUri, IProject project) throws ConfigurationException, IOException {
		this.configMgr = configMgr;
		this.project  = project;
		setModels(configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS), configMgr.getOntModel(publicUri, Scope.LOCALONLY));
	}
	
	/**
	 * Method that generates the ontology graph RestultSet, which is fed in to the handler to turn into a 
	 * graphviz graph.
	 * 
	 * @param ontologyResults 	- List of data gathered from the ontology imports, (no longer used)
	 * @param publicUri 		- The uri of the file being graphed
	 * @return 					- The ResultSet containing all the classes, instances, and properties.  
	 */
	public ResultSet generateOntologyResultSet(List<String[]> ontologyResults, String publicUri){
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		boolean isImport = false;
		
		ExtendedIterator<OntClass> classIter = getLocalModel().listClasses();
		try{
			//Add all of the class triples
			while(classIter.hasNext()){
				Object obj = classIter.next();			
				if(obj instanceof OntClass || obj instanceof OntClassImpl){
					OntClass classInst = obj instanceof OntClass ? (OntClass)obj : (OntClassImpl)obj;
					//check to see if this is an imported class
					if(isInImports(classInst, publicUri)){
						isImport = true;
					}else{
						isImport = false;
					}
					if(!classInst.isRestriction() && !classInst.isUnionClass() && !classInst.isEnumeratedClass() && !classInst.isIntersectionClass()){
						if(classInst.hasProperty(RDFS.subClassOf, getLocalModel().getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI))){
							//if it's a list
							continue;	
						}
						
						
						//if not an import do the next three steps
						if(!isImport){
							//add parents to this node
							data = addParentClasses(classInst, data, ontologyResults, publicUri);
							//add instances of this node
							data = addClassInstances(classInst, data, ontologyResults, publicUri);
							//add properties of this node
							data = addClassProperties(classInst, data, publicUri);
							//add children of this node
							data = generateClassSubclasses(classInst, data, publicUri);
						}		
					}
				}

			}
			//check for any single classes (no parents, children, or properties)
			addSingleNodes(publicUri, data);
			
			//Add all instances that have an imported parent:
			addInstancesWithImportParent(publicUri, data); 
			
			
		}catch(Exception e){
			e.printStackTrace(System.err);
			System.err.println(e.getMessage());
		}
		
		ResultSet rs = convertDataToResultSet(data);

		return rs;
	}

	/**
	 * This method is used to add instances contained in the ontology file whose parent class
	 * is not defined in this ontology file. 
	 * 
	 * @param publicUri	- The uri of the ontology file being graphed
	 * @param data		- The GraphSegment list containing all the ontology graph data up to this point
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	private void addInstancesWithImportParent(String publicUri, List<GraphSegment> data)
			throws ConfigurationException, IOException {
		ExtendedIterator<Individual> individualIter = getLocalModel().listIndividuals();
		//Add all instances that have an imported parent:
		while(individualIter.hasNext()){
			Individual inst = individualIter.next();
			if(!isInImports(inst, publicUri)){
				OntClass parent = inst.getOntClass();
				GraphSegment gs = new GraphSegment((Resource)inst, "is a",parent , configMgr);
				gs.addHeadAttribute(SHAPE, DIAMOND);
				if(isInImports(parent, publicUri)){
					if(getImportUrl(parent) != null) gs.addTailAttribute(LINK_URL, getImportUrl(parent));
					gs.addTailAttribute(IS_IMPORT, "true");
				}else{
					gs.addTailAttribute(STYLE, FILLED);
					gs.addTailAttribute(FILL_COLOR, INSTANCE_BLUE);
					gs.addTailAttribute(FONTCOLOR, WHITE);
				}
				gs.addHeadAttribute(STYLE, FILLED);
				gs.addHeadAttribute(FILL_COLOR, INSTANCE_BLUE);
				gs.addHeadAttribute(FONTCOLOR, WHITE);
				if (!data.contains(gs)) {
					data.add(gs);
				}
				//add properties of the instance
				//data = addInstanceProperties(inst, data, publicUri);
			}
			
		}
	}

	/**
	 * Method used to add single class nodes that don't have any child or parent nodes
	 * 
	 * @param publicUri	- URI of ontology file being graphed
 	 * @param data		- GraphSegment list containing all graph data up to this point
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	private void addSingleNodes(String publicUri, List<GraphSegment> data) throws ConfigurationException, IOException {
		ExtendedIterator<OntClass> classIter2 = getLocalModel().listClasses();
		while(classIter2.hasNext()){
			Object obj2 = classIter2.next();			
			if(obj2 instanceof OntClass){
				OntClass classInst = (OntClass)obj2;
				if(!isInImports(classInst, publicUri)){
					if(!classInst.isComplementClass() && !classInst.isRestriction() && 
							!classInst.isUnionClass() && !classInst.isEnumeratedClass() && 
							!classInst.isIntersectionClass()){
						//add a single node for the class instance
						if(classInst.hasProperty(RDFS.subClassOf, getLocalModel().getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI))){
							//if it's a list
							continue;	
						}
						
						GraphSegment gs = new GraphSegment(classInst, null, null, configMgr);
						if (!hasClassNode(data, classInst)) {
							data.add(gs);
							gs.addHeadAttribute(FILL_COLOR, CLASS_BLUE);
							gs.addHeadAttribute(STYLE, FILLED);
							gs.addHeadAttribute(FONTCOLOR, WHITE);
						}
					}
				}	
			}
		}
	}

	/**
	 * Method used to find the File URL of the graph associated with an imported class
	 * 
	 * @param	- Imported class or concept
	 * @return	- File URL to be added to node hyperlink
	 */
	private String getImportUrl(OntResource rsrc) {
		if (!rsrc.isURIResource()) {
			//int i = 0;
			return rsrc.toString();
		}
		String ns = rsrc.getNameSpace();
		if (ns.endsWith("#")) {
			ns = ns.substring(0, ns.length() - 1);
		}
		// get the prefix and if there is one generate qname
		String prefix = configMgr.getGlobalPrefix(ns);
		//get the graph folder file path
		String tempDir = SadlActionHandler.convertProjectRelativePathToAbsolutePath(project.getFullPath().append("Temp").append("Graphs").toPortableString()); 
		
		if(prefix!=null){
			return "\"file:///" + tempDir + "/" + prefix + "_ONT" + "Graph.dot.svg\"";
		}
		return null;
	}

//	private List<GraphSegment> addInstanceProperties(Individual inst, List<GraphSegment> data, String publicUri) throws ConfigurationException, IOException {
//		OntClass parent = inst.getOntClass();
//		StmtIterator sitr = getLocalModel().listStatements(null, RDFS.domain, parent);
//		while (sitr.hasNext()) {
//			Statement stmt = sitr.nextStatement();
//			Resource prop = stmt.getSubject();
//			String name = prop.getLocalName();
//			RDFNode obj = stmt.getObject();
//			String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
//			qstr += "select ?prop where {"
//					+ "?iface <rdf:ID>  <" + inst.getURI() + "> ."
//							+ "}";
//			QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getLocalModel());;		
//			com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
//			
//			if (prop.canAs(OntProperty.class)) {
//				if(parent.hasProperty((OntProperty)prop)){
//					data = generateInstancePropertyRange(inst, prop, data, publicUri);
//				}
//			}
//		}
//		// now look for unions? or intersections containing the cls
//		String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
//		qstr += "select ?prop where {?prop rdfs:domain/(owl:unionOf/rdf:rest*/rdf:first)? <" + inst.getURI() + ">}";
//		QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getLocalModel());;		
//		com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
//		while (results.hasNext()) {
//			QuerySolution soln = results.next();
//			RDFNode prop = soln.get("?prop");
//			if (prop.canAs(OntProperty.class)) {
//				data = generateInstancePropertyRange(inst, prop.as(OntProperty.class), data, publicUri);
//			}
//		}
//		return data;
//	}
//
//	private List<GraphSegment> generateInstancePropertyRange(Individual inst, Resource prop, List<GraphSegment> data,
//			String publicUri) throws ConfigurationException, IOException {
//		boolean isList = false;
//		Statement stmt = prop.getProperty(getLocalModel().getAnnotationProperty(SadlConstants.LIST_RANGE_ANNOTATION_PROPERTY));
//		if (stmt != null) {
//			RDFNode obj = stmt.getObject();
//			if (obj.isLiteral()) {
//				Object lit = obj.asLiteral().getValue();
//				if (lit != null && lit.toString().equals("LIST")) {
//					isList = true;
//				}
//			}
//		}
//		ExtendedIterator<? extends OntResource> eitr = prop.as(OntProperty.class).listRange();
//		while (eitr.hasNext()) {
//			//get range of prop
//			OntResource rng = eitr.next();
//			//if it's a list and range class
//			if (isList && rng.canAs(OntClass.class)) {
//				//get list class
//				//Check for an all values from restriction
//				Resource listClass = getLocalModel().getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
//				//if list class exists or the range has a superclass that is a list
//				if (listClass == null || rng.as(OntClass.class).hasSuperClass(listClass)) {
//					//iterate across all of the statements that are subclasses of range?
//					StmtIterator stmtitr = getLocalModel().listStatements(rng, RDFS.subClassOf, (RDFNode)null);
//					while (stmtitr.hasNext()) {
////					ExtendedIterator<OntClass> scitr = rng.as(OntClass.class).listSuperClasses(true);
////					while (scitr.hasNext()) {
//						Statement supclsstmt = stmtitr.nextStatement();
//						RDFNode supclsnode = supclsstmt.getObject();
//						//if the superclass is a class
//						if (supclsnode.canAs(OntClass.class)){
//							//get the subclass
//							OntClass subcls = supclsnode.as(OntClass.class);  // scitr.next();
//							if (subcls.hasProperty(OWL.onProperty, getLocalModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
//								Statement avf = subcls.getProperty(OWL.allValuesFrom);
//								if (avf != null) {
//									RDFNode listtype = avf.getObject();
//									if (listtype.canAs(OntResource.class)){
//										rng = listtype.as(OntResource.class);
//									}
//								}
//								stmtitr.close();
//								break;
//							}
//						}
//					}
//				}
//			}
//			GraphSegment sg = isList ? new GraphSegment(inst, prop, rng, isList, configMgr) : new GraphSegment(inst, prop, rng, configMgr);
//			if (!data.contains(sg)) {
//				data.add(sg);
//				sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
//				//String s = sg.restrictionToString(cls.as(OntClass.class));
//				//sg.addEdgeAttribute("labeltooltip", s);
//				if(!isInImports(rng, publicUri)){
//					sg.addTailAttribute(STYLE, FILLED);
//					sg.addTailAttribute(FILL_COLOR, CLASS_BLUE);
//					sg.addTailAttribute(FONTCOLOR, WHITE);
//				}else{
//					if(getImportUrl(rng) != null) sg.addTailAttribute(LINK_URL, getImportUrl(rng));
//					sg.addTailAttribute(IS_IMPORT, "true");
//				}
//				
//				if(!isInImports(inst, publicUri)){
//					sg.addHeadAttribute(STYLE, FILLED);
//					sg.addHeadAttribute(FILL_COLOR, CLASS_BLUE);
//					sg.addHeadAttribute(FONTCOLOR, WHITE);
//				}else{
//					if(getImportUrl(inst) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(inst));
//					sg.addHeadAttribute(IS_IMPORT, "true");
//				}
//
//				if (prop.as(OntProperty.class).isObjectProperty()) {
//					//data = addClassProperties(rng.as(OntClass.class), data, parentPublicUri);
//				}
//			}
//		}
//		return data;
//	}

	/**
	 * Method used to check if a node exists in the graph segment list
	 * 
	 * @param gsList	- Graph segment List
	 * @param oclass	- The class being searched for
	 * @return
	 */
	private boolean hasClassNode(List<GraphSegment> gsList, OntClass oclass){
		Iterator<GraphSegment> itr = gsList.iterator();
		while(itr.hasNext()){
			GraphSegment gs = itr.next();
			if(gs.subject == oclass || gs.object == oclass){
				return true;
			}
		}
		return false;
	}

	

	/**
	 * Method called to add properties of a class to the ontology graph
	 * 
	 * @param cls				- The class whose properties are being added
	 * @param data				- The list of GraphSegments containing graph data added to this point
	 * @param parentPublicUri	- The URI of the ontology file being graphed
	 * @return					- The list of GraphSegments with the property edges and nodes added
	 * @throws Exception 
	 */
	private List<GraphSegment> addClassProperties(OntClass cls,
			List<GraphSegment> data, String parentPublicUri) throws Exception {
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

	/**
	 * Method called in addClassProperties to generate the range values added to properties
	 * 
	 * @param cls				- Class whose properties are being added
	 * @param prop				- Property that is being added
	 * @param data				- List of GraphSegment containing ontology graph data
	 * @param parentPublicUri	- URI of ontology file being graphed
	 * @return					- List of Graph Segments with Range nodes added
	 * @throws Exception 
	 */
	private List<GraphSegment> generatePropertyRange(OntClass cls,
			Resource prop, List<GraphSegment> data, String parentPublicUri) throws Exception {
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
			//-------------------------check for union classes--------------------------
			
			try{
			if(rng.canAs(OntClass.class) && rng.as(OntClass.class).isUnionClass()) {
				
				//get list of union classes
				List<OntClass> unionclasses = getUnionClasses(rng);
				Iterator<OntClass> clsiter = unionclasses.iterator();
				while(clsiter.hasNext()){
					OntClass newcls = clsiter.next();
					GraphSegment sg = isList ? new GraphSegment(cls, prop, newcls, isList, configMgr) : new GraphSegment(cls, prop, newcls, configMgr);
					
					if (!data.contains(sg)) {	
						
						data.add(sg);
						sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
						//boolean test = cls.asRestriction().isAllValuesFromRestriction();
						//String s = sg.restrictionToString(cls.as(OntClass.class));
						//sg.addEdgeAttribute("labeltooltip", s);
						if(!isInImports(newcls, parentPublicUri)){
							sg.addTailAttribute(STYLE, FILLED);
							sg.addTailAttribute(FILL_COLOR, CLASS_BLUE);
							sg.addTailAttribute(FONTCOLOR, WHITE);
						}else{
							if(getImportUrl(newcls) != null) sg.addTailAttribute(LINK_URL, getImportUrl(newcls));
							sg.addTailAttribute(IS_IMPORT, "true");
						}
						
						ExtendedIterator<OntClass> iter = cls.listSuperClasses(true);
						StringBuilder rstrString = new StringBuilder();
						while(iter.hasNext()){
							OntClass superClass = iter.next();
							if(superClass.isRestriction()){
								OntProperty restrictionProperty = superClass.asRestriction().getOnProperty();
								if(restrictionProperty.equals(prop)){
									if(prop.canAs(Property.class)){
										rstrString.append(getRestrictionString(superClass.asRestriction(),prop.as(Property.class),newcls, isList));
									}else{
										throw new Exception("prop is not a property");
									}
								}
							}
						}
						if(rstrString.length() > 0){	
							sg.addEdgeAttribute("URL",getCurrentFileLink(parentPublicUri));
							sg.addEdgeAttribute(FONTCOLOR, RED);
							String str = "\"" + rstrString.toString() + "\"";
							sg.addEdgeAttribute("labeltooltip", str);
						}
						
						if(!isInImports(cls, parentPublicUri)){
							sg.addHeadAttribute(STYLE, FILLED);
							sg.addHeadAttribute(FILL_COLOR, CLASS_BLUE);
							sg.addHeadAttribute(FONTCOLOR, WHITE);
						}else{
							sg.addHeadAttribute(IS_IMPORT, "true");
							if(getImportUrl(cls) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(cls));
						}
					}
					
					
					
				}	
			}else{
				GraphSegment sg = isList ? new GraphSegment(cls, prop, rng, isList, configMgr) : new GraphSegment(cls, prop, rng, configMgr);
				if (!data.contains(sg)) {	
					
					data.add(sg);
					sg.addEdgeAttribute(COLOR, PROPERTY_GREEN);
					//boolean test = cls.asRestriction().isAllValuesFromRestriction();
					//String s = sg.restrictionToString(cls.as(OntClass.class));
					//sg.addEdgeAttribute("labeltooltip", s);
					if(!isInImports(rng, parentPublicUri)){
						sg.addTailAttribute(STYLE, FILLED);
						sg.addTailAttribute(FILL_COLOR, CLASS_BLUE);
						sg.addTailAttribute(FONTCOLOR, WHITE);
					}else{
						if(getImportUrl(rng) != null) sg.addTailAttribute(LINK_URL, getImportUrl(rng));
						sg.addTailAttribute(IS_IMPORT, "true");
					}
					
					ExtendedIterator<OntClass> iter = cls.listSuperClasses(true);
					StringBuilder rstrString = new StringBuilder();
					while(iter.hasNext()){
						OntClass superClass = iter.next();
						if(superClass.isRestriction()){
							OntProperty restrictionProperty = superClass.asRestriction().getOnProperty();
							if(restrictionProperty.equals(prop)){
								if(prop.canAs(Property.class)){
									rstrString.append(getRestrictionString(superClass.asRestriction(),prop.as(Property.class),rng, isList));
								}else{
									throw new Exception("prop is not a property");
								}
							}
						}
					}
					if(rstrString.length() > 0){	
						sg.addEdgeAttribute("URL",getCurrentFileLink(parentPublicUri));
						sg.addEdgeAttribute(FONTCOLOR, RED);
						String str = "\"" + rstrString.toString() + "\"";
						sg.addEdgeAttribute("labeltooltip", str);
					}
					
					if(!isInImports(cls, parentPublicUri)){
						sg.addHeadAttribute(STYLE, FILLED);
						sg.addHeadAttribute(FILL_COLOR, CLASS_BLUE);
						sg.addHeadAttribute(FONTCOLOR, WHITE);
					}else{
						sg.addHeadAttribute(IS_IMPORT, "true");
						if(getImportUrl(cls) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(cls));
					}
	
					if (prop.as(OntProperty.class).isObjectProperty()) {
						//data = addClassProperties(rng.as(OntClass.class), data, parentPublicUri);
					}
				}
			}
			}catch(Exception e){
				e.printStackTrace(System.err);
			}
		}
		return data;
	}
	
	
	private List<OntClass> getUnionClasses(OntResource rng) {
		List<OntClass> classList = new ArrayList<OntClass>();
		ExtendedIterator<? extends OntClass> eunitr = ((OntClass)rng).asUnionClass().listOperands();
		while(eunitr.hasNext()){
			OntClass newcls = eunitr.next();
			if(newcls.canAs(OntClass.class) && ((OntClass)newcls).isUnionClass()){
				classList = getUnionClasses(newcls);
			}else{
				classList.add(newcls);
			}
		}
		return classList;
	}

	private String getCurrentFileLink(String parentUri) throws Exception{
		String[] splitFile = parentUri.split("/");
		String filename = splitFile[splitFile.length-1];
		
		// get the prefix and if there is one generate qname
		String tempDir = SadlActionHandler.convertProjectRelativePathToAbsolutePath(project.getFullPath().append("Temp").append("Graphs").toPortableString()); 
		
		if(filename!=null){
			return "\"file:///" + tempDir + "/" + filename + "_ONTGraph.dot.svg\"";
		}
		throw new Exception("Cannot find graph file in getCurrentFileLink()");
	}
//	if(superClass.asRestriction().isCardinalityRestriction() && (superClass.asRestriction().getProperty(OWL2.onClass) != null)){
//		Statement var = superClass.asRestriction().getProperty(OWL2.onClass);
//		RDFNode obj = var.getObject();
//		if(obj.equals(rng)){
//			if(prop.canAs(Property.class)){
//				String rst = getRestrictionString(superClass.asRestriction(),prop.as(Property.class));
//				
//				//rstrString.append();
//			}else{
//				throw new Exception("prop is not a property");
//			}
//		}
//	}
	
	private String getRestrictionString(Restriction rstr, Property prop, OntResource rng, boolean isList) throws Exception{
		
		StringBuilder sb = new StringBuilder();
		if(rstr.isCardinalityRestriction() || rstr.isMinCardinalityRestriction() || rstr.isMaxCardinalityRestriction()){
			if(rstr.isCardinalityRestriction()){
				sb.append("Must have exactly ");
				int num = rstr.asCardinalityRestriction().getCardinality();
				sb.append(num);
				if(num == 1){
					sb.append(" value");
				} else{
					sb.append(" values");
				}

			}else if(rstr.isMinCardinalityRestriction()){
				sb.append("Must have at least ");
				int num = rstr.asMinCardinalityRestriction().getMinCardinality();
				sb.append(num);
				if(num == 1){
					sb.append(" value");
				} else{
					sb.append(" values");
				}
			}else if(rstr.isMaxCardinalityRestriction()){
				sb.append("Must have at most ");
				int num = rstr.asMaxCardinalityRestriction().getMaxCardinality();
				sb.append(num);
				if(num == 1){
					sb.append(" value");
				} else{
					sb.append(" values");
				}
			}
			if((rstr.getProperty(OWL2.onClass) != null)){
				Statement var = rstr.getProperty(OWL2.onClass);
				RDFNode obj = var.getObject();
				if(obj.equals(rng)){
					if(prop.canAs(Property.class)){
						sb.append(" of this type.");
						//rstrString.append();
					}else{
						throw new Exception("prop is not a property");
					}
				}
			}else{
				sb.append(". ");
			}
			
		}else if(rstr.isHasValueRestriction()){
			sb.append("Must have value ");
			RDFNode node = rstr.asHasValueRestriction().getHasValue();
			//TODO may need to change this toString
			sb.append(node.toString());
			sb.append(isList ? " list" : "");
			sb.append(". ");
			
		}else if(rstr.isSomeValuesFromRestriction()){
			sb.append("Must have some value of type ");
			Resource node = rstr.asSomeValuesFromRestriction().getSomeValuesFrom();
			//TODO may need to change this toString
			sb.append(node.toString());
			sb.append(isList ? " list" : "");
			sb.append(". ");
			
		}else if(rstr.isAllValuesFromRestriction()){
			sb.append("Can only have values of type ");
			Resource node = rstr.asAllValuesFromRestriction().getAllValuesFrom();
			//TODO may need to change this toString
			sb.append(node.toString());
			sb.append(isList ? " list" : "");
			sb.append(". ");
			
		}else{
			throw new Exception("no restriction type found");
		}
		return sb.toString();
	}
	
	
	/**
	 * Method used to add class instances to the ontology graph
	 * 
	 * @param classInst			- Class whose instances will be added
	 * @param data				- List of GraphSegments containing current graph data
	 * @param ontologyResults	- No longer used
	 * @param publicUri			- URI of ontology file being graphed
	 * @return
	 */
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
					gs.addHeadAttribute(FILL_COLOR, INSTANCE_BLUE);
					gs.addHeadAttribute(STYLE, FILLED);
					gs.addHeadAttribute(FONTCOLOR, WHITE);
					if(!isInImports(classInst, publicUri)){
						gs.addTailAttribute(FILL_COLOR, CLASS_BLUE);
						gs.addTailAttribute(STYLE, FILLED);
						gs.addTailAttribute(FONTCOLOR, WHITE);
					}else{
						if(getImportUrl(classInst) != null) gs.addTailAttribute(LINK_URL, getImportUrl(classInst));
						gs.addTailAttribute(IS_IMPORT, "true");
					}
				}
			}
		}catch(Exception e){
			System.err.println(e.getMessage());
		}
		
		return data;
	}

	/**
	 * Adds parent classes of classInst to the ontology graph
	 * 
	 * @param classInst			- Class whose parents are being added to graph
	 * @param data				- List of GraphSegments containing current graph data
	 * @param ontologyResults	- No longer used
	 * @param publicUri			- URI of ontology file being graphed
	 * @return
	 */
	private List<GraphSegment> addParentClasses(OntClass classInst, List<GraphSegment> data, List<String[]> ontologyResults, String publicUri) {
		try {
			boolean hasParents = false;
			ExtendedIterator<OntClass> eitr = classInst.listSuperClasses(true);
			while (eitr.hasNext()) {
				hasParents = true;
				Resource scr = eitr.next();	
				if (scr != null && !scr.equals(classInst) && scr.canAs(OntClass.class)) {
					OntClass supercls = scr.as(OntClass.class);					
					GraphSegment sg;
					if (supercls.isRestriction()) {
//						sg = new GraphSegment(supercls, "restricts", classInst, configMgr);
//						sg.addHeadAttribute(COLOR, RED);
//						sg.addEdgeAttribute(COLOR, RED);
					}
					else {
						sg = new GraphSegment(supercls, "subClass", classInst, configMgr);
						sg.addEdgeAttribute(COLOR, BLUE);
						sg.addEdgeAttribute(STYLE, "dashed");
						if (!data.contains(sg)) {
							data.add(sg);
						}
						
						if(!isInImports(supercls, publicUri)){
							sg.addHeadAttribute(STYLE, FILLED);
							sg.addHeadAttribute(FILL_COLOR,CLASS_BLUE);
							sg.addHeadAttribute(FONTCOLOR, WHITE);
						}else{
							if(getImportUrl(supercls) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(supercls));
							sg.addHeadAttribute(IS_IMPORT, "true");
						}
						if(!isInImports(classInst, publicUri)){
							sg.addTailAttribute(STYLE, FILLED);
							sg.addTailAttribute(FILL_COLOR,CLASS_BLUE);
							sg.addTailAttribute(FONTCOLOR, WHITE);
						}else{
							if(getImportUrl(classInst) != null) sg.addTailAttribute(LINK_URL, getImportUrl(classInst));
							sg.addTailAttribute(IS_IMPORT, "true");
						}
					}
					
					//data = addParentClasses(supercls,data,ontologyResults,publicUri);
				}
			}
			if(!hasParents){
				
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
	
	/**
	 * Adds subclasses of input class to the ontology graph
	 * 
	 * @param cls		- The class whose subclasses are being added to graph
	 * @param data		- List of GraphSegments containing current graph data
	 * @param publicUri	- URI of ontology file being graphed
	 * @return
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	private List<GraphSegment> generateClassSubclasses(OntClass cls, List<GraphSegment> data, String publicUri) throws ConfigurationException, IOException {
		ExtendedIterator<OntClass> eitr = cls.listSubClasses(true);
		while (eitr.hasNext()) {
			OntClass subcls = eitr.next();
			GraphSegment sg;
			if (subcls.isRestriction() || cls.isRestriction()) {
//				sg = new GraphSegment(cls, "restricts", subcls, configMgr);
//				sg.addHeadAttribute(COLOR, RED);
//				sg.addEdgeAttribute(COLOR, RED);
			}
			else {
				sg = new GraphSegment(cls, "subClass", subcls, configMgr);
				sg.addEdgeAttribute(COLOR, BLUE);
				sg.addEdgeAttribute(STYLE, "dashed");
				
				if(!isInImports(cls, publicUri)){
					sg.addHeadAttribute(STYLE, FILLED);
					sg.addHeadAttribute(FILL_COLOR,CLASS_BLUE);
					sg.addHeadAttribute(FONTCOLOR, WHITE);
				}else{
					if(getImportUrl(cls) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(cls));
					sg.addHeadAttribute(IS_IMPORT, "true");
				}
				if(!isInImports(subcls, publicUri)){
					sg.addTailAttribute(STYLE, FILLED);
					sg.addTailAttribute(FILL_COLOR,CLASS_BLUE);
					sg.addTailAttribute(FONTCOLOR, WHITE);
				}else{
					if(getImportUrl(subcls) != null) sg.addTailAttribute(LINK_URL, getImportUrl(subcls));
					sg.addTailAttribute(IS_IMPORT, "true");
				}
				if (!data.contains(sg)) {
					data.add(sg);
					//data = generateClassSubclasses(subcls, data, publicUri);
				}
			}
			
			
		}
		
		// if this class is part of an intersection, then the superclass of the intersection is a subclass of this class
		if (cls.isURIResource()) {
			String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
	//		qstr += "select ?subclass ?superclass where {?subclass rdfs:subClassOf/(owl:intersectionOf/rdf:rest*/rdf:first)? ?superclass}";
			qstr += "select ?subclass ?superclass where {?subclass rdfs:subClassOf/(owl:intersectionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
			QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), getModelWithImports());;		
			com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
			while (results.hasNext()) {
				QuerySolution soln = results.next();
				RDFNode sub = soln.get("?subclass");
				if (sub.canAs(OntClass.class)) {
					OntClass subcls = sub.as(OntClass.class);
					GraphSegment sg = new GraphSegment(cls, "subClass", subcls, configMgr);
					sg.addEdgeAttribute(COLOR, BLUE);
					sg.addEdgeAttribute(STYLE, "dashed");
					
					if(!isInImports(cls, publicUri)){
						sg.addHeadAttribute(STYLE, FILLED);
						sg.addHeadAttribute(FILL_COLOR,CLASS_BLUE);
						sg.addHeadAttribute(FONTCOLOR, WHITE);
					}else{
						if(getImportUrl(cls) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(cls));
						sg.addHeadAttribute(IS_IMPORT, "true");
					}
					if(!isInImports(subcls, publicUri)){
						sg.addTailAttribute(STYLE, FILLED);
						sg.addTailAttribute(FILL_COLOR,CLASS_BLUE);
						sg.addTailAttribute(FONTCOLOR, WHITE);
					}else{
						if(getImportUrl(subcls) != null) sg.addTailAttribute(LINK_URL, getImportUrl(subcls));
						sg.addTailAttribute(IS_IMPORT, "true");
					}
					if (!data.contains(sg)) {
						data.add(sg);
						//data = generateClassSubclasses(subcls, data, publicUri);
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
	
	
	/**
	 * Method used to see if a class/instance/etc. is an imported item 
	 * (i.e) not defined in this file/namespace
	 * 
	 * @param classInst			- Item being checked against imports 
	 * @param parentPublicUri	- URI of file being graphed
	 * @return
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	private boolean isInImports(RDFNode classInst, String parentPublicUri) throws ConfigurationException, IOException{
		try{
			if(classInst.isURIResource()){
				String[] uri = classInst.asResource().getURI().split("#");
				if(uri != null && uri[0].equals(parentPublicUri)){
					return false;
				}else{
					return true;
				}
			}else{
				if(getLocalModel().getBaseModel().containsResource(classInst)){
					return false;
					
				}else{
					return true;
				}
			}
		}catch(NullPointerException e){
			return false;
			//TODO fix issues with OnClassImpl throwing null exception
		}
		
		
		//		Map<String,String> map = configMgr.getImports(parentPublicUri, Scope.INCLUDEIMPORTS);
//		if (map != null) {
//			Set<String> keys = map.keySet();
//			String[] uri = null;
//			try{
//				if(classInst.canAs(OntClassImpl.class)){
//					uri = ((OntClassImpl)classInst).getURI().split("#") ;
//				}else if(classInst.canAs(OntResourceImpl.class)){
//					uri = ((OntResourceImpl)classInst).getURI().split("#");
//				}else if(classInst.canAs(OntClass.class)){
//					uri = ((OntClass)classInst).getURI().split("#");
//				}else{
//					//uri = classInst.getURI().split("#");
//				}
//			
//			
//			if(uri != null && keys.contains(uri[0])){
//				return true;
//			}else{
//				return false;
//			}
//		}else{
//			return false;
//		}
	}

	/**
	 * Adds class data to graph if it's an intersection class
	 * 
	 * @param classInst
	 * @param data
	 * @return
	 */
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

	/**
	 * Adds class data to graph if it's a union class
	 * 
	 * @param classInst
	 * @param data
	 * @return
	 */
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

	/**
	 * @return
	 */
	protected OntModel getLocalModel() {
		return localModel;
	}
	
	/**
	 * @return
	 */
	protected OntModel getModelWithImports() {
		return withImportModel;
	}

	/**
	 * @param imports
	 * @param local
	 */
	protected void setModels(OntModel imports, OntModel local) {
		this.withImportModel = imports;
		this.localModel = local;
	}
	
	/**
	 * @param data
	 * @return
	 */
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
			String s = isAnImport(gs, true) ? gs.subjectToString() : gs.subjectToStringNoPrefix();
			String p = gs.predicateToStringNoPrefix();
			String o = isAnImport(gs, false) ? gs.objectToString() : gs.objectToStringNoPrefix();
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

	/**
	 * @param graphPart
	 * @param map
	 * @param columnList
	 * @param array
	 * @param i
	 * @param gs
	 * @return
	 */
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
	
	/**
	 * Used to see if a node in a GraphSegment is marked as an import
	 * through the IS_IMPORT attribute.
	 * 
	 * @param gs
	 * @param isHead
	 * @return
	 */
	private boolean isAnImport(GraphSegment gs, boolean isHead){
		Map<String, String> attr = isHead ? gs.getHeadAttributes() : gs.getTailAttributes();
		if(attr != null && attr.containsKey(IS_IMPORT)){
			return true;
		}else{
			return false;
		}
	}
	
	
}
