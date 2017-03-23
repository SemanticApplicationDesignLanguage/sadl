package com.ge.research.sadl.ui.visualize;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;

import com.ge.research.sadl.ui.handlers.SadlActionHandler;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.EnumeratedClass;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

/**
 * @author Tyler Dicks
 *
 */
public class OntologyGraphGenerator extends GraphGenerator {

	
	
	
//	public enum Orientation {TD, LR, BD, RL}
	
	/**
	 * Constructor for the OntologyGraphGenerator class for use without a particular input model
	 * 
	 * @param configMgr
	 * @param publicUri
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	public OntologyGraphGenerator(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, IProject project) throws ConfigurationException, IOException {
		super(configMgr, visualizer, project, null, null);
	}
	
	/**
	 * Constructor for the OntologyGraphGenerator class with ProgressMonitor for use without a particular input model
	 * @param configMgr
	 * @param visualizer
	 * @param project
	 * @param monitor
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	public OntologyGraphGenerator(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, IProject project, IProgressMonitor monitor) throws ConfigurationException, IOException {
		super(configMgr, visualizer, project, null, null, monitor);
	}
	
	/**
	 * Constructor for the OntologyGraphGenerator class for use with a particular input model identified by publicUri
	 * 
	 * @param configMgr
	 * @param publicUri
	 * @param project
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	public OntologyGraphGenerator(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, IProject project, String publicUri) throws ConfigurationException, IOException {
		super(configMgr, visualizer, project, publicUri, null);
		this.setConfigMgr(configMgr);
	}
	
	/**
	 * Constructor for the OntologyGraphGenerator class with ProgressMonitor for use with a particular input model identified by publicUri
	 * @param configMgr
	 * @param visualizer
	 * @param project
	 * @param monitor
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	public OntologyGraphGenerator(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, IProject project, String publicUri, IProgressMonitor monitor) throws ConfigurationException, IOException {
		super(configMgr, visualizer, project, publicUri, null, monitor);
		this.setConfigMgr(configMgr);
	}

	/**
	 * Method that generates the ontology graph ResultSet, which is fed in to the handler to turn into a 
	 * graphviz graph.
	 * 
	 * @param ontologyResults 	- List of data gathered from the ontology imports, (no longer used)
	 * @param publicUri 		- The uri of the file being graphed
	 * @return 					- The ResultSet containing all the classes, instances, and properties.  
	 * @throws InvalidNameException 
	 */
	public ResultSet generateOntologyResultSet(List<String[]> ontologyResults, String publicUri, UriStrategy uriStrategy) throws InvalidNameException{
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		try {
			addByStatements(publicUri, data);
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ResultSet rs = convertDataToResultSet(data, uriStrategy, publicUri);
		return rs;
	}

	private void addByStatements(String publicUri, List<GraphSegment> data) throws ConfigurationException, IOException, URISyntaxException, Exception {
		StmtIterator stmtitr = getLocalModel().listStatements();
		Map<Resource, RDFNode> propertyDomains = null;
		Map<Resource, RDFNode> propertyRanges = null;
		Map<Resource, RDFNode> listTypes = null;
		while (stmtitr.hasNext()) {
			Statement stmt = stmtitr.nextStatement();
			if (!getTheJenaModel().getBaseModel().contains(stmt)) {
				continue;
			}
			Property p = stmt.getPredicate();
			Resource subj = stmt.getSubject();
			RDFNode obj = stmt.getObject();
			if (p.equals(OWL.equivalentClass)) {
				addEquivalentClass(publicUri, subj, obj, data);
			}
			else if (p.equals(OWL.sameAs)) {
				addSameAsClass(publicUri, subj, obj, data);
			}
			else if (p.equals(RDFS.comment) || p.equals(RDF.rest) || p.equals(RDF.first) || 
					p.equals(getListFirstProp()) || p.equals(getListRestProp()) || p.getNameSpace().equals(OWL.NAMESPACE.getURI()) ||
					(p.isURIResource() && p.getURI().equals(SadlConstants.SADL_LIST_MODEL_RANGE_ANNOTATION_PROPERTY))) {
				continue;
			}
			else if (subj.isURIResource() && (subj.getNameSpace().equals(SadlConstants.SADL_BASE_MODEL_URI + "#") || 
					subj.getNameSpace().equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI + "#") || 
					subj.getNameSpace().equals(SadlConstants.SADL_IMPLICIT_MODEL_URI + "#"))) {
				continue;
			}
			else if (obj.isURIResource() && (obj.asResource().equals(OWL.Class) || obj.asResource().equals(OWL.Ontology)
					|| obj.asResource().equals(OWL.Restriction) || obj.asResource().equals(OWL.ObjectProperty) ||
					obj.asResource().equals(OWL.DatatypeProperty) || obj.asResource().equals(OWL.AnnotationProperty) ||
					obj.asResource().equals(RDF.Property))) {
				continue;
			}
			else if (p.equals(RDF.type)) {
				if (subj.canAs(Individual.class)){ 
					addInstanceIsAClassToGraph(publicUri, subj.as(Individual.class), obj, data);
				}
			}
			else if (p.equals(RDFS.subClassOf)) {
				if (subj.canAs(OntClass.class)){ 
					OntClass classInst = subj.as(OntClass.class);
					if (obj.canAs(OntClass.class) && !obj.as(OntClass.class).equals(getListClass())) { 
						addIsATypeOfToGraph(publicUri, classInst, obj.as(OntClass.class), data);
					}
				}
			}
			else if (p.equals(RDFS.domain)) {
				if (propertyDomains == null) propertyDomains = new HashMap<Resource, RDFNode>();
				propertyDomains.put(stmt.getSubject(), stmt.getObject());
			}
			else if (p.equals(RDFS.range)){
				if (propertyRanges == null) propertyRanges = new HashMap<Resource, RDFNode>();
				RDFNode rng = stmt.getObject();
				RDFNode listtype = null;
				boolean isList = isPropertyAnnotatedAsListRange(subj) || (rng.canAs(OntClass.class)&&  rng.as(OntClass.class).hasSuperClass(getListClass()));
				//if it's a list and range class
				if (isList) {
					//Check for an all values from restriction
					//if list class exists or the range has a superclass that is a list
					if (rng.canAs(OntClass.class)) {
						//iterate across all of the statements that are subclasses of range?
						listtype = getListType(rng.as(OntClass.class));
						if (listtype != null) {
							if (listTypes  == null) listTypes = new HashMap<Resource, RDFNode>();
							listTypes.put(rng.as(OntClass.class), listtype);
						}
					}
				}
				propertyRanges.put(stmt.getSubject(), rng);
			}
			else {
				if (subj.canAs(Individual.class)){
					addInstancePropertyToGraph(publicUri, subj.as(Individual.class), p, obj, data);				}
			}
		}
		if (propertyDomains != null && propertyRanges != null) {
			// what if we only have a domain or a range, not both? hard to display as graph edge...
			Iterator<Resource> domainitr = propertyDomains.keySet().iterator();
			while (domainitr.hasNext()) {
				Resource prop = domainitr.next();
				if (propertyRanges.containsKey(prop)) {
					RDFNode domain = propertyDomains.get(prop);
					RDFNode range = propertyRanges.get(prop);
					GraphSegment sg = null;
					if (range.canAs(OntClass.class) && listTypes != null) {
						if (listTypes.containsKey(range.as(OntClass.class))) {
							RDFNode lst = addTypeListToGraphData(data, publicUri, range.as(OntResource.class), listTypes.get(range.as(OntClass.class)));
							sg = new GraphSegment(publicUri, domain, prop, range, null, listTypes.get(range.as(OntClass.class)), configMgr);
						}
						else if (domain.canAs(OntClass.class) &&  listTypes.containsKey(domain.as(OntClass.class))) {
							RDFNode lst = addTypeListToGraphData(data, publicUri, domain.as(OntResource.class), listTypes.get(domain.as(OntClass.class)));
							sg = new GraphSegment(publicUri, domain, prop, range, listTypes.get(domain.as(OntClass.class)), null, configMgr);
						}
					}
					if (sg == null) {
						sg = new GraphSegment(publicUri, domain, prop, range, configMgr);
					}
					if (!data.contains(sg)) {
						data.add(sg);
						annotateHeadAsClass(sg);					// head
						if (isInImports(domain, publicUri)) {
							if(getImportUrl(domain) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(domain));
							sg.addHeadAttribute(IS_IMPORT, "true");
						}
						annotateEdge(sg, PROPERTY_GREEN, null, null);	// edge
						if(range.isResource()) {
							if (isInImports(range, publicUri)){
								if(getImportUrl(range) != null) {
									sg.addTailAttribute(LINK_URL, getImportUrl(range));
								}
								sg.addTailAttribute(IS_IMPORT, "true");
							}
							annotateTailAsClass(sg);
						}		
					}
				}
			}
			
		}
	}

	private void addSameAsClass(String publicUri, Resource subj, RDFNode obj, List<GraphSegment> data) throws InvalidNameException {
		throw new InvalidNameException("Same as not yet implmemented");
	
	}

	private void addEquivalentClass(String publicUri, Resource subj, RDFNode obj, List<GraphSegment> data) {
		if (subj.canAs(OntClass.class) && obj.canAs(OntClass.class)) {
			OntClass eqcls = obj.as(OntClass.class);
			if (eqcls.isEnumeratedClass()) {
				EnumeratedClass enumcls = eqcls.asEnumeratedClass();
				GraphSegment gs = new GraphSegment(publicUri, subj, "same as", enumcls, getConfigMgr());
				if (!data.contains(gs)) {
					data.add(gs);
					annotateHeadAsClass(gs);
					gs.addTailAttribute(COLOR, RED);
					gs.addEdgeAttribute(COLOR, RED);
				}
			}
		}
	}

	private void addInstancePropertyToGraph(String publicUri, Individual inst, Resource prop, RDFNode rng,
			List<GraphSegment> data)
			throws ConfigurationException, IOException, URISyntaxException, Exception {
		RDFNode listtype = null;
		boolean isList = isPropertyAnnotatedAsListRange(prop);
		if (isList && rng.canAs(OntResource.class)) {
			listtype = getListType(rng.as(OntResource.class));
		}
		addInstancePropertyToGraph(publicUri, inst, prop,  rng, listtype, data);
	}
	
	private void addInstancePropertyToGraph(String publicUri, Individual inst, Resource prop, RDFNode rng,
			RDFNode listtype, List<GraphSegment> data)
			throws ConfigurationException, IOException, URISyntaxException, Exception {
		GraphSegment sg;
		if (listtype != null) {
			sg = new GraphSegment(publicUri, inst, prop, rng, null, listtype, configMgr);
		}
		else {
			sg = new GraphSegment(publicUri, inst, prop, rng, configMgr);
		}
		if (!data.contains(sg)) {
			data.add(sg);
			annotateHeadAsIndividual(sg);					// head
			if (isInImports(inst, publicUri)) {
				if(getImportUrl(inst) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(inst));
				sg.addHeadAttribute(IS_IMPORT, "true");
			}
			annotateEdge(sg, PROPERTY_GREEN, null, null);	// edge
			if(rng.isResource()) {
				ConceptType rngCT = getConceptType(rng);
				if (isInImports(rng, publicUri)){
					if(getImportUrl(rng) != null) {
						sg.addTailAttribute(LINK_URL, getImportUrl(rng));
					}
					sg.addTailAttribute(IS_IMPORT, "true");
				}
				if (rngCT != null) {
					if (isProperty(rngCT)) {
						annotateTailAsProperty(sg, PROPERTY_GREEN);
					}
					else if (rngCT.equals(ConceptType.INDIVIDUAL)){
						annotateTailAsIndividual(sg);
					}
					else if (rngCT.equals(ConceptType.ONTCLASS)) {
						annotateTailAsClass(sg);
					}
				}
			}		
		}
	}
	
	private boolean isProperty(ConceptType ct) {
		if (ct.equals(ConceptType.ANNOTATIONPROPERTY) ||
				ct.equals(ConceptType.DATATYPEPROPERTY) ||
				ct.equals(ConceptType.OBJECTPROPERTY) ||
				ct.equals(ConceptType.RDFPROPERTY)) {
			return true;
		}
		return false;
	}

	private ConceptType getConceptType(RDFNode concept) {
		// get concept in model with imports
		if (concept.isURIResource()) {
			Resource newconcept = getTheJenaModelWithImports().getResource(concept.asResource().getURI());
			if (newconcept != null) {
				if (newconcept.canAs(OntClass.class)){
					return ConceptType.ONTCLASS;
				}
				else if (newconcept.canAs(ObjectProperty.class)){
					return ConceptType.OBJECTPROPERTY;
				}
				else if (newconcept.canAs(DatatypeProperty.class)) {
					return ConceptType.DATATYPEPROPERTY;
				}
				else if (newconcept.canAs(AnnotationProperty.class)) {
					return ConceptType.ANNOTATIONPROPERTY;
				}
				else if (newconcept.canAs(Individual.class)) {
					return ConceptType.INDIVIDUAL;
				}
				else if (newconcept.canAs(Property.class)){
					return ConceptType.RDFPROPERTY;
				}
			}
		}
		else if (concept.canAs(OntClass.class)){
			return ConceptType.ONTCLASS;
		}
		else if (concept.canAs(Individual.class)){
			return ConceptType.INDIVIDUAL;
		}
		return null;
	}

	/**
	 * Method to get the type of the elements of a typed List from the restriction in the definition
	 * @param rng
	 * @return
	 */
	private RDFNode getListType(OntResource rng) {
		StmtIterator stmtitr = getLocalModel().listStatements(rng, RDFS.subClassOf, (RDFNode)null);
		RDFNode listtype = null;
		while (stmtitr.hasNext()) {
			Statement supclsstmt = stmtitr.nextStatement();
			RDFNode supclsnode = supclsstmt.getObject();
			//if the superclass is a class
			if (supclsnode.canAs(OntClass.class)){
				//get the superclass as an OntClass
				OntClass supcls = supclsnode.as(OntClass.class);  // scitr.next();
				if (supcls.hasProperty(OWL.onProperty, getLocalModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
					Statement avf = supcls.getProperty(OWL.allValuesFrom);
					if (avf != null) {
						listtype = avf.getObject();
					}
					stmtitr.close();
					break;
				}
			}
		}
		return listtype;
	}
	
	public String getCurrentFileLink(String parentUri) throws Exception{
		String baseFilename = getBaseFilenameFromPublicUri(parentUri);
		
		// get the prefix and if there is one generate qname
		String tempDir = SadlActionHandler.convertProjectRelativePathToAbsolutePath(SadlActionHandler.getGraphDir(getProject())); 
		
		if(baseFilename!=null){
			return "\"file:///" + tempDir + "/" + baseFilename + getGraphFilenameExtension() + "\"";
		}
		throw new Exception("Cannot find graph file in getCurrentFileLink()");
	}
	
	private void addInstanceIsAClassToGraph(String publicUri, Individual or, RDFNode classInst, List<GraphSegment> data)
			throws ConfigurationException, IOException, URISyntaxException, Exception {
		GraphSegment gs = new GraphSegment(publicUri, or, "is a", classInst, getConfigMgr());
		if (!data.contains(gs)) {
			data.add(gs);
			annotateHeadAsIndividual(gs);
			annotateTailAsClass(gs);
			if(isInImports(classInst, publicUri)){
				if(getImportUrl(classInst) != null) gs.addTailAttribute(LINK_URL, getImportUrl(classInst));
				gs.addTailAttribute(IS_IMPORT, "true");
			}
		}
	}

	private void addIsATypeOfToGraph(String publicUri, OntClass classInst, OntClass supercls, List<GraphSegment> data)
			throws ConfigurationException, IOException, URISyntaxException, Exception {
		GraphSegment sg;
		if (supercls.isRestriction()) {
			StmtIterator stir = getTheJenaModel().listStatements(supercls.asResource(), OWL.onProperty, (RDFNode)null);
			if (stir.hasNext()) {
				RDFNode pn = stir.next().getObject();
				if (pn.canAs(Property.class)){ 
					Property onprop = pn.as(Property.class);
					if (!onprop.equals(getListFirstProp()) && !onprop.equals(getListRestProp())) {
						RDFNode subjlisttype = getListType(supercls);
						RDFNode objlisttype = getListType(classInst);
						if (subjlisttype != null || objlisttype != null) {
							sg = new GraphSegment(publicUri, supercls, "restricts", classInst, subjlisttype, objlisttype, configMgr);
						}
						else {
							sg = new GraphSegment(publicUri, supercls, "restricts", classInst, configMgr);
						}
						if (!data.contains(sg)) {
							data.add(sg);
							sg.addHeadAttribute(COLOR, RED);
							sg.addEdgeAttribute(COLOR, RED);
							annotateTailAsClass(sg);
						}
					}
				}
				else {
					throw new InvalidNameException("Unable to convert restriction's property to a property");
				}
			}
			else {
				throw new InvalidNameException("Unable to get restriction's property");
			}
		}
		else {
			sg = new GraphSegment(publicUri, supercls, "subClass", classInst, getConfigMgr());
			sg.addEdgeAttribute(COLOR, BLUE);
			sg.addEdgeAttribute(STYLE, DASHED);
			if (!data.contains(sg)) {
				data.add(sg);
				annotateHeadAsClass(sg);
				if(isInImports(supercls, publicUri)){
					if(getImportUrl(supercls) != null) sg.addHeadAttribute(LINK_URL, getImportUrl(supercls));
					sg.addHeadAttribute(IS_IMPORT, "true");
				}
				annotateTailAsClass(sg);
				if(isInImports(classInst, publicUri)){
					if(getImportUrl(classInst) != null) sg.addTailAttribute(LINK_URL, getImportUrl(classInst));
					sg.addTailAttribute(IS_IMPORT, "true");
				}
			}
		}
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
	Object[][] attributeToDataArray(String graphPart, Map<String,String> map, List<String> columnList, Object[][] array, int i, GraphSegment gs) {
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

	public List<GraphSegment> getImports(IConfigurationManagerForIDE configMgr, String publicUri) {
		List<GraphSegment> importList = null;
		try {
			String prefix = configMgr.getGlobalPrefix(publicUri);
			Map<String, String> imports = configMgr.getImports(publicUri, Scope.LOCALONLY);
			if (imports != null) {
				importList = new ArrayList<GraphSegment>();
				Iterator<String> keyitr = imports.keySet().iterator();
				while (keyitr.hasNext()) {
					// head: node ID is prefix, tooltip is publicUri, URL is graph file for publicUri
					// tail: node ID is value, tooltip is key, URL is graph file for key
					String key = keyitr.next();
					String pred = null;
					String value = null;
					String headUrl = null;
					String headTooltip = null;
					if (!isUbiquitousImport(key)) {
						pred = "imported by";
						value = imports.get(key);
						headUrl = getCurrentFileLink(key);
						headTooltip = "\"" + key + "\"";
//						System.out.println("found import for '" + publicUri + "': key = '" + key + "', value = '" + value + "'");
						GraphSegment gs = new GraphSegment(publicUri, value, pred, prefix, configMgr);
						gs.addTailAttribute("URL", getCurrentFileLink(publicUri));
						String str = "\"" + publicUri + "\"";
						gs.addTailAttribute("tooltip", str);
						if (headUrl != null) {
							gs.addHeadAttribute("URL", headUrl);
						}
						if (headTooltip != null) {
							gs.addHeadAttribute("tooltip", headTooltip);
						}
						importList.add(gs);
					}
				}
			}
			else {
//				System.out.println("no imports for '" + publicUri + "'");
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return importList;
	}

	private boolean isUbiquitousImport(String key) {
		if (key != null) {
			if (key.equals(SadlConstants.SADL_BASE_MODEL_URI) ||
					key.equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI) ||
					key.equals(SadlConstants.SADL_IMPLICIT_MODEL_URI) ||
					key.equals(SadlConstants.SADL_LIST_MODEL_URI)) {
				return true;
			}
		}
		return false;
	}
}
