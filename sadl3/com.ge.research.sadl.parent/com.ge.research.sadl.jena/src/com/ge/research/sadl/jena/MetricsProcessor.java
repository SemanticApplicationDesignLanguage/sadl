package com.ge.research.sadl.jena;

import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFWriter;

public class MetricsProcessor implements IMetricsProcessor {
	private String filename;
	private OntModel theJenaModel;
	private String baseUri;
	private HashMap<String,OntClass> markerClasses = null;
	private HashMap<String,Individual> markerTypeInstances = null;
	private Property markerTypeProperty = null;
	private Property markerProperty = null;
	private IConfigurationManagerForEditing configMgr = null;
	private Ontology modelOntology;
	private SadlModelProcessor modelProcessor = null;
	
	public static String SADL_METRICS_NS = "http://com.ge.research.sadl/sadlmetricsmodel#";
	public static String MARKER_CLASS_URI = SADL_METRICS_NS + "Marker";
	public static String MARKER_PROP_URI = SADL_METRICS_NS + "marker";
	public static String MARKER_TYPE_URI = SADL_METRICS_NS + "MarkerType";
	public static String MARKER_TYPE_PROP_URI = SADL_METRICS_NS + "markerType";
	public static String ERROR_MARKER_URI = SADL_METRICS_NS + "Error";
	public static String WARNING_MARKER_URI = SADL_METRICS_NS + "Warning";
	public static String INFO_MARKER_URI = SADL_METRICS_NS + "Information";
	
	public static String TYPE_CHECK_FAILURE_URI = SADL_METRICS_NS + "TypeCheckFailure";
	public static String TYPE_CHECK_ERROR_URI = SADL_METRICS_NS + "TypeCheckError";
	public static String UNCLASSIFIED_FAILURE_URI = SADL_METRICS_NS + "UnclassifiedFailure";
	public static String RANGE_REDEFINITION_URI = SADL_METRICS_NS + "RangeRedefinition";
	public static String DOMAIN_REDEFINITION_URI = SADL_METRICS_NS + "DomainRedefinition";
	public static String CIRCULAR_IMPORT_URI = SADL_METRICS_NS + "CircularImport";
	public static String CIRCULAR_DEFINITION_URI = SADL_METRICS_NS + "CircularDefinition";
	public static String INVALID_EXPRESSION_URI = SADL_METRICS_NS + "InvalidExpression";
	public static String NESTED_EQUATION_URI = SADL_METRICS_NS + "NestedEquation";
	public static String INVALID_TABLE_FORMAT_URI = SADL_METRICS_NS + "InvalidTableFormat";
	public static String DUPLICATE_NAME_URI = SADL_METRICS_NS + "DuplicateName";
	public static String UNDEFINED_FUNCTION_URI = SADL_METRICS_NS + "UndefinedFunction";

	public MetricsProcessor() {
		super();
	}
	
	public MetricsProcessor(String uri, org.eclipse.emf.ecore.resource.Resource resource, IConfigurationManagerForIDE configMgr, SadlModelProcessor modelProcessor) throws JenaProcessorException, ConfigurationException {
		if (resource == null) {
			throw new JenaProcessorException("MetricsProcessor constructor called with null resource");
		}
		this.modelProcessor = modelProcessor;
		String modelFolder = getModelFolderPath(resource);
		if (modelFolder != null) {
			// for JUnit tests, this will be null
			String fn = modelFolder + "/" + resource.getURI().segment(resource.getURI().segmentCount() - 1);
			baseUri = uri + ".metrics";
			filename = fn + ".metrics.owl";
			setTheJenaModel(ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM));
			this.configMgr = configMgr;
			modelOntology = getTheJenaModel().createOntology(baseUri);
			modelOntology.addComment("SADL model metrics", "en");
		}
		else {
			throw new ConfigurationException("Model folder not found");
		}
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.jena.IMetricsProcessor#addMarker(java.lang.String, java.lang.String)
	 */
	@Override
	public boolean addMarker(String subjectUri, String markerClassUri, String markerTypeUri) {
		Individual marker = getTheJenaModel().createIndividual(getMarkerClass(markerClassUri));
		Individual markerType = getMarkerTypeInstance(markerTypeUri);
		if (marker != null && markerType != null) {
			modelOntology.addProperty(getMarkerProperty(), marker);
			marker.addProperty(getMarkerTypeProperty(), markerType);
			return true;
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.jena.IMetricsProcessor#saveMetrics(java.lang.String)
	 */
	@Override
	public boolean saveMetrics(String format) throws IOException, ConfigurationException, URISyntaxException {
		RDFWriter w = getTheJenaModel().getWriter(format);
		w.setProperty("xmlbase", baseUri);
		FileOutputStream out = new FileOutputStream(filename);
		w.write(getTheJenaModel().getBaseModel(), out, baseUri);
		out.close();
		SadlUtils su = new SadlUtils();
		configMgr.addMapping(su.fileNameToFileUrl(filename), baseUri, null, false, "SRL_Metrics");
		return true;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.jena.IMetricsProcessor#queryProjectMetrics(java.lang.String, java.lang.String)
	 */
	@Override
	public ResultSet queryProjectMetrics(String queryName, String modelFolder) {
		
		return null;
	}

	private OntModel getTheJenaModel() {
		return theJenaModel;
	}

	private void setTheJenaModel(OntModel theJenaModel) {
		this.theJenaModel = theJenaModel;
	}
	
	private String createUri(String name) {
		return baseUri + "#" + name;
	}

	private Property getMarkerTypeProperty() {
		if (markerTypeProperty == null) {
			markerTypeProperty = getTheJenaModel().createObjectProperty(MARKER_TYPE_PROP_URI);
		}
		return markerTypeProperty;
	}

	private Property getMarkerProperty() {
		if (markerProperty == null) {
			markerProperty = getTheJenaModel().createObjectProperty(MARKER_PROP_URI);
		}
		return markerProperty;
	}

	private OntClass getMarkerClass(String markerClassUri) {
		OntClass markerClass = null;
		if (markerClasses == null) {
			markerClasses = new HashMap<String, OntClass>();
		}
		if (!markerClasses.containsKey(markerClassUri)) {
			markerClass = getTheJenaModel().createClass(markerClassUri);
			markerClasses.put(markerClassUri, markerClass);
		}
		else {
			markerClass = markerClasses.get(markerClassUri);
		}
		return markerClass;
	}

	private Individual getMarkerTypeInstance(String markerTypeUri) {
		Individual markerType = null;
		if (markerTypeInstances == null) {
			markerTypeInstances = new HashMap<String,Individual>();
		}
		if (!markerTypeInstances.containsKey(markerTypeUri)) {
			markerType = getTheJenaModel().createIndividual(markerTypeUri, getTheJenaModel().createClass(MARKER_TYPE_URI));
			markerTypeInstances.put(markerTypeUri, markerType);
		}
		else {
			markerType = markerTypeInstances.get(markerTypeUri);
		}
		return markerType;
	}

	public String getModelFolderPath(org.eclipse.emf.ecore.resource.Resource resource) {
		URI v = resource.getURI().trimSegments(resource.getURI().segmentCount() - 2);
		v = v.appendSegment(UtilsForJena.OWL_MODELS_FOLDER_NAME);
		String modelFolderPathname;
		if (v.isPlatform()) {
			 IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(v.toPlatformString(true)));
			 modelFolderPathname = file.getRawLocation().toPortableString();
		}
		else {
			modelFolderPathname = JenaBasedSadlModelProcessor.findModelFolderPath(resource.getURI());
			if(modelFolderPathname == null) {
				modelFolderPathname = v.toFileString();
			}
		}
		return modelFolderPathname;
	}

	@Override
	public boolean addControlledOrMonitoredProperty(String specName, String propertyUri) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addEffectiveRangeAndDomain(String specName, String property, String classname, String range, boolean isList) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setMetricsTargetModel(OntModel metricsTargetModel) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getBaseUri() {
		return baseUri;
	}

}
