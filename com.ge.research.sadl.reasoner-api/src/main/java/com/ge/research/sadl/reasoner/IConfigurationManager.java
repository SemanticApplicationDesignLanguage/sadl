package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ge.research.sadl.model.ImportMapping;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.Model;

public interface IConfigurationManager {
    public static final String SADL = "SADL";

    public static final String FILE_SHORT_PREFIX = "file:/";
    public static final String FILE_URL_PREFIX = "file://";
    public static final String FILE_ABS_URL_PREFIX = "file:///";
    public static final String HTTP_URL_PREFIX = "http://";
    public static final String OWLDIR = "OwlModels";
    public static final String OWLFILEEXT = "owl";
    public static final String OWLFILEEXTWITHPREFIX = ".owl";
    public static final String SADLEXT = "sadl";
    public static final String SADLEXTWITHPREFIX = ".sadl";
    
	public static final String ACUITY_DEFAULTS_URI = "http://research.ge.com/Acuity/defaults.owl";
	public static final String ACUITY_DEFAULTS_NS = ACUITY_DEFAULTS_URI + "#";
	public static final String ACUITY_DEFAULTS_PREFIX = "defs";
	public static final String ACUITY_DEFAULTS_OWL_FN = "defaults.owl";

	public static final String ServicesConfigurationURI = "http://com.ge.research.sadl/sadlserver/Services";
	public static final String ServicesConfigurationPrefix = "SadlServicesConfigurationConcepts";
	public static final String ServicesConfigurationConcepts_FN = "SadlServicesConfigurationConcepts.owl";
	public static final String ServicesConf_FN = "ServicesConfig.owl";
	public static final String ServicesConf_SFN = "ServicesConfig.sadl";

	// Constants used to manage configuration of Translators and Reasoners
	public static final String ONT_POLICY_RDF = "ont-policy.rdf";
	public static final String CONFIG_FILENAME = "configuration.rdf";
	public static final String CONFIG_NAMESPACE = "http://com.ge.research.sadl.configuration#";
	public static final String CATEGORY_KW = CONFIG_NAMESPACE + "Category";
	public static final String SUBCATEGORY_PROP = CONFIG_NAMESPACE
			+ "subcategory";
	public static final String BuiltinCategory = "Builtin";
	// Note: there is an implicit heirarchy in the configuration file:
	//	Builtins is a subcategory of the ReasonerCategory
	//	?? what else??
	
	// Other configuration constants
	public static final String DateFormat = "DateFormat";
	public static final String dmyOrder = "dmyOrder";	
	public static final String dmyOrderDMY = "dmy";
	public static final String dmyOrderMDY = "mdy";
	
	/*
	 * From the Jena OntModel.java file:
	 * <p>The language in which to write the model is specified by the
	 * <code>lang</code> argument.  Predefined values are "RDF/XML",
	 * "RDF/XML-ABBREV", "N-TRIPLE" and "N3".  The default value,
	 * represented by <code>null</code> is "RDF/XML".</p>
	 */
	public static final String RDF_XML_FORMAT = "RDF/XML"; // default
	public static final String RDF_XML_ABBREV_FORMAT = "RDF/XML-ABBREV";
	public static final String N_TRIPLE_FORMAT = "N-TRIPLE";
	public static final String N3_FORMAT = "N3";
	public static final String JENA_TDB = "Jena TDB";
//	public static final String OWL_Func_SWRL = "OWL Functional with SWRL";
	
	public final boolean inferenceCanceled = false;
	
	public abstract boolean setInferenceCanceled(boolean canceled);
	
	public abstract boolean getInferenceCanceled();
		
	public abstract boolean loadGlobalPrefixes(OntModel model);

	public abstract ITranslator getTranslator() throws ConfigurationException;

	public abstract IReasoner getReasoner() throws ConfigurationException;

	/**
	 * Method to get a copy of the reasoner when required. Calling this
	 * 	method should either provide a new reasoner that matches the ones previously 
	 * 	created, at the time of their creation, or create a deep copy of the reasoner.
	 * 
	 * @return
	 * @throws ConfigurationException
	 */
	public abstract IReasoner getCloneReasoner() throws ConfigurationException;
	
	/**
	 * Method to get a different reasoner than the one specified in the configuration file
	 * 
	 * @param reasonerClassName
	 * @return
	 * @throws ConfigurationException
	 */
	public abstract IReasoner getOtherReasoner(String reasonerClassName) throws ConfigurationException;

		/** Clear the current reasoner (if any) because something may have changed and a new
	 * reasoner needs to be created for inference to occur.
	 * @return
	 */
	public abstract boolean clearReasoner();

	/**
	 * Method to return the fully qualified name of the class for the current IReasoner as specified in the configuration.
	 * @return
	 * @throws ConfigurationException 
	 */
	public abstract String getReasonerClassName() throws ConfigurationException;

	/**
	 * Method to return the fully qualified name of the class for the current ITranslator as specified in the configuration.
	 * @return
	 * @throws ConfigurationException 
	 */
	public abstract String getTranslatorClassName()
			throws ConfigurationException;

	/**
	 * This method identifies all of a model's imports and their prefixes, direct and indirect.
	 * 
	 * @param m - the OntModel to be investigated
	 * @param modelUri - the URI of the model
	 * 
	 * @return - a Map of model URIs to prefixes
	 */
	public abstract Map<String, String> listSubModelsAndPrefixes(OntModel m,
			String modelUri);

	/**
	 * Method to load an imported model, along with any indirect imports, and return the
	 * loaded models and information about each model as a list of ImportMappings. The 
	 * models are only stored in the Jena model cache long enough to retrieve them for 
	 * inclusion in the list. Jena caching is then turned off as it seems to create memory problems.
	 * 
	 * Normally either the public URI or the actual URL of the imported model will be known.
	 * 
	 * @param importingModel - the OntModel that is importing another model
	 * @param publicImportUri - the public URI of the other (imported) model
	 * @param altImportUrl - the actual URL of the other (imported) model
	 * @return
	 * @throws ConfigurationException 
	 */
	public abstract List<ImportMapping> loadImportedModel(
			Ontology importingOntology, OntModel importingModel,
			String publicImportUri, String altImportUrl)
			throws ConfigurationException;

	/**
	 * This method reads the policy model from the policy file in the models folder and returns the mappings from
	 * public URIs to alternative URLs.
	 * 
	 * @param folderName
	 * @param modelName
	 * @return
	 */
	public abstract HashMap<String, String> getMappings();

	/**
	 * Method to return the global prefix of a public URI or  null
	 * @param uri - public URI
	 * @return - prefix else null if none found
	 */
	public abstract String getGlobalPrefix(String uri);

	/**
	 * Method to return the public URI that matches a prefix if
	 * the prefix is in the global prefix Map
	 * 
	 * @param prefix -- the prefix to be matched to a URI
	 * 
	 * @return -- the URI matching the prefix or null
	 */
	public abstract String getUriFromGlobalPrefix(String prefix);

	/**
	 * Get all of the configuration items of a particular category hierarchy.
	 * 
	 *  @param categoryHierarchy - the category hierarchy desired
	 *  @return - a list of ConfigurationItems matching the request
	 * @throws ConfigurationException 
	 */
	public abstract List<ConfigurationItem> getConfiguration(
			String[] categoryHierarchy, boolean includeSubcategories)
			throws ConfigurationException;

	public abstract void setModelFolderPath(File modelFolder);

	public abstract File getModelFolderPath();

	/**
	 * Method to get the model folder as a String regardless of
	 * whether it is local (file system) or remote (http URL)
	 * 
	 * @return
	 * @throws IOException
	 */
	public abstract String getModelFolder() throws IOException;

	/**
	 * Use the LocationMapper created from the ont-policy.rdf file and updated with any added mappings to find 
	 * the publicUri corresponding to an actualURL.
	 * 
	 * @param altURL
	 * @return
	 * @throws ConfigurationException
	 */
	public abstract String getPublicUriFromActualUrl(String altURL)
			throws ConfigurationException;

	/**
	 * Use the LocationMapper created from the ont-policy.rdf file on initialization and updated with any added mappings
	 * to find the altURL corresponding to a publicUri.
	 * 
	 * @param publicUri
	 * @return
	 * @throws ConfigurationException
	 */
	public abstract String getAltUrlFromPublicUri(String publicUri)
			throws ConfigurationException;

	public abstract Model getMappingModel();

	public abstract String findPublicUriOfOwlFile(String sadlFile);

	/**
	 * Call this method to specify the Translator class to use for this model.
	 * 
	 * @param translatorClassName
	 * @return - true if the configuration was updated else false
	 * @throws ConfigurationException 
	 */
	public abstract boolean setTranslatorClassName(String translatorClassName)
			throws ConfigurationException;

	/**
	 * Call this method to specify the Reasoner class to use for this model.
	 * 
	 * @param reasonerClassName
	 * @return - true if the configuration was updated else false
	 */
	public abstract boolean setReasonerClassName(String reasonerClassName);
	
	/**
	 * Get the TDB folder for this model folder
	 * @throws IOException 
	 */
	public abstract String getTdbFolder() throws IOException;

	/**
	 * Call this method to get the model getter to be used to load models from the repository
	 * @return
	 */
	public abstract ISadlJenaModelGetter getModelGetter();
	
	/**
	 * Call this method to set the model getter to be used to load models from the repository
	 * @param modelGetter
	 */
	public abstract void setModelGetter(ISadlJenaModelGetter modelGetter);

	/**
	 * Call this method to get the translator associated with a particular reasoner class
	 * @param reasonerClassName
	 * @return
	 * @throws ConfigurationException 
	 */
	public abstract ITranslator getTranslatorForReasoner(String reasonerClassName) throws ConfigurationException;

	/**
	 * Method to determine if a URI (could be public URI or alt URL) is mapped in this ConfigurationManager (project)
	 * @param uri
	 * @return
	 */
	public abstract boolean containsMappingForURI(String uri);
	
	/**
	 * Method to get the Jena OntDocumentManager that knows about the mappings for this kbase.
	 * @return
	 */
	public abstract OntDocumentManager getJenaDocumentMgr();
	
	/**
	 * Method to get the OntModelSpec for this ConfigurationManager. When a model's
	 * getDocumentManager is called, it uses the OntModelSpec, so to be thread safe
	 * each Configuration manager needs it's own.
	 * @param toCopySpec--if not null copy the provided spec else copy OntModelSpec.OWL_MEM
	 * @return
	 */
	public abstract OntModelSpec getOntModelSpec(OntModelSpec toCopySpec);

	/**
	 * Method to convert a URL string to a File-compatible string
	 * url
	 * @throws MalformedURLException 
	 */
	public abstract String fileUrlToFileName(String url) throws MalformedURLException;
	
	/**
	 * Metod to convert a File-compatible tring to a file URL string
	 * @param fn
	 * @return
	 */
	public abstract String fileNameToFileUrl(String fn);
}