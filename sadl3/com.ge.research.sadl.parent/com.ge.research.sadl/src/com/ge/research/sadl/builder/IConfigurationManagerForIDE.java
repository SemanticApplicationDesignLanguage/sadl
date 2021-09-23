
package com.ge.research.sadl.builder;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModel;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.reasoner.AvailablePlugin;
import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;

public interface IConfigurationManagerForIDE extends IConfigurationManagerForEditing {

	public static final String DEFAULTS_PREFIX = "def";

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
	 * Method to determine if the configuration has changed since this ConfigurationManager was initialized.
	 * 
	 * @return true if modified (stale) else false
	 */
	public abstract boolean isConfigurationStale();

	/**
	 * Method to get an instance of a class by reflection
	 * @param name
	 * @param clazz
	 * @return
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws ClassNotFoundException
	 */
	public <T> T getClassInstance(String name, Class<? extends T> clazz)
			throws InstantiationException, IllegalAccessException,
			ClassNotFoundException;
			
	/**
	 * Method to clear an old version of a model from cache when a new version of the model is saved
	 * 
	 * @param newOntModel
	 * @param publicUri
	 * @return - true if removed else false
	 */
	public abstract boolean replaceJenaModelCache(OntModel model,
			String publicUri);

	public abstract boolean resetJena();

	public abstract boolean clearJenaModelContent(OntModel model);

//	/**
//	 * Call this method to add a new mapping or update an existing one for a given altURL. (The assumption is that
//	 * the file name will not change but the model name (uri) may be easily changed.)
//	 * 
//	 * @param altUrl - the actual URL of the OWL file of the model
//	 * @param publicUri - the model name (uri) of the model
//	 * @return -- true if the mappings were changed else false
//	 * @throws MalformedURLException 
//	 * @throws ConfigurationException 
//	 */
//	public abstract boolean addMapping(String altUrl, String publicUri,
//			String globalPrefix) throws MalformedURLException,
//			ConfigurationException;

//	/**
//	 * Call this method to add a new mapping or update an existing one for a given altURL. (The assumption is that
//	 * the file name will not change but the model name (uri) may be easily changed.)
//	 * 
//	 * @param altv - the actual URL of the OWL file of the model
//	 * @param pubv - the model name (uri) of the model
//	 * @return - true if a change was made else false if everything was as needed
//	 * @throws ConfigurationException 
//	 */
//	public abstract boolean addMapping(Resource altv, Resource pubv,
//			Literal prefix) throws ConfigurationException;
//
	/**
	 * Method to add a altUrl<->publicUri mapping to the ont-policy.rdf file.
	 *
	 * @param publicUri
	 * @param altUrl
	 * @param policyFile
	 * @return
	 * @throws IOException
	 * @throws URISyntaxException
	 * @throws ConfigurationException 
	 */
	public abstract boolean addJenaMapping(String publicUri, String altUrl)
			throws IOException, URISyntaxException, ConfigurationException;
	
	/**
	 * Method to record dependencies of this Project (KBase) on other Projects (KBases)
	 * @param dependencies
	 * @return
	 * @throws ConfigurationException 
	 */
	public abstract boolean addProjectDependencies(List<java.net.URI> dependencies) throws ConfigurationException;

	/**
	 * Method to get a list of all the available translator plugins for the current reasoner using a {@link ServiceLoader}
	 * @return A list of all available translator plugins that are compatible (same Reasoner Family) with the current reasoner
	 * @throws ConfigurationException 
	 */
	public abstract List<AvailablePlugin> getAvailableTranslatorPluginsForCurrentReasoner()
			throws ConfigurationException;

	/**
	 * Method to get a list of all the available builtins for the current reasoner using a {@link ServiceLoader}
	 * 
	 * @return A list of all available builtins for the family of the current reasoner
	 * @throws ConfigurationException 
	 */
	public abstract List<BuiltinInfo> getAvailableBuiltinsForCurrentReasoner()
			throws ConfigurationException;

	/**
	 * Method to add a ConfigurationItem to the in-memory configuration model and to the persistent cache.
	 * Note that for items of type Bag (e.g., Builtins) and Sequence, the last element in the array returned
	 * by the call to getCatgegoryHierchy is not a an instance name for a Category but rather the type of the
	 * elements of the Bag or Sequence.
	 * 
	 * @param newItem
	 * @throws ConfigurationException
	 */
	// When adding or updating, we must take the implicit Category hierarchy into account, 
	//	e.g., Builtins is a sub-category of the reasoner's Category instance

	public abstract void addConfiguration(ConfigurationItem newItem)
			throws ConfigurationException;

	/**
	 * Call this method to add or update a ConfigurationItem to the configuration model.
	 * 
	 * @param newItem - the new or update ConfigurationItem
	 * 
	 * @throws ConfigurationException
	 */
	public abstract void updateConfiguration(ConfigurationItem newItem)
			throws ConfigurationException;

	/**
	 * Call this method to save the configuration model to a "configuration.rdf" file in the OwlModels folder.
	 * 
	 * @return - true if successful else false
	 */
	public abstract boolean saveConfiguration();

	public abstract boolean isConfigChanged();

	/**
	 * Call this method to set the mapping for the "defaults.owl" model. This should be called if a default is added
	 * to a model to make sure that the definition of default value concepts is available as an import model.
	 * 
	 * @throws IOException
	 * @throws URISyntaxException
	 * @throws ConfigurationException 
	 */
	public abstract void setDefaultsAltUrlMapping() throws IOException,
			URISyntaxException, ConfigurationException;

	public abstract void setProjectFolderPath(String _projectFolderPath,
			String _modelFolderPath) throws ConfigurationException;

//	public abstract String getProjectFolderPath();

	public abstract void addGlobalPrefix(String modelName, String globalPrefix);

	public abstract String checkForDuplicateSadlFile(URI actualUri) throws URISyntaxException, MalformedURLException;

	public abstract void setProjectFolderPath(String _projectFolderPath);

	/**
	 * Get the concept names of everything in this model with this namespace and matching type
	 * @param localModel
	 * @param modelname
	 * @param cType
	 * @param scope
	 * @return
	 * @throws InvalidNameException
	 */
	public abstract List<ConceptName> getNamedConceptsInModel(
			OntModel localModel, String modelName, ConceptType cType,
			Scope scope) throws InvalidNameException;

	/**
	 * Method to get the Jena OntDocumentManager 
	 */
	public abstract OntDocumentManager getJenaDocumentMgr();

	/**
	 * Method to determine if a particular model, identified by public URI, is derived
	 * from translating a SADL model file.
	 * 
	 * @param publicUri
	 * @return
	 * @throws ConfigurationException 
	 * @throws MalformedURLException 
	 */
	public abstract boolean isSadlDerivedPublicUri(String publicUri) throws ConfigurationException, MalformedURLException;

	/**
	 * Method to determine if a particular model, identified by actual URL, is derived
	 * from translating a SADL model file.
	 * 
	 * @param altUrl
	 * @return
	 * @throws ConfigurationException 
	 * @throws MalformedURLException 
	 */
	public abstract boolean isSadlDerivedAltUrl(URI altUrl) throws ConfigurationException, MalformedURLException;

	/**
	 * Method to find the imports, with prefixes, for a given model identified by uri
	 * @param publicUri
	 * @param scope -- Scope.LOCALONLY or Scope.INCLUDEIMPORTS
	 * @return
	 * @throws ConfigurationException 
	 * @throws IOException 
	 * @throws TranslationException 
	 */
	public abstract Map<String, String> getImports(String publicUri, Scope scope) throws ConfigurationException, IOException, TranslationException;

	/**
	 * Get the concept names of everything in the named model matching type and scope
	 * @param localModel
	 * @param publicUri
	 * @param cType
	 * @param scope
	 * @return
	 * @throws InvalidNameException
	 * @throws ConfigurationException 
	 * @throws TranslationException 
	 */
	public abstract List<ConceptName> getNamedConceptsInModel(String publicUri, 
			ConceptType cType, Scope scope) throws InvalidNameException, ConfigurationException, IOException, TranslationException;
	
	/**
	 * Gets the URI of a SADL resource for a public URI.
	 * @param publicUri The public URI
	 * @return Sadl resource URI, or <code>null</code>.
	 * @throws ConfigurationException
	 * @throws IOException
	 */
	public abstract URI getSadlUriFromPublicUri (ResourceSet resourceSet, URI publicUri) throws ConfigurationException, IOException;
	
	/**
	 * Get a SadlUtils instance.
	 * @return
	 */
	public abstract SadlUtils getSadlUtils();
	
	/**
	 * Call this method to validate that an OWL model being imported actually exists (is valid)
	 * @param publicUri
	 * @param altUrl
	 * @return
	 * @throws MalformedURLException 
	 * @throws IOException 
	 * @throws TranslationException 
	 * @throws ConfigurationException 
	 */
	public abstract boolean validateImport(String publicUri, String altUrl) throws MalformedURLException, ConfigurationException, TranslationException, IOException;

	/**
	 * Call this method to get a list of the actual URLs (as class URI) of all RDF and OWL
	 * models that should be available to the project but are not actually in the project
	 * or projects upon which this project depends.
	 * @return
	 */
	public abstract List<URI> getExternalModelURIs();

	/**
	 * Call this method to add a new mapping or update an existing one for a given altURL. (The assumption is that
	 * the file name will not change but the model name (uri) may be easily changed.)
	 * 
	 * @param mappings -- list of String[] where each element of the list contains: 0-altUrl, 1-publicUri, 2-global prefix
	 * @param bKeepPrefix -- if true keep the old prefix even if other things change
	 * @param source -- an identifier of what created the mapping, e.g., SADL
	 * @return -- true if the mappings were changed else false
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract boolean addMappings(List<String[]> mappings, boolean bKeepPrefix, String source) throws ConfigurationException, IOException, URISyntaxException;
	/**
	 * Call this method to get an OntModel from a URI (or URL)
	 * 
	 * @param publicUri--URI of model (will also work with actual URL)
	 * @param scope--Scope.LOCALONLY or Scope.INCLUDEIMPORTS
	 * @return
	 * @throws ConfigurationException
	 * @throws IOException
	 * @throws TranslationException 
	 */
	public abstract OntModel getOntModel(String publicUri, Scope scope) throws ConfigurationException, IOException, TranslationException;

	/**
	 * Call this method to find a file treating it first as an absolute path and that failing as a project-relative path
	 * @param fn
	 * @return
	 */
	public abstract File resolveFilename(String fn);
	
	/**
	 * Call this method to find the base URI that is identified in an OWL file. Normally this will be the 
	 * namespace that has an empty string as the prefix.
	 * @param owlFilename--the complete path and filename of the OWL model
	 * @return the base URI identified in the file if found else null
	 */
	public abstract String getBaseUriFromOwlFile(String owlFilename);
	
	/**
	 * Call this method to find the base URI that is identified in an OWL model. Normally this will be the 
	 * namespace that has an empty string as the prefix.
	 * @param owlFilename--the complete path and filename of the OWL model
	 * @return the base URI identified in the file if found else null
	 */
	public abstract String getBaseUriFromOwlModel(String owlFilename, OntModel om);

	/**
	 * Call this method to load an OWL file and return the resulting Jena OntModel
	 * @param owlFilename
	 * @return Jena OntModel
	 */
	public abstract OntModel loadOntModel(String owlFilename);

	/**
	 * Call this method to load an OWL file with the option of loading imports and return the resulting Jena OntModel
	 * @param owlFilename
	 * @param loadImports
	 * @return Jena OntModel
	 */
	public abstract OntModel loadOntModel(String owlFilename, boolean loadImports);

	/**
	 * Method to persist a private key/value pair at the project scope
	 * @param key
	 * @param value
	 */
	public abstract void addPrivateKeyValuePair(String key, Object value);
	
	/**
	 * Method to retrieve a private key/value pair by key at the project scope
	 * @param key
	 * @return
	 */
	public abstract Object getPrivateKeyValuePair(String key);

	/**
	 * Method to retrieve a private key value from a Map using Resource URI as Map key
	 * @param key
	 * @param rsrc
	 * @param value
	 */
	public abstract void addPrivateKeyMapValueByResource(String key, URI rsrcUri, Object value);
	
	/**
	 * Method to persist a private key value in a Map using Resource as Map key
	 * @param key
	 * @param rsrc
	 * @return
	 */
	public abstract Object getPrivateKeyMapValueByResource(String key, URI rsrcUri);
	
	/**
	 * Method to clean up the TDB folder if the SadlSerializationFormat isn't TDB
	 * 
	 * @return
	 */
	public abstract boolean cleanTdbFolder();

}
