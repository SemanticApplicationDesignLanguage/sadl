package com.ge.research.sadl.reasoner;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.ServiceLoader;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Resource;

public interface IConfigurationManagerForEditing extends IConfigurationManager {

    /**
	 * This enum defines the scope of a request for a list of matching concept
	 * names
	 * 
	 */
	public enum Scope {
		LOCALONLY, INCLUDEIMPORTS
	}
	
	/**
	 * Method to determine if the configuration has changed since this ConfigurationManager was initialized.
	 * 
	 * @return true if modified (stale) else false
	 */
	public abstract boolean isConfigurationStale();

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

	/**
	 * Call this method to add a new mapping or update an existing one for a given altURL. (The assumption is that
	 * the file name will not change but the model name (uri) may be easily changed.)
	 * 
	 * @param altUrl - the actual URL of the OWL file of the model
	 * @param publicUri - the model name (uri) of the model
	 * @param bKeepPrefix -- if true keep the old prefix even if other things change
	 * @param source -- an identifier of what created the mapping, e.g., SADL
	 * @return -- true if the mappings were changed else false
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract boolean addMapping(String altUrl, String publicUri,
			String globalPrefix, boolean bKeepPrefix, String source) throws ConfigurationException, IOException,
			URISyntaxException;

	/**
	 * Call this method to add a new mapping or update an existing one for a given altURL. (The assumption is that
	 * the file name will not change but the model name (uri) may be easily changed.)
	 * 
	 * @param altv - the actual URL of the OWL file of the model
	 * @param pubv - the model name (uri) of the model
	 * @param bKeepPrefix -- if true keep the old prefix even if other things change
	 * @param source -- an identifier of what created the mapping, e.g., SADL
	 * @return - true if a change was made else false if everything was as needed
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract boolean addMapping(Resource altv, Resource pubv,
			Literal prefix, boolean bKeepPrefix, String source) throws ConfigurationException, IOException,
			URISyntaxException;
	
	/**
	 * Call this method to delete a mapping
	 * 
	 * @param altUrl
	 * @param publicUri
	 * @return
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract boolean deleteMapping(String altUrl, String publicUri) throws IOException, URISyntaxException, ConfigurationException;

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

	public abstract boolean deleteModel(String publicUri)
			throws ConfigurationException, IOException, URISyntaxException;

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

	/**
	 * Call this method to save the mapping model to the "ont-policy.rdf" file in the OwlModels folder.
	 * 
	 * @return - true if successful else false
	 */
	public abstract boolean saveOntPolicyFile();

	public abstract boolean isConfigChanged();

	public abstract String getProjectFolderPath() throws URISyntaxException;

	public abstract void addGlobalPrefix(String modelName, String globalPrefix);

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

	public abstract OntDocumentManager getJenaDocumentMgr();

}