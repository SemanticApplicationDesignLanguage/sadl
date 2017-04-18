/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.4 $ Last modified on   $Date: 2014/11/03 19:20:22 $
 ***********************************************************************/

package com.ge.research.sadl.builder;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.SADLPackage;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;

/**
 * this class extension supports configuration tasks unique to the IDE (development environment)
 * @author 200005201
 *
 */
public class ConfigurationManagerForIDE extends ConfigurationManagerForEditing implements IConfigurationManagerForIDE {
	
	public static class Provider implements javax.inject.Provider<IConfigurationManagerForIDE> {
		private String modelFolder;
		private String repoType = ConfigurationManagerForIDE.getOWLFormat();

		public void setModelFolder(String modelFolder) {
			this.modelFolder = modelFolder;
		}
		
		public void setRepoType(String repoType) {
			this.repoType = repoType;
		}

		@Override
		public IConfigurationManagerForIDE get() {
			try {
				return new ConfigurationManagerForIDE(modelFolder,
						repoType);
			} catch (ConfigurationException e) {
				throw new IllegalStateException(e);
			}
		}
	}
	
	boolean closing = false;
	private SadlUtils sadlUtils = new SadlUtils();
	@Inject
	private ResourceDescriptionsProvider resourceDescriptionsProvider;
	
	// TODO: Do not use directly
	public ConfigurationManagerForIDE(String modelFolderPathname, String _repoType) throws ConfigurationException {
		super(modelFolderPathname, _repoType);
		// if there is no mapping file create one
		File mf = new File(modelFolderPathname + File.separator + ONT_POLICY_RDF);
		if (!mf.exists()) {
			try {
				new SadlUtils().stringToFile(mf, getDefaultPolicyFileContent(), false);
			} catch (IOException e) {
				e.printStackTrace();
				throw new ConfigurationException("Failed to create missing 'ont-policy.rdf' file", e);
			}
		}
	}
	
   public ConfigurationManagerForIDE(String modelFolderPathname, String _repoType, boolean noModelFolderNeeded) throws ConfigurationException {
		super(modelFolderPathname, _repoType, noModelFolderNeeded);
		if (!noModelFolderNeeded) {
			// if there is no mapping file create one
			File mf = new File(modelFolderPathname + File.separator + ONT_POLICY_RDF);
			if (!mf.exists()) {
				try {
					new SadlUtils().stringToFile(mf, getDefaultPolicyFileContent(), false);
				} catch (IOException e) {
					e.printStackTrace();
					throw new ConfigurationException("Failed to create missing 'ont-policy.rdf' file", e);
				}
			}
		}
	}

	@Override
	public synchronized boolean addMappings(List<String[]> mappings, boolean bKeepPrefix, String source)
			throws ConfigurationException, IOException, URISyntaxException {
		boolean bChanged = false;
		for (int i = 0; i < mappings.size(); i++) {
			String[] mapping = mappings.get(i);
			if (addMapping(mapping[0], mapping[1], mapping[2], bKeepPrefix, source, false)) {
				bChanged = true;
			}
		}
		if (bChanged) {
			logger.debug("saving mapping file on change after add");
			super.saveOntPolicyFile();
		}
		return bChanged;
	}
	
	@Override
	public synchronized boolean addMapping(String altUrl, String publicUri,
			String globalPrefix, boolean bKeepPrefix, String source) throws ConfigurationException, IOException, URISyntaxException {
		boolean bChanged = super.addMapping(altUrl, publicUri, globalPrefix, bKeepPrefix, source);
		if (bChanged) {
			logger.debug("saving mapping file on change after add");
			super.saveOntPolicyFile();
		}
		return bChanged;
	}
	
	private synchronized boolean addMapping(String altUrl, String publicUri,
			String globalPrefix, boolean bKeepPrefix, String source, boolean bSave) throws ConfigurationException,
			IOException, URISyntaxException {
		boolean bChanged = false;
		bChanged = super.addMapping(altUrl, publicUri, globalPrefix, bKeepPrefix, source);
		if (bChanged && bSave) {
			logger.debug("saving mapping file on change after add");
			super.saveOntPolicyFile();
		}
		return bChanged;
	}
	
	/**
	 * Call this method to add a new mapping or update an existing one for a given altURL. (The assumption is that
	 * the file name will not change but the model name (uri) may be easily changed.)
	 * 
	 * @param altv - the actual URL of the OWL file of the model
	 * @param pubv - the model name (uri) of the model
	 * @return - true if a change was made else false if everything was as needed
	 * @throws ConfigurationException 
	 */
	public synchronized boolean addMapping(Resource altv, Resource pubv, Literal prefix, String source) throws ConfigurationException, IOException, URISyntaxException {
		logger.debug("addMapping: " + altv.getURI() + " <--> " + pubv.getURI() + "(prefix: " + prefix + ")");
		boolean bChanged = false;
		boolean mappingFound = false;
		List<Statement> pendingDeletions = null;
		// Get all the statements that have this public URI
    	StmtIterator pubitr = getMappingModel().listStatements(null, publicUrlProp, pubv);
    	if (pubitr.hasNext()) {
    		mappingFound = true;
    		int cntr = 0;
            while (pubitr.hasNext()) {
                Statement s = pubitr.nextStatement();
                if (cntr > 0) {
                	// there are multiple entries for this public URI
                	if (pendingDeletions == null) {
                		pendingDeletions = new ArrayList<Statement>();
                	}
                	pendingDeletions.add(s);
                }
                else {
	                Resource subj = s.getSubject();
	                // find the corresponding altURL
	                Statement s2 = subj.getProperty(altUrlProp);
	                if (s2 != null) {
	                	// Is the old and the new actual URL the same? If not then change the statement for the actual URL
	                	if (!s2.getObject().equals(altv)) {
	                    	if (pendingDeletions == null) {
	                    		pendingDeletions = new ArrayList<Statement>();
	                    	}
	                    	pendingDeletions.add(s2);
	                        subj.addProperty(altUrlProp, altv);
	                        bChanged = true;
	                	}
	                }
	                else {
	                	subj.addProperty(altUrlProp, altv);
	                	bChanged = true;
	                }
	                Statement s3 = subj.getProperty(prefixProp);
	                if (s3 != null) {
	                	if (!s3.getObject().equals(prefix)) {
	                    	if (pendingDeletions == null) {
	                    		pendingDeletions = new ArrayList<Statement>();
	                    	}
	                    	pendingDeletions.add(s3);
	                    	if (prefix != null) {
	                    		subj.addProperty(prefixProp, prefix);
	                    	}
	                        bChanged = true;
	                	}
	                }
	                else if (prefix != null) {
	                	subj.addProperty(prefixProp, prefix);
	                	bChanged = true;
	                }
                }
                cntr++;
            }
    	}
    	StmtIterator altitr = getMappingModel().listStatements(null, altUrlProp, altv);
    	if (altitr.hasNext()) {
			mappingFound = true;
    		int cntr = 0;
    		while (altitr.hasNext()) {
    			Statement s = altitr.nextStatement();
    			if (cntr > 0) {
    				// there are mulitiple statements for this alt URL
                	if (pendingDeletions == null) {
                		pendingDeletions = new ArrayList<Statement>();
                	}
                	pendingDeletions.add(s);
    			}
    			else {
    				if (!bChanged) {
    					// if bChanged is true then we must have already fixed the one mapping in the section above--no need to do it again
    					Resource subj = s.getSubject();
    					// 	find the corresponding publicUri
    					Statement s2 = subj.getProperty(publicUrlProp);
    					if (s2 != null) {
    						// is the old and the new public URI the same? If not then change the statement for the new public URI
    						if (!s2.getObject().equals(pubv)) {
    		                	if (pendingDeletions == null) {
    		                		pendingDeletions = new ArrayList<Statement>();
    		                	}
    		                	pendingDeletions.add(s2);
    		                	subj.addProperty(publicUrlProp, pubv);
    		                	bChanged = true;
    						}
    					}
    					else {
    						subj.addProperty(publicUrlProp, pubv);
    						bChanged = true;
    					}
    					
    	                Statement s3 = subj.getProperty(prefixProp);
    	                if (s3 != null) {
    	                	if (!s3.getObject().equals(prefix)) {
    	                    	if (pendingDeletions == null) {
    	                    		pendingDeletions = new ArrayList<Statement>();
    	                    	}
    	                    	pendingDeletions.add(s3);
    	                    	if (prefix != null) {
    	                    		subj.addProperty(prefixProp, prefix);
    	                    	}
    	                        bChanged = true;
    	                	}
    	                }
    	                else if (prefix != null) {
    	                	subj.addProperty(prefixProp, prefix);
    	                	bChanged = true;
    	                }
    				}
    			}
    			cntr++;
    		}
    	}
    	
    	// remove extra and obsolete entries
    	if (pendingDeletions != null && pendingDeletions.size() > 0) {
    		for (int i = 0; i < pendingDeletions.size(); i++) {
    			Statement s = pendingDeletions.get(i);
    			if (s.getPredicate().getLocalName().equals("altURL")) {
    				String sadlFileName = ResourceManager.sadlFileNameOfOwlAltUrl(s.getObject().toString(), true);
    				File owlfile = new File(fileUrlToFileName(s.getObject().toString()));
    				String sadlfile = ResourceManager.findSadlFileInProject(owlfile.getParentFile().getParent(), sadlFileName);
    				if (sadlfile != null) {
    					String stmtFileName = owlfile.getName();
    					String altvFileName = new File(fileUrlToFileName(altv.toString())).getName();
    					if (stmtFileName != null && altvFileName != null && !stmtFileName.equals(altvFileName)) {
	    					// duplicate model uri
	    					throw new ConfigurationException("Model name '" + pubv.toString() + "' is used by more than one SADL model: " 
	    							+ owlfile.getName() + " and " + new File(fileUrlToFileName(altv.toString())).getName());
    					}
    				}
    			}
    			getMappingModel().remove(s);
    			bChanged = true;
    		}
    	}
    	
    	if (!mappingFound) {
    		// create a new entry from scratch
        	if (sadlNode == null) {
        		sadlNode = getMappingModel().createTypedLiteral(SADL);
        		createdBy = getMappingModel().createProperty(ONT_MANAGER_CREATED_BY);
        		altUrlProp = getMappingModel().createProperty(ONT_MANAGER_ALT_URL);
        		publicUrlProp = getMappingModel().createProperty(ONT_MANAGER_PUBLIC_URI);
        		prefixProp = getMappingModel().createProperty(ONT_MANAGER_PREFIX);
        	}
    		com.hp.hpl.jena.rdf.model.Resource type = getMappingModel().createResource(ONT_MANAGER_ONTOLOGY_SPEC);
    		com.hp.hpl.jena.rdf.model.Resource newOntSpec = getMappingModel().createResource(type);
    		Property langp = getMappingModel().getProperty(ONT_MANAGER_LANGUAGE);
    		RDFNode langv = getMappingModel().createResource(OWL_ONT_MANAGER_PUBLIC_URINS);
    		getMappingModel().add(newOntSpec, publicUrlProp, pubv);
    		getMappingModel().add(newOntSpec, altUrlProp, altv);
    		getMappingModel().add(newOntSpec, langp, langv);
    		if (source != null && !source.equalsIgnoreCase(SADL)) {
    			getMappingModel().add(newOntSpec, createdBy, source);
    		}
    		else {
    			getMappingModel().add(newOntSpec, createdBy, SADL);
    		}
    		if (prefix != null) {
    			getMappingModel().add(newOntSpec, prefixProp, prefix);
    		}
    		logger.debug("Created new mapping for '" + pubv.toString() + "', '" + altv.toString() + "'");
    		bChanged = true;
    	}
    	try {
    		// add mapping to Jena OntDocumentManager
			if (addJenaMapping(pubv.getURI().toString(), altv.getURI().toString())) {
				bChanged = true;
			}
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (bChanged) {
			setMappingChanged(true);
        	logger.debug("Modified mapping for '" + pubv.toString() + "', '" + altv.toString() + "'");
		}
		if (this.mappings == null) {
			mappings = new HashMap<String, String>();
		}
		mappings.put(rdfNodeToString(pubv), rdfNodeToString(altv));
		return bChanged;
	}
	
	@Override
	public boolean saveOntPolicyFile() {
		System.err.println("ConfigurationManagerForIDE should not call saveOntPolicyFile as changes are saved in synchronized methods that make changes.");
		return false;
	};

//	public void convertProjectToTDB(Dataset repoDataset, String owlModelsFolderPath) throws IOException, URISyntaxException, ConfigurationException {
//		File modelsFolder = new File(owlModelsFolderPath);
//		if (!modelsFolder.exists()) {
//			throw new IOException("OWL models folder '" + owlModelsFolderPath + "' does not exist!");
//		}
//		
//		FilenameFilter owlFilter = new FilenameFilter() {
//			public boolean accept(File dir, String name) {
//				String lowercaseName = name.toLowerCase();
//				if (lowercaseName.endsWith(".owl")) {
//					return true;
//				} else {
//					return false;
//				}
//			}
//		};
//
//		// get all the OWL files for SADL models (in OwlModels folder) and load them and their mapped dependencies into TDB
//		File[] owlFiles = modelsFolder.listFiles(owlFilter);
//		for (int i = 0; i < owlFiles.length; i++) {
//			// load OWL files and dependencies into TDB repo
//			String actualUrl = sadlUtils.fileNameToFileUrl(owlFiles[i].getCanonicalPath());
//			String publicUri = getPublicUriFromActualUrl(actualUrl);
//			OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
//			model.getDocumentManager().setProcessImports(true);	// we don't want to do the import yet, just load the model
//			model.read(actualUrl);
//			
//	        // if the model is not in the repository, add it
//			if (!repoDataset.containsNamedModel(publicUri)) {
//	        	repoDataset.addNamedModel(publicUri, model);
//	        }
//		}
//		
//		// now delete the OWL files; they are no longer needed
//		for (int i = 0; i < owlFiles.length; i++) {
//			owlFiles[i].delete();
//		}
//		
//	}


	/**
	 * Call this method to get the repository type (format) from preferences
	 * @return
	 */
	public static String getOWLFormat() {
		IPreferencesService service = Platform.isRunning() ? Platform.getPreferencesService() : null;
		if (service != null) {
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
			return format;
		}
		else {
			return ConfigurationManager.RDF_XML_ABBREV_FORMAT;
		}
	}

    private boolean isOwlFileCreatedBySadl(File file) {
		try {
			String val = getSadlUtils().fileNameToFileUrl(file.getCanonicalPath());
			String key = null;
			if (getMappings().containsValue(val)) {
				Iterator<String> kitr = getMappings().keySet().iterator();
				while (kitr.hasNext()) {
					key = kitr.next();
					if (getMappings().get(key).equals(val)) {
						break;
					}
					key = null;
				}
				if (key != null) {
					Resource r = getMappingModel().getResource(key);
					StmtIterator sitr = getMappingModel().listStatements(null, publicUrlProp, r);
					if (sitr.hasNext()) {
						Resource ontSpec = sitr.nextStatement().getSubject();
						Statement stmt = ontSpec.getProperty(createdBy);
						if (stmt != null && stmt.getObject().equals(createdBySadlLiteral)) {
							return true;
						}
					}
				}
			}
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	protected void finalize() throws Throwable {
		
    }

	/**
	 * Call this method to set the mapping for the "defaults.owl" model. This should be called if a default is added
	 * to a model to make sure that the definition of default value concepts is available as an import model.
	 * 
	 * @throws IOException
	 * @throws URISyntaxException
	 * @throws ConfigurationException 
	 */
	public void setDefaultsAltUrlMapping() throws IOException, URISyntaxException, ConfigurationException {
		if (getModelFolderPath() != null) {
			String defaultsActual = getModelFolderPath().getAbsolutePath() + File.separator + ACUITY_DEFAULTS_OWL_FN;
			addMapping(getSadlUtils().fileNameToFileUrl(defaultsActual), ACUITY_DEFAULTS_URI, DEFAULTS_PREFIX, false, SADL);
			File defact = new File(defaultsActual);
			if (!defact.exists()) {
				if (!ResourceManager.copyDefaultsFileToOwlModelsDirectory(defaultsActual)) {
					throw new ConfigurationException("Unable to copy a 'defaults.owl' file from the plug-in to the OwlModels folder.");
				}
			}
		}
	}

	/**
	 * Call this method to set the mapping for the "SadServicesConfigurationConcepts.owl" model. This should be called if a default is added
	 * to a model to make sure that the definition of default value concepts is available as an import model.
	 * 
	 * @throws IOException
	 * @throws URISyntaxException
	 * @throws ConfigurationException 
	 */
	@Override
	public void setServicesConfigurationAltUrlMapping() throws IOException, URISyntaxException, ConfigurationException {
		if (getModelFolderPath() != null) {
			String servicesConfigurationActual = getModelFolderPath().getAbsolutePath() + File.separator + ServicesConfigurationConcepts_FN;
			addMapping(getSadlUtils().fileNameToFileUrl(servicesConfigurationActual), ServicesConfigurationURI, ServicesConfigurationPrefix, false, SADL);
			File defact = new File(servicesConfigurationActual);
			if (!defact.exists()) {
				if (!ResourceManager.copyServicesConfigurationFileToOwlModelsDirectory(servicesConfigurationActual)) {
					throw new ConfigurationException("Unable to copy a '" + ServicesConfigurationConcepts_FN + "' file from the plug-in to the OwlModels folder.");
				}
			}
		}
	}

	public void setProjectFolderPath(String _projectFolderPath, String _modelFolderPath) throws ConfigurationException {
		if (projectFolderPath != null && !projectFolderPath.equals(_projectFolderPath)) {
			// we are changing projects; reset the mappings and cached models
			System.out.println("Project changing from '" + projectFolderPath + "' to '" + _projectFolderPath + "'");
			OntDocumentManager.getInstance().reset(true);
			init(_modelFolderPath);
		}
		projectFolderPath = _projectFolderPath;
	}

	public String checkForDuplicateSadlFile(URI actualUri) throws URISyntaxException, MalformedURLException {
		String filename = actualUri.lastSegment();
		String actualPath = null;
		if (actualUri.isFile()) {
			actualPath = actualUri.toFileString(); //path();
		} else {
			actualPath = URI.createFileURI(actualUri.toString()).toFileString();
		}
		List<File> sadlFiles = ResourceManager.findSadlFilesInDir(new File(fileUrlToFileName(getProjectFolderPath())));
		for (int i = 0; i < sadlFiles.size(); i++) {
			File aFile = sadlFiles.get(i);
			try {
				if (aFile.getName().equals(filename) && !aFile.getCanonicalPath().equals(actualPath)) {
					return aFile.getCanonicalPath();
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		return null;
	}
	
	@Override
	public <T> T getClassInstance(String name, Class<? extends T> clazz)
			throws InstantiationException, IllegalAccessException,
			ClassNotFoundException {

		if (name == null) {
			throw new NullPointerException("Service class name cannot be null.");
		}
		
		if (clazz == null) {
			throw new NullPointerException("Service class API (inertface / abstract class) cannot be null.");
		}
		
		if (Platform.isRunning()) {
			final Object instance = Platform.getBundle("com.ge.research.sadl").loadClass(name).newInstance();
			return clazz.cast(instance);
		}
		
		return super.getClassInstance(name, clazz);
	}

	protected static ServiceLoader<ITranslator> getTranslatorsFromServiceLoader(Class<ITranslator> cls) {
		return ServiceLoader.load(cls);
	}

	protected static ServiceLoader<IReasoner> getReasonersFromServiceLoader(Class<IReasoner> cls) {
		return ServiceLoader.load(cls);
	}

	/**
	 * Method to get a list of all the available reasoners using a {@link ServiceLoader}
	 * @return A list of all available reasoners
	 */
	public static List<IReasoner> getAvailableReasoners() {
		List<IReasoner> reasoners = new ArrayList<IReasoner>();
		try {
			ServiceLoader<IReasoner> serviceLoader = getReasonersFromServiceLoader(IReasoner.class);
			if( serviceLoader != null ){
				for( Iterator<IReasoner> itr = serviceLoader.iterator(); itr.hasNext() ; ){
					try {
						reasoners.add(itr.next());
					}
					catch (Throwable t) {
						System.err.println("Error getting available reasoners: " + t.getMessage());
					}
				}
			}
		}
		catch (Throwable t) {
			System.err.println("Error getting available reasoners: " + t.getMessage());
		}
		return reasoners;
	}
	
	/**
	 * Method to get a list of all the available translators using a {@link ServiceLoader}
	 * @return A list of all available translators
	 */
	public static List<ITranslator> getAvailableTranslators() {
		List<ITranslator> translators = new ArrayList<ITranslator>();
		try {
			ServiceLoader<ITranslator> serviceLoader = getTranslatorsFromServiceLoader(ITranslator.class);
			if( serviceLoader != null ){
				for( Iterator<ITranslator> itr = serviceLoader.iterator(); itr.hasNext() ; ){
					try {
						translators.add(itr.next());
					}
					catch (Throwable t) {
						System.err.println("Error getting available translators: " + t.getMessage());
					}
				}
			}
		}
		catch (Throwable t) {
			System.err.println("Error getting available translators: " + t.getMessage());
		}
		return translators;
	}

	/**
	 * Call this method to validate that an OWL model being imported actually exists (is valid)
	 * @param publicUri
	 * @param altUrl
	 * @return
	 * @throws MalformedURLException 
	 */
	public boolean validateImport(String publicUri, String altUrl) throws MalformedURLException {
		if (getModelGetter().modelExists(publicUri, altUrl)) {
			return true;
		}
		return false;
	}

	public SadlUtils getSadlUtils() {
		if (sadlUtils   == null) {
			sadlUtils = new SadlUtils();
		}
		return sadlUtils;
	}

	@Override
	public boolean isSadlDerivedPublicUri(String publicUri) throws ConfigurationException, MalformedURLException {
		if (publicUri.endsWith("rdf-syntax-ns#") || 
				publicUri.endsWith("rdf-schema#")) {
			return false;
		}
		String altUrl = getAltUrlFromPublicUri(publicUri);
		if (altUrl !=  null && !altUrl.equals(publicUri)) {
			String altFN = fileUrlToFileName(altUrl);
			return isOwlFileCreatedBySadl(new File(altFN));
		}
//		else {
//			String auri = getPublicUriFromActualUrl(publicUri);
//			if (auri != null) {
//				return isSadlDerived(auri);
//			}
//		}
		return false;
	}


	@Override
	public boolean isSadlDerivedAltUrl(URI altUrl) throws ConfigurationException, MalformedURLException {
		if (altUrl.isPlatformPlugin()) {
			return false;
		}
		else if ("http".equals(altUrl.scheme())) {
			return false;
		}
		if (altUrl.fileExtension().equals(ResourceManager.OWLFILEEXT)) {
			String sadlFN = altUrl.trimFileExtension().appendFileExtension(ResourceManager.SADLEXT).lastSegment();
			List<File> sadlFiles = ResourceManager.findSadlFilesInDir(getModelFolderPath().getParentFile());
			if (sadlFiles != null) {
				for (int i = 0; i < sadlFiles.size(); i++) {
					if (sadlFiles.get(i).getName().equals(sadlFN)) {
						return true;
					}
				}
			}
		}
//		String altUrl = getAltUrlFromPublicUri(altUrl);
//		if (altUrl !=  null && !altUrl.equals(altUrl)) {
//			String altFN = getSadlUtils().fileUrlToFileName(altUrl);
//			return isOwlFileCreatedBySadl(new File(altFN));
//		}
//		else {
//			String auri = getPublicUriFromActualUrl(altUrl);
//			if (auri != null) {
//				return isSadlDerived(auri);
//			}
//		}
		return false;
	}


	public synchronized Map<String, String> getImports(String publicUri, Scope scope) throws ConfigurationException, IOException {
		OntModel theModel = getOntModel(publicUri, scope);
		if (theModel != null) {
			Ontology onto = theModel.getOntology(publicUri);
			if (onto == null) {
				ExtendedIterator<Ontology> ontoLst = theModel.listOntologies();
				if (ontoLst.hasNext()) {
					Map<String, String> map = new HashMap<String, String>();
					while (ontoLst.hasNext()) {
						onto = ontoLst.next();
						ExtendedIterator<OntResource> importsItr = onto.listImports();
						if (importsItr.hasNext()) {
							while (importsItr.hasNext()) {
								OntResource or = importsItr.next();
								String importUri = or.toString();
								String prefix = theModel.getNsURIPrefix(importUri);
								if (prefix == null) {
									prefix = getGlobalPrefix(importUri);
								}
								logger.debug("Ontology of model '" + publicUri + "' has import '" + importUri + "' with prefix '" + prefix + "'");
								if (!map.containsKey(importUri)) {
									map.put(importUri, prefix);
								}
							}
						}
					}
					return map;
				}
			}
			else {
				ExtendedIterator<OntResource> importsItr = onto.listImports();
				if (importsItr.hasNext()) {
					Map<String, String> map = new HashMap<String, String>();
					while (importsItr.hasNext()) {
						OntResource or = importsItr.next();
						String importUri = or.toString();
						String prefix = theModel.getNsURIPrefix(importUri);
						if (prefix == null) {
							prefix = getGlobalPrefix(importUri);
						}
						logger.debug("Ontology of model '" + publicUri + "' has import '" + importUri + "' with prefix '" + prefix + "'");
						if (!map.containsKey(importUri)) {
							map.put(importUri, prefix);
						}
					}
					return map;
				}
			}
		}
		return Collections.emptyMap();
	}
	
	@Override
	public List<ConceptName> getNamedConceptsInModel(String publicUri,
			ConceptType cType, Scope scope) throws InvalidNameException, ConfigurationException, IOException {
		OntModel theModel = getOntModel(publicUri, scope);
		if (theModel != null) {
			return getNamedConceptsInModel(theModel, publicUri, cType, scope);
		}
		return null;
	}


	public OntModel getOntModel(String publicUri, Scope scope) throws ConfigurationException, IOException {
		OntModel theModel = null;
		String altUrl = getAltUrlFromPublicUri(publicUri);
		if (repoType == null) {
	    	repoType = ConfigurationManagerForIDE.getOWLFormat();	
		    SadlJenaModelGetterPutter modelGetter = new SadlJenaModelGetterPutter(this, getTdbFolder(), repoType);
		    setModelGetter(modelGetter);
		}
		if (repoType != null && repoType.equals(IConfigurationManager.JENA_TDB)) {
			try {
				theModel = getModelGetter().getOntModel(publicUri, altUrl,
						IConfigurationManager.JENA_TDB);
			} catch (Throwable t) {
				// ok to fail; may not exist
			}
		} else {
			if (getModelGetter() == null) {
			    SadlJenaModelGetterPutter modelGetter = new SadlJenaModelGetterPutter(this, getTdbFolder(), repoType);
			    setModelGetter(modelGetter);
			}
			if (getModelGetter() != null) {
				boolean resetProcessImports = false;
				boolean processImports = OntDocumentManager.getInstance().getProcessImports();
				if (scope != null && scope.equals(Scope.LOCALONLY) && processImports) {
					OntDocumentManager.getInstance().setProcessImports(false);
					resetProcessImports = true;
				}
				else if (!processImports) {
					OntDocumentManager.getInstance().setProcessImports(true);
					resetProcessImports = true;
				}
				theModel = getModelGetter().getOntModel(publicUri, altUrl, repoType);
				if (resetProcessImports) {
					OntDocumentManager.getInstance().setProcessImports(processImports);
				}
			}
		}
		return theModel;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public URI getSadlUriFromPublicUri(ResourceSet resourceSet, URI publicUri)
			throws ConfigurationException, IOException {
		if (isSadlDerivedPublicUri(publicUri.toString())) {
			// TODO: Use ResourceManager#sadlFileNameOfOwlAltUrl
			IResourceDescriptions descriptions = resourceDescriptionsProvider.getResourceDescriptions(resourceSet);
			Iterable<IEObjectDescription> matchingModels = descriptions.getExportedObjects(SADLPackage.Literals.SADL_MODEL, QualifiedName.create(publicUri.toString()), false);
			Iterator<IEObjectDescription> it = matchingModels.iterator();
			if (it.hasNext()) {
				IEObjectDescription description = it.next();
				// This will be the URI of the SADL file
				return description.getEObjectURI().trimFragment();
			}
		}
		return null;
		
	}

	@Override
	public List<URI> getExternalModelURIs() {
		List<URI> externals = null;
		HashMap<String, String> map = getMappings();
		if (map != null) {
			Iterator<String> itr = map.keySet().iterator();
			while (itr.hasNext()) {
				String key = itr.next();
				String val = map.get(key);
				try {
					if (!isInProject(val)) {
						if (externals == null) {
							externals = new ArrayList<URI>();
						}
						URI proposed = URI.createURI(val);
						if (!externals.contains(proposed)) {
//							System.out.println("Mapping to external resource: " + key + ", " + val );
							externals.add(proposed);
						}
					}
				} catch (MalformedURLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return externals;
	}

	/**
	 * Method to determine if a resourcePath is in the current Project. This does not check the existence of the resource.
	 * @param resourcePath
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	private boolean isInProject(String resourcePath) throws MalformedURLException, IOException {
//		URI prjUri = ResourceManager.getProjectUri(URI.createURI(getModelFolder()));
		URI prjUri = URI.createFileURI(getModelFolder()).trimSegments(1);
		String rsrcFN = fileUrlToFileName(resourcePath);
		String prjFP = fileUrlToFileName(prjUri.toString());
		if (rsrcFN.startsWith(prjFP)) {
			return true;
		}
		return false;
	}

	private String getDefaultPolicyFileContent() {
		logger.debug("Getting default policy file content");
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version=\"1.0\"?>\n");
		sb.append("<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\" xmlns=\"http://jena.hpl.hp.com/schemas/2003/03/ont-manager#\" xml:base=\"http://jena.hpl.hp.com/schemas/2003/03/ont-manager#\">\n");
		sb.append("<OntologySpec>\n");
		sb.append("<language rdf:resource=\"http://www.w3.org/2002/07/owl\"/>\n");
		sb.append("<publicURI rdf:resource=\"http://www.w3.org/2002/07/owl\"/>\n");
		sb.append("<prefix rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">owl</prefix>\n");
		sb.append("</OntologySpec>\n");
		sb.append("<OntologySpec>\n");
		sb.append("<altURL rdf:resource=\"http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl\"/>\n");
		sb.append("<publicURI rdf:resource=\"http://purl.org/dc/elements/1.1/\"/>\n");
		sb.append("<language rdf:resource=\"http://www.w3.org/2002/07/owl\"/>\n");
		sb.append("<prefix rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">dc</prefix>\n");
		sb.append("</OntologySpec>\n");
		sb.append("<DocumentManagerPolicy>\n");
		sb.append("<cacheModels rdf:datatype=\"http://www.w3.org/2001/XMLSchema#boolean\">true</cacheModels>\n");
		sb.append("<processImports rdf:datatype=\"http://www.w3.org/2001/XMLSchema#boolean\">true</processImports>\n");
		sb.append("</DocumentManagerPolicy>\n");
		sb.append("</rdf:RDF>\n");
		return sb.toString();
	}

	public File resolveFilename(String fn) {
		File f = new File(fn);
		if (f.exists() && !f.isDirectory()) {
			// must be an absolute path
			return f;
		}
		// must be a project-relative path
		File modelFolder = getModelFolderPath();
		File prjFolder = modelFolder.getParentFile();
		String fullpath = ResourceManager.findFile(prjFolder.getAbsolutePath(), fn, null);
		if (fullpath != null) {
			f = new File(fullpath);
			if (f.exists() && !f.isDirectory()) {
				return f;
			}
		}
		return null;
	}

	@Override
	public boolean setTranslatorClassName(String translatorClassName)
			throws ConfigurationException {
		if (translatorClassName != null) {
			// delete the ImplicitModels/SadlBuiltinFunctions.sadl file (it will be rebuilt elsewhere)
			try {
				String sbffn = getModelFolderPath().getParentFile().getCanonicalPath() + "/" + SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/" + SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME;
				File sbff = new File(sbffn);
				if (sbff.exists()) {
					sbff.delete();
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return super.setTranslatorClassName(translatorClassName);
	}
}
