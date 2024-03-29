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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Ontology;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;
import org.eclipse.xtext.util.StringInputStream;

import com.ge.research.sadl.external.XMLHelper;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.persistence.SadlPersistenceFormat;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.SADLPackage;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.common.base.Optional;
import com.google.inject.Inject;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper;

/**
 * this class extension supports configuration tasks unique to the IDE (development environment)
 * @author 200005201
 *
 */
public class ConfigurationManagerForIDE extends ConfigurationManagerForEditing implements IConfigurationManagerForIDE {
	
	public static class Provider implements javax.inject.Provider<IConfigurationManagerForIDE> {
		private String modelFolder;
		private String repoType = ConfigurationManagerForIDE.getPersistenceFormatFromPreferences();
		//TODO: AG
//		private String repoURL = ConfigurationManagerForIDE.getPersistenceSemTKServerUrlFromPreferences();

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
	
	Map<String, Object> privateKeyValueStore = null;	
	
	boolean closing = false;
	private SadlUtils sadlUtils = new SadlUtils();
	@Inject
	private ResourceDescriptionsProvider resourceDescriptionsProvider;

	private Property sadlSourceURLProp = null;;
	
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
			if (mapping.length > 3) {
				if (addSadlSourceUri(mapping[1], mapping[3])) {
					bChanged = true;
				}
			}
			else {
				if (removeSadlSourceUri(mapping[1])) {
					bChanged = true;
				}
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
		if (!source.equals(SADL)) {
			if (removeSadlSourceUri(publicUri)) {
				bChanged = true;
			}
		}
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
	 * Method to add the sadlSourceURL information to the mapping
	 * @param string
	 * @param string2
	 */
	private boolean addSadlSourceUri(String pubUri, String sadlSourceURL) {
		logger.debug("adding sadlSourcURL to mapping: " + pubUri + " --> " + sadlSourceURL);
    	StmtIterator pubitr = getMappingModel().listStatements(null, publicUrlProp, getMappingModel().getResource(pubUri));
    	if (pubitr.hasNext()) {
    		Statement s = pubitr.next();
    		StmtIterator ssuitr = getMappingModel().listStatements(s.getSubject(), getSadlSourceURLProp(), (RDFNode)null);
    		if (ssuitr.hasNext()) {
    			Statement ssustmt = ssuitr.next();
    			ssuitr.close();
    			getMappingModel().remove(ssustmt);
    		}
    		getMappingModel().add(s.getSubject(), getSadlSourceURLProp(), getMappingModel().createTypedLiteral(sadlSourceURL));
    		pubitr.close();
    		return true;
    	}
		return false;
	}
	
	/**
	 * Method to remove a sadlSourceURL property value from a mapping
	 * @param pubUri
	 * @return
	 */
	private boolean removeSadlSourceUri(String pubUri) {
		logger.debug("removing sadlSourcURL from mapping: " + pubUri);
    	StmtIterator pubitr = getMappingModel().listStatements(null, publicUrlProp, getMappingModel().getResource(pubUri));
    	if (pubitr.hasNext()) {
    		Statement s = pubitr.next();
    		StmtIterator ssuitr = getMappingModel().listStatements(s.getSubject(), getSadlSourceURLProp(), (RDFNode)null);
    		if (ssuitr.hasNext()) {
    			Statement ssustmt = ssuitr.next();
    			ssuitr.close();
    			getMappingModel().remove(ssustmt);
         		pubitr.close();
        		return true;
    		}
    	}
		return false;
	}
	
	/**
	 * Method to get the SADL source URL from the mapping file
	 * @param pubUri
	 * @return
	 */
	private String getSadlSourceUriFromMappings(String pubUri) {
		logger.debug("getting sadlSourcURL from mapping: " + pubUri );
    	StmtIterator pubitr = getMappingModel().listStatements(null, publicUrlProp, getMappingModel().getResource(pubUri));
    	if (pubitr.hasNext()) {
     		List<Statement> pendingDeletions = null;
    		Statement s = pubitr.next();
    		StmtIterator ssuitr = getMappingModel().listStatements(s.getSubject(), getSadlSourceURLProp(), (RDFNode)null);
    		if (ssuitr.hasNext()) {
    			Statement ssustmt = ssuitr.next();
    			return ssustmt.getObject().toString();
    		}
    	}
    	return null;
	}
	
	/**
	 * Method to get the sadlSourceURL property
	 * @return
	 */
	private Property getSadlSourceURLProp() {
		if (sadlSourceURLProp == null) {
    		sadlSourceURLProp = getMappingModel().createProperty(ONT_MANAGER_SADL_SOURCE);
		}
		return sadlSourceURLProp;
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
    				String sadlfile = ResourceManager.findSadlFilesInDir(owlfile.getParentFile().getParent(), sadlFileName);
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
        		sadlSourceURLProp = getMappingModel().createProperty(ONT_MANAGER_SADL_SOURCE);
        		altUrlProp = getMappingModel().createProperty(ONT_MANAGER_ALT_URL);
        		publicUrlProp = getMappingModel().createProperty(ONT_MANAGER_PUBLIC_URI);
        		prefixProp = getMappingModel().createProperty(ONT_MANAGER_PREFIX);
        	}
    		org.apache.jena.rdf.model.Resource type = getMappingModel().createResource(ONT_MANAGER_ONTOLOGY_SPEC);
    		org.apache.jena.rdf.model.Resource newOntSpec = getMappingModel().createResource(type);
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
			e.printStackTrace();
		} catch (ConfigurationException e) {
			e.printStackTrace();
		} catch (IOException e) {
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
	 * Call this method to get the repository type (format) from preferences.
	 * This method used to be called getOWLFormat
	 * @return
	 */
	public static String getPersistenceFormatFromPreferences() {
		IPreferencesService service = Platform.isRunning() ? Platform.getPreferencesService() : null;
		if (service != null) {
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT, null);
			return format;
		}
		else {
			return SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT;
		}
	}

	
	//TODO: AG: 
//	/**
//	 * Call this method to get the SemTK server type (e.g. Fuseki) from preferences
//	 * @return
//	 */
//	public static String getPersistenceSemTKServerTypeFromPreferences() {
//		IPreferencesService service = Platform.isRunning() ? Platform.getPreferencesService() : null;
//		if (service != null) {
//			String serverType = service.getString("com.ge.research.sadl.Sadl", "storeType", SadlPersistenceFormat.SEMTK_STORE_TYPE, null);
//			return serverType;
//		}
//		else {
//			return SadlPersistenceFormat.SEMTK_STORE_TYPE;
//		}
//	}
//
//	/**
//	 * Call this method to get the SemTK server type (e.g. Fuseki) from preferences
//	 * @return
//	 */
//	public static String getPersistenceSemTKServerUrlFromPreferences() {
//		IPreferencesService service = Platform.isRunning() ? Platform.getPreferencesService() : null;
//		if (service != null) {
//			String serverType = service.getString("com.ge.research.sadl.Sadl", "endpoint", SadlPersistenceFormat.SEMTK_ENDPOINT, null);
//			return serverType;
//		}
//		else {
//			return SadlPersistenceFormat.SEMTK_ENDPOINT;
//		}
//	}
	
	
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
			e.printStackTrace();
		} catch (IOException e) {
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
			try {
				final Object instance = Platform.getBundle("com.ge.research.sadl").loadClass(name).getDeclaredConstructor().newInstance();
				return clazz.cast(instance);
			} catch (InvocationTargetException | NoSuchMethodException e) {
				// Wrap and throw a CNFE
				throw new ClassNotFoundException("Cannot find service class for name: " + name + " for service API " + clazz + ".", e);
			}
		}
		
		return super.getClassInstance(name, clazz);
	}

	protected static ServiceLoader<ITranslator> getTranslatorsFromServiceLoader(Class<ITranslator> cls) {
		return ServiceLoader.load(cls);
	}

	protected static ServiceLoader<IReasoner> getReasonersFromServiceLoader(Class<IReasoner> cls) {
		return ServiceLoader.load(cls);
	}
	
	protected static ServiceLoader<ITypedBuiltinFunctionHelper> getTypedBuiltinFunctionHelpersFromService(Class<ITypedBuiltinFunctionHelper> cls) {
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
	 * Method to get a list of all the available implementations of ITypedBuiltinFunctionHelper using a {@link ServiceLoader}
	 * @return A list of all available implementations of ITypedBuiltinFunctionHelper
	 */
	public static List<ITypedBuiltinFunctionHelper> getAvailableTypedBuiltinFunctionHelpers() {
		List<ITypedBuiltinFunctionHelper> tbfHelpers = new ArrayList<ITypedBuiltinFunctionHelper>();
		try {
			ServiceLoader<ITypedBuiltinFunctionHelper> serviceLoader = getTypedBuiltinFunctionHelpersFromService(ITypedBuiltinFunctionHelper.class);
			if( serviceLoader != null ){
				for( Iterator<ITypedBuiltinFunctionHelper> itr = serviceLoader.iterator(); itr.hasNext() ; ){
					try {
						tbfHelpers.add(itr.next());
					}
					catch (Throwable t) {
						System.err.println("Error getting available UnittedQuantity handlers: " + t.getMessage());
					}
				}
			}
		}
		catch (Throwable t) {
			System.err.println("Error getting available UnittedQuantity handlers: " + t.getMessage());
		}
		return tbfHelpers;
	}

	/**
	 * Call this method to validate that an OWL model being imported actually exists (is valid)
	 * @param publicUri
	 * @param altUrl
	 * @return
	 * @throws IOException 
	 * @throws TranslationException 
	 * @throws ConfigurationException 
	 */
	public boolean validateImport(String publicUri, String altUrl) throws ConfigurationException, TranslationException, IOException {
		if (getSadlModelGetter(null).modelExists(publicUri)) {
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


	public synchronized Map<String, String> getImports(String publicUri, Scope scope) throws ConfigurationException, IOException, TranslationException {
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
			ConceptType cType, Scope scope) throws InvalidNameException, ConfigurationException, IOException, TranslationException {
		OntModel theModel = getOntModel(publicUri, scope);
		if (theModel != null) {
			return getNamedConceptsInModel(theModel, publicUri, cType, scope);
		}
		return null;
	}


	public OntModel getOntModel(String publicUri, Scope scope) throws ConfigurationException, IOException, TranslationException {
		OntModel theModel = null;
		String altUrl = getAltUrlFromPublicUri(publicUri);
		if (getRepoType() == null) {
	    	setRepoType(ConfigurationManagerForIDE.getPersistenceFormatFromPreferences());	
		}
		if (getRepoType() != null && getRepoType().equals(SadlPersistenceFormat.JENA_TDB_FORMAT)) {
			try {
				theModel = getSadlModelGetter(null).getOntModel(publicUri);
			} catch (Throwable t) {
				// ok to fail; may not exist
			}
		} else {
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
			theModel = getSadlModelGetter(null).getOntModel(publicUri);
			if (resetProcessImports) {
				OntDocumentManager.getInstance().setProcessImports(processImports);
			}
		}
		return theModel;
	}

	public OntModel getOntModel(String publicUri, String serializedGraph, String format) {
		OntModel theModel = null;
		try {
			theModel = getSadlModelGetter(null).getOntModel(publicUri, serializedGraph, format);
		} catch (Throwable t) {
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
			String ssUrl = getSadlSourceUriFromMappings(publicUri.toString());
			if (ssUrl != null) {
				return URI.createURI(ssUrl);
			}
			// TODO: Use ResourceManager#sadlFileNameOfOwlAltUrl
			if (resourceDescriptionsProvider != null) {
				IResourceDescriptions descriptions = resourceDescriptionsProvider.getResourceDescriptions(resourceSet);
				Iterable<IEObjectDescription> matchingModels = descriptions.getExportedObjects(SADLPackage.Literals.SADL_MODEL, QualifiedName.create(publicUri.toString()), false);
				Iterator<IEObjectDescription> it = matchingModels.iterator();
				if (it.hasNext()) {
					IEObjectDescription description = it.next();
					// This will be the URI of the SADL file
					return description.getEObjectURI().trimFragment();
				}
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
					e.printStackTrace();
				} catch (IOException e) {
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
				e.printStackTrace();
			}
		}
		return super.setTranslatorClassName(translatorClassName);
	}

	@Override
	public String getBaseUriFromOwlFile(String owlFilename) {
		OntModel om = loadOntModel(owlFilename);
		return getBaseUriFromOwlModel(owlFilename, om);
	}

	
	@Override
	public String getBaseUriFromOwlModel(String owlFilename, OntModel om) {
		String uriForEmptyString = om.getNsPrefixURI("");
		if (uriForEmptyString != null && uriForEmptyString.endsWith("#")) {
			return uriForEmptyString.substring(0, uriForEmptyString.length() - 1);
		}
		if (owlFilename != null) {
			try {
				Optional<String> buri = new XMLHelper().tryReadBaseUri(new SadlUtils().fileToString(new File(owlFilename)));
				if (buri.isPresent()) {
					uriForEmptyString = buri.get();
					return uriForEmptyString.endsWith("#") ? uriForEmptyString.substring(0, uriForEmptyString.length() - 1) : uriForEmptyString;
	
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return uriForEmptyString;
	}

	@Override
	public OntModel loadOntModel(String owlFilename) {
		try {
			FileInputStream is = new FileInputStream(new File(owlFilename));
			return initOntModel(is, getOwlFormatFromFile(owlFilename));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		OntModel ontModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		boolean savePI = ontModel.getDocumentManager().getProcessImports();
		ontModel.getDocumentManager().setProcessImports(false);
		try {
			ontModel.read(owlFilename, getOwlFormatFromFile(owlFilename));
		}
		finally {
			ontModel.getDocumentManager().setProcessImports(savePI);
		}
		return ontModel;
	}

	@Override
	public OntModel loadOntModel(String owlFilename, boolean loadImports) {
		try {
			FileInputStream is = new FileInputStream(new File(owlFilename));
			return initOntModel(is, getOwlFormatFromFile(owlFilename), loadImports);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		OntModel ontModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		boolean savePI = ontModel.getDocumentManager().getProcessImports();
		ontModel.getDocumentManager().setProcessImports(loadImports);
		try {
			ontModel.read(owlFilename, getOwlFormatFromFile(owlFilename));
		}
		finally {
			ontModel.getDocumentManager().setProcessImports(savePI);
		}
		return ontModel;
	}

	public OntModel initOntModel(InputStream is, String format) {
		return initOntModel(is, format, false);
	}


	public OntModel initOntModel(InputStream is, String format, boolean loadImports) {
		getOntModelSpec(null).setDocumentManager(getJenaDocumentMgr());
		OntModel ontModel = ModelFactory.createOntologyModel(getOntModelSpec(null));
		boolean savePI = ontModel.getDocumentManager().getProcessImports();
		ontModel.getDocumentManager().setProcessImports(loadImports);
		try {
			ontModel.read(is, null, format);
		}
		finally {
			ontModel.getDocumentManager().setProcessImports(savePI);
		}
		return ontModel;
	}

	private String getOwlFormatFromFile(String owlFilename) {
		return SadlPersistenceFormat.getSadlPersistenceFormatFromFilename(owlFilename);
	}
	
	public void addPrivateKeyValuePair(String key, Object value) {
		if (privateKeyValueStore == null) {
			privateKeyValueStore = new HashMap<String, Object>();
		}
		if (value != null) {
			privateKeyValueStore.put(key, value);
		}
		else if (privateKeyValueStore.containsKey(key)) {
			privateKeyValueStore.remove(key);
		}
	}
	
	public Object getPrivateKeyValuePair(String key) {
		if (privateKeyValueStore != null) {
			Object val = privateKeyValueStore.get(key);
			return val;
		}
		return null;
	}

	@Override
	public synchronized void addPrivateKeyMapValueByResource(String key, URI rsrcUri,
			Object value) {
		if (rsrcUri != null) {
			Object map = getPrivateKeyValuePair(key);
			if (value != null && map == null) {
				map = new HashMap<Resource,Object>();
				addPrivateKeyValuePair(key, map);
			}
			if (map instanceof Map<?,?>) {
				if (value != null) {
					((Map<URI,Object>)map).put(rsrcUri, value);
				}
				else {
					((Map<URI,Object>)map).remove(rsrcUri);
				}
			}
		}
	}

	@Override
	public synchronized Object getPrivateKeyMapValueByResource(String key, URI rsrcUri) {
		if (rsrcUri != null) {
			Object map = getPrivateKeyValuePair(key);
			if (map != null && map instanceof Map<?,?>) {
				return (((Map<URI,Object>)map).get(rsrcUri));
			}
		}
		return null;
	}
	
	/**
	 * Method to remove all non-existent mappings with specified sources
	 * @param sources -- the sources to be removed if non-existent
	 */
	public void cleanNonExisting(List<String> sources) {
		try {
			String prjname = new File(getProjectFolderPath()).getName();
			List<Statement> toBeRemoved = new ArrayList<Statement>();
			Model mm = getMappingModel();
			StmtIterator apitr = mm.listStatements(null, altUrlProp, (RDFNode)null);
			while (apitr.hasNext()) {
				Statement stmt = apitr.nextStatement();
				RDFNode altUrl = stmt.getObject();
				if (altUrl.isURIResource()) {
					String altUrlUrl = altUrl.asResource().getURI();
					int prjloc = altUrlUrl.lastIndexOf(prjname);
					if (prjloc > 0) {
						prjloc = prjloc + prjname.length() + 1;
						altUrlUrl = getProjectFolderPath() + altUrlUrl.substring(prjloc);
					}
					try {
						String fn = (new SadlUtils()).fileUrlToFileName(altUrlUrl);
						File fnf = new File(fn);
						if (!fnf.exists()) {
							StmtIterator cbitr = mm.listStatements(stmt.getSubject(), createdBy, (RDFNode)null);
							while (cbitr.hasNext()) {
								String cbstr = cbitr.nextStatement().getObject().toString();
								if (sources.contains(cbstr)) {
									StmtIterator delitr = mm.listStatements(stmt.getSubject(), null, (RDFNode)null);
									while (delitr.hasNext()) {
										toBeRemoved.add(delitr.nextStatement());
									}
								}
							}
						}
					} catch (MalformedURLException e) {
						e.printStackTrace();
					}
				}
			}
			if (toBeRemoved.size() > 0 ) {
				mm.remove(toBeRemoved);
				setMappingChanged(true);
				super.saveOntPolicyFile();
			}
		} catch (URISyntaxException e1) {
			e1.printStackTrace();
		}
	}

	@Override
	public boolean cleanTdbFolder() {
		String tdbfolder;
		try {
			tdbfolder = getTdbFolder();
			File tdbf = new File(tdbfolder);
			if (tdbf.exists()) {
				cleanContents(tdbf);
				tdbf.delete();
			}
			return true;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	private void cleanContents(File dir) {
		File[] contents = dir.listFiles();
		if (contents != null) {
			for (File c : contents) {
				if (c.isDirectory()) {
					cleanContents(c);
				}
				c.delete();
			}
		}
	}

	@Override
	public boolean addProjectDependencies(List<java.net.URI> projectDependencies) throws ConfigurationException {
		if (logger.isDebugEnabled()) {
			File mfp = getModelFolderPath();
			String thisProjPath = mfp.getParent();
			for (java.net.URI pduri : projectDependencies) {
				logger.debug("Project '" + thisProjPath + "' depends on '" + pduri.toString());
			}
		}
		String[] categoryHierarchy = PROJECT_DEPENDENCIES_CATEGORY;
		
		List<ConfigurationItem> configItems = getConfiguration(categoryHierarchy, false);
		ConfigurationItem configItem;
		if (configItems != null) {
			configItem = configItems.get(0);
		}
		else {
			configItem = new ConfigurationItem(categoryHierarchy);
		}
		boolean bChanged = false;
		List<NameValuePair> nvps = configItem.getNameValuePairs();
		if (nvps.size() != projectDependencies.size()) {
			bChanged = true;
		}
		if (!bChanged) {
			List<String> dpStrings = new ArrayList<String>();
			for (java.net.URI pd : projectDependencies) {
				dpStrings.add(pd.toString());
			}
			for (NameValuePair nvp : nvps) {
				String nvpv = nvp.getValue().toString();
				if (!dpStrings.contains(nvpv)) {
					bChanged = true;
					break;
				}
			}
		}
		if (bChanged) {
			clearOtherProjectConfigMgrs();
			configItem.clearNameValuePairs();
			if (projectDependencies != null) {
				for (java.net.URI pduri : projectDependencies) {
					ConfigurationItem.NameValuePair nv = configItem.new NameValuePair(pdependsOn, pduri.toString());
					configItem.addNameValuePair(nv);
				}
			}
			updateConfiguration(configItem);;
			saveConfiguration();
		}
		return true;
	}
	
	@Override
	public OntDocumentManager getJenaDocumentMgr() {
		if (jenaDocumentMgr == null) {
			if (getMappingModel() != null) {
				setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
				if (getOntModelSpec(null) != null) {
					getOntModelSpec(null).setDocumentManager(getJenaDocumentMgr());
				}
			}
			else {
				setJenaDocumentMgr(OntDocumentManager.getInstance());
			}
		}
		return jenaDocumentMgr;
	}
}
