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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
//import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.FileUtils;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;

/**
 * this class extension supports configuration tasks unique to the IDE (development environment)
 * @author 200005201
 *
 */
public class ConfigurationManagerForIDE extends ConfigurationManagerForEditing 
	implements IConfigurationManagerForIDE, IResourceChangeListener, IPropertyChangeListener {
	
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
		if (ResourcesPlugin.getPlugin() != null) {
			ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
		}
	}
	
   /**
     * Call with a list of SADL files to see if all are mapped and if not add the mappings
     * 
     * @param sadlFiles
     * @return - true if mappings are added (changes occur)
     * @throws CoreException 
     * @throws IOException 
     */
    private synchronized boolean setMappingsFromProjectFiles(List<File> sadlFiles) throws CoreException, IOException {
    	boolean bChange = false;
        for (int i = 0; sadlFiles != null && i < sadlFiles.size(); i++) {
        	File sadlfile = sadlFiles.get(i);
        	try {
	        	URI altUrl = ResourceManager.validateAndReturnOwlUrlOfSadlUri(URI.createFileURI(sadlfile.getAbsolutePath()));
	        	String publicUri = ResourceManager.getModelNameFromSadlFile(sadlfile);
	        	if (publicUri != null) {
	        		if (addMapping(altUrl.toString(), publicUri, null, true, SADL)) {
	        			bChange = true;
	        		}
	        	}
        	}
        	catch (Exception e) {
        		throw new IOException("Error finding mappings for SADL file '" + sadlfile.getAbsolutePath() + "'", e);
        	}
        }

    	return bChange;
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
    					subj.addProperty(publicUrlProp, pubv);
    					bChanged = true;
    					
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
     * Method to clean a SADL Project as a result of a clean event. Cleaning consists of the following steps:
     *  1) Identify all SADL files currently in the project with their relative paths
     *  2) Remove all OWL files in the OwlModels folder which are not associated with a Project SADL file but are marked as created by SADL
     *  3) Remove all SADL-generated entries in the Jena OwlModels/ont-policy.rdf file of mappings that do not correspond to a project SADL file
     *  
     *  Modifications for Version 2.3.0:
     *  1) No longer necessary to 1, 2, or 3 as the deltas in the SadlBuilder will identify deleted files and they are cleaned up there.
     *
     * @param folder
	 * @throws ConfigurationException 
     */
	public void cleanProject(IProject project, IFolder folder) throws ConfigurationException {
		// get a list of all SADL files in project
		try {
			IProject[] referencedProjects = project.getReferencedProjects();
			if (referencedProjects != null && referencedProjects.length > 0) {
				System.out.println("Project '" + project.getName() + "' depends on:");
				for (int i = 0; referencedProjects != null && i < referencedProjects.length; i++) {
					System.out.println("  Project " + referencedProjects[i].getName());
					System.out.println("    at location: " + referencedProjects[i].getLocation());
				}
			}
		} catch (CoreException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
        File projectDir = folder.getRawLocation().removeLastSegments(1).makeAbsolute().toFile(); //policyResource.getRawLocation().removeLastSegments(2).makeAbsolute().toFile();
		List<File> sadlFiles = null;
		if (projectDir.isDirectory()) {
			sadlFiles = ResourceManager.findSadlFilesInDir(projectDir);
		}

		// get a list of files in OwlModels directory and delete any "extra" OWL files not corresponding to a SADL file but created by SADL [in the past]
		String servicesConfigConcepts = null;
		File owlModelDir = folder.getRawLocation().makeAbsolute().toFile(); //policyResource.getRawLocation().removeLastSegments(1).toFile();
		File[] owlModelDirFiles = owlModelDir.listFiles();
		List<String> goodOwlFilenames = new ArrayList<String>();
		Iterator<File> sfiter = sadlFiles.iterator();
		while (sfiter.hasNext()) {
			File sadlFile = sfiter.next();
			String sadlFileName = sadlFile.getName();
			// get the OWL filename by changing the extension from .sadl to {.owl, .n3, or .nt}
			String owlFileName = sadlFileName.substring(0, sadlFileName.lastIndexOf('.') + 1) + ResourceManager.getOwlFileExtension();
			if (goodOwlFilenames.contains(owlFileName)) {
				// this must be a duplicate name!
				String errMsg = "There appear to be multiple files named '" + sadlFileName + "' in the project. This is not supported.";
				logger.error(errMsg);
			}
			for (int i = 0; owlModelDirFiles != null && i < owlModelDirFiles.length; i++) {
				if (owlModelDirFiles[i] != null) {
					if (owlModelDirFiles[i].getName().equals(owlFileName)) {
						// this is a "good" OWL file--it corresponds to an existing SADL file
						String goodOwlFile = owlModelDirFiles[i].getName();
						goodOwlFilenames.add(goodOwlFile);
						owlModelDirFiles[i] = null;
					}
					else if (owlModelDirFiles[i].getName().equals(ResourceManager.ACUITY_DEFAULTS_OWL_FN)) {
						owlModelDirFiles[i] = null;
					}
					else if (owlModelDirFiles[i].getName().equals(ResourceManager.ServicesConfigurationConcepts_FN)) {
						// keep this if there is also a SADL services config file
						File scf = new File(owlModelDirFiles[i].getParent() + File.separator + ResourceManager.ServicesConf_FN);
						if (scf.exists()) {
							servicesConfigConcepts = owlModelDirFiles[i].getAbsolutePath();
							owlModelDirFiles[i] = null;
						}
					}
					else if (!owlModelDirFiles[i].getName().endsWith(ResourceManager.getOwlFileExtensionWithPrefix())) {
						// THIS HAS TO BE AFTER THE SPECIFIC FILE NAME CHECKS ABOVE
						// this is not an OWL file in current format (doesn't end with {.owl, .n3, or .nt})
						// is it in a different format, previously used?
						String ofn = owlModelDirFiles[i].getName();
						if (!ofn.endsWith(".owl") && !ofn.endsWith(".n3") && !ofn.endsWith(".nt")) {
							owlModelDirFiles[i] = null;
						}
					}
				}
			}
		}
		
		// now delete any OWL files in the OwlModels directory that are not associated with a SADL file

		// TODO make this only do files with "created by SADL" entries in the ont-policy.rdf file
		//  (these are presumably obsolete OWL files that were at one time associated with a SADL file that no longer exists)
		for (int i = 0; owlModelDirFiles != null && i < owlModelDirFiles.length; i++) {
			if (owlModelDirFiles[i] != null) {
				if (!isOwlFileCreatedBySadl(owlModelDirFiles[i])) {
					if (!owlModelDirFiles[i].getName().equals(IConfigurationManager.CONFIG_FILENAME) && 
							!owlModelDirFiles[i].getName().equals(IConfigurationManager.ONT_POLICY_RDF)) {
						try {
							owlModelDirFiles[i].delete();
						}
						catch (Exception e) {
							logger.debug("Failed to delete OWL file '" + owlModelDirFiles[i].getAbsolutePath() + "': " + e.getMessage());
						}
					}
				}
			}
		}

		// Now we are ready to update the ont-policy.rdf file. This is done by:
		//	1) reading in the RDF model
		//	2) removing all entries created by SADL that are not in the goodOwlFilenames list
		//  3) go through the list of all SADL files and make sure there is mapping in the policy file for that model
        String configPath = folder.getRawLocation().makeAbsolute().addTrailingSeparator().append(ONT_POLICY_RDF).toPortableString(); // policyResource.getRawLocation().toPortableString();
		cleanAndPopulatePolicyFile(configPath, sadlFiles);
		if (servicesConfigConcepts != null) {
			String gp = "SadlServicesConfigurationConcepts";
			try {
				addMapping(getSadlUtils().fileNameToFileUrl(servicesConfigConcepts),
						IConfigurationManager.ServicesConfigurationURI,
						IConfigurationManager.ServicesConfigurationPrefix, false, SADL);
				addJenaMapping(
						ResourceManager.ServicesConfigurationURI,
						sadlUtils.fileNameToFileUrl(servicesConfigConcepts));
				addGlobalPrefix(
						ResourceManager.ServicesConfigurationURI,
						gp);
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
    	String format = getOWLFormat();
    	
    	// this can always be done
    	getModelGetter().close();
    	
//    	File tdbFolder;
		try {
//			tdbFolder = new File(getTdbFolder());
	    	if (format.equals(IConfigurationManager.JENA_TDB)) {
	    		getModelGetter().setTdbFolder(getTdbFolder());
//	    		for (int i = 0; i < goodOwlFilenames.size(); i++) {
//	    			File ftd = new File(owlModelDir + File.separator + goodOwlFilenames.get(i));
//	    			if (ftd.exists()) {
//	    				if (!ftd.delete()) {
//	    					logger.warn("Failed to delete OWL file '" + goodOwlFilenames.get(i) + "'; should not be needed for TDB-backed repository.");
//	    				}
//	    			}
//	    		}
	    	}
//	    	else {
//		    	if (tdbFolder.exists()) {
//		    		if (!tdbFolder.delete()) {
//			    		getModelGetter().setTdbFolder(getTdbFolder());
//		    			((SadlJenaModelGetterPutter) getModelGetter()).clean();
//		    			getModelGetter().close();
//		    			tdbFolder.delete();
//		    		}
//		    	}
//	    		if (tdbFolder.exists()) {
//	    			logger.warn("Format is not TDB but unable to delete existing TDB folder");
//	    		}
//	    	}
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

    		
    	// refresh Eclipse resources in the OwlModels directly	     
        try {
			folder.refreshLocal(IResource.DEPTH_ONE, null);
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

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
        try {
        	if (ResourcesPlugin.getPlugin() != null) {
        		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
        	}

        } finally {
            super.finalize();
        }
    }

    /**
     * Clean up mappings file by deleting all SADL-generated entries that aren't still good files
     * 
     * @param configFilename
     * @param sadlFiles 
     */
	public synchronized void cleanAndPopulatePolicyFile(String configFilename, List<File> sadlFiles) {
        String uri = null ;
        InputStream in = null;
        try {
            Object[] returned = findInputStream(configFilename);
            if (returned != null && returned[0] != null) {
                in =  (InputStream) returned[0];
                uri = (String) returned[1];
                logger.debug("Found existing ont-policy file at '" + uri + "'; ready to clean file.");
	            if (in != null) {	            	
	                boolean bChanged = false;
	                setMappingModel(ModelFactory.createDefaultModel()) ;
	                String syntax = FileUtils.guessLang(uri);
	                getMappingModel().read( in, null, syntax );
	                
	                // List subjects will be a list of all of the entries created by SADL
	                List<com.hp.hpl.jena.rdf.model.Resource> subjects = new ArrayList<com.hp.hpl.jena.rdf.model.Resource>();
	                // add all entries to subjects List that were created by SADL
	                StmtIterator sitr = getMappingModel().listStatements(null, createdBy, sadlNode);
	                while (sitr.hasNext()) {
	                    Statement s = sitr.nextStatement();
	                    subjects.add(s.getSubject());
	                }
	                
	                // for each Resource in subjects List, check if the altUrl is valid and if not
	                //  remove all triples for that subject
	                //  Note: there should not be multiple entries for the same altUrl as the adding method
	                //   should check and make sure that never happens
	                //  Note: there should also not be multiple subjects with the same altUrl.
	                for (int i = 0; subjects != null && i < subjects.size(); i++) {
	                	com.hp.hpl.jena.rdf.model.Resource subject = subjects.get(i);
	                	Statement altStmt = subject.getProperty(altUrlProp);
	                	boolean subjectValid = true;
	                	if (altStmt == null) {
	                		subjectValid = false;
	                	}
	                	else {
		                	RDFNode altUrl = altStmt.getObject();
		                	// now make sure that there is an actual OWL file at this altUrl; if not delete the entry as it is obsolete
		                	String fn;
		                	try {
		                		fn =fileUrlToFileName(altUrl.toString());
			                	File altFn = new File(fn);
			                	if (altFn.exists() ) {
			                		// now make sure there aren't any old entries for this file but never delete a mapping for defaults.owl as 
			                		//	this will cause build problems if it is still used
			                		if (!altFn.getName().equals(IConfigurationManager.ACUITY_DEFAULTS_OWL_FN)) {	
				                		StmtIterator sitr2 = getMappingModel().listStatements(null, altUrlProp, altUrl);
				                		int cnt = 0;
				                		List<Statement> extras = null;
				                		while (sitr2.hasNext()) {
			                				Statement badStmt = sitr2.nextStatement();
				                			if (cnt > 0) {
				                				Resource badSubject = badStmt.getSubject();
				                				if (extras == null) {
				                					extras = new ArrayList<Statement>();
				                				}
				                				StmtIterator sitr3 = getMappingModel().listStatements(badSubject, (Property)null, (RDFNode)null);
				                				while (sitr3.hasNext()) {
				                					extras.add(sitr3.nextStatement());
				                				}
				                			}
				                			cnt++;
				                		}
				                		if (extras != null) {
				                			for (int j = 0; j < extras.size(); j++) {
				                				getMappingModel().remove(extras.get(j));
				                				bChanged = true;
				                			}
				                		}
			                		}
			                	}
			                	else {
			                		subjectValid = false;
			                	}
		                	}
		                	catch (Exception e) {
		                		subjectValid = false;
		                	}
	                	}
	                	if (!subjectValid) {
		                	getMappingModel().removeAll(subject, null, null);
				            bChanged = true;
		               	}
	                }
	                
	                // now make sure that every SADL file has an entry in the policy model
	                boolean additions = setMappingsFromProjectFiles(sadlFiles);
	                
	                if (bChanged || additions) {
	                    FileOutputStream fps = null;
	                    try {
	                        // save model
	                        fps = new FileOutputStream(uri);
	                        getMappingModel().write(fps, RDF_XML_ABBREV_FORMAT);
	                    }
	                    catch (Exception e) {
	                    	logger.error("Failed to save ont-policy file", e);
	                    }
	                    finally {
	                        if (fps != null) {
	                            try {
									fps.close();
								} catch (IOException e) {
									logger.error("Failed to close ont-policy file", e);
								}
	                        }
	                    }
	                }
	            }
            }
        }
        catch (Exception e) {
        	logger.error("Failed to read or update ont-policy file", e);
        }
        finally {
            if (in != null) {
                try {
					in.close();
				} catch (IOException e) {
					logger.error("Failed to close ont-policy input file", e);
				}
            }
        }

	}

	@Override
	/** Listen for events that indicate that the project may be closing so that any configuration changes
	 *  can be saved.
	 */
	public void resourceChanged(IResourceChangeEvent event) {
		//TODO why does this get a POST_CHANGE and not a PRE_CLOSE? Is this the best listener to use?
    	if(event.getType() == IResourceChangeEvent.PRE_CLOSE ||
    			event.getType() == IResourceChangeEvent.POST_CHANGE){
    		//event.getResource() instanceof IProject
    		if (isConfigChanged()) {
    			saveConfiguration();
    		}
    		if (isMappingChanged()) {
    			saveOntPolicyFile();
    		}
    		// this is as if it had been initialized :-)
    		timeConfigFileLastModifiedAtInitialization = System.currentTimeMillis();
    		if (event.getType() == IResourceChangeEvent.PRE_CLOSE) {
    			if (getModelGetter() != null) {
    				getModelGetter().sync();
    			}
    			closing = true;
    		}
    		if (event.getType() == IResourceChangeEvent.POST_CHANGE && closing) {
    			this.mappings.clear();
    			this.globalPrefixes.clear();
    		}
    	}	
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
	public Object getClassInstance(String reasonerClassName)
			throws InstantiationException, IllegalAccessException,
			ClassNotFoundException {
		if (Platform.isRunning()) {
			return Platform.getBundle("com.ge.research.sadl").loadClass(reasonerClassName).newInstance();
		}
		try {
		   Class c = Class.forName(reasonerClassName);
		   return c.newInstance();
		} catch (Exception e) {
		   e.printStackTrace();
		}
		return null;
	}

	protected static ServiceLoader<ITranslator> getTranslatorsFromServiceLoader(Class<ITranslator> cls) {
		return ServiceLoader.load(cls);
	}

	protected static ServiceLoader<Class<?>> getServiceLoader(Class<?> bcls) {
		return (ServiceLoader<Class<?>>) ServiceLoader.load(bcls);
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

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (event.getProperty() != null) {
			System.out.println(event.getProperty().toString());
		}
		
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
			if (onto != null) {
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
			Iterable<IEObjectDescription> matchingModels = descriptions.getExportedObjects(SadlPackage.Literals.MODEL, QualifiedName.create(publicUri.toString()), false);
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

	@Override
	public boolean removeMappingByActualUrl(String canonicalPath) throws URISyntaxException {
		boolean bChanged = false;
		String altUrl = getSadlUtils().fileNameToFileUrl(canonicalPath);
		StmtIterator sitr2 = getMappingModel().listStatements(null, altUrlProp, getMappingModel().getResource(altUrl));
		List<Statement> extras = null;
		while (sitr2.hasNext()) {
			Statement badStmt = sitr2.nextStatement();
			Resource badSubject = badStmt.getSubject();
			if (extras == null) {
				extras = new ArrayList<Statement>();
			}
			StmtIterator sitr3 = getMappingModel().listStatements(badSubject, (Property)null, (RDFNode)null);
			while (sitr3.hasNext()) {
				extras.add(sitr3.nextStatement());
			}
		}
		if (extras != null) {
			for (int j = 0; j < extras.size(); j++) {
				getMappingModel().remove(extras.get(j));
				bChanged = true;
			}
		}
		if (bChanged) {
			setMappingChanged(true);
		}
		return bChanged;
	}


}
