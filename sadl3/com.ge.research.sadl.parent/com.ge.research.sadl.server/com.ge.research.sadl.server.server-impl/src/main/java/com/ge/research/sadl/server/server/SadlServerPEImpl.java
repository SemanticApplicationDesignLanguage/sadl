/************************************************************************
 * Copyright (c) 2007-2015 - General Electric Company, All Rights Reserved
 *
 * Project: SADL Knowledge Server
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language. 
 * 
 * The SADL Knowledge Server is a set of Java classes implementing 
 * a service interface for deploying ontology-based knowledge bases
 * for use in a client-server environment.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/09 14:06:51 $
 ***********************************************************************/

package com.ge.research.sadl.server.server;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jakarta.activation.DataSource;

import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.reasoner.AmbiguousNameException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.server.ISadlServerPE;
import com.ge.research.sadl.server.SessionNotFoundException;
import org.apache.jena.ontology.AllValuesFromRestriction;
import org.apache.jena.ontology.CardinalityRestriction;
import org.apache.jena.ontology.HasValueRestriction;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.MaxCardinalityRestriction;
import org.apache.jena.ontology.MinCardinalityRestriction;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Ontology;
import org.apache.jena.ontology.SomeValuesFromRestriction;
import org.apache.jena.ontology.UnionClass;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ModelGetter;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFList;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.RDFReader;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.reasoner.rulesys.Rule;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.XSD;

/**
 * This Implementation class of the interface ISadlServerPE which provides additional methods
 * beyond those implemented by its super class, SadlServerImpl.
 *  
 * @author Andy Crapo
 *
 */
public class SadlServerPEImpl extends SadlServerImpl implements ISadlServerPE {
	public static final String SADLSERVER_PE_SOURCE = "SadlServerPE";
	private ConfigurationManagerForEditing configurationMgr = null;
	private Map<String, OntModel> instanceDataModels = null;		// new models not previously existing
	private Map<String, OntModel> editedServiceModels = null;			// models already existing in kbase
	private Map<String, List<Rule>> editedRules = null;
	private List<ModelError> errors = null;
	private List<String> newUnsavedNamespaces = null;
	private String serviceVersion = "$Revision: 1.16 $";
	
	private String lastChanceDefaultInstanceUri = "http://org.sadl/default_inst_data";
	
	public SadlServerPEImpl() {
	}
	
	public SadlServerPEImpl(String kbroot) throws ConfigurationException {
		super(kbroot);	
	}

	@Override
	public String getServiceVersion() throws SessionNotFoundException {
		return this.getClass().getCanonicalName() + " version " + serviceVersion;
	}

	@Override
	public boolean createServiceModel(String kbid, String serviceName, String modelName, String prefix, String owlFileName) {
		
		return noErrors();
	}
	
	@Override
	public boolean reset() throws ReasonerNotFoundException {
		if (instanceDataModels != null) {
			Iterator<String> modelNamesIter = instanceDataModels.keySet().iterator();
			while (modelNamesIter.hasNext()) {
				String modelName = modelNamesIter.next();
				OntModel model = instanceDataModels.get(modelName);
				if (getServiceModelName() != null && getServiceModelName().equals(modelName)) {
					// reload the model
					if (reasoner != null) {
						reasoner.reset();		// this isn't right awc 9/25/18
					}
				}
				else {
					model.removeAll();
				}
			}
			instanceDataModels.clear();
		}
		return super.reset();
	}
	
	@Override
    public String selectServiceModel(String knowledgeBaseIdentifier, String modelName) throws ConfigurationException, ReasonerNotFoundException {
		return selectServiceModel(knowledgeBaseIdentifier, modelName, null);
    }

	protected void setConfigurationManagerModelGetter()
			throws ConfigurationException, MalformedURLException {
		if (getConfigurationMgr().getModelGetter() == null) {
        	try {
				getConfigurationMgr().setModelGetter(new SadlJenaModelGetterPutter(getConfigurationMgr(), getConfigurationMgr().getModelFolder() + "/TDB"));
			} catch (IOException e) {
				logger.error("Exception setting ModelGetter: " + e.getMessage());
				e.printStackTrace();
			}
        }
	}
	
	public boolean persistInstanceModel(String owlInstanceFileName, String globalPrefix) 
				throws ConfigurationException, SessionNotFoundException, IOException {
		return persistInstanceModel(getDefaultModelName(), owlInstanceFileName, globalPrefix);
	}


	public boolean persistInstanceModel(String modelName, String owlFileName, String globalPrefix) throws ConfigurationException, IOException {
		String fullyQualifiedOwlFilename = null;
		boolean updateMapping = true;
		if (modelName == null) {
			if (owlFileName == null) {
				throw new IOException("Both model name and model file name cannot be null.");
			}
			try {
				modelName = getConfigurationMgr().getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(owlFileName));
				updateMapping = false;	// this is not a change in mapping
			} catch (URISyntaxException e) {
				throw new IOException(e.getMessage());
			}
		}
		else {
			// modelName is given
			if (owlFileName != null) {
				// OWL file name id given
				// If either is in the policy mapping then the other must match what is in the policy mapping
				// convert, if necessary, to a full-path URL
				if (!owlFileName.contains("\\") && !owlFileName.contains("/")) {
					String fp = getConfigurationMgr().getModelFolder() + "/" + owlFileName;
					try {
						fp = new SadlUtils().fileNameToFileUrl(fp);
					} catch (URISyntaxException e) {
						throw new IOException(e.getMessage());
					}
					owlFileName = fp;
				}
				String mappingFN = getConfigurationMgr().getAltUrlFromPublicUri(modelName);
				if (mappingFN != null && !mappingFN.equals(modelName)) {	// if no mapping is found mappingFN will = modelName
					if (!mappingFN.equals(owlFileName)) {
						throw new IOException("A model with name '" + modelName + " already exists but is persisted in a different model file ("
								+ mappingFN + "). It is not allowed to have the same model namespace in multiple model files.");
					}
				}
				else {
					// try the other way
					try {
						String mappingMN = getConfigurationMgr().getPublicUriFromActualUrl(owlFileName);
						if (mappingMN != null) {
							if (!mappingMN.equals(modelName)) {
								throw new IOException("A model with file name '" + owlFileName + " already exists but has a different model namespace ("
									+ mappingMN + "). It is not allowed to have safe models with different namespaces to the same file.");
							}
							else {
								updateMapping = false;	// everything matches (so far)
							}
						}
					} catch (ConfigurationException e) {
						// this is OK
					}
				}
			}
			else {
				updateMapping = false;	// this is not a change in mapping
			}
		}
		if (globalPrefix != null) {
			String mappingPrefix = getConfigurationMgr().getGlobalPrefix(modelName);
			if (mappingPrefix != null) {
				if (!mappingPrefix.equals(globalPrefix)) {
					throw new IOException("A model with name '" + modelName + " already exists but has a different prefix ("
							+ mappingPrefix + "). It is not allowed to have different global prefixes for the same model namespace.");
				}
				else {
					updateMapping = false;	// everything matches
				}
			}
			else {
				updateMapping = true;	// no global prefix in mapping but one is given
			}
		}

		if (owlFileName == null) {
			owlFileName = getConfigurationMgr().getAltUrlFromPublicUri(modelName);
		}
		if (owlFileName == null) {
			throw new IOException("File name for persistence must not be null.");
		}
		try {		
			if (owlFileName.startsWith("file:/")) {
        		SadlUtils su = new SadlUtils();
				fullyQualifiedOwlFilename = su.fileUrlToFileName(owlFileName);
			}
			else {
				fullyQualifiedOwlFilename = getConfigurationMgr().getModelFolderPath().getCanonicalPath() + File.separator + owlFileName;
			}
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to save mapping for model file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to save mapping for model file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		}
		String format = "RDF/XML-ABBREV";
		try {
			OntModel theOntModel = getInstanceData(); // getOntModelForEditing(modelName);
			if (theOntModel == null) {
				// we don't have a model for editing so we're going to save the instance data model
				theOntModel = getOntModelForEditing(getDefaultModelName());
			}
			if (writeOntModel(theOntModel, modelName, fullyQualifiedOwlFilename, format)) {
				if (updateMapping) {
				    try {
		        		SadlUtils su = new SadlUtils();
				    	getConfigurationMgr().addMapping(su.fileNameToFileUrl(fullyQualifiedOwlFilename), modelName, globalPrefix, true, "SadlServerPE");
				    	getConfigurationMgr().saveOntPolicyFile();
				    }
				    catch (Exception e) {
				    	return addError(new ModelError("Failed to save mapping for model file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
				    }
				}
			}
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to save mapping for model file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to save mapping for model file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (InvalidNameException e) {
	    	return addError(new ModelError("Failed to save mapping for model file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return noErrors();
	}

	@Override
	public boolean persistChangesToServiceModels() {
		if (editedServiceModels != null && editedServiceModels.size() > 0) {
			List<String> savedModels = new ArrayList<String>();
			Iterator<String> moditr = editedServiceModels.keySet().iterator();
			while (moditr.hasNext()) {
				String modelName = moditr.next();
				OntModel ontModel = editedServiceModels.get(modelName);
				String format = "RDF/XML-ABBREV";	
				String fullyQualifiedOwlFilename = null;
				try {
					fullyQualifiedOwlFilename = getConfigurationMgr().getAltUrlFromPublicUri(modelName);
				} catch (ConfigurationException e) {
			    	return addError(new ModelError("Failed to save model to file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
				} catch (MalformedURLException e) {
			    	return addError(new ModelError("Failed to save model to file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
				}
				// add a comment to the ontology
				Ontology ont = ontModel.getOntology(modelName);
				ont.addComment("This model was modified by SadlServerPE.", "en");
			    try {
					writeOntModel(ontModel, modelName, fullyQualifiedOwlFilename, format);
				} catch (MalformedURLException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			    if (noErrors()) {
			    	try {
						getConfigurationMgr().replaceJenaModelCache(ontModel, modelName);
					} catch (Exception e) {
						addError(new ModelError("Unexpected error trying to refresh Jena cache with saved model '" + modelName + "': " + e.getMessage(), ErrorType.ERROR));
					}
			    }
			    if (editedRules != null) {
			    	if (editedRules.containsKey(modelName)) {
			    		try {
							writeRules(ontModel, modelName);
						} catch (Exception e) {
							addError(new ModelError("Unexpected error trying to save rules for model '" + modelName + "': " + e.getMessage(), ErrorType.ERROR));
						}
			    	}
			    }
			    savedModels.add(modelName);
			}
			for (int i = 0; i < savedModels.size(); i++) {
				editedServiceModels.remove(savedModels.get(i));
			}
		}
		return noErrors();
	}

	private boolean writeRules(OntModel ontModel, String modelName) throws ConfigurationException, IOException {
		ConfigurationManagerForEditing cmgr = getConfigurationMgr();
		String altUrl = cmgr.getAltUrlFromPublicUri(modelName);
		String rulefn = altUrl.substring(0, altUrl.lastIndexOf(".")) + ".rules";
		SadlUtils su = new SadlUtils();
		File ruleFile = new File(su.fileUrlToFileName(rulefn));
		List<Rule> rules = editedRules.get(modelName);
		
		StringBuilder contents = new StringBuilder();
		contents.append("# Jena Rules file generated by SadlServerPE.\n");
		
		StringBuilder ruleContent = new StringBuilder();
		for (int i = 0; i < rules.size(); i++) {
			Rule rule = rules.get(i);
			ruleContent.append(rule.toString());
			ruleContent.append("\n");
		}
		
		// We will assume for the moment that there are no prefixes  but that all names are the FQName
		
		contents.append("\n");
		
		// Because rule files are loaded for each sub-model, there is no need to put in includes
		if (ruleContent.length() > 0) {
			contents.append(ruleContent);
		}
		su.stringToFile(ruleFile, contents.toString(), false);
		return true;
	}

	private boolean writeOntModel(OntModel ontModel, String modelName,
			String fullyQualifiedOwlFilename, String format) throws MalformedURLException {
		FileOutputStream fps = null;
		try {
    		SadlUtils su = new SadlUtils();
    		fps = new FileOutputStream(su.fileUrlToFileName(fullyQualifiedOwlFilename));
		} catch (FileNotFoundException e) {
			return addError(new ModelError("Failed to save model to file '" + fullyQualifiedOwlFilename + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		}
		try {
		    RDFWriter rdfw = ontModel.getWriter(format);
		    if (format.startsWith("RDF/XML")) {
		        rdfw.setProperty("xmlbase", modelName); 
		        rdfw.setProperty("relativeURIs", "");
		        rdfw.setProperty("minimalPrefixes", true);
		    }
		    
		    ontModel.setNsPrefix("", getModelNamespace(modelName));
		    rdfw.write(ontModel.getBaseModel(), fps, modelName);
		    fps.close();
		}
		catch (Throwable t) {
			t.printStackTrace();
			logger.error("Fatal error saving model file '" + modelName + "' to '" + fullyQualifiedOwlFilename + "': " + t.getLocalizedMessage());
			return addError(new ModelError("Failed to save model to file '" + fullyQualifiedOwlFilename + "': " + t.getLocalizedMessage(), ErrorType.ERROR));
		}
		finally {
			if (fps != null) {
				try {
					fps.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return noErrors();
	}

	/**
	 * Method to add an error to the list of errors.
	 * @param modelError
	 * @return -- return false meaning operation failed on error
	 */
	private boolean addError(ModelError modelError) {
		if (errors == null) {
			errors = new ArrayList<ModelError>();
		}
		errors.add(modelError);
		return false;
	}

	protected String getModelNamespace(String modelName) {
		if (!modelName.endsWith("#")) {
			return modelName + "#";
		}
		return modelName;
	}

	@Override
	public boolean addTriple(String modelName, String subject,
			String predicate, Object value) throws TripleNotFoundException, ReasonerNotFoundException, ConfigurationException {
		try {
			logger.debug("In addTriple - modelName: "+modelName);
			OntModel ontModel = getOntModelForEditing(modelName);
			if (ontModel != null) {
				Individual subj = null;
				try {
					subject = getUri(modelName, subject);
					subj = ontModel.getIndividual(subject);
				} catch (PrefixNotFoundException e) {
					return addError(new ModelError("Failed to find triple subject '" + subject + "' in model '" + modelName + "'", ErrorType.ERROR));					
				}
				if (subj == null) {
					return addError(new ModelError("Failed to find triple subject '" + subject + "' in model '" + modelName + "'", ErrorType.ERROR));					
				}
				
				OntProperty pred = null;
				Property nonOntPred = null;
				if (predicate != null) {
					try {
						pred = ontModel.getOntProperty(getUri(modelName, predicate));
						if (pred == null) {
							nonOntPred = ontModel.getProperty(getUri(modelName, predicate));
							if (nonOntPred != null) {
								logger.debug("Found predicate but it isn't an OntProperty");
							}
						}
					} catch (PrefixNotFoundException e) {
						return addError(new ModelError("Failed to find triple predicate '" + predicate + "' in model '" + modelName + "'", ErrorType.ERROR));
					}
				}
				
				RDFNode val;
				if (pred != null && pred.isDatatypeProperty()) {
					if (pred.getRange() != null) {
						val = ontModel.createTypedLiteral(value, pred.getRange().getURI());
					}
					else {
						val = ontModel.createTypedLiteral(value);
					}
				}
				else {
					try {
						if (pred == null && value instanceof String && !(((String)value).startsWith("http://" ) && ((String)value).contains("#"))) {
							val = ontModel.createTypedLiteral(value);
						}
						else {
							value = getUri(modelName, value.toString());
							val = ontModel.getIndividual(getUri(modelName, value.toString()));
						}
					} catch (PrefixNotFoundException e) {
						return addError(new ModelError("Failed to find triple object '" + value + "' in model '" + modelName + "'", ErrorType.ERROR));					
					}
				}
				if (val == null) {
					return addError(new ModelError("Failed to find triple object '" + value + "' in model '" + modelName + "'", ErrorType.ERROR));					
				}
				ontModel.add(subj, pred != null ? pred : nonOntPred, val);
			}
		} catch (ConfigurationException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (InvalidNameException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (AmbiguousNameException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		return super.addTriple(subject, predicate, value);
	}

	private String getUri(String modelName, String fragment) throws PrefixNotFoundException, ConfigurationException, IOException, InvalidNameException, URISyntaxException, AmbiguousNameException {
		if (fragment.contains("#")) {
			return fragment;
		}
		else if (fragment.contains(":")) {
            int colonLoc = fragment.indexOf(":");
            if (colonLoc > 0) {
            	OntModel model = getOntModelForEditing(modelName);
                if (model != null) {
                    String prefix = fragment.substring(0, colonLoc);
                    String ns = model.getNsPrefixURI(prefix);
                    if (ns == null) {
                    	// prefix was not found
                    	throw new PrefixNotFoundException("No namespace was found for prefix '" + prefix + "'");
                    }
                    return ns + fragment;
                }
            }
		}
		else {
			return JenaTranslatorPlugin.findNameNs(getOntModelForEditing(modelName), fragment);
		}
		return fragment;
	}

	private OntModel getOntModelForEditing(String thisModelName) throws IOException, ConfigurationException, InvalidNameException, URISyntaxException {
		if (editedServiceModels != null && editedServiceModels.containsKey(thisModelName)) {
			return editedServiceModels.get(thisModelName);
		}
		try {
			if (getServiceModelName() != null && !getServiceModelName().equals(thisModelName)) {
				if (instanceDataModels != null && instanceDataModels.containsKey(thisModelName)) {
					return instanceDataModels.get(thisModelName);
				}
				else if (getDefaultModelName() == null || thisModelName.equals(getDefaultModelName())) {
					if (getDefaultModelName() == null) {
						setInstanceDataNamespace(thisModelName + "#");
					}
					return createInstanceModel(thisModelName);
				}
			}
		} catch (ConfigurationException e) {
			// it's ok for instance names space to fail
		}
		String altUrl = getModifiableModelAltUrl(thisModelName);
		if (altUrl != null) {
			OntModel ontModel = ModelFactory.createOntologyModel(getConfigurationMgr().getOntModelSpec(null));
			altUrl = getConfigurationMgr().fileNameToFileUrl(altUrl);
			ontModel.getSpecification().setImportModelGetter((ModelGetter) configurationMgr.getModelGetter());
			ontModel.read(altUrl);
			ontModel.getDocumentManager().setProcessImports(true);
			ontModel.loadImports();
			ontModel.getDocumentManager().setProcessImports(false);
			if (editedServiceModels == null) {
				editedServiceModels = new HashMap<String, OntModel>();
			}
			editedServiceModels.put(thisModelName, ontModel);
			return ontModel;
		}
		return null;
	}

	private OntModel createInstanceModel(String thisModelName)
			throws IOException,
			ConfigurationException {
		if (instanceDataModels != null && instanceDataModels.containsKey(thisModelName)) {
			return instanceDataModels.get(thisModelName);
		}
		if (editedServiceModels != null && editedServiceModels.containsKey(thisModelName)) {
			return editedServiceModels.get(thisModelName);
		}
		OntModel model = ModelFactory.createOntologyModel(getConfigurationMgr().getOntModelSpec(null));
		model.getDocumentManager().setFileManager(getConfigurationMgr().getJenaDocumentMgr().getFileManager());
		Resource importOnt = model.getResource(getServiceModelName());
		Ontology ont = model.createOntology(thisModelName);
		ont.addImport(importOnt);
		ont.addComment("This ontology model was created by SadlServerPE.", "en");
		model.getDocumentManager().setProcessImports(true);
		model.loadImports();
		if (logger.isDebugEnabled()) {
			Iterator<String> importItr = model.listImportedOntologyURIs().iterator();
			while (importItr.hasNext()) {
				logger.debug("Model '" + thisModelName + "' imports '" + importItr.next() + "'");
			}
		}
		model.getDocumentManager().setProcessImports(false);
		if (instanceDataModels == null) {
			instanceDataModels = new HashMap<String, OntModel>(); 
		}
		instanceDataModels.put(thisModelName, model);
		return model;
	}

	private String getModifiableModelAltUrl(String modelName) throws ConfigurationException, IOException, URISyntaxException {
		SadlUtils su = new SadlUtils();
		String altUrl = getConfigurationMgr().getAltUrlFromPublicUri(su.fileNameToFileUrl(modelName));
		if (altUrl.startsWith("http:/")) {
			return null;
		}
		return altUrl;
	}

	public boolean deleteTriple(String modelName, String subject,
			String predicate, Object value) throws TripleNotFoundException, ReasonerNotFoundException, ConfigurationException {
		boolean tripleDeleted = false;
		try {
			OntModel ontModel = getOntModelForEditing(modelName);
			if (ontModel != null) {
				Individual subj = null;
				if (subject != null) {
					try {
						subj = ontModel.getIndividual(getUri(modelName, subject));
					} catch (PrefixNotFoundException e) {
						return addError(new ModelError("Failed to find triple subject '" + subject + "' in model '" + modelName + "'", ErrorType.ERROR));					
					}
					if (subj == null) {
						return addError(new ModelError("Failed to find triple subject '" + subject + "' in model '" + modelName + "'", ErrorType.ERROR));					
					}
				}
				
				OntProperty pred = null;
				Property nonOntPred = null;
				if (predicate != null) {
					try {
						pred = ontModel.getOntProperty(getUri(modelName, predicate));
						if (pred == null) {
							nonOntPred = ontModel.getProperty(getUri(modelName, predicate));
							if (nonOntPred != null) {
								logger.debug("Found predicate but it isn't an OntProperty");
							}
						}
					} catch (PrefixNotFoundException e) {
						return addError(new ModelError("Failed to find triple predicate '" + predicate + "' in model '" + modelName + "'", ErrorType.ERROR));
					}
				}
				
				RDFNode val = null;
				if (value != null) {
					if (pred != null && pred.isDatatypeProperty()) {
						val = ontModel.createTypedLiteral(value);
					}
					else {
						try {
							if (pred == null && value instanceof String && !(((String)value).startsWith("http://" ) && ((String)value).contains("#"))) {
								val = ontModel.createTypedLiteral(value);
							}
							else {
								value = getUri(modelName, value.toString());
								val = ontModel.getIndividual(getUri(modelName, value.toString()));
							}
						} catch (PrefixNotFoundException e) {
							return addError(new ModelError("Failed to find triple object '" + value + "' in model '" + modelName + "'", ErrorType.ERROR));					
						}
					}
					if (val == null) {
						return addError(new ModelError("Failed to find triple object '" + value + "' in model '" + modelName + "'", ErrorType.ERROR));					
					}
				}
				StmtIterator sitr = ontModel.listStatements(subj, pred != null ? pred : nonOntPred, val);
				if (sitr.hasNext()) {
					List<Statement> deletions = new ArrayList<Statement>();
					while (sitr.hasNext()) {
						deletions.add(sitr.nextStatement());
					}
					sitr.close();
					ontModel.remove(deletions);
					tripleDeleted = true;
				}
			}
		} catch (ConfigurationException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (InvalidNameException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (AmbiguousNameException e) {
			return addError(new ModelError("Failed to edit model '" + modelName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		return (super.deleteTriple(subject, predicate, value) || tripleDeleted);
	}
	
	public String getUniqueInstanceUri(String namespace, String baseLocalName) throws InvalidNameException, SessionNotFoundException {
		return getUniqueInstanceUri(getDefaultModelName(), namespace, baseLocalName);
	}
	
	public String getUniqueInstanceUri(String modelName, String namespace,
			String baseLocalName) throws InvalidNameException,
			SessionNotFoundException {
		OntModel ontModel;
		try {
			String baseUri = namespace + baseLocalName;
			String error = SadlUtils.validateRdfUri(baseUri);
			if (error != null) {
				throw new InvalidNameException("Invalid instance name (" + baseUri + "): " + error);
			}
			String uri = baseUri;
			ontModel = getOntModelForEditing(modelName);
			if (ontModel == null) {
				throw new InvalidNameException("Namespace '" + namespace + "' does not correspond with any known model.");
			}
			int cntr = 1;
			uri = SadlUtils.getUniqueOntUri(ontModel, baseUri);
			while (ontModel.getOntResource(uri) != null) {
				uri = baseUri + "_" + cntr++;
			}
			return uri;
			
		} catch (ConfigurationException e) {
	    	addError(new ModelError("Failed to create a unique URI for namespace '" + namespace + "' and base local name '" + baseLocalName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	addError(new ModelError("Failed to create a unique URI for namespace '" + namespace + "' and base local name '" + baseLocalName + "': " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	addError(new ModelError("Failed to create a unique URI for namespace '" + namespace + "' and base local name '" + baseLocalName + "' for unknown reasons. ", ErrorType.ERROR));
		return null;
	}

	@Override
	public String getUniqueNamespaceUri(String baseUri)
			throws InvalidNameException, SessionNotFoundException, MalformedURLException, ConfigurationException {
		Object[] split = SadlUtils.splitUriIntoBaseAndCounter(baseUri);
		baseUri = (String) split[0];
		long cntr = (long) split[1];
		String newNSUri = null;
		do {
			String uri = baseUri + cntr++;
			String actualUrl = getConfigurationMgr().getMappings().get(uri);
			if (actualUrl != null) {
				continue;
			}
			if (newUnsavedNamespaces != null && newUnsavedNamespaces.contains(uri)) {
				continue;
			}
			newNSUri = uri;
			if (newUnsavedNamespaces == null) {
				newUnsavedNamespaces = new ArrayList<String>();
			}
			newUnsavedNamespaces.add(newNSUri);
		} while (newNSUri == null);
		return newNSUri;
	}

	public boolean addClass(String className, String superClassName) throws InvalidNameException {
		return addClass(getDefaultModelName(), className, superClassName);
	}
	
	public boolean addClass(String modelName, String className, String superClassName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.createClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to create class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to create class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (IOException e) {
				return addError(new ModelError("Failed to create class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to create class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to create class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to create class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			if (superClassName != null) {
				OntClass supcls = null;
				try {
					supcls = ontModel.getOntClass(getUri(modelName, superClassName));
				} catch (PrefixNotFoundException e) {
					return addError(new ModelError("Failed to find super class '" + superClassName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (ConfigurationException e) {
					return addError(new ModelError("Failed to find super class '" + superClassName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (IOException e) {
					return addError(new ModelError("Failed to find super class '" + superClassName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (InvalidNameException e) {
					return addError(new ModelError("Failed to find super class '" + superClassName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (AmbiguousNameException e) {
					return addError(new ModelError("Failed to find super class '" + superClassName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (supcls != null) {
					cls.addSuperClass(supcls);
				}
				else {
					return addError(new ModelError("Failed to find super class '" + superClassName + "' in model '" + modelName + "'", ErrorType.ERROR));
				}
			}
		}
		return noErrors();
	}
	
	@Override
	public boolean addOntProperty(String modelName, String propertyName, String superPropertyName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntProperty prop = null;
			try {
				prop = ontModel.createOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to create property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to create property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (IOException e) {
				return addError(new ModelError("Failed to create property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to create property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to create property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (superPropertyName != null) {
				OntProperty supprop = null;
				try {
					supprop = ontModel.getOntProperty(getUri(modelName, superPropertyName));
				} catch (PrefixNotFoundException e) {
					return addError(new ModelError("Failed to find super property '" + superPropertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (ConfigurationException e) {
					return addError(new ModelError("Failed to find super property '" + superPropertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (IOException e) {
					return addError(new ModelError("Failed to find super property '" + superPropertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (InvalidNameException e) {
					return addError(new ModelError("Failed to find super property '" + superPropertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (AmbiguousNameException e) {
					return addError(new ModelError("Failed to find super property '" + superPropertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (supprop != null) {
					prop.addSuperProperty(supprop);
				}
				else {
					return addError(new ModelError("Failed to find super property '" + superPropertyName + "' in model '" + modelName + "'", ErrorType.ERROR));
				}
			}
		}
		return noErrors();
	}
	
	@Override
	public boolean addOntPropertyDomainClass(String modelName, String propertyName, String domainClassName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, domainClassName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find domain class '" + domainClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find domain class '" + domainClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find domain class '" + domainClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find domain class '" + domainClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find domain class '" + domainClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find domain class '" + domainClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntResource existingDomain = prop.getDomain();
			if (existingDomain != null) {
				OntResource newDomain = (OntClass) addClassToUnionClass(ontModel, existingDomain, cls);
				if (!existingDomain.equals(newDomain)) {
					prop.removeDomain(existingDomain);
					prop.addDomain(newDomain);
				}
			} else {
				prop.addDomain(cls);
			}

		}
		return noErrors();
	}
	
	@Override
	public boolean addObjectPropertyRangeClass(String modelName, String propertyName, String rangeClassName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, rangeClassName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find range class '" + rangeClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find range class '" + rangeClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find range class '" + rangeClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find range class '" + rangeClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find range class '" + rangeClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find range class '" + rangeClassName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntResource existingRange = prop.getRange();
			if (existingRange != null) {
				if (cls != null && !existingRange.equals(cls)) {
					prop.removeRange(existingRange);
					prop.addRange(addClassToUnionClass(ontModel, existingRange, cls));
				}
			} else {
				prop.addRange(cls);
			}
		}
		return noErrors();
	}
	
	@Override
	public boolean setDatatypePropertyRange(String modelName, String propertyName, String xsdRange) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			if (xsdRange != null) {
				String rngUri = XSD.getURI() + xsdRange;
				Resource r = ontModel.getResource(rngUri);
				if (r != null) {
					OntResource existingRange = prop.getRange();
					if (existingRange != null
							&& !existingRange.equals(r)) {
						addError(new ModelError("Property '" + prop.getURI() + "' already has range '"
										+ existingRange.toString() + "; can't add second range (" + r.toString()
										+ ") to data property.", ErrorType.ERROR));
					} else {
						prop.addRange(r);
						prop.setRDFType(OWL.DatatypeProperty);
					}
				}
				else {
					return addError(new ModelError("Failed to find range '" + xsdRange + "' in model '" + modelName + "'", ErrorType.ERROR));				
				}
			}
		}
		return noErrors();
	}
	
	private OntResource addClassToUnionClass(OntModel model, OntResource existingCls, OntResource cls) {
		if (existingCls != null && !existingCls.equals(cls)) {
			RDFList classes = null;
			if (existingCls.canAs(UnionClass.class)) {
				try {
					classes = ((UnionClass) existingCls.as(UnionClass.class))
							.getOperands();
					if (classes.contains(cls)) {
						return existingCls;
					}
					classes = classes.with(cls);
				} catch (Exception e) {
					// don't know why this is happening
					logger.error("Union class error that hasn't been resolved or understood.");
					return cls;
				}
			} else {
				if (cls.equals(existingCls)) {
					return existingCls;
				}
				classes = model.createList();
				classes = classes.with(existingCls);
				classes = classes.with(cls);
			}
			OntResource unionClass = model.createUnionClass(null,
					classes);
			return unionClass;
		} else {
			return cls;
		}
	}

	public boolean addAllValuesFromRestriction(String className, String propertyName, String restrictionName) throws InvalidNameException {
		return addAllValuesFromRestriction(getDefaultModelName(), className, propertyName, restrictionName);
	}
	
	public boolean addAllValuesFromRestriction(String modelName, String className, String propertyName, String restrictionName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntClass restCls = null;
			try {
				restCls = ontModel.getOntClass(getUri(modelName, restrictionName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (restCls == null) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			}
			AllValuesFromRestriction avf = ontModel.createAllValuesFromRestriction(null, prop, restCls);
			if (avf != null) {
				cls.addSuperClass(avf);
			}
			else {
				addError(new ModelError("Unable to create allValuesFromRestriction for unknown reason.", ErrorType.ERROR));
			}
		}
		return noErrors();
	}
	
	@Override
	public boolean addSomeValuesFromRestriction(String modelName, String className, String propertyName, String restrictionName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntClass restCls = null;
			try {
				restCls = ontModel.getOntClass(getUri(modelName, restrictionName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (restCls == null) {
				return addError(new ModelError("Failed to find restricted to class '" + restrictionName + "' in model '" + modelName + "'", ErrorType.ERROR));
			}
			SomeValuesFromRestriction svf = ontModel.createSomeValuesFromRestriction(null, prop, restCls);
			if (svf != null) {
				cls.addSuperClass(svf);
			}
			else {
				addError(new ModelError("Unable to create someValuesFromRestriction for unknown reason.", ErrorType.ERROR));
			}
		}
		return noErrors();
	}

	public boolean addHasValueRestriction(String className, String propertyName, String valueInstanceName) throws InvalidNameException {
		return addHasValueRestriction(getDefaultModelName(), className, propertyName, valueInstanceName);
	}
	
	public boolean addHasValueRestriction(String modelName, String className, String propertyName, String valueInstanceName) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			Individual restInst = null;
			try {
				restInst = ontModel.getIndividual(getUri(modelName, valueInstanceName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricted to instance '" + restInst + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricted to instance '" + restInst + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricted to instance '" + restInst + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricted to instance '" + restInst + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricted to instance '" + restInst + "' in model '" + modelName + "'", ErrorType.ERROR));
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (restInst == null) {
				return addError(new ModelError("Failed to find restricted to instance '" + restInst + "' in model '" + modelName + "'", ErrorType.ERROR));
			}
			HasValueRestriction hvf = ontModel.createHasValueRestriction(null, prop, restInst);
			if (hvf != null) {
				cls.addSuperClass(hvf);
			}
			else {
				addError(new ModelError("Unable to create has value Restriction for unknown reason.", ErrorType.ERROR));
			}
		}
		return noErrors();
	}
	
	public boolean addCardinalityRestriction(String className, String propertyName, int cardValue) throws InvalidNameException {
		return addCardinalityRestriction(getDefaultModelName(), className, propertyName, cardValue);
	}

	public boolean addCardinalityRestriction(String modelName, String className, String propertyName, int cardValue) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			CardinalityRestriction cr = ontModel.createCardinalityRestriction(null, prop, cardValue);
			if (cr != null) {
				cls.addSuperClass(cr);
			}
			else {
				addError(new ModelError("Unable to create cardinality Restriction for unknown reason.", ErrorType.ERROR));
			}
		}
		return noErrors();
	}
	
	@Override
	public boolean addMinCardinalityRestriction(String modelName, String className, String propertyName, int cardValue) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			MinCardinalityRestriction cr = ontModel.createMinCardinalityRestriction(null, prop, cardValue);
			if (cr != null) {
				cls.addSuperClass(cr);
			}
			else {
				addError(new ModelError("Unable to create min cardinality Restriction for unknown reason.", ErrorType.ERROR));
			}
		}
		return noErrors();
	}
	
	@Override
	public boolean addMaxCardinalityRestriction(String modelName, String className, String propertyName, int cardValue) throws InvalidNameException {
		OntModel ontModel = null;
		try {
			ontModel = getOntModelForEditing(modelName);
		} catch (ConfigurationException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (IOException e) {
	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (ontModel != null) {
			OntClass cls = null;
			try {
				cls = ontModel.getOntClass(getUri(modelName, className));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (cls == null) {
				return addError(new ModelError("Failed to find restriction class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			OntProperty prop = null;
			try {
				prop = ontModel.getOntProperty(getUri(modelName, propertyName));
			} catch (PrefixNotFoundException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (ConfigurationException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (IOException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (InvalidNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (AmbiguousNameException e) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (prop == null) {
				return addError(new ModelError("Failed to find restricion property '" + propertyName + "' in model '" + modelName + "'", ErrorType.ERROR));				
			}
			MaxCardinalityRestriction cr = ontModel.createMaxCardinalityRestriction(null, prop, cardValue);
			if (cr != null) {
				cls.addSuperClass(cr);
			}
			else {
				addError(new ModelError("Unable to create max cardinality Restriction for unknown reason.", ErrorType.ERROR));
			}
		}
		return noErrors();
	}

	@Override
	public boolean addMaxQualifiedCardinalityRestriction(String modelName, String className, String propertyName,
			int cardValue, String restrictedToType) throws SessionNotFoundException, InvalidNameException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addMinQualifiedCardinalityRestriction(String modelName, String className, String propertyName,
			int cardValue, String restrictedToType) throws SessionNotFoundException, InvalidNameException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addQualifiedCardinalityRestriction(String modelName, String className, String propertyName,
			int cardValue, String restrictedToType) throws SessionNotFoundException, InvalidNameException {
		// TODO Auto-generated method stub
		return false;
	}

	private boolean noErrors() {
		if (errors == null || errors.size() < 1) {
			return true;
		}
		for (int i = 0; i < errors.size(); i++) {
			if (errors.get(i).getErrorType().equals(ErrorType.ERROR)) {
				return false;
			}
		}
		return true;
	}

	void setConfigurationMgr(ConfigurationManagerForEditing configurationMgr) {
		this.configurationMgr = configurationMgr;
	}

	protected ConfigurationManagerForEditing getConfigurationMgr() throws ConfigurationException, MalformedURLException {
		if (configurationMgr == null) {
			String modelFolder = getModelFolder();
			File mff = new File(modelFolder);
			if (mff.exists()) {
				String repoType = getRepoType(modelFolder + "/TDB");
				configurationMgr = new ConfigurationManagerForEditing(modelFolder, repoType);
			}
			else {
				throw new ConfigurationException("Cannot create configuration manager. Model folder '" + modelFolder + "' does not exist.");
			}
		}
		return configurationMgr;
	}

	@Override
	public List<ModelError> getErrors() {
		if (errors != null && errors.size() > 0) {
			List<ModelError> retErrors = errors;
			errors = null;
			return retErrors;
		}
		return errors;
	}
	
	@Override
	public boolean addTriple(String subjName, String predName, Object objValue) 
	    			throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException  {
		return addTriple(getDefaultModelName(), subjName, predName, objValue);
	}

	@Override
	public boolean deleteTriple(String subjName, String predName, Object objValue) 
    		throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException {
		return deleteTriple(getDefaultModelName(), subjName, predName, objValue);
	}
	
	private OntModel getInstanceData() 
				throws IOException, ConfigurationException, InvalidNameException, ConfigurationException, URISyntaxException {
		return getOntModelForEditing(getDefaultModelName());
	}
	
	@Override
	public String createInstance(String name, String className) 
			throws ConfigurationException, InvalidNameException, IOException, SessionNotFoundException, AmbiguousNameException {
		String instanceModelName = getDefaultModelName();
		if (instanceModelName == null) {
			if (name.indexOf("#") > 0) {
				// we have a namespace explicit in the new instance name
				String nameNs = name.substring(0, name.indexOf("#") + 1);
				setInstanceDataNamespace(nameNs);
				instanceModelName = nameNs.substring(0, nameNs.length() - 1);
			}
			else {
				throw new ConfigurationException("Instance data namespace must be set before SadlServerPE can support this operation.");
			}
		}
		return createInstance(getDefaultModelName(), name, className);
	}
	
//	public S createInstance(String modelName, String instName, String className) throws ConfigurationException, InvalidNameException {
//		OntModel ontModel = null;
//		try {
//			if (instName.indexOf('#') < 0) {
//				instName = getUri(modelName, instName);
//			}
//			String error = SadlUtils.validateRdfUri(instName);
//			if (error != null) {
//				throw new InvalidNameException("Invalid URI (" + instName + ") for new instance: " + error + ".");
//			}
//			ontModel = getOntModelForEditing(modelName);
//		} catch (ConfigurationException e) {
//	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
//		} catch (IOException e) {
//	    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
//		} catch (URISyntaxException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (PrefixNotFoundException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		if (ontModel != null) {
//			OntClass cls = null;
//			try {
//				cls = ontModel.getOntClass(getUri(modelName, className));
//			} catch (PrefixNotFoundException e) {
//		    	return addError(new ModelError("Failed to find class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
//			} catch (ConfigurationException e) {
//		    	return addError(new ModelError("Failed to find class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
//			} catch (IOException e) {
//		    	return addError(new ModelError("Failed to find class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
//			} catch (InvalidNameException e) {
//		    	return addError(new ModelError("Failed to find class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
//			} catch (URISyntaxException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//			if (cls == null) {
//		    	return addError(new ModelError("Failed to find class '" + className + "' in model '" + modelName + "'", ErrorType.ERROR));
//			}
//			try {
//				ontModel.createIndividual(instName, cls);
//				super.createInstance(instName, className);
//			} catch (ConfigurationException e) {
//		    	return addError(new ModelError("Failed to find create instance '" + instName + "' in model '" + modelName + "'", ErrorType.ERROR));
//			} catch (IOException e) {
//		    	return addError(new ModelError("Failed to find create instance '" + instName + "' in model '" + modelName + "'", ErrorType.ERROR));
//			} catch (InvalidNameException e) {
//		    	return addError(new ModelError("Failed to find create instance '" + instName + "' in model '" + modelName + "'", ErrorType.ERROR));
//			}
//		}
//		return noErrors();
//	}

	@Override
	public String createInstance(String modelName, String name, String className)
			throws ConfigurationException, InvalidNameException, IOException, SessionNotFoundException, AmbiguousNameException {
		String instname = null;
		try {
			if (!(name.indexOf('#') > 0)) {
				name = getUri(modelName, name);
			}
			instname = super.createInstance(name, className);
		} catch (InvalidNameException e) {
			// this is ok--it just means that the name doesn't exist yet--
			name = modelName + (modelName.endsWith("#") ? name : "#" + name);
			instname = super.createInstance(name, className);
		} catch (PrefixNotFoundException e) {
			throw new InvalidNameException(e.getMessage());
		} catch (ConfigurationException e) {
			throw new ConfigurationException(e.getMessage());
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		OntClass cls;
		try {
			cls = getOntModelForEditing(modelName).getOntClass(className);
			if (cls == null) {
				addError(new ModelError("Unable to create instance because class '" + className + "' was not found.", ErrorType.ERROR));
				return instname;
			}
			return getOntModelForEditing(modelName).createIndividual(instname, cls).getURI();
		} catch (ConfigurationException e) {
			addError(new ModelError("Unable to create instance: " + e.getMessage(), ErrorType.ERROR));
		} catch (IOException e) {
			addError(new ModelError("Unable to create instance: " + e.getMessage(), ErrorType.ERROR));
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return instname;
	}

	@Override
	public String setInstanceDataNamespace(String namespace) throws InvalidNameException {
		if (!namespace.endsWith("#")) {
			throw new InvalidNameException("A namespace should end with '#' ('" + namespace + "' does not)");
		}
		String modelName = namespace.substring(0, namespace.length() - 1);
		if (modelName.equals(getInstanceDataModelName())) {
			// this is a no-op
			return modelName;
		}
		String oldNS = getInstanceDataModelName();
		if (instanceDataModels != null && instanceDataModels.containsKey(modelName)) {
			setInstanceDataModelName(modelName);
			return oldNS;
		}
		if (getServiceModelName().equals(modelName)) {
			setInstanceDataModelName(modelName);
			return oldNS;
		}
		try {
			createInstanceModel(modelName);
		} catch (Exception e) {
			throw new InvalidNameException(e.getMessage());
		}
		setInstanceDataNamespace(namespace);
		if (reasoner != null) {
			reasoner.setInstanceDataNamespace(namespace);
		}
		return oldNS;
	}
	
	public OntModel getDefaultModel() {
		OntModel theModel = null;
		
		return theModel;
	}

	@Override
	public boolean addRule(String modelName, String ruleAsString) throws ConfigurationException, IOException {
		String altUrl;
		try {
			altUrl = getConfigurationMgr().getAltUrlFromPublicUri(modelName);
		} catch (ConfigurationException e1) {
			throw new ConfigurationException(e1.getMessage());
		}
		String rulefn = altUrl.substring(0, altUrl.lastIndexOf(".")) + ".rules";
		SadlUtils su = new SadlUtils();
		File ruleFile = new File(su.fileUrlToFileName(rulefn));
		List<Rule> rules = null;
		if (ruleFile.exists()) {
			rules = Rule.rulesFromURL(rulefn);
		}
		else {
			rules = new ArrayList<Rule>();
		}
		if (rules != null) {
			Rule jenarule = Rule.parseRule(ruleAsString);
			rules.add(jenarule);
			if (reasoner != null) {
				reasoner.addRule(ruleAsString);
			}
			if (editedRules == null) {
				editedRules = new HashMap<String, List<Rule>>();
			}
			editedRules.put(modelName, rules);
			
			// need to put this model (if it isn't already) in the edited list so it will write out rules
			try {
				getOntModelForEditing(modelName);
			} catch (ConfigurationException e) {
		    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
			} catch (IOException e) {
		    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
			} catch (InvalidNameException e) {
		    	return addError(new ModelError("Failed to find model '" + modelName + "' for editing: " + e.getLocalizedMessage(), ErrorType.ERROR));
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			return true;
		}
		return false;
	}

    @Override
    public boolean sendData(DataSource dataSrc)
            throws IOException, ReasonerNotFoundException, ConfigurationException {
//    	                                                   ConfigurationException
        logger.info("Calling sendData({})", dataSrc);
        if (reasoner != null) {
        	String inputFormat = "N-TRIPLE";
    		String srcName = dataSrc.getName();
			if (srcName != null) {
				if (srcName.endsWith("n-triple")) {
					inputFormat = "N-TRIPLE";
				}
				else if (srcName.endsWith("n3")) {
					inputFormat = "N3";
				}
				else if (srcName.endsWith("owl")) {
					inputFormat = "RDF/XML";
				}
			}
			return sendData(dataSrc, inputFormat);
        }
		throw new ReasonerNotFoundException("No reasoner found.");
    }
    
	@Override
	public boolean sendData(DataSource dataSrc, String inputFormat)
			throws IOException, ReasonerNotFoundException, ConfigurationException {
		logger.info("Calling sendData({})", dataSrc);
		if (reasoner != null) {
			boolean reasonerStat = reasoner.loadInstanceData(dataSrc.getInputStream(), inputFormat);
			BufferedReader in = new BufferedReader(new InputStreamReader(dataSrc.getInputStream()));
			String base = null;
			RDFReader reader;
			try {
				reader = getInstanceData().getReader();
				reader.read(getInstanceData(), dataSrc.getInputStream(), base);
				in.close();
			} catch (InvalidNameException e) {
				throw new ConfigurationException(e.getMessage());
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return reasonerStat;
		}
		throw new ReasonerNotFoundException("No reasoner found.");
	}

	@Override
    public boolean loadData(String serverDataLocator)
            throws IOException, ReasonerNotFoundException, ConfigurationException {
        logger.info("Calling loadData(\"{}\")", serverDataLocator);
        if (reasoner != null) {
        	boolean reasonerStat = reasoner.loadInstanceData(serverDataLocator);
        	//TODO add this data to the SadlServerPE instance data model
    		if (!serverDataLocator.startsWith("file:") && !serverDataLocator.startsWith("http:")) {
    			serverDataLocator = "file:///" +  serverDataLocator;		
    		}
    		String altUrl = null;
    		String pubUri = null;
    		Model dataModel = null;
    		if (serverDataLocator.startsWith("http://")) {
    			// this might be a public URI; if it is it should be in the mapping and we'll find the actual
    			try {
					altUrl = getConfigurationMgr().getAltUrlFromPublicUri(serverDataLocator);
					pubUri = serverDataLocator;
				} catch (ConfigurationException e) {
					// this is ok--there will not be a mapping if this is actual, not public
				}
    		}
    		if (altUrl == null) {
				// no actual found so assume this is the actual and try to find a public URI mapping
				try {
					pubUri = getConfigurationMgr().getPublicUriFromActualUrl(serverDataLocator);
					altUrl = serverDataLocator;
				} catch (ConfigurationException e1) {
					// no mapping found, either as public or as actual, so use the default public 
					pubUri = getDefaultModelName();
				}

    		}
    		try {
    			if (pubUri != null) {
    				createInstanceModel(pubUri).add(getInstanceData().getDocumentManager().getFileManager().loadModel(serverDataLocator));
    			}
    			else {
					// no default
					dataModel = getConfigurationMgr().getJenaDocumentMgr().getFileManager().loadModel(serverDataLocator);
					if (dataModel != null) {
						createInstanceModel(lastChanceDefaultInstanceUri);
						try {
							setInstanceDataNamespace(lastChanceDefaultInstanceUri);
						} catch (InvalidNameException e2) {
							throw new ConfigurationException(e2.getMessage());
						}
					}
    			}
			} catch (Exception e) {
				throw new ConfigurationException(e.getMessage());
			}
        	return reasonerStat;
        }
		throw new ReasonerNotFoundException("No reasoner found.");
    }

	public boolean deleteModel(String modelName) throws ConfigurationException, IOException {
		try {
			return getConfigurationMgr().deleteModel(modelName);
		} catch (ConfigurationException e) {
			throw new ConfigurationException(e.getMessage());
		} catch (URISyntaxException e) {
			throw new ConfigurationException(e.getMessage());
		}
	}

	public boolean addExistingModel(String modelPublicUri, String modelAltUrl,
			String modelPrefix) throws InvalidNameException, ConfigurationException, IOException, URISyntaxException {
		try {
			String altUrl = getConfigurationMgr().getAltUrlFromPublicUri(modelPublicUri);
			if (altUrl != null && !(altUrl.equals(modelPublicUri))) {
				throw new InvalidNameException("Model '" + modelPublicUri + "' is already in the knowledge base with actual URL '" + altUrl + "'");
			}
		}
		catch (ConfigurationException e) {
			// this should happen if the model doesn't already exist
		}
		String existingPrefixUri = getConfigurationMgr().getUriFromGlobalPrefix(modelPrefix);
		if (existingPrefixUri != null) {
			throw new InvalidNameException("Prefix '" + modelPrefix + "' is already used for model '" + existingPrefixUri + "'");
		}
		try {
			String existingPublicUri = getConfigurationMgr().getPublicUriFromActualUrl(modelAltUrl);
			throw new InvalidNameException("The knowledge base already has a mapping of public URI '" + existingPublicUri + "' to location '" + modelAltUrl + "'");
		}
		catch (ConfigurationException e) {
			// this should happen if the model at the specified alternate URL isn't already in use in the model
		}
		
		// OK, we are good to go!
		if (!getConfigurationMgr().addMapping(modelAltUrl, modelPublicUri, modelPrefix, true, SADLSERVER_PE_SOURCE)) {
			return false;
		}
    	if (!getConfigurationMgr().saveOntPolicyFile()) {
    		return false;
    	}
		return true;
	}

	public boolean addImport( String importedModelUri) throws ConfigurationException, IOException, InvalidNameException, URISyntaxException {
		return addImport(getDefaultModelName(), importedModelUri);
	}
	
	public boolean addImport(String modelName, String importedModelUri) throws ConfigurationException, IOException, InvalidNameException, URISyntaxException {
		try {
			String altImportUrl = getConfigurationMgr().getAltUrlFromPublicUri(importedModelUri);
			OntModel model = getOntModelForEditing(modelName);
			Resource importingOntology = model.getOntology(modelName);
			if (importingOntology == null) {
				importingOntology = model.createResource(modelName);
			}
			model.getOntology(modelName).addImport(importingOntology);
			model.loadImports();
			return true;
		} catch (ConfigurationException e) {
			throw new ConfigurationException("Model cannot be imported as its location is unknown.", e);
		}
	}
	
	public boolean updateRdfsLabel(String uri, String label, String language) throws ConfigurationException, IOException, InvalidNameException, URISyntaxException {
		return updateRdfsLabel(getDefaultModelName(),  uri, label, language);
	}

	public boolean updateRdfsLabel(String modelName, String uri, String label, String language) throws IOException, ConfigurationException, InvalidNameException, URISyntaxException {
		OntModel ontModel = getOntModelForEditing(modelName);
		boolean retval = false;
		if (ontModel != null) {
			OntResource rsrc = ontModel.getOntResource(uri);
			if (rsrc == null) {
				throw new InvalidNameException("'" + uri + "' was not found in model '" + modelName + "'");
			}
			String lbl = rsrc.getLabel(language);
			if (lbl != null) {
				if (!lbl.equals(label)) {
					rsrc.removeLabel(lbl, language);
					retval = true;
				}
			}
			rsrc.addLabel(label, language);
		}
		return retval;
	}

	@Override
	public ResultSet ask(String modelName, String subjName, String propName,
			Object objValue) throws TripleNotFoundException,
			ReasonerNotFoundException, QueryCancelledException,
			SessionNotFoundException, IOException, ConfigurationException, InvalidNameException, URISyntaxException {
		if (reasoner == null) {
			throw new ReasonerNotFoundException("No Reasoner found; can't do query");
		}
		reasoner.reset();
		OntModel ontModel = getOntModelForEditing(modelName);
		reasoner.loadInstanceData(ontModel);
		return reasoner.ask(subjName, propName, objValue != null ? objValue.toString() : null);
	}

	@Override
	public ResultSet query(String query) throws QueryParseException, ReasonerNotFoundException, QueryCancelledException, IOException, ConfigurationException, InvalidNameException, URISyntaxException {
		if (query.startsWith("delete") || query.startsWith("insert")) {
			UpdateRequest urequest = UpdateFactory.create(query);
			OntModel ontModel = getOntModelForEditing(getDefaultModelName());
			UpdateAction.execute(urequest, ontModel);
		}
		try {
			return query(getDefaultModelName(), query);
		} catch (SessionNotFoundException e) {
			throw new IOException(e);
		}
	}
	
	public ResultSet query(String modelName, String query)
			throws QueryCancelledException, QueryParseException,
			ReasonerNotFoundException, SessionNotFoundException, IOException, ConfigurationException, InvalidNameException, URISyntaxException {
		if (reasoner == null) {
			throw new ReasonerNotFoundException("No Reasoner found; can't do query");
		}
		reasoner.reset();
		OntModel ontModel = getOntModelForEditing(modelName);
		reasoner.loadInstanceData(ontModel);
		return super.query(query);
	}

	public String prepareQuery(String modelName, String query) throws InvalidNameException, ReasonerNotFoundException,
			ConfigurationException, InvalidNameException, SessionNotFoundException, AmbiguousNameException {
		return super.prepareQuery(query);
	}

	public String parameterizeQuery(String modelName, String query, List<Object> values)
			throws InvalidNameException, ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, AmbiguousNameException {
		return super.parameterizeQuery(query, values);
	}

}
