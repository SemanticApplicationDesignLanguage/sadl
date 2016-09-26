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
 * $Revision: 1.18 $ Last modified on   $Date: 2015/07/31 11:32:33 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.reasoner.AvailablePlugin.PluginType;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ConfigurationItem.ConfigurationType;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.Bag;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.FileUtils;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * this class extension supports configuration tasks unique to the IDE
 * (development environment)
 * 
 * @author 200005201
 * 
 */
public class ConfigurationManagerForEditing extends ConfigurationManager
		implements IConfigurationManagerForEditing {

	protected String projectFolderPath = null;

	private static final String pSeqSizeLimit = "pSeqSizeLimit";

	private boolean configChanged = false;
	private boolean mappingChanged = false;

	protected long timeConfigFileLastModifiedAtInitialization = 0L;

	public ConfigurationManagerForEditing(String modelFolderPathname,
			String repoType) throws ConfigurationException {
		super(modelFolderPathname, repoType);
		init(modelFolderPathname);
	}

	public ConfigurationManagerForEditing(String modelFolderPathname,
			String repoType, boolean noModelFolderNeeded) throws ConfigurationException {
		super(modelFolderPathname, repoType, noModelFolderNeeded);
		if (!noModelFolderNeeded) {
			init(modelFolderPathname);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * setTranslatorClassName(java.lang.String)
	 */
	public boolean setTranslatorClassName(String translatorClassName)
			throws ConfigurationException {
		boolean bChanged = false;
		Property translatorClassNameProperty = getConfigModel().createProperty(
				pTRANSLATOR_CLASSNAME);
		Literal tcn = translatorClassName != null ? getConfigModel()
				.createTypedLiteral(translatorClassName) : null;
		IReasoner reasonerInst = getReasonerInstance();
		Resource reasonerCategory = getConfigModel().getResource(
				CONFIG_NAMESPACE + reasonerInst.getConfigurationCategory());
		List<Statement> stmts = new ArrayList<Statement>();
		StmtIterator sitr = getConfigModel().listStatements(reasonerCategory,
				getConfigModel().getProperty(pTRANSLATOR_CLASSNAME),
				(RDFNode) null);
		int stmtcntr = 0;
		if (sitr.hasNext()) {
			Statement stmtToChange = null;
			while (sitr.hasNext()) {
				Statement s = sitr.nextStatement();
				if (tcn == null || !s.getObject().equals(tcn)) {
					if (stmtcntr++ == 0) {
						stmtToChange = s;
						bChanged = true;
						setTranslator(null);
					} else {
						stmts.add(s);
					}
				}
			}
			sitr.close();

			if (stmtToChange != null) {
				if (tcn != null) {
					stmtToChange.changeObject(tcn);
				} else {
					getConfigModel().remove(stmtToChange);
				}
			}

			// remove triple for each stmt after first (only one translator
			// allowed)
			for (int i = 0; stmts != null && i < stmts.size(); i++) {
				Statement stmt = stmts.get(i);
				getConfigModel().remove(stmt);
				bChanged = true;
			}
		} else {
			// create entry from scratch
			getConfigModel().add(reasonerCategory, translatorClassNameProperty,
					tcn);
			bChanged = true;
		}
		if (bChanged) {
			saveConfigurationModel();
			return true;
		}
		return false;
	}

	/**
	 * Save the configuration model
	 * 
	 * @return
	 */
	protected synchronized boolean saveConfigurationModel() {
		FileOutputStream fps = null;
		try {
			String configFilename = getModelFolderPath().getCanonicalPath()
					+ File.separator + CONFIG_FILENAME;
			// save model
			fps = new FileOutputStream(configFilename);
			getConfigModel().write(fps, RDF_XML_ABBREV_FORMAT);
			return true;
		} catch (Exception e) {
			logger.error("Failed to save ont-policy file", e);
		} finally {
			if (fps != null) {
				try {
					fps.close();
				} catch (IOException e) {
					logger.error("Failed to close ont-policy file", e);
				}
			}
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * setReasonerClassName(java.lang.String)
	 */
	public boolean setReasonerClassName(String reasonerClassName) {
		boolean bChanged = false;
		Property reasonerClassNameProperty = getConfigModel().createProperty(
				pREASONER_CLASSNAME);
		Literal rcn = getConfigModel().createTypedLiteral(reasonerClassName);

		Resource reasonerSpec = getReasonerSpecResource();

		List<Statement> stmts = new ArrayList<Statement>();
		StmtIterator sitr = getConfigModel().listStatements(reasonerSpec,
				reasonerClassNameProperty, (RDFNode) null);
		int stmtcntr = 0;
		if (sitr.hasNext()) {
			Statement stmtToChange = null;
			while (sitr.hasNext()) {
				Statement s = sitr.nextStatement();
				if (!s.getObject().equals(rcn)) {
					if (stmtcntr++ == 0) {
						stmtToChange = s;
						bChanged = true;
						setReasoner(null);
						setTranslator(null); // if you change reasoner you
												// remove translator as it may
												// change as well
					} else {
						stmts.add(s);
					}
				}
			}
			sitr.close();

			if (stmtToChange != null) {
				stmtToChange.changeObject(rcn);
			}

			// remove triple for each stmt after the first--only one reasoner is
			// allowed
			for (int i = 0; stmts != null && i < stmts.size(); i++) {
				Statement stmt = stmts.get(i);
				getConfigModel().remove(stmt);
				bChanged = true;
			}
		} else {
			// create entry from scratch
			getConfigModel().add(reasonerSpec, reasonerClassNameProperty, rcn);
			bChanged = true;
		}
		if (bChanged) {
			saveConfigurationModel();
			return true;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * isConfigurationStale()
	 */
	public boolean isConfigurationStale() {
		String configFilename;
		try {
			configFilename = getConfigurationFilename();
			File configFile = new File(configFilename);
			if (configFile.exists()) {
				long curlastmod = configFile.lastModified();
				if (curlastmod > timeConfigFileLastModifiedAtInitialization) {
					return true;
				}
			}
		} catch (IOException e) {
			logger.error("Failed to get last modified from config file: "
					+ e.getLocalizedMessage());
		}
		return false;
	}

	protected String getConfigurationFilename() throws IOException {
		return getModelFolderPath().getCanonicalPath() + File.separator
				+ CONFIG_FILENAME;
	}

	protected boolean init(String modelFolderPathname)
			throws ConfigurationException {
		setModelFolderPath(new File(modelFolderPathname));

		String configFilename;
		try {
			configFilename = getConfigurationFilename();
			File configFile = new File(configFilename);
			if (configFile.exists()) {
				timeConfigFileLastModifiedAtInitialization = configFile
						.lastModified();
			}
		} catch (IOException e) {
			e.printStackTrace();
			throw new ConfigurationException(
					"Failed to get configuration File in folder '"
							+ modelFolderPathname + "'", e);
		} catch (Throwable t) {
			t.printStackTrace();
			throw new ConfigurationException(
					"Unexpected configuration File exception in folder '"
							+ modelFolderPathname + "': " + t.getMessage());
		}
		return true;

	}

	protected void initializeReasoner() throws ConfigurationException {
		if (isConfigurationStale()) {
			// read in a new config model
			setConfigModel(ModelFactory.createDefaultModel());
			getConfigModel().setNsPrefix("", CONFIG_NAMESPACE);
			String configFilename = null;
			try {
				configFilename = getConfigurationFilename();
				File configFile = new File(configFilename);
				if (configFile.exists()) {
					// load configuration info from file
					String syntax = FileUtils.guessLang(configFilename);
					FileInputStream in = new FileInputStream(configFile);
					getConfigModel().read(in, null, syntax);
					timeConfigFileLastModifiedAtInitialization = configFile
							.lastModified();
				} else {
					throw new ConfigurationException("Configuration file '"
							+ configFilename + "' not found.");
				}
			} catch (IOException e) {
				throw new ConfigurationException(
						"Exception getting configuration from file: "
								+ e.getMessage());
			}
		}
		super.initializeReasoner();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * replaceJenaModelCache(com.hp.hpl.jena.ontology.OntModel,
	 * java.lang.String)
	 */
	public boolean replaceJenaModelCache(OntModel model, String publicUri) {
		// model.getDocumentManager().setCacheModels(true);
		Model oldModel = model.getDocumentManager().getFileManager()
				.getFromCache(publicUri);
		if (oldModel != null && !model.equals(oldModel)) {
			model.getDocumentManager().getFileManager()
					.removeCacheModel(publicUri);
			model.getDocumentManager().addModel(publicUri, model, true);
		}
		if (model.getImportModelMaker().hasModel(publicUri)) {
			model.getImportModelMaker().removeModel(publicUri);
		}
		// model.getDocumentManager().setCacheModels(false);
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#resetJena()
	 */
	public boolean resetJena() {
		getJenaDocumentMgr().clearCache();
		getJenaDocumentMgr().getFileManager().resetCache();
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * clearJenaModelContent(com.hp.hpl.jena.ontology.OntModel)
	 */
	public boolean clearJenaModelContent(OntModel model) {
		ExtendedIterator<OntModel> smitr = model.listSubModels();
		while (smitr.hasNext()) {
			OntModel nsm = smitr.next();
			model.remove(nsm);
			model.removeSubModel(nsm);
		}
		try {
			model.removeAll(); // also clear all statements from the model so
								// it's as if it were freshly created.
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#addMapping
	 * (java.lang.String, java.lang.String, java.lang.String)
	 */
	public synchronized boolean addMapping(String altUrl, String publicUri,
			String globalPrefix, boolean bKeepPrefix, String source) throws ConfigurationException,
			IOException, URISyntaxException {
		SadlUtils su = new SadlUtils();
		su.validateHTTP_URI(publicUri);
		Resource pubv = getMappingModel().createResource(publicUri);
		Resource altv = getMappingModel().createResource(altUrl);
		Literal pref = null;
		if (globalPrefix != null) {
			pref = getMappingModel().createTypedLiteral(globalPrefix);
		}
		return addMapping(altv, pubv, pref, bKeepPrefix, source);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#addMapping
	 * (com.hp.hpl.jena.rdf.model.Resource, com.hp.hpl.jena.rdf.model.Resource,
	 * com.hp.hpl.jena.rdf.model.Literal)
	 */
	public synchronized boolean addMapping(Resource altv, Resource pubv, Literal prefix, boolean bKeepPrefix,
			String source) throws ConfigurationException, IOException,
			URISyntaxException {
		boolean bChanged = false;
		boolean mappingFound = false;
		List<Statement> pendingDeletions = null;
		// Get all the statements that have this public URI
		StmtIterator pubitr = getMappingModel().listStatements(null,
				publicUrlProp, pubv);
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
				} else {
					Resource subj = s.getSubject();
					// find the corresponding altURL
					Statement s2 = subj.getProperty(altUrlProp);
					if (s2 != null) {
						// Is the old and the new actual URL the same? If not
						// then change the statement for the actual URL
						if (!s2.getObject().equals(altv)) {
							if (pendingDeletions == null) {
								pendingDeletions = new ArrayList<Statement>();
							}
							pendingDeletions.add(s2);
							subj.addProperty(altUrlProp, altv);
							bChanged = true;
						}
					} else {
						subj.addProperty(altUrlProp, altv);
						bChanged = true;
					}
					Statement s3 = subj.getProperty(prefixProp);
					if (s3 != null) {
						// there is already a prefix in the model
						if (prefix != null) {
							// we have another which is not null
							if (!s3.getObject().equals(prefix)) {
								if (!bKeepPrefix) {
									// only make the change if not keeping old prefix (when the new prefix is null)
									if (pendingDeletions == null) {
										pendingDeletions = new ArrayList<Statement>();
									}
									pendingDeletions.add(s3);
								}
								if (prefix != null) {
									subj.addProperty(prefixProp, prefix);
								}
								bChanged = true;
							}
						}
					} else if (prefix != null) {
						subj.addProperty(prefixProp, prefix);
						bChanged = true;
					}
				}
				cntr++;
			}
		}
		StmtIterator altitr = getMappingModel().listStatements(null,
				altUrlProp, altv);
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
				} else {
					if (!bChanged) {
						// if bChanged is true then we must have already fixed
						// the one mapping in the section above--no need to do
						// it again
						Resource subj = s.getSubject();
						// find the corresponding publicUri
						Statement s2 = subj.getProperty(publicUrlProp);
						if (s2 != null) {
							// is the old and the new public URI the same? If
							// not then change the statement for the new public
							// URI
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
							// there is already a prefix in the model
							if (prefix != null) {
								// we have another which is not null
								if (!s3.getObject().equals(prefix)) {
									if (!bKeepPrefix) {
										// only make the change if not keeping old prefix (when the new prefix is null)
										if (pendingDeletions == null) {
											pendingDeletions = new ArrayList<Statement>();
										}
										pendingDeletions.add(s3);
									}
									if (prefix != null) {
										subj.addProperty(prefixProp, prefix);
									}
									bChanged = true;
								}
							}
						} else if (prefix != null) {
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
				getMappingModel().remove(s);
				bChanged = true;
			}
		}

		if (!mappingFound) {
			// create a new entry from scratch
			if (sadlNode == null) {
				sadlNode = getMappingModel().createTypedLiteral(SADL);
				createdBy = getMappingModel().createProperty(
						ONT_MANAGER_CREATED_BY);
				altUrlProp = getMappingModel().createProperty(
						ONT_MANAGER_ALT_URL);
				publicUrlProp = getMappingModel().createProperty(
						ONT_MANAGER_PUBLIC_URI);
				prefixProp = getMappingModel().createProperty(
						ONT_MANAGER_PREFIX);
			}
			com.hp.hpl.jena.rdf.model.Resource type = getMappingModel()
					.createResource(ONT_MANAGER_ONTOLOGY_SPEC);
			com.hp.hpl.jena.rdf.model.Resource newOntSpec = getMappingModel()
					.createResource(type);
			Property langp = getMappingModel()
					.getProperty(ONT_MANAGER_LANGUAGE);
			RDFNode langv = getMappingModel().createResource(
					OWL_ONT_MANAGER_PUBLIC_URINS);
			getMappingModel().add(newOntSpec, publicUrlProp, pubv);
			getMappingModel().add(newOntSpec, altUrlProp, altv);
			getMappingModel().add(newOntSpec, langp, langv);
			if (source != null && !source.equalsIgnoreCase(SADL)) {
				getMappingModel().add(newOntSpec, createdBy, source);
			} else {
				getMappingModel().add(newOntSpec, createdBy, SADL);
			}
			if (prefix != null) {
				getMappingModel().add(newOntSpec, prefixProp, prefix);
			}
			logger.debug("Created new mapping for '" + pubv.toString() + "', '"
					+ altv.toString() + "'");
			bChanged = true;
		}
		try {
			// add mapping to Jena OntDocumentManager
			if (addJenaMapping(pubv.getURI().toString(), altv.getURI()
					.toString())) {
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
			logger.debug("Modified mapping for '" + pubv.toString() + "', '"
					+ altv.toString() + "'");
		}
		if (this.mappings == null) {
			mappings = new HashMap<String, String>();
		}
		mappings.put(rdfNodeToString(pubv), rdfNodeToString(altv));
		return bChanged;
	}

	public synchronized Model getMappingModel() {
		if (mappingModel == null) {
			setMappingModel(ModelFactory.createDefaultModel());
			mappingChanged = true;
		}
		return mappingModel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#addJenaMapping
	 * (java.lang.String, java.lang.String)
	 */
	public synchronized boolean addJenaMapping(String publicUri, String altUrl)
			throws IOException, URISyntaxException, ConfigurationException {
		if (publicUri == null) {
			throw new ConfigurationException(
					"Mapping can't have a null publicURI.");
		}
		if (altUrl == null) {
			throw new ConfigurationException("Mapping of publicURI '"
					+ publicUri + "' can't be to null.");
		}
		getJenaDocumentMgr().addAltEntry(publicUri, altUrl);
		return true;
	}

	public synchronized boolean deleteMapping(String publicUri, String altUrl)
			throws IOException, URISyntaxException, ConfigurationException {
		if (publicUri == null) {
			throw new ConfigurationException(
					"Mapping can't have a null publicURI.");
		}
		Resource altv = getMappingModel().createResource(altUrl);
		StmtIterator sitr = getMappingModel().listStatements(null, altUrlProp,
				altv);
		if (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource rsrc = stmt.getSubject();
			getMappingModel().removeAll(rsrc, null, null);
			saveOntPolicyFile();
		}
		mappings.remove(publicUri);
		globalPrefixes.remove(publicUri);
		getJenaDocumentMgr().forget(publicUri);
		getJenaDocumentMgr().getFileManager()
				.removeCacheModel(publicUri);
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#deleteModel
	 * (java.lang.String)
	 */
	public boolean deleteModel(String publicUri) throws ConfigurationException,
			IOException, URISyntaxException {
		String altUrl = getAltUrlFromPublicUri(publicUri);
		if (repoType.equals(IConfigurationManager.JENA_TDB)) {
			try {
				OntModel modelToDelete = getModelGetter().getOntModel(
						publicUri, altUrl, IConfigurationManager.JENA_TDB);
				modelToDelete.removeAll();
				getModelGetter().sync();
			} catch (Throwable t) {
				// ok to fail; may not exist
			}
		} else {
			if (altUrl.startsWith("http:")) {
				throw new IOException(
						"Can't delete a model with only an 'http://' URL");
			} else {
				SadlUtils su = new SadlUtils();
				File modelFile = new File(su.fileUrlToFileName(altUrl));
				if (modelFile.exists()) {
					if (!modelFile.delete()) {
						throw new IOException("Unable to delete '" + altUrl
								+ "'");
					}
				}
			}
		}
		return deleteMapping(publicUri, altUrl);
	}

	/**
	 * Method to get a list of all the available reasoner plugins using a
	 * {@link ServiceLoader}
	 * 
	 * @return A list of all available reasoner plugins
	 */
	public static List<AvailablePlugin> getAvailableReasonerPlugins() {
		List<AvailablePlugin> rval = new ArrayList<AvailablePlugin>();
		List<IReasoner> reasoners = getAvailableReasoners();

		for (Iterator<IReasoner> itr = reasoners.iterator(); itr.hasNext();) {
			IReasoner rsnr = itr.next();
			String category = rsnr.getConfigurationCategory();
			String family = rsnr.getReasonerFamily();
			String clsNm = rsnr.getClass().getCanonicalName();
			AvailablePlugin avplg = new AvailablePlugin(PluginType.Reasoner,
					category, family, clsNm);
			rval.add(avplg);
		}
		return rval;
	}

	/**
	 * Method to get a list of all the available reasoners using a
	 * {@link ServiceLoader}
	 * 
	 * @return A list of all available reasoners
	 */
	public static List<IReasoner> getAvailableReasoners() {
		List<IReasoner> reasoners = new ArrayList<IReasoner>();
		ServiceLoader<IReasoner> serviceLoader = getReasonersFromServiceLoader(IReasoner.class);
		if (serviceLoader != null) {
			for (Iterator<IReasoner> itr = serviceLoader.iterator(); itr
					.hasNext();) {
				reasoners.add(itr.next());
			}
		}
		return reasoners;
	}

	/**
	 * Method to get a list of all the available implementations of IGraphVisualizer using a
	 * {@link ServiceLoader}
	 * 
	 * @return A list of all available IGraphVisualizer implementations
	 */
	public List<IGraphVisualizer> getAvailableGraphRenderers() {
		List<IGraphVisualizer> renderers = new ArrayList<IGraphVisualizer>();
		ServiceLoader<IGraphVisualizer> serviceLoader = getGraphRenderersFromServiceLoader(IGraphVisualizer.class);
		if (serviceLoader != null) {
			for (Iterator<IGraphVisualizer> itr = serviceLoader.iterator(); itr
					.hasNext();) {
				renderers.add(itr.next());
			}
		}
		return renderers;
	}

	/**
	 * Method to get a list of all the available translator plugins using a
	 * {@link ServiceLoader}
	 * 
	 * @return A list of all available translator plugins that belong to the
	 *         same family as the current reasoner
	 */
	public static List<AvailablePlugin> getAvailableTranslatorPlugins() {
		List<AvailablePlugin> rval = new ArrayList<AvailablePlugin>();
		List<ITranslator> translators = getAvailableTranslators();
		for (Iterator<ITranslator> itr = translators.iterator(); itr.hasNext();) {
			ITranslator trans = itr.next();
			String category = trans.getConfigurationCategory();
			String family = trans.getReasonerFamily();
			String clsNm = trans.getClass().getCanonicalName();
			AvailablePlugin avplg = new AvailablePlugin(PluginType.Translator,
					category, family, clsNm);
			rval.add(avplg);
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * getAvailableTranslatorPluginsForCurrentReasoner()
	 */
	public List<AvailablePlugin> getAvailableTranslatorPluginsForCurrentReasoner()
			throws ConfigurationException {
		IReasoner reasonerInst = getReasonerInstance();
		List<AvailablePlugin> rval = new ArrayList<AvailablePlugin>();
		List<ITranslator> translators = getAvailableTranslators();
		for (Iterator<ITranslator> itr = translators.iterator(); itr.hasNext();) {
			ITranslator trans = itr.next();
			String category = trans.getConfigurationCategory();
			String family = trans.getReasonerFamily();
			if (family != null
					&& family.equals(reasonerInst.getReasonerFamily())) {
				String clsNm = trans.getClass().getCanonicalName();
				AvailablePlugin avplg = new AvailablePlugin(
						PluginType.Translator, category, family, clsNm);
				rval.add(avplg);
			}
		}
		return rval;
	}

	protected static ServiceLoader<ITranslator> getTranslatorsFromServiceLoader(
			Class<ITranslator> cls) {
		return ServiceLoader.load(cls);
	}

	public ServiceLoader<Class<?>> getServiceLoader(Class<?> bcls) {
		return (ServiceLoader<Class<?>>) ServiceLoader.load(bcls);
	}

	protected static ServiceLoader<IReasoner> getReasonersFromServiceLoader(
			Class<IReasoner> cls) {
		return ServiceLoader.load(cls);
	}

	protected static ServiceLoader<IGraphVisualizer> getGraphRenderersFromServiceLoader(
			Class<IGraphVisualizer> cls) {
		return ServiceLoader.load(cls);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * getAvailableBuiltinsForCurrentReasoner()
	 */
	public List<BuiltinInfo> getAvailableBuiltinsForCurrentReasoner()
			throws ConfigurationException {
		IReasoner reasonerInst = getReasonerInstance();

		// get the implicit builtins
		List<BuiltinInfo> rval = reasonerInst.getImplicitBuiltins();

		// get the service registered builtins
		Class<?> bcls = reasonerInst.getBuiltinClass();
		ServiceLoader<Class<?>> serviceLoader = getServiceLoader(bcls);

		if (serviceLoader != null) {
			for (Iterator<Class<?>> itr = serviceLoader.iterator(); itr
					.hasNext();) {
				try {
					Object trans = itr.next();
					BuiltinInfo binfo = reasonerInst.getBuiltinInfo(trans
							.getClass());
					if (binfo != null) {
						rval.add(binfo);
					}
				} catch (Throwable t) {
					t.printStackTrace();
				}
			}
		}

		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * addConfiguration(com.ge.research.sadl.reasoner.ConfigurationItem)
	 */
	// When adding or updating, we must take the implicit Category hierarchy
	// into account,
	// e.g., Builtins is a sub-category of the reasoner's Category instance

	public void addConfiguration(ConfigurationItem newItem)
			throws ConfigurationException {
		if (getConfigModel() != null) {
			Resource type = getConfigModel().createResource(CATEGORY_KW);
			Property subcatprop = getConfigModel().createProperty(
					SUBCATEGORY_PROP); // we'll use this repeatedly
			String[] categoryHierarchy = newItem.getCategoryHierarchy();
			String lastCategory = null;
			Resource lastCategoryResource = null;
			Resource beforeLastCategory = null;
			List<NameValuePair> nvplist = newItem.getNameValuePairs();
			for (int i = 0; i < categoryHierarchy.length; i++) {
				String newCategory = categoryHierarchy[i];
				Resource newCategoryResource = null;
				if (i < (categoryHierarchy.length - 1)) {
					// don't add the last one unless it's a SingleValue, in
					// which case do it before
					newCategoryResource = getConfigModel().createResource(
							CONFIG_NAMESPACE + newCategory, type);
					if (i > 0) {
						lastCategoryResource = getConfigModel().createResource(
								CONFIG_NAMESPACE + lastCategory, type);
						getConfigModel().add(lastCategoryResource, subcatprop,
								newCategoryResource);
					}
				}
				if (lastCategoryResource != null) {
					beforeLastCategory = lastCategoryResource;
				}
				lastCategory = newCategory;
				if (newCategoryResource != null) {
					lastCategoryResource = newCategoryResource;
				}
			}
			if (lastCategory != null) {
				Resource subject = null;
				for (int i = 0; nvplist != null && i < nvplist.size(); i++) {
					NameValuePair nvp = nvplist.get(i);
					Property prop = getConfigModel().createProperty(
							CONFIG_NAMESPACE + nvp.getName());
					if (nvp.getValue() != null) {
						if (nvp.getConfigType().equals(ConfigurationType.Bag)) {
							if (beforeLastCategory != null) {
								Statement stmt = beforeLastCategory
										.getProperty(subcatprop);
								RDFNode bag;
								if (stmt != null) {
									bag = stmt.getObject();
								} else {
									bag = getConfigModel().createBag();
									getConfigModel().add(beforeLastCategory,
											subcatprop, bag);
								}
								if (bag != null
										&& bag instanceof Resource
										&& ((Resource) bag).hasProperty(
												RDF.type, RDF.Bag)) {
									if (subject == null) {
										Resource bagItemType = getConfigModel()
												.createResource(
														CONFIG_NAMESPACE
																+ lastCategory);
										subject = getConfigModel()
												.createResource(bagItemType);
										((Bag) bag.as(Bag.class)).add(subject);
									}
									getConfigModel().add(
											subject,
											prop,
											getConfigModel()
													.createTypedLiteral(
															nvp.getValue()));
								}
							} else if (lastCategory != null) {
								subject = getConfigModel().createResource(
										CONFIG_NAMESPACE + lastCategory);
								Statement stmt = subject.getProperty(prop);
								RDFNode bag;
								if (stmt != null) {
									bag = stmt.getObject();
								} else {
									bag = getConfigModel().createBag();
									subject.addProperty(prop, bag);
								}
								if (bag != null
										&& bag instanceof Resource
										&& ((Resource) bag).hasProperty(
												RDF.type, RDF.Bag)) {
									((Bag) bag.as(Bag.class))
											.add(getConfigModel()
													.createTypedLiteral(
															nvp.getValue()));
								}
							} else {
								throw new ConfigurationException(
										"Unable to add Bag to single-level category ("
												+ lastCategory + ")");
							}
						} else if (nvp.getConfigType().equals(
								ConfigurationType.Sequence)) {
							if (beforeLastCategory != null) {
								Statement stmt = beforeLastCategory
										.getProperty(subcatprop);
								RDFNode seq;
								if (stmt != null) {
									seq = stmt.getObject();
								} else {
									seq = getConfigModel().createSeq();
									getConfigModel().add(beforeLastCategory,
											subcatprop, seq);
								}
								if (seq != null
										&& seq instanceof Resource
										&& ((Resource) seq).hasProperty(
												RDF.type, RDF.Seq)) {
									if (subject == null) {
										Resource bagItemType = getConfigModel()
												.createResource(
														CONFIG_NAMESPACE
																+ lastCategory);
										subject = getConfigModel()
												.createResource(bagItemType);
										((Seq) seq.as(Seq.class)).add(subject);
									}
									getConfigModel().add(
											subject,
											prop,
											getConfigModel()
													.createTypedLiteral(
															nvp.getValue()));
								}
							} else if (lastCategory != null) {
								subject = getConfigModel().createResource(
										CONFIG_NAMESPACE + lastCategory);
								Statement stmt = subject.getProperty(prop);
								RDFNode seq;
								if (stmt != null) {
									seq = stmt.getObject();
								} else {
									seq = getConfigModel().createSeq();
									subject.addProperty(prop, seq);
								}
								if (seq != null
										&& seq instanceof Resource
										&& ((Resource) seq).hasProperty(
												RDF.type, RDF.Seq)) {
									((Seq) seq.as(Seq.class))
											.add(getConfigModel()
													.createTypedLiteral(
															nvp.getValue()));
								}
								// check the size limit
								Statement sslStmt = subject
										.getProperty(getConfigModel()
												.createProperty(
														CONFIG_NAMESPACE
																+ pSeqSizeLimit));
								if (sslStmt != null) {
									RDFNode sizeLimitNode = sslStmt.getObject();
									if (sizeLimitNode instanceof Literal) {
										int sslval = ((Literal) sizeLimitNode)
												.getInt();
										int cursize = ((Seq) seq.as(Seq.class))
												.size();
										if (cursize >= sslval) {
											((Seq) seq.as(Seq.class)).remove(1);
										}
									}
								}
							} else {
								throw new ConfigurationException(
										"Unable to add Sequence to single-level category ("
												+ lastCategory + ")");
							}
						} else {
							if (subject == null) {
								if (beforeLastCategory == null
										&& lastCategory != null) {
									subject = getConfigModel().createResource(
											CONFIG_NAMESPACE + lastCategory,
											type);
								} else {
									getConfigModel().add(beforeLastCategory,
											subcatprop, subject);
								}
							}
							subject.removeAll(prop);
							subject.addProperty(prop, getConfigModel()
									.createTypedLiteral(nvp.getValue()));
						}
					} else {
						// the value is null
						throw new ConfigurationException(
								"Value of ConfigurationItem is null!");
					}
				}
			}
			clearReasoner();
			setConfigChanged(true);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * updateConfiguration(com.ge.research.sadl.reasoner.ConfigurationItem)
	 */
	public void updateConfiguration(ConfigurationItem newItem)
			throws ConfigurationException {
		if (getConfigModel() != null) {
			Resource type = getConfigModel().createResource(CATEGORY_KW);
			Property subcatprop = getConfigModel().createProperty(
					SUBCATEGORY_PROP); // we'll use this repeatedly
			String[] categoryHierarchy = newItem.getCategoryHierarchy();
			Resource lastCategory = null;
			List<NameValuePair> nvplist = newItem.getNameValuePairs();
			for (int i = 0; i < categoryHierarchy.length; i++) {
				Resource newCategory = getConfigModel().createResource(
						CONFIG_NAMESPACE + categoryHierarchy[i], type);
				if (i > 0) {
					getConfigModel().add(lastCategory, subcatprop, newCategory);
				}
				lastCategory = newCategory;
			}
			if (lastCategory != null) {
				for (int i = 0; nvplist != null && i < nvplist.size(); i++) {
					NameValuePair nvp = nvplist.get(i);
					Property prop = getConfigModel().createProperty(
							CONFIG_NAMESPACE + nvp.getName());
					Statement stmt = lastCategory.getProperty(prop);
					if (stmt != null) {
						if (nvp.getConfigType().equals(ConfigurationType.Bag)) {
							throw new ConfigurationException(
									"Can't update a Bag");
						} else if (nvp.getConfigType().equals(
								ConfigurationType.Sequence)) {
							throw new ConfigurationException(
									"Can't update a Sequence");
						} else {
							Statement updateStmt = lastCategory
									.getProperty(getConfigModel()
											.createProperty(
													CONFIG_NAMESPACE
															+ nvp.getName()));
							updateStmt.changeObject(getConfigModel()
									.createTypedLiteral(nvp.getValue()));
						}
					} else {
						getConfigModel().add(
								lastCategory,
								prop,
								getConfigModel().createTypedLiteral(
										nvp.getValue()));
					}
				}
			}
			clearReasoner();
			setConfigChanged(true);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * saveConfiguration()
	 */
	public synchronized boolean saveConfiguration() {
		if (getConfigModel() == null || !isConfigChanged()) {
			return false;
		}
		String configFilename;
		try {
			configFilename = getConfigurationFilename();
			File configFile = new File(configFilename);
			FileOutputStream fps = null;
			try {
				// save model
				fps = new FileOutputStream(configFile);
				getConfigModel().write(fps, RDF_XML_ABBREV_FORMAT);
				setConfigChanged(false);
				return true;
			} catch (Exception e) {
				logger.error("Failed to save configuration file", e);
			} finally {
				if (fps != null) {
					fps.close();
				}
				// refresh Eclipse Resource
				// ResourceManager.refreshResource(configFilename);
			}
			setConfigChanged(false);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * saveOntPolicyFile()
	 */
	public synchronized boolean saveOntPolicyFile() {
		if (getMappingModel() == null || !isMappingChanged()) {
			return false;
		}
		String mappingFilename;
		try {
			mappingFilename = getModelFolderPath().getCanonicalPath()
					+ File.separator + ONT_POLICY_RDF;
			File mappingFile = new File(mappingFilename);
			FileOutputStream fps = null;
			try {
				// save model
				fps = new FileOutputStream(mappingFile);
				getMappingModel().write(fps, RDF_XML_ABBREV_FORMAT);
				setMappingChanged(false);
				logger.debug("saved mapping file '" + mappingFilename + "'");
				return true;
			} catch (Exception e) {
				logger.error("Failed to save ont-policy file", e);
			} finally {
				if (fps != null) {
					fps.close();
				}
				try {
					// refresh Eclipse Resource
					// ResourceManager.refreshResource(mappingFilename);
				} catch (Throwable t) {
					logger.error("Failed to refresh ont-policy file: "
							+ t.getLocalizedMessage());
				}
			}
			setMappingChanged(false);
			logger.debug("Successfully saved ont-policy file.");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	protected void setConfigChanged(boolean configChanged) {
		this.configChanged = configChanged;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#isConfigChanged
	 * ()
	 */
	public boolean isConfigChanged() {
		return configChanged;
	}

	protected void validateModelFolderPath(String modelFolderPathname)
			throws ConfigurationException {
		File mfpnf = new File(modelFolderPathname);
		if (!mfpnf.exists()) {
			boolean stat = mfpnf.mkdir();
			if (!stat) {
				throw new ConfigurationException(
						"Unable to create model folder '" + modelFolderPathname
								+ "'");
			}
		}
		if (!mfpnf.isDirectory()) {
			throw new ConfigurationException("'" + modelFolderPathname
					+ "' is not a directory.");
		}
		// setModelFolderPath(mfpnf);
		super.validateModelFolderPath(modelFolderPathname);
	}

	protected void setMappingChanged(boolean mappingChanged) {
		this.mappingChanged = mappingChanged;
	}

	protected boolean isMappingChanged() {
		return mappingChanged;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * getProjectFolderPath()
	 */
	public String getProjectFolderPath() throws URISyntaxException {
		if (projectFolderPath == null) {
			if (modelFolderPath != null) {
				SadlUtils su = new SadlUtils();
				projectFolderPath = su.fileNameToFileUrl(modelFolderPath
						.getParent());
			}
		}
		return projectFolderPath;
	}

	/**
	 * Method to get a list of all the available translators using a
	 * {@link ServiceLoader}
	 * 
	 * @return A list of all available translators
	 */
	public static List<ITranslator> getAvailableTranslators() {
		List<ITranslator> translators = new ArrayList<ITranslator>();
		ServiceLoader<ITranslator> serviceLoader = getTranslatorsFromServiceLoader(ITranslator.class);
		if (serviceLoader != null) {
			for (Iterator<ITranslator> itr = serviceLoader.iterator(); itr
					.hasNext();) {
				translators.add(itr.next());
			}
		}
		return translators;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#addGlobalPrefix
	 * (java.lang.String, java.lang.String)
	 */
	public synchronized void addGlobalPrefix(String modelName, String globalPrefix) {
		String policyFilePrefix = getMappingPrefix(modelName);
		if (globalPrefix != null) {
			if (globalPrefixes == null) {
				globalPrefixes = new HashMap<String, String>();
			}
			globalPrefixes.put(modelName, globalPrefix);
		} else if (globalPrefixes != null
				&& globalPrefixes.containsKey(modelName)) {
			globalPrefixes.remove(modelName);
		}

		boolean bChanged = false;
		if ((globalPrefix != null && policyFilePrefix == null)
				|| (globalPrefix == null && policyFilePrefix != null)) {
			bChanged = true;
		} else if (globalPrefix != null && policyFilePrefix != null
				&& !policyFilePrefix.equals(globalPrefix)) {
			bChanged = true;
		}
		if (bChanged) {
			setMappingPrefix(modelName, globalPrefix);
			setMappingChanged(true);
			logger.debug("Changed global prefix for '" + modelName + "' to '"
					+ globalPrefix + "'");
		}

	}

	private void setMappingPrefix(String modelName, String globalPrefix) {
		Resource publicUri = getMappingModel().createResource(modelName);
		StmtIterator sitr = getMappingModel().listStatements(null,
				publicUrlProp, publicUri);
		Resource ontSpec = null;
		Statement prefixStmt = null;
		if (sitr.hasNext()) {
			ontSpec = sitr.nextStatement().getSubject();
			prefixStmt = ontSpec.getProperty(prefixProp);
		}
		sitr.close();
		if (prefixStmt != null) {
			getMappingModel().remove(prefixStmt);
		}
		if (ontSpec != null) {
			ontSpec.addProperty(prefixProp, getMappingModel()
					.createTypedLiteral(globalPrefix));
		}
	}

	private synchronized String getMappingPrefix(String modelName) {
		Resource publicUri = getMappingModel().createResource(modelName);
		StmtIterator sitr = getMappingModel().listStatements(null,
				publicUrlProp, publicUri);
		if (sitr.hasNext()) {
			Resource ontSpec = sitr.nextStatement().getSubject();
			Statement stmt = ontSpec.getProperty(prefixProp);
			if (stmt != null) {
				RDFNode val = stmt.getObject();
				if (val.isLiteral()) {
					return ((Literal) val).getLexicalForm();
				}
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * setProjectFolderPath(java.lang.String)
	 */
	public void setProjectFolderPath(String _projectFolderPath) {
		projectFolderPath = _projectFolderPath;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * getNamedConceptsInModel(com.hp.hpl.jena.ontology.OntModel,
	 * java.lang.String, com.ge.research.sadl.utils.SadlUtils.ConceptType,
	 * com.ge.research.sadl.reasoner.ConfigurationManagerForEditing.Scope)
	 */
	public List<ConceptName> getNamedConceptsInModel(OntModel localModel,
			String modelName, ConceptType cType, Scope scope)
			throws InvalidNameException {
		List<ConceptName> lst = new ArrayList<ConceptName>();
		List<ConceptType> typesToInclude;
		logger.debug("getNamedConceptsInModel called with ConceptType '"
				+ cType + "'");

		// this is for concepts in models, not models (default model name or
		// possible imports)
		if (cType == null) {
			// null type -> all types
			typesToInclude = new ArrayList<ConceptType>(5);
			typesToInclude.add(ConceptType.ANNOTATIONPROPERTY);
			typesToInclude.add(ConceptType.DATATYPEPROPERTY);
			typesToInclude.add(ConceptType.INDIVIDUAL);
			typesToInclude.add(ConceptType.OBJECTPROPERTY);
			typesToInclude.add(ConceptType.ONTCLASS);
		} else {
			typesToInclude = new ArrayList<ConceptType>(1);
			typesToInclude.add(cType);
		}

		String modelns = ConfigurationManager
				.addHashToNonTerminatedNamespace(modelName);

		for (int i = 0; i < typesToInclude.size(); i++) {
			ConceptType actype = typesToInclude.get(i);
			switch (actype) {
			case ANNOTATIONPROPERTY:
				ExtendedIterator<AnnotationProperty> apitr = localModel
						.listAnnotationProperties();
				if (apitr.hasNext()) {
					while (apitr.hasNext()) {
						AnnotationProperty aprop = apitr.next();
						if (!aprop.isAnon()) {
							ConceptName inclname = getNamedConceptFromModel(
									localModel, aprop, modelns, scope,
									localModel);
							if (inclname != null) {
								inclname.setType(ConceptType.ANNOTATIONPROPERTY);
								inclname.setNamespace(aprop.getNameSpace());
								lst.add(inclname);
							}
						}
					}
				}
				break;
			case DATATYPEPROPERTY:
				ExtendedIterator<DatatypeProperty> dpitr = localModel
						.listDatatypeProperties();
				if (dpitr.hasNext()) {
					while (dpitr.hasNext()) {
						DatatypeProperty dprop = dpitr.next();
						if (!dprop.isAnon()) {
							ConceptName inclname = getNamedConceptFromModel(
									localModel, dprop, modelns, scope,
									localModel);
							if (inclname != null) {
								inclname.setType(ConceptType.DATATYPEPROPERTY);
								inclname.setNamespace(dprop.getNameSpace());
								lst.add(inclname);
							}
						}
					}
				}
				break;
			case INDIVIDUAL:
				ExtendedIterator<Individual> iitr = localModel
						.listIndividuals();
				if (iitr.hasNext()) {
					while (iitr.hasNext()) {
						Individual inst = iitr.next();
						if (!inst.isAnon()) {
							ConceptName inclname = getNamedConceptFromModel(
									localModel, inst, modelns, scope,
									localModel);
							if (inclname != null) {
								inclname.setType(ConceptType.INDIVIDUAL);
								lst.add(inclname);
							}
						}
					}
				}
				break;
			case OBJECTPROPERTY:
				ExtendedIterator<ObjectProperty> opitr = localModel
						.listObjectProperties();
				if (opitr.hasNext()) {
					while (opitr.hasNext()) {
						ObjectProperty oprop = opitr.next();
						if (!oprop.isAnon()) {
							ConceptName inclname = getNamedConceptFromModel(
									localModel, oprop, modelns, scope,
									localModel);
							if (inclname != null) {
								inclname.setType(ConceptType.OBJECTPROPERTY);
								inclname.setNamespace(oprop.getNameSpace());
								lst.add(inclname);
							}
						}
					}
				}
				break;
			case ONTCLASS:
				ExtendedIterator<OntClass> citr = localModel.listClasses();
				if (citr.hasNext()) {
					while (citr.hasNext()) {
						OntClass aclass = citr.next();
						if (!aclass.isAnon()) {
							ConceptName inclname = getNamedConceptFromModel(
									localModel, aclass, modelns, scope,
									localModel);
							if (inclname != null) {
								inclname.setType(ConceptType.ONTCLASS);
								inclname.setNamespace(aclass.getNameSpace());
								lst.add(inclname);
							}
						}
					}
				}
				break;
			default:
				break;
			}
		}
		return lst;
	}

	protected ConceptName getNamedConceptFromModel(OntModel om, Resource aprop,
			String namespace, Scope scope, OntModel localModel)
			throws InvalidNameException {
		if (aprop.isURIResource()
				&& ((scope != null && scope.equals(Scope.INCLUDEIMPORTS)) || aprop
						.getNameSpace().equals(namespace))) {
			String prefix = getPrefix(om, aprop.getNameSpace());
			ConceptName cn = new ConceptName(prefix, aprop.getLocalName());
			cn.setNamespace(aprop.getNameSpace());
			return cn;
		}
		return null;
	}

	protected String getPrefix(OntModel om, String uri) {
		String prefix = om.getNsURIPrefix(uri);
		if (prefix == null || prefix.length() == 0) {
			if (uri.endsWith("#")) {
				uri = uri.substring(0, uri.length() - 1);
			}
			return getGlobalPrefix(uri);
		}
		return prefix;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.reasoner.IConfigurationManagerForEditing#
	 * getJenaDocumentMgr()
	 */
	public OntDocumentManager getJenaDocumentMgr() {
		// Note: for an editing configuration manager, 
		//	the OntDocumentManager must be set explicitly
		return jenaDocumentMgr;
	}

}
