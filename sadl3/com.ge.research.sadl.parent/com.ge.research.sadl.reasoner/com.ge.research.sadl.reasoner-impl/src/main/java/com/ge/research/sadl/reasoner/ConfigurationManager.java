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
 * $Revision: 1.21 $ Last modified on   $Date: 2015/07/31 11:32:33 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ServiceLoader;
import java.util.StringTokenizer;

import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntDocumentManager.ReadFailureHandler;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Ontology;
import org.apache.jena.rdf.model.Bag;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.NodeIterator;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Seq;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.util.FileUtils;
import org.apache.jena.util.XMLChar;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;
import org.pojava.datetime.DateTimeConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.model.persistence.ISadlModelGetter;
import com.ge.research.sadl.model.persistence.ISadlModelGetterPutter;
import com.ge.research.sadl.model.persistence.SadlJenaFileGetter;
import com.ge.research.sadl.model.persistence.SadlJenaFileGetterPutter;
import com.ge.research.sadl.model.persistence.SadlJenaSemTKGetter;
import com.ge.research.sadl.model.persistence.SadlJenaSemTKGetterPutter;
import com.ge.research.sadl.model.persistence.SadlJenaTDBGetter;
import com.ge.research.sadl.model.persistence.SadlJenaTDBGetterPutter;
import com.ge.research.sadl.model.persistence.SadlPersistenceFormat;

/**
 * This is a general purpose configuration manager suitable for model deployment environments.
 * This class handles two kinds of configuration information.
 * 1) Configuration information such as Reasoner/Translator pair selection and configuration
 * 2) Mappings of model namespaces to actual URLs for the translated model. 
 *    This is the Jena ont-policy.rdf file, although it may not be useful
 *    for all translations.
 *    Note that in the case of non-file-based triple stores, the mappings aren't relevant.
 *    
 * @author crapo
 *
 */
public class ConfigurationManager implements IConfigurationManager {
	protected static final Logger logger = LoggerFactory.getLogger(ConfigurationManager.class);
	
	// Constants used to manage mappings between model namespace (publicURI) and model file (altURL) in ont-policy.rdf file
	protected static final String OWL_ONT_MANAGER_PUBLIC_URINS = "http://www.w3.org/2002/07/owl";
	protected static final String ONT_MANAGER_LANGUAGE = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#language";
	protected static final String ONT_MANAGER_CREATED_BY = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#createdBy";
	protected static final String ONT_MANAGER_SADL_SOURCE = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#sadlSourceURL";
	protected static final String ONT_MANAGER_ALT_URL = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#altURL";
	protected static final String ONT_MANAGER_PUBLIC_URI = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#publicURI";
	protected static final String ONT_MANAGER_PREFIX = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#prefix";
	protected static final String ONT_MANAGER_ONTOLOGY_SPEC = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#OntologySpec";
	
	public static final String DEFAULT_TRANSLATOR = "com.ge.research.sadl.jena.translator.JenaTranslatorPlugin";
//	private static final String DEFAULT_TRANSLATOR = "com.ge.research.sadl.geonly.jena.JenaOptimizingTranslator";
	public static final String DEFAULT_REASONER = "com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin";
	private static final String REASONER_SPEC = CONFIG_NAMESPACE + "ReasonerSpec";
//	private static final String TRANSLATOR_SPEC = CONFIG_NAMESPACE + "TranslatorSpec";
	protected static final String pREASONER_CLASSNAME = CONFIG_NAMESPACE + "reasonerClassName";
	protected static final String pTRANSLATOR_CLASSNAME = CONFIG_NAMESPACE + "translatorClassName";
	// Note: the ReasonerCategory value, e.g., "Jena", is obtained from the IReasoner (plugin) by calling getConfigurationCategory()
	
	private String projectPath = null;
	protected File modelFolderPath = null;
	protected URL modelFolderUrl = null;
	private Model configModel = null;
	protected Model mappingModel = null;
	protected HashMap<String, String> mappings = null;	// HashMap<publicURI, altURL>
	protected HashMap<String, String> globalPrefixes = null;	// HashMap<publicURI, prefix>
	private HashMap<String, IConfigurationManager> otherProjectConfigurationManagers = null;	// HashMap<publicUri, otherProjectConfigMgr>
	private ITranslator translator = null;
	private IReasoner reasoner = null;
	protected OntDocumentManager jenaDocumentMgr;
	private OntModelSpec ontModelSpec = null;
	
	private String repoType = null;

	protected RDFNode sadlNode = null;
	protected Property createdBy;
	protected Property altUrlProp;
	protected Property publicUrlProp;
	protected Property prefixProp;
	protected RDFNode createdBySadlLiteral;

	private String readError;
	
	private boolean inferenceCanceled = false;

	private String reasonerClassName = null;

	private String translatorClassName = null;

	private ISadlModelGetter sadlModelGetter;

	private ISadlModelGetterPutter sadlModelGetterPutter;
	
	/**
	 * Required constructor for subclass call
	 */
	public ConfigurationManager() {
		logger.debug("null-argument constructor called");
	}
	
	/**
	 * Constructor
	 * 
	 * @param modelFolderPathname -- absolute path to the model folder where the configuration file is to be found and written
	 * @throws ConfigurationException 
	 */
	public ConfigurationManager(String modelFolderPathname, String _repoType) throws ConfigurationException {
		setRepoType(_repoType);
		validateModelFolderPath(modelFolderPathname);
		setConfigModel(ModelFactory.createDefaultModel()) ;
		getConfigModel().setNsPrefix("", CONFIG_NAMESPACE);
		loadConfigurationFile();
		loadMappingFile();
		try {
			loadMappings();
		} catch (IOException e) {
			throw new ConfigurationException("Failed to load ontology mappings: " + e.getMessage(), e);
		}
	}
	
	public ConfigurationManager(String modelFolderPathname, String _repoType, boolean noModelFolderNeeded) throws ConfigurationException {
		setRepoType(_repoType);
		if (!noModelFolderNeeded) {
			validateModelFolderPath(modelFolderPathname);
			setConfigModel(ModelFactory.createDefaultModel()) ;
			getConfigModel().setNsPrefix("", CONFIG_NAMESPACE);
			loadConfigurationFile();
			loadMappingFile();
			try {
				loadMappings();
			} catch (IOException e) {
				throw new ConfigurationException("Failed to load ontology mappings: " + e.getMessage(), e);
			}
		}
	}
	
	protected synchronized void loadMappingFile() throws ConfigurationException {
		if (getModelFolderPath() != null) {
			String modelFolderPathname = getModelFolderPath().getAbsolutePath();
			String mappingFilename = modelFolderPathname + File.separator + ONT_POLICY_RDF;
			File mappingFile = new File(mappingFilename);
			if (mappingFile.exists()) {
				logger.debug("reading existing mapping model '" + mappingFilename + "'");
				// load mapping info from file
				setMappingModel(ModelFactory.createDefaultModel()) ;
			    InputStream in = RDFDataMgr.open(mappingFilename);
			    if (in == null) {
			    	throw new IllegalArgumentException("File: " + mappingFilename + " not found");
			    }
				try {
					getMappingModel().read(in, "");
				}
				catch (Throwable t) {
					t.printStackTrace();
					logger.error("Failed to read mapping file in folder '" + modelFolderPathname + "': " + t.getLocalizedMessage());
				}
			}
			else {
				logger.warn("Model folder '" + modelFolderPathname + "' has no ont-policy.rdf file.");
			} 
		}
		else if (getModelFolderUrl() != null && !getModelFolderUrl().toString().equals(dummyModelFolderPath)) {
			logger.debug("creating new mapping model for '" + getModelFolderUrl() + "'");
			setMappingModel(ModelFactory.createDefaultModel()) ;
			getMappingModel().read(getModelFolderUrl() + "/" + ONT_POLICY_RDF);
		}
		if (getMappingModel() != null) {
			setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
		}
	}
	
	/**
	 * Call this method to actually apply the mappings to the Jena Document Mgr
	 * @throws IOException
	 */
	protected void loadMappings() throws IOException {
		mappings = getMappings();
		if (mappings != null) {
			this.getJenaDocumentMgr().reset();
			this.getJenaDocumentMgr().clearCache();
			Iterator<String> iterator = mappings.keySet().iterator();
			boolean needFileLocator = false;
			while (iterator.hasNext()) {
				String url = iterator.next();
				logger.debug("loading mapping url ="+url);
				String altUrl = mappings.get(url);
				getJenaDocumentMgr().addAltEntry(url, altUrl);
				if (altUrl != null && altUrl.startsWith(FILE_SHORT_PREFIX)) {
					needFileLocator = true;
				}
			}
			if (needFileLocator) {
				setupJenaFileManager();
			}
		}
	}
	
	private void setupJenaFileManager() throws IOException {
		getJenaDocumentMgr().getFileManager().addLocatorFile(getModelFolder());
		getJenaDocumentMgr().getFileManager().addLocatorURL();
		SadlReadFailureHandler rfHandler = new SadlReadFailureHandler(logger );	
		rfHandler.setSadlConfigMgr(this);
		getJenaDocumentMgr().setReadFailureHandler(rfHandler);
	}
	
	private void loadConfigurationFile() throws ConfigurationException {
		if (getModelFolderPath() != null) {
			String modelFolderPathname = getModelFolderPath().getAbsolutePath();
			String configFilename;
			try {
				configFilename = getModelFolderPath().getCanonicalPath() + File.separator + CONFIG_FILENAME;
				File configFile = new File(configFilename);
				if (configFile.exists()) {
					// load configuration info from file
		            String syntax = FileUtils.guessLang(configFilename);
		            FileInputStream in = new FileInputStream(configFile);
		            getConfigModel().read( in, null, syntax );
		            if (!getConfigModel().contains(getConfigModel().getResource(CONFIG_NAMESPACE + BuiltinCategory), RDF.type, getConfigModel().getResource(CATEGORY_KW))) {
		            	// this statement must be there to find built-ins but seems to disappear from time to time--this is a workaround
		            	getConfigModel().add(getConfigModel().getResource(CONFIG_NAMESPACE + BuiltinCategory), RDF.type, getConfigModel().getResource(CATEGORY_KW));
		            }
		            Resource df = getConfigModel().getResource(CONFIG_NAMESPACE + DateFormat);
		            Property dmyp = getConfigModel().getProperty(CONFIG_NAMESPACE + dmyOrder);
		            Statement stmt = getConfigModel().getProperty(df, dmyp);
		            if (stmt != null) {
		            	String val = stmt.getObject().asLiteral().getLexicalForm();
		            	if (val.equals(IConfigurationManager.dmyOrderDMY)
		            			&& !DateTimeConfig.getGlobalDefault().isDmyOrder()) {
		            		logger.warn("Cannot configure DMY order for model");
		            	}
		            }
				}
				else {
					logger.warn("Model folder '" + modelFolderPathname + "' has no configuration file.");
				}
			} catch (IOException e) {
				e.printStackTrace();
				throw new ConfigurationException("Failed to load configuration file", e);
			}
		}
		else if (getModelFolderUrl() != null && !getModelFolderUrl().toString().equals(dummyModelFolderPath)) {
			getConfigModel().read(getModelFolderUrl() + "/" + CONFIG_FILENAME);
		}
	}
	
	private String getModelFolderUrl() {
		if (modelFolderUrl != null) {
			return modelFolderUrl.toString();
		}
		return null;
	}
	public boolean loadGlobalPrefixes(OntModel model) {
		if (globalPrefixes != null) {
			Iterator<String> uriItr = globalPrefixes.keySet().iterator();
			while (uriItr.hasNext()) {
				String uri = uriItr.next();
				String prefix = globalPrefixes.get(uri);
				if (!uri.endsWith("#")) {
					uri += "#";
				}
				model.setNsPrefix(prefix, uri);
			}
			return true;
		}
		return false;
	}
	
	protected void validateModelFolderPath(String modelFolderPathname) throws ConfigurationException {
		if (modelFolderPathname == null) {
			throw new ConfigurationException("model folder path is null!");
		}
		if (modelFolderPathname.startsWith("http://")) {
			// remote kbase to be accessed via HTTP
			try {
				setModelFolderUrl(modelFolderPathname);
			} catch (MalformedURLException e) {
				e.printStackTrace();
				throw new ConfigurationException("Invalid kbase URL", e);
			}
		}
		else {
			// local file system kbase
			if (modelFolderPathname.startsWith("file:")) {
				modelFolderPathname = modelFolderPathname.substring(5);
			}
			File mfpnf = new File(modelFolderPathname);
			if (!mfpnf.exists()) {
				throw new ConfigurationException("Model folder '" + modelFolderPathname + "' does not exist.");
			}
			if (!mfpnf.isDirectory()) {
				throw new ConfigurationException("'" + modelFolderPathname + "' is not a directory.");
			}		
			setModelFolderPath(mfpnf);
			setProjectPath(mfpnf.getParentFile().getAbsolutePath());
		}
	}

	public ITranslator getTranslator() throws ConfigurationException {
		if (translator == null) {
			initializeTranslator();
		}
		return translator;
	}

	@Override
	public ITranslator getTranslatorForReasoner(String reasonerName) throws ConfigurationException {
		if (getConfigModel() != null) {
			Resource subject = getConfigModel().getResource("http://com.ge.research.sadl.configuration#" + reasonerName);
			if (subject != null) {
				Property predicate = getConfigModel().getProperty("http://com.ge.research.sadl.configuration#translatorClassName");
				if (predicate != null) {
					StmtIterator sitr = getConfigModel().listStatements(subject, predicate, (RDFNode)null);
					if (sitr.hasNext()) {
						RDFNode rcls = sitr.next().getObject();
						if (rcls instanceof Literal) {
							String clsName = rcls.asLiteral().getString();
							return getTranslatorInstanceByClass(clsName);
						}
					}
				}
			}
		}
		if (getReasoner() != null && getReasoner().getClass().getCanonicalName().equals(reasonerName)) {
			String transClsName = getReasoner().getDefaultTranslatorClassName();
			if (transClsName != null) {
				return getTranslatorInstanceByClass(transClsName);
			}
		}
		return null;
	}

	@Override
	public ITranslator getTranslatorForReasoner(IReasoner reasoner) throws ConfigurationException {
		return getTranslatorInstanceByClass(reasoner.getDefaultTranslatorClassName());
	}

	public IReasoner getReasoner() throws ConfigurationException {
		if (reasoner == null) {
			initializeReasoner();
		}
		return reasoner;
	}
	
	public IReasoner getOtherReasoner(String reasonerClassName) throws ConfigurationException {
		IReasoner otherReasoner = null;
		try {
			otherReasoner = getClassInstance(reasonerClassName, IReasoner.class);
			otherReasoner.setConfigurationManager(this);
			if (getConfigModel() != null) {
				// first apply configuration for the reasoner family
				Resource family = getConfigModel().getResource(CONFIG_NAMESPACE + otherReasoner.getReasonerFamily());
				applyConfigurationToReasoner(otherReasoner, family);
				// then apply configuration for the specified reasoner specifically
				Resource category = getConfigModel().getResource(CONFIG_NAMESPACE + otherReasoner.getConfigurationCategory());
				applyConfigurationToReasoner(otherReasoner, category);
			}
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "'", e);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "' due to an illegal access exception", e);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			throw new ConfigurationException("Reasoner class '" + reasonerClassName + "' not found", e);
		} catch (Throwable t) {
			t.printStackTrace();
		}

		if (otherReasoner == null) {
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "'");
		}
		return otherReasoner;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#clearReasoner()
	 */
	public boolean clearReasoner() {
		if (reasoner != null) {
			reasoner = null;
			translator = null;	// don't keep around a translator that might be an incorrect type.
			return true;
		}
		return false;
	}
	
	@Override
	public boolean clearTranslator() {
		if (translator != null) {
			translator = null;
			return true;
		}
		return false;
	}

	protected Resource getReasonerSpecResource() {
		if (getConfigModel() != null) {
            Resource type = getConfigModel().createResource(CATEGORY_KW);
            return getConfigModel().createResource(REASONER_SPEC, type);
		}
		return null;
	}
	/**
	 * Method to return the fully qualified name of the class for the current IReasoner as specified in the configuration.
	 * @return
	 * @throws ConfigurationException 
	 */
	public String getReasonerClassName() throws ConfigurationException {
		IReasoner reasonerInst = getReasonerInstance();
		if (reasonerInst != null) {
			return reasonerInst.getClass().getCanonicalName();
		}
		throw new ConfigurationException("Unable to get current reasoner for unknown reason.");
	}
	
	/**
	 * Method to return the fully qualified name of the class for the current ITranslator as specified in the configuration.
	 * @return
	 * @throws ConfigurationException 
	 */
	public String getTranslatorClassName() throws ConfigurationException {
		try {
			IReasoner reasonerInst = getReasonerInstance();
			if (getConfigModel() != null) {
				Resource reasonerCategory = getConfigModel().getResource(CONFIG_NAMESPACE + reasonerInst.getConfigurationCategory());
		 		StmtIterator sitr = getConfigModel().listStatements(reasonerCategory, 
						getConfigModel().getProperty(pTRANSLATOR_CLASSNAME), (RDFNode)null);
		        if (sitr.hasNext()) { 
		        	RDFNode clsnmnode = sitr.nextStatement().getObject();
		        	if (clsnmnode instanceof Literal) {
		        		return ((Literal)clsnmnode).getValue().toString();
		        	}
		        }
			}
	        ITranslator translator = getTranslator();
			if (translator != null) {
				return translator.getClass().getCanonicalName();
			}
			throw new ConfigurationException("Unable to get current translator for unknown reason.");
		}
		catch (Throwable t) {
			return ConfigurationManager.DEFAULT_TRANSLATOR;
		}
	}

	protected void setTranslator(ITranslator translator) {
		this.translator = translator;
	}

	protected void setReasoner(IReasoner reasoner) {
		this.reasoner = reasoner;
	}
	
	private void initializeTranslator() throws ConfigurationException {
		translator = getTranslatorInstance();
		if (translator != null) {
			translator.setConfigurationManager(this);
			if (getConfigModel() != null) {
				// first apply configuration for the reasoner family
				Resource family = getConfigModel().getResource(CONFIG_NAMESPACE + translator.getReasonerFamily());
				applyConfigurationToTranslator(family);
				// then apply configuration for the specified translator specifically
				Resource category = getConfigModel().getResource(CONFIG_NAMESPACE + translator.getConfigurationCategory());
				applyConfigurationToTranslator(category);
			}
		}
	}

	private ITranslator getTranslatorInstance() throws ConfigurationException {
		if (translatorClassName == null) {
			if (getConfigModel() != null) {
				IReasoner reasonerInst = getReasonerInstance();
				Resource reasonerCategory = getConfigModel().getResource(CONFIG_NAMESPACE + reasonerInst.getConfigurationCategory());
				StmtIterator sitr = getConfigModel().listStatements(reasonerCategory, 
						getConfigModel().getProperty(pTRANSLATOR_CLASSNAME), (RDFNode)null);
				if (sitr.hasNext()) {
					RDFNode cnobj = sitr.next().getObject();
					if (cnobj instanceof Literal) {
						translatorClassName = ((Literal)cnobj).getLexicalForm();
					}
				}
				if (translatorClassName == null) {
					translatorClassName = reasonerInst.getDefaultTranslatorClassName();
				}
				clearReasoner();	// this reasoner isn't initialized properly for inference so don't keep it around awc 2/12/2019
			}
		}
		if (translatorClassName == null) {
			translatorClassName = DEFAULT_TRANSLATOR;
		}
		return getTranslatorInstanceByClass(translatorClassName);
	}
	
	private ITranslator getTranslatorInstanceByClass(String translatorClassName) throws ConfigurationException {
		ITranslator translatorClass = null;
		try {
			translatorClass = getClassInstance(translatorClassName, ITranslator.class);
			if (translatorClass == null) {
				throw new ConfigurationException("Unable to instantiate Translator '" + translatorClassName + "'");
			}
			translatorClass.setConfigurationManager(this);
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Translator '" + translatorClassName + "'", e);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Translator '" + translatorClassName + "' due to an illegal access exception", e);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			throw new ConfigurationException("Translator class '" + translatorClassName + "' not found", e);
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return translatorClass;
	}

	protected void initializeReasoner() throws ConfigurationException {
		reasoner = getReasonerInstance();
		reasoner.setConfigurationManager(this);
		if (getConfigModel() != null) {
			// first apply configuration for the reasoner family
			Resource family = getConfigModel().getResource(CONFIG_NAMESPACE + reasoner.getReasonerFamily());
			applyConfigurationToReasoner(reasoner, family);
			// then apply configuration for the specified reasoner specifically
			Resource category = getConfigModel().getResource(CONFIG_NAMESPACE + reasoner.getConfigurationCategory());
			applyConfigurationToReasoner(reasoner, category);
		}
	}
	
	protected IReasoner getReasonerInstance() throws ConfigurationException {
		if (reasoner != null) {
			return reasoner;
		}
		if (reasonerClassName == null) {
			if (getConfigModel() != null) {
				 StmtIterator sitr = getConfigModel().listStatements(getReasonerSpecResource(), getConfigModel().getProperty(pREASONER_CLASSNAME), (RDFNode)null);
				 if (sitr.hasNext()) {
					 RDFNode cnobj = sitr.next().getObject();
					 if (cnobj instanceof Literal) {
						 reasonerClassName = ((Literal)cnobj).getLexicalForm();
					 }
				 }
			}
		}
		if (reasonerClassName == null) {
			reasonerClassName = DEFAULT_REASONER;
		}
		try {
			reasoner = getClassInstance(reasonerClassName, IReasoner.class);
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "'", e);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "' due to an illegal access exception", e);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			throw new ConfigurationException("Reasoner class '" + reasonerClassName + "' not found", e);
		} catch (Throwable t) {
			t.printStackTrace();
		}

		if (reasoner == null) {
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "'");
		}
		return reasoner;
	}
	
	/**
	 * This method works in plain (not OSGi) Java code. The method is overriden by the subclass (ConfigurationMangerForIDE) when used in Eclipse.
	 * How to handle it in non-Eclipse OSGi is an unresolved issue 
	 * (see http://stackoverflow.com/questions/5892341/what-is-pure-osgi-equvalent-for-eclipse-platform-getbundle)
	 * 
	 * @param className
	 * @return
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws ClassNotFoundException
	 * 
	 */
	public <T> T getClassInstance(String name, Class<? extends T> clazz)
			throws InstantiationException, IllegalAccessException,
			ClassNotFoundException {

		if (name == null) {
			throw new NullPointerException("Service class name cannot be null.");
		}
		
		if (clazz == null) {
			throw new NullPointerException("Service class API (inertface / abstract class) cannot be null.");
		}
		
		final Iterator<? extends T> itr = ServiceLoader.load(clazz).iterator();
		while (itr.hasNext()) {
			T service = itr.next();
			if (name.equals(service.getClass().getName())) {
				return service;
			}
		}
		
		try {
			Class<?> serviceClass = this.getClass().getClassLoader().loadClass(name);
			if (serviceClass != null && clazz.isAssignableFrom(serviceClass)) {
				Object service = serviceClass.getDeclaredConstructor().newInstance();
				return clazz.cast(service);
			}
		} catch (ClassNotFoundException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			// Ignored, we will throw a proper CNFE anyway. 
		}
		
		throw new ClassNotFoundException("Cannot find service class for name: " + name + " for service API " + clazz + ".");
	}

	private void applyConfigurationToReasoner(IReasoner theReasoner, Resource category) throws ConfigurationException {
		if (category != null) {
			String[] reasonerCategory = new String[1];
			reasonerCategory[0] = category.getLocalName();
			List<ConfigurationItem> configItems = getConfiguration(reasonerCategory, true);
			
			for (int i = 0; configItems != null && i < configItems.size(); i++) {
				ConfigurationItem configItem = configItems.get(i);
				theReasoner.configure(configItem);
			}
		}
	}
	
	private void applyConfigurationToTranslator(Resource category) throws ConfigurationException {
		if (category != null) {
			String[] reasonerCategory = new String[1];
			reasonerCategory[0] = category.getLocalName();
			List<ConfigurationItem> configItems = getConfiguration(reasonerCategory, true);
			
			for (int i = 0; configItems != null && i < configItems.size(); i++) {
				ConfigurationItem configItem = configItems.get(i);
				translator.configure(configItem);
			}
		}
	}
	
	/**
	 * This method identifies all of a model's imports and their prefixes, direct and indirect.
	 * 
	 * @param m - the OntModel to be investigated
	 * @param modelUri - the URI of the model
	 * 
	 * @return - a Map of model URIs to prefixes
	 */
	public Map<String, String> listSubModelsAndPrefixes(OntModel m, String modelUri) {
		return listSubModelsAndPrefixes(new HashMap<String, String>(), m, modelUri);
	}
	
	private Map<String, String> listSubModelsAndPrefixes(Map<String, String> map, OntModel m, String modelUri) {
		Ontology onto = m.getOntology(modelUri);
		if (onto != null) {
			String prefix = m.getNsURIPrefix(modelUri);
			if (prefix == null) {
				prefix = getGlobalPrefix(modelUri);
			}
			if (!map.containsKey(modelUri)) {
				map.put(modelUri, prefix);
			}
			ExtendedIterator<OntResource> importsItr = onto.listImports();
			if (importsItr.hasNext()) {
				while (importsItr.hasNext()) {
					OntResource or = importsItr.next();
					logger.debug("Ontology of model '" + modelUri + "' has import '" + or.toString() + "' with prefix '" + prefix + "'");
					if (!map.containsKey(or.toString())) {
						OntModel submodel = m.getImportedModel(or.getURI());
						if (submodel != null) {
							map = listSubModelsAndPrefixes(map, submodel, or.getURI());
						}
					}
				}
			}
		}
		return map;
	}
	
	protected boolean useRepoDatasetForImport(String importedUri) {
		try {
			if (getTdbFolder() == null || importedUri.equals(IConfigurationManager.ServicesConfigurationURI)) {
				return false;
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#getMappings()
	 */
	public HashMap<String, String> getMappings(){
		if (getMappingModel() == null) {
			return null;
		}
    	if (sadlNode == null) {
    		sadlNode = getMappingModel().createTypedLiteral(SADL);
    		createdBy = getMappingModel().createProperty(ONT_MANAGER_CREATED_BY);
    		altUrlProp = getMappingModel().createProperty(ONT_MANAGER_ALT_URL);
    		publicUrlProp = getMappingModel().createProperty(ONT_MANAGER_PUBLIC_URI);
    		prefixProp = getMappingModel().createProperty(ONT_MANAGER_PREFIX);
    	}
        RDFNode pubv;
        RDFNode altv;
        createdBySadlLiteral = getMappingModel().createLiteral("SADL");
        String fileName = new String();
        String actualFilePath = new String();
        StmtIterator sitr = getMappingModel().listStatements(null, altUrlProp, (RDFNode)null);
        
		HashMap<String, String> importUrlMappings = null;
        if (sitr.hasNext()) {
        	importUrlMappings = new HashMap<String, String>();
        }
        while (sitr.hasNext()) {
        	fileName = null;
        	actualFilePath = null;
            Statement s = sitr.nextStatement();
            org.apache.jena.rdf.model.Resource subj = s.getSubject();	
            Statement salt = subj.getProperty(altUrlProp);
            Statement spub = subj.getProperty(publicUrlProp);
            Statement sprefix = subj.getProperty(prefixProp);
            if (salt != null && spub != null) {
	            altv = salt.getObject();
            	String strAltv = rdfNodeToString(altv);
	            pubv = spub.getObject();
	            Statement isSadlStmt = subj.getProperty(createdBy);
	            if (isSadlStmt != null && isSadlStmt.getObject().equals(createdBySadlLiteral)) {         
	            	// this mapping was created by SADL
		            StringTokenizer st1 = new StringTokenizer(strAltv, "/");
		 			while(st1.hasMoreTokens()) {
		            	fileName = st1.nextToken();
		 			}
	            	actualFilePath = getActualUrl(fileName);
	            }
	            else {
	            	// this handles mappings that are not created by SADL
	            	//  1) if the mapping is of type "file:"
	            	//		a) if the path has a match to the project and there is a file in the current project matching the rest of the path, use this matching path
	            	//		b) if the file exists assume it is correct
	            	//	2) else if their is a file of that name in the same folder as the policy file assume that file is the correct one
	            	//	3) else if there is a sibling folder to the folder of the policy file that contains a file of that name assume it is the correct one
	            	if (strAltv.startsWith(IConfigurationManager.FILE_SHORT_PREFIX)) {
			            StringTokenizer st1 = new StringTokenizer(strAltv, "/");
			            String lastToken = null;
	 					String projectName = getModelFolderPath().getParentFile().getName();
	 					StringBuilder relativePath = null;
			 			while(st1.hasMoreTokens()) {
			 				lastToken = fileName;
			            	fileName = st1.nextToken();
			            	if (relativePath != null) {
			            		relativePath.append("/");
			            		relativePath.append(fileName);
			            	}
			            	else if (fileName.equals(projectName)) {
			            		relativePath = new StringBuilder();
			            	}
			 			}
			 			String testName = strAltv;
		 				try {
		 					boolean found = false;
		 					if (relativePath != null) {
		 						String relTestName = getProjectPath() + relativePath.toString();
		 						File relTestFile = new File(relTestName);
		 						if (relTestFile.exists()) {
		 							actualFilePath = fileNameToFileUrl(relTestName);
		 							found = true;
		 						}
		 					}
		 					if (!found) {
					 			File testFile = new File(fileUrlToFileName(testName));
					 			if (testFile.exists()) {
					 				// the actualUrl exists as is so use it
					 				actualFilePath = testName;
					 			}
					 			else {
					 				testName =  getActualUrl(fileName);
									testFile = new File(fileUrlToFileName(testName));
					 				if (testFile.exists()) {
					 					// the actualUrl adjusted to have the relative location of the models folder exists so use it
					 					actualFilePath = testName;
					 				}
					 				else {
					 					String siblingName = siblingFolderUrl(fileName, lastToken);
					 					boolean siblingFound = false;
					 					if (siblingName != null) {
					 						File sibling = new File(fileUrlToFileName(siblingName));
					 						if (sibling.exists()) {
					 							// the named file exists in a sibling directory; use it
					 							siblingFound = true;
					 							actualFilePath = siblingName;
					 						}
					 					}
					 					if (!siblingFound) {
					 						if (getModelFolderPath() != null) {
									 			String folderPath = fileNameToFileUrl(getModelFolderPath().getAbsolutePath());
							 					testName = folderPath.substring(0, folderPath.length() - (1 + lastToken.length())) + "/" + fileName;
							 					testFile = new File(fileUrlToFileName(testName));
							 					if (testFile.exists()) {
							 						// folder above??
							 						actualFilePath = testName;
							 					}
							 					else {
							 						logger.warn("Mapping file has actual URL '" + testName + "' but it does not appear to exist and could not be found in adjacent folders.");
							 					}
					 						}
					 						else {
					 							actualFilePath = testName;
					 							if (!actualFilePath.startsWith("http:")) {
					 								logger.warn("Mapping file '" + strAltv + "'; using '" + actualFilePath + "'");
					 							}
					 						}
					 					}
					 				}
					 			}
		 					}
		 				}
						catch (MalformedURLException e) {
							// oh well, we tried
						}
	            	}
	            }
	            if (actualFilePath == null) {
	            	actualFilePath = strAltv;
	            }
	            String publicUri = rdfNodeToString(pubv);
	            logger.debug("Found mapping from public URI '" + publicUri + "' to alternative URL '" + actualFilePath + "'");
	            importUrlMappings.put(publicUri, actualFilePath);
	            if (sprefix != null) {
	            	RDFNode prefixNode = sprefix.getObject();
	            	if (prefixNode instanceof Literal) {
	            		String prefix = ((Literal)prefixNode).getString();
	            		if (globalPrefixes == null) {
	            			globalPrefixes = new HashMap<String, String>();
	            		}
	            		globalPrefixes.put(publicUri, prefix);
	            	}
	            }
            }
        }
        return importUrlMappings;
	}
	
	private String siblingFolderUrl(String fileName, String lastToken) {
		if (getModelFolderPath() != null) {
			String modelFolderPathname = getModelFolderPath().getAbsolutePath();
			File folder = new File(modelFolderPathname);
			File parent = folder.getParentFile();
			File[] siblings = parent.listFiles();
			for (int i = 0; i < siblings.length; i++) {
				if (siblings[i].getName().equals(lastToken)) {
					return fileNameToFileUrl(siblings[i].getAbsolutePath() + File.separator + fileName);
				}
			}
		}
		return null;
	}
	
	private String getActualUrl(String fileName) {
		if (getModelFolderPath() != null) {
			String modelFolderPathname = getModelFolderPath().getAbsolutePath();
//			String mappingFilename = modelFolderPathname + File.separator + ONT_POLICY_RDF;
			return fileNameToFileUrl(modelFolderPathname + File.separator + fileName);
		}
		else if (getModelFolderUrl() != null) {
			return getModelFolderUrl() + "/" + fileName;
		}
		return null;
	}
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#getGlobalPrefix(java.lang.String)
	 */
	public String getGlobalPrefix(String uri) {
		if (globalPrefixes != null) {
			String prefix = globalPrefixes.get(uri);
			if ((prefix == null || prefix.length() == 0) && uri.endsWith("#")) {
				prefix = globalPrefixes.get(uri.substring(0, uri.length() - 1));
			}
			if (prefix != null) {
				return prefix;
			}
		}
		String insp = getImplicitNamespacePrefix(uri);
		if (insp != null) {
			return insp;
		}
		try {
			if (isNamespaceInProjectDependency(uri)) {
				return getOtherProjectPrefixFromPubliceUri(uri);
			}
		} catch (ConfigurationException e) {
			// it's OK if there isn't one from a depends-on project
			return null;
		}
		return null;
	}
	
	@Override
	public boolean isNamespaceImplicit(String uri) {
		if (getImplicitNamespacePrefix(uri) != null) {
			return true;
		}
		return false;
	}
	
	/** 
	 * Method to get the prefix for a publicUri if it is an implicitly imported namespace
	 * @param uri
	 * @return
	 */
	protected String getImplicitNamespacePrefix(String uri) {
		if (uri != null) {
			if (!uri.endsWith("#")) {
				uri += "#";
			}
			if (uri.equals(RDF.getURI())) {
				return "rdf";
			}
			else if (uri.equals(RDFS.getURI())) {
				return "rdfs";
			}
			else if (uri.equals(OWL.getURI())) {
				return "owl";
			}
			else if (uri.equals(XSD.getURI())) {
				return "xsd";
			}
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#getUriFromGlobalPrefix(java.lang.String)
	 */
	public String getUriFromGlobalPrefix(String prefix) {
		if (globalPrefixes != null) {
			if (globalPrefixes.containsValue(prefix)) {
				// the prefix is in the Map
				Iterator<Entry<String, String>> valuesIter = globalPrefixes.entrySet().iterator();
				while (valuesIter.hasNext()) {
					Entry<String, String> entry = valuesIter.next();
					if (entry.getValue().equals(prefix)) {
						return entry.getKey();
					}
				}
			}
		}
		return null;
	}
	
	/**
     * This method converts an OS filename (e.g., "C:\\folder\file.ext")
     * to a file URL
     *
     * @param fileName
     * @return
     */
    public String fileNameToFileUrl(String fileName) {
    	URI fileUri = null;
        if (fileName.startsWith("http:") || fileName.startsWith("file:")) {
            fileUri = URI.create(fileName);
        }
        else {
        	File file = new File(fileName);
        	fileUri = file.toURI();
        	try {
				return fileUri.toURL().toExternalForm();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return FILE_ABS_URL_PREFIX + fileName.replace("\\", "/");
			}
        }
        return fileUri.toString();
    }

    /**
     * This method converts the string form of a file URL to an
     * OS filename
     *
     * @param urlstr
     * @return
     * @throws MalformedURLException 
     */
    public String fileUrlToFileName(String urlstr) throws MalformedURLException {
        URI fileUri = URI.create(urlstr);
        if (fileUri != null) {
        	return fileUri.toURL().getPath();
        }
        throw new MalformedURLException("Unable to convert '" + urlstr + "' to a file name");
    }

	/**
	 * Given a namespace, add a '#' character to the end of the namespace if it ends with a
	 * valid NCName character. If it does not then it already has a termination that allows
	 * splitting namespace from localname in resource URIs so return the input namespace.
	 *
	 * @param inputNamespace
	 * @return
	 */
	public static synchronized String addHashToNonTerminatedNamespace(String inputNamespace) {
        if (inputNamespace.length() > 0 && XMLChar.isNCName( inputNamespace.charAt(inputNamespace.length() -1) )) {
        	// this is for backward compatibility--in V1 a valid ending character was not required--
        	//  and for convenience (so the user doesn't have to provide an ending character from a
        	//  "secret" set of characters)
        	return inputNamespace + "#";
        }
        else {
        	return inputNamespace;
        }
	}

	// When getting a configuration value we must take into account the implicit hierarchy,
	//	e.g., Builtins is a sub-category of the reasoner's Category instance.
	
	/**
	 * Get all of the configuration items of a particular category hierarchy.
	 * 
	 *  @param categoryHierarchy - the category hierarchy desired
	 *  @return - a list of ConfigurationItems matching the request
	 * @throws ConfigurationException 
	 */
	public List<ConfigurationItem> getConfiguration(String[] categoryHierarchy, boolean includeSubcategories) throws ConfigurationException {	
		List<ConfigurationItem> configItems = null;
		if (getConfigModel() != null) {
			if (categoryHierarchy != null && categoryHierarchy.length > 0) {
				Property subcatprop =  getConfigModel().createProperty(SUBCATEGORY_PROP);  // we'll use this repeatedly
				List<Resource> matchingCategoryInstances = null;	// this is the list that we'll use to construct the results
				
				List<Resource> lastCategoryList = null;
				for (int i = 0; i < categoryHierarchy.length; i++) {
					List<Resource> thisCategoryList = new ArrayList<Resource>();
					Resource currentCategory = getConfigModel().getResource(CONFIG_NAMESPACE + categoryHierarchy[i]);
					if (currentCategory == null) {
						throw new ConfigurationException("Configuration category instance '" + categoryHierarchy[i] + "' not found.");
					}
					if (lastCategoryList != null) {
						for (int j = 0; j < lastCategoryList.size(); j++) {
							StmtIterator sitr = getConfigModel().listStatements(lastCategoryList.get(j), subcatprop, (RDFNode)null);
							while (sitr.hasNext()) {
								Statement stmt = sitr.nextStatement();
								if (stmt.getObject().equals(currentCategory)) {
									// this is a direct subcategory
									thisCategoryList.add(currentCategory);
								}
								else if (stmt.getBag() != null) {
									Bag bag = stmt.getBag();
									NodeIterator nitr = bag.iterator();
									while (nitr.hasNext()) {
										RDFNode n = nitr.nextNode();
										if (n instanceof Resource && ((Resource)n).hasProperty(RDF.type, currentCategory)) {
											// this is a subcategory in a Bag
											thisCategoryList.add((Resource) n);
										}
									}
								}
								else if (stmt.getSeq() != null) {
									Seq seq = stmt.getSeq();
									NodeIterator nitr = seq.iterator();
									while (nitr.hasNext()) {
										RDFNode n = nitr.nextNode();
										if (n instanceof Resource && ((Resource)n).hasProperty(RDF.type, currentCategory)) {
											// this is a subcategory in a Sequence
											thisCategoryList.add((Resource) n);
										}
									}
								}
							}
						}
						if (thisCategoryList.isEmpty()) {
							throw new ConfigurationException("Configuration category instance '" + 
											categoryHierarchy[i] + "' not connected to instance '" + categoryHierarchy[i - 1] + "'.");
						}
					}
					else {
						thisCategoryList.add(currentCategory);
					}
					if (i < (categoryHierarchy.length - 1)) {
						lastCategoryList = thisCategoryList;
					}
					else {
						matchingCategoryInstances = thisCategoryList;
					}
				}
			
				// Now find the configuration information for the matching category instances
				//	Each instance will generate a ConfigurationItem. Instances can be:
				//	1. a named category instance, e.g., Jena or AdHocQuery
				//	2. an unnamed Bag or Seq
				//	If includeSubcategories is true, any Statement object that is also a Category instance will cause recursion
				if (matchingCategoryInstances != null) {
					for (int i = 0; i < matchingCategoryInstances.size(); i++) {
						ConfigurationItem newItem = null;
						StmtIterator sitr = getConfigModel().listStatements(matchingCategoryInstances.get(i), (Property)null, (RDFNode)null);
						while (sitr.hasNext()) {
							Statement stmt = sitr.nextStatement();
							Property name = stmt.getPredicate();
							if (name.equals(RDF.type)) {
								continue;
							}
							RDFNode value = stmt.getObject();
							Object valObj = null;
							if (value instanceof Resource) {
								if (stmt.getPredicate().equals(subcatprop)) {
									// this handles Category instances that are in Bags or Sequences
									String[] newCatHier = extendCategories(categoryHierarchy, getNodeCategory(value));
									if (newCatHier != null) {
										List<ConfigurationItem> subitems = getConfiguration(newCatHier, includeSubcategories);
										if (subitems != null) {
											if (configItems == null) {
												configItems = subitems;
											}
											else {
												configItems.addAll(subitems);
											}
										}
									}
									// that's all we want to do for subcategories
									continue;
								}
								else if (!((Resource)value).isAnon()){
									valObj = ((Resource)value).getLocalName();
								}
							}
							else if (value instanceof Literal) {
								valObj = ((Literal)value).getValue();
							}
							else {
								valObj = value.toString();
							}
							if (valObj != null) {
								// we have a name value pair
								if (newItem == null) {
									newItem = new ConfigurationItem(categoryHierarchy);
									if (configItems == null) {
										configItems = new ArrayList<ConfigurationItem>();
									}
									configItems.add(newItem);
								}
								newItem.addNameValuePair(newItem.new NameValuePair(name.getLocalName(), valObj));
							}
							else if (value instanceof Resource) {
							// this value isn't part of a name value pair--is it a Bag or Sequence?
								NodeIterator nitr = null;
								if (((Resource)value).hasProperty(RDF.type, RDF.Bag)) {
									Bag bag = ((Resource)value).as(Bag.class);
									nitr = bag.iterator();
								}
								else if (((Resource)value).hasProperty(RDF.type, RDF.Seq)) {
									Seq seq = ((Resource)value).as(Seq.class);
									nitr = seq.iterator();
								}
								if (nitr != null) {
									// it is a Bag or Sequence so the members should be Literals of name value pairs
									while (nitr.hasNext()) {
										RDFNode node = nitr.nextNode();
										if (node instanceof Literal) {
											valObj = ((Literal)node).getValue();
										}
										else {
											valObj = node.toString();
										}
										if (newItem == null) {
											newItem = new ConfigurationItem(categoryHierarchy);
											if (configItems == null) {
												configItems = new ArrayList<ConfigurationItem>();
											}
											configItems.add(newItem);
										}
										newItem.addNameValuePair(newItem.new NameValuePair(name.getLocalName(), valObj));
									}
								}
							}
							else {
								System.out.println("Should we ever get to here????");
							}
						}
					}
				}
			}
		}
		return configItems;
	}

	/**
	 * Given a list of Categories (categoryHierarchy), add the name of one additional Resource and return the new
	 * longer String array
	 * 
	 * @param categoryHierarchy
	 * @param value
	 * @return
	 */
	private String[] extendCategories(String[] categoryHierarchy, Resource value) {
		if (value == null) {
			return null;
		}
		String [] newCatHier = new String[categoryHierarchy.length + 1];
		for (int j = 0; j < categoryHierarchy.length; j++) {
			newCatHier[j] = categoryHierarchy[j];
		}
		newCatHier[newCatHier.length - 1] = value.getLocalName();
		return newCatHier;
	}

	/**
	 * Given an RDFNode, find the lowest Category of which the node is an instance
	 * 
	 * @param node
	 * @return
	 */
	private Resource getNodeCategory(RDFNode node) {
		if (node instanceof Resource) {
			Resource categoryType = getConfigModel().getResource(CATEGORY_KW);
			NodeIterator nitr = null;
			if (((Resource) node).hasProperty(RDF.type, RDF.Bag)) {
				// this is a Bag so get an iterator over the members
				Bag bag = ((Resource)node).as(Bag.class);
				nitr = bag.iterator();
			}
			else if (((Resource) node).hasProperty(RDF.type, RDF.Seq)) {
				// this is a Sequence so get an iterator over the members
				Seq seq = ((Resource)node).as(Seq.class);
				nitr = seq.iterator();
			}
			if (nitr != null) {
				while (nitr.hasNext()) {
					RDFNode memberNode = nitr.nextNode();	// a member of the Bag or Sequence
//					System.out.println(memberNode);
					if (memberNode instanceof Resource) {
						StmtIterator sitr = ((Resource)memberNode).listProperties(RDF.type);
						while (sitr.hasNext()) {
							Statement stmt = sitr.nextStatement();
//							System.out.println(stmt);
							RDFNode memberNodeType = stmt.getObject();
							if (memberNodeType instanceof Resource && ((Resource)memberNodeType).hasProperty(RDF.type, categoryType)) {
								// the memberNodeType is of type Category so return the memberNodeType
								nitr.close();
								sitr.close();
								return (Resource)memberNodeType;
							}
						}
					}
				}
			}
			StmtIterator sitr = ((Resource)node).listProperties(RDF.type);
			while (sitr != null && sitr.hasNext()) {
				Statement stmt = sitr.nextStatement(); 
				RDFNode objnode = stmt.getObject();
				if (objnode instanceof Resource) {
					Resource catres = getNodeCategory(objnode);
					if (catres != null) {
						sitr.close();
						return catres;
					}
				}
			}
		}
		return null;
	}

	/**
	 * This method walks the Category tree specified by the categoryHierarchy and returns the Resource(s) corresponding
	 * to the last Category(ies). If the create flag is true a hierarchy will be created if it doesn't already exist.
	 * 
	 * @param categoryHierarchy - a list of Category names starting with the root of the tree
	 * @param bCreate - if true create the category hierarchy if it doesn't already exist
	 * @return - the Resource representing the leaf of the 
	 * @throws ConfigurationException
	 */
	protected Resource[] getSubjectResources(String[] categoryHierarchy, boolean bCreate) throws ConfigurationException {
		List<Resource> subjResources = new ArrayList<Resource>();
		if (categoryHierarchy != null && categoryHierarchy.length > 0) {
			Resource parentRes = null;
			for (int i = 0; i < categoryHierarchy.length; i++) {
				List<Resource> childResList = new ArrayList<Resource>();
				Resource childRes = getConfigModel().getResource(CONFIG_NAMESPACE + categoryHierarchy[i]);
				if (childRes == null && bCreate) {
					Resource type = getConfigModel().createResource(CATEGORY_KW);
					childRes = getConfigModel().createResource(CONFIG_NAMESPACE + categoryHierarchy, type);
				}
				else {
					throw new ConfigurationException("Configuration category instance '" + categoryHierarchy[i] + "' not found.");
				}
				if (parentRes != null) {
					Property subcatprop =  getConfigModel().createProperty(SUBCATEGORY_PROP);
					StmtIterator sitr = getConfigModel().listStatements(parentRes, subcatprop, childRes);
					if (!sitr.hasNext()) {
						if (bCreate) {
							getConfigModel().add(parentRes, subcatprop, childRes);
							childResList.add(childRes);
						}
						else {
							throw new ConfigurationException("Configuration category instance '" + 
									categoryHierarchy[i] + "' not connected to instance '" + categoryHierarchy[i - 1] + "'.");
						}
					}
					while (sitr.hasNext()) {
						Statement stmt = sitr.nextStatement();
						childResList.add(stmt.getSubject());
					}
				}
				if (i < (categoryHierarchy.length - 1)) {
					parentRes = childRes;
				}
				else {
					subjResources.add(childRes);
					break;
				}
			}
		
		}
		if (subjResources.size() > 0) {
			
		}
		String tree = "";
		for (int i = 0; i < categoryHierarchy.length; i++) {
			if (i > 0) tree += ", ";
			tree += categoryHierarchy[i];
		}
		throw new ConfigurationException("Failed to find" + (bCreate ? "/create " : " ") + "category tree " + tree);
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#setModelFolderPath(java.io.File)
	 */
	public void setModelFolderPath(File modelFolder) {
		this.modelFolderPath = modelFolder;
	}
	
	private void setModelFolderUrl(String url) throws MalformedURLException {
		URL validateUrl = new URL(url);
		modelFolderUrl = validateUrl;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#getModelFolderPath()
	 */
	public File getModelFolderPath() {
		return modelFolderPath;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#getModelFolder()
	 */
	public String getModelFolder() throws IOException {
		if (modelFolderPath != null) {
			return modelFolderPath.getCanonicalPath();
		}
		else if (modelFolderUrl != null) {
			return modelFolderUrl.toString();
		}
		else {
			return null;
		}
	}

	/**
	 * Use the LocationMapper created from the ont-policy.rdf file and updated with any added mappings to find 
	 * the publicUri corresponding to an actualURL.
	 * 
	 * @param altURL
	 * @return
	 * @throws ConfigurationException
	 */
	public String getPublicUriFromActualUrl(String altURL) throws ConfigurationException { 
		if (mappings != null) {
			if (mappings.containsValue(altURL)) {
				Iterator<String> keyitr = mappings.keySet().iterator();
				while (keyitr.hasNext()) {
					String pubUri = keyitr.next();
					if (mappings.get(pubUri).equals(altURL)) {
						return pubUri;
					}
				}
			}
		}
//		LocationMapper mapper = OntDocumentManager.getInstance().getFileManager().getLocationMapper();
//		Iterator<String> mapiter = mapper.listAltEntries();
//		while (mapiter.hasNext()) {
//			String pub = mapiter.next();
//			String alt = mapper.altMapping(pub);
//			if (alt.equals(altURL)) {
//				return pub;
//			}
//		}
		throw new ConfigurationException("AltURL '" + altURL + "' not found in mappings.");
	}

	/**
	 * Use the LocationMapper created from the ont-policy.rdf file on initialization and updated with any added mappings
	 * to find the altURL corresponding to a publicUri.
	 * 
	 * @param publicUri
	 * @return
	 * @throws ConfigurationException
	 */
	public String getAltUrlFromPublicUri(String publicUri) throws ConfigurationException {
		if (getImplicitNamespacePrefix(publicUri) != null) {
			// this is implicit--it will not have an altUrl
			return null;
		}
		if (mappings != null) {
			if (mappings.containsKey(publicUri)) {
				return mappings.get(publicUri);
			}
		}
		OntDocumentManager jdm = getJenaDocumentMgr();
		if (jdm != null) {
			String alt = getJenaDocumentMgr().doAltURLMapping(publicUri);
			if (alt != null) {
				if (alt.equals(publicUri)) {
					String otherProjectAlt = getOtherProjectAltUrlFromPubliceUri(publicUri);
					if (otherProjectAlt != null) {
						return otherProjectAlt;
					}
				}
				return alt;
			}
		}
		if (isNamespaceInProjectDependency(publicUri)) {
			String otherProjectAlt = getOtherProjectAltUrlFromPubliceUri(publicUri);
			if (otherProjectAlt != null) {
				return otherProjectAlt;
			}
		}
		throw new ConfigurationException("PublicURI '" + publicUri + "' not found in mappings.");
	}
	
	public static final String[] PROJECT_DEPENDENCIES_CATEGORY = {"ProjectDependencies"};
	public static final String pdependsOn = "pDependsOn";

	@Override
	public List<String> getProjectDependencies() throws ConfigurationException {
		List<ConfigurationItem> config = getConfiguration(PROJECT_DEPENDENCIES_CATEGORY, false);
		List<Object> previousQueries = null;
		if (config != null && config.size() > 0) {
			previousQueries  = config.get(0).getAllValuesOfName(pdependsOn);
		}
		if (previousQueries != null && previousQueries.size() >= 1) {
			List<String> dependsOnProjects = new ArrayList<String>();
			for (Object dependsOn : previousQueries) {
				dependsOnProjects.add(dependsOn.toString());
			}
			return dependsOnProjects;
		}
		return null;
	}
	
	/**
	 * Method to determine if a URI (could be public URI or alt URL) is mapped in this ConfigurationManager (project)
	 * @param uri
	 * @return
	 */
	public boolean containsMappingForURI(String uri) {
		if (mappings != null) {
			if (mappings.containsKey(uri)) {
				return true;
			}
			if (mappings.containsValue(uri)) {
				return true;
			}
		}
		return false;
	}

    protected String rdfNodeToString(RDFNode node) {
    	if (node != null) {
			if (node instanceof Literal) {
				return ((Literal)node).getValue().toString();
			}
			return node.toString();
    	}
    	return null;
	}
	protected void setConfigModel(Model configModel) {
		this.configModel = configModel;
	}
	
	// get the configuration Model
	protected Model getConfigModel() {
		return configModel;
	}
	protected void setMappingModel(Model mappingModel) {
		this.mappingModel = mappingModel;
	}
	public Model getMappingModel() {
		return mappingModel;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.IConfigurationManager#findPublicUriOfOwlFile(java.lang.String)
	 */
	public String findPublicUriOfOwlFile(String sadlFile) {
		if (mappings != null) {
			String owlEquiv = sadlFile.substring(0, sadlFile.indexOf('.')) + ".owl";
			Iterator<String> mitr = mappings.keySet().iterator();
			while (mitr.hasNext()) {
				String pubUri = mitr.next();
				String altUrl = mappings.get(pubUri);
				if (altUrl.endsWith(owlEquiv)) {
					return pubUri;
				}
			}
		}
		return null;
	}
	
	public OntDocumentManager getJenaDocumentMgr() {
		if (jenaDocumentMgr == null) {
			if (getMappingModel() != null) {
				setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
				if (ontModelSpec != null) {
					ontModelSpec.setDocumentManager(getJenaDocumentMgr());
				}
			}
			else {
				setJenaDocumentMgr(OntDocumentManager.getInstance());
			}
		}
		return jenaDocumentMgr;
	}
	
	protected void setJenaDocumentMgr(OntDocumentManager jenaDocumentMgr) {
		this.jenaDocumentMgr = jenaDocumentMgr;
	}
	
	public boolean setTranslatorClassName(String translatorClassName)
			throws ConfigurationException {
		this.translatorClassName = translatorClassName;
		return true;
	}
	
	public boolean setReasonerClassName(String reasonerClassName) {
		this.reasonerClassName = reasonerClassName;
		return true;
	}

	public void setReadError(String message) {
		readError = message;
	}

	public String getTdbFolder() throws IOException {
		return getModelFolder() + "/TDB";
	}

	@Override
	public boolean setInferenceCanceled(boolean canceled) {
		boolean oldVal = inferenceCanceled;
		inferenceCanceled = canceled;
		return oldVal;
	}
	@Override
	public boolean getInferenceCanceled() {
		return inferenceCanceled;
	}
	@Override
	public IReasoner getCloneReasoner() throws ConfigurationException {
		IReasoner cloneReasonerInstance = null;
		
		String reasonerClassName = null;

		if (getConfigModel() != null) {
			 StmtIterator sitr = getConfigModel().listStatements(getReasonerSpecResource(), getConfigModel().getProperty(pREASONER_CLASSNAME), (RDFNode)null);
			 if (sitr.hasNext()) {
				 RDFNode cnobj = sitr.next().getObject();
				 if (cnobj instanceof Literal) {
					 reasonerClassName = ((Literal)cnobj).getLexicalForm();
				 }
			 }
		}
		if (reasonerClassName == null) {
			reasonerClassName = DEFAULT_REASONER;
		}
		try {
			cloneReasonerInstance = getClassInstance(reasonerClassName, IReasoner.class);
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "'", e);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "' due to an illegal access exception", e);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			throw new ConfigurationException("Reasoner class '" + reasonerClassName + "' not found", e);
		} catch (Throwable t) {
			t.printStackTrace();
		}

		if (cloneReasonerInstance == null) {
			throw new ConfigurationException("Unable to instantiate Reasoner '" + reasonerClassName + "'");
		}
		return cloneReasonerInstance;
	}
	
	public String getProjectPath() {
		return projectPath;
	}
	
	public void setProjectPath(String projectPath) {
		this.projectPath = projectPath;
	}
	
	public String toString() {
		return "ConfigurationManager for project '" + getProjectPath() + "'";
	}
	public OntModelSpec getOntModelSpec(OntModelSpec toCopySpec) {
		if (ontModelSpec == null) {
			ontModelSpec = new OntModelSpec(toCopySpec != null ? toCopySpec : OntModelSpec.OWL_MEM);
			if (jenaDocumentMgr != null) {
				ontModelSpec.setDocumentManager(jenaDocumentMgr);
			}
		}
		return ontModelSpec;
	}
	public void setOntModelSpec(OntModelSpec ontModelSpec) {
		this.ontModelSpec = ontModelSpec;
	}
	@Override
	public List<ImportMapping> loadImportedModel(Ontology importingOntology, OntModel importingModel,
			String publicImportUri, String altImportUrl) throws ConfigurationException {
    	// if not given the publicImportUri, then we must have the altImportUrl so find the publicImportUri in mapping
    	if (publicImportUri == null) {
    		if (altImportUrl == null) {
    			throw new ConfigurationException("Must have either a public URI or an actual URL to import a model.");
    		}
    		publicImportUri = getPublicUriFromActualUrl(altImportUrl);
    	}
    	
    	if (importingOntology == null) {
    		throw new ConfigurationException("Importing ontology is null!");
    	}
    	    	    	
    	// if not given altImportUrl, then we must have the publicImportUri so find the altImportUrl in mapping
    	if (altImportUrl == null && !importingOntology.getURI().equals(publicImportUri)) {
    		altImportUrl = getAltUrlFromPublicUri(publicImportUri);
    	}
    	
    	String importingOntologyUri = importingOntology.getURI();
    	if (importingOntologyUri == null) {
    		throw new ConfigurationException("Importing ontology '" + importingOntology.toString() + "' does not have an ontology declaration.");
    	}
    	if (!importingOntologyUri.equals(publicImportUri)) {	// don't import to self
	    	// Now load import model (with setCachedModels true so it loads any indirect imports)
	       	// and add all import OntModels to importing mappings
	    	Resource importedOntology = importingModel.createResource(publicImportUri);
	    	importingOntology.addImport(importedOntology);
    	}
//    	this.getJenaDocumentMgr().setCacheModels(true);
   		this.getJenaDocumentMgr().setProcessImports(true);
   		ReadFailureHandler rfh = this.getJenaDocumentMgr().getReadFailureHandler();
   		if (rfh instanceof SadlReadFailureHandler) {
   			((SadlReadFailureHandler)rfh).setSadlConfigMgr(this);
   		}
//   		getModelGetter().configureToModel(importingModel);
		importingModel.loadImports();
		if (readError != null) {
			String err = readError;
			readError = null;
			if (importingModel.hasLoadedImport(publicImportUri)) {
				// this must be removed or it will prevent correct loading from another project
				importingModel.removeLoadedImport(publicImportUri);
			}
	   		this.getJenaDocumentMgr().setProcessImports(false);
			throw new ConfigurationException(err);
		}
   		this.getJenaDocumentMgr().setProcessImports(false);
    	
    	List<ImportMapping> map = new ArrayList<ImportMapping>();
		Iterator<String> itr = importingModel.listImportedOntologyURIs(true).iterator();
		while (itr.hasNext()) {
			String impUri = itr.next();
			if (impUri.equals(importingOntology.getURI())) {
				// don't count ourselves as an import
				importingModel.setNsPrefix("", ConfigurationManager.addHashToNonTerminatedNamespace(impUri));
				continue;
			}
			String prefix = importingModel.getNsURIPrefix(ConfigurationManager.addHashToNonTerminatedNamespace(impUri));
			if (prefix == null) {
				prefix = getGlobalPrefix(impUri);
				if (prefix != null) {
					importingModel.setNsPrefix(prefix, ConfigurationManager.addHashToNonTerminatedNamespace(impUri));
				}
			}
			OntModel impModel = importingModel.getImportedModel(impUri);
			String actImpUrl = getAltUrlFromPublicUri(impUri);
			logger.debug("processing importingModel, url = "+actImpUrl);
			ImportMapping im = new ImportMapping(impUri, actImpUrl, null);
			im.setModel(impModel);
			im.setPrefix(prefix);
			map.add(im);
		}	
		this.getJenaDocumentMgr().setProcessImports(false);
//		this.getJenaDocumentMgr().setCacheModels(false);
    	return map;
	}

	@Override
	public boolean isNamespaceInProjectDependency(String ns) throws ConfigurationException {
		if (getOtherProjectConfigMgr(ns) != null) {
			return true;
		}
		if (getOtherProjectAltUrlFromPubliceUri(ns) != null) {
			Object opcm = getOtherProjectConfigMgr(ns);
			if (opcm != null) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Method to get an altUrl for a publicUri from a project upon which this project depends
	 * @param publicUri
	 * @return
	 * @throws ConfigurationException
	 */
	protected String getOtherProjectAltUrlFromPubliceUri(String publicUri) throws ConfigurationException {
		IConfigurationManager opcm = getOtherProjectConfigMgr(publicUri);
		if (opcm != null) {
			return opcm.getAltUrlFromPublicUri(publicUri);
		}
		List<String> dps = getProjectDependencies();
		if (dps != null) {
			for (String dp : dps) {
				String projectUrl = dp + "/OwlModels";
				IConfigurationManager dpCM = ConfigurationManagerFactory.getConfigurationManager(projectUrl, null);
				String dpAltUrl = dpCM.getAltUrlFromPublicUri(publicUri);
				if (dpAltUrl != null && !dpAltUrl.equals(publicUri)) {
					addOtherProjectConfigMgr(publicUri, dpCM);
					return dpAltUrl;
				}
			}
		}
		return null;
	}

	/**
	 * Method to get a global prefix for a publicUri from a project upon which this project depends
	 * @param publicUri
	 * @return
	 * @throws ConfigurationException
	 */
	protected String getOtherProjectPrefixFromPubliceUri(String publicUri) throws ConfigurationException {
		IConfigurationManager opcm = getOtherProjectConfigMgr(publicUri);
		if (opcm != null) {
			return opcm.getGlobalPrefix(publicUri);
		}
		if (getOtherProjectAltUrlFromPubliceUri(publicUri) != null) {
			opcm = getOtherProjectConfigMgr(publicUri);
			if (opcm != null) {
				return opcm.getGlobalPrefix(publicUri);
			}
		}
		return null;
	}

	protected IConfigurationManager getOtherProjectConfigMgr(String publicUri) {
		if (otherProjectConfigurationManagers != null) {
			return otherProjectConfigurationManagers.get(publicUri);
		}
		return null;
	}

	protected void addOtherProjectConfigMgr(String publicUri, IConfigurationManager otherProjectConfigMgr) {
		if (otherProjectConfigurationManagers == null) {
			otherProjectConfigurationManagers = new HashMap<String, IConfigurationManager>();
		}
		if (!otherProjectConfigurationManagers.containsKey(publicUri)) {
			otherProjectConfigurationManagers.put(publicUri, otherProjectConfigMgr);
		}
	}

	protected void clearOtherProjectConfigMgrs() {
		if (otherProjectConfigurationManagers != null) {
			otherProjectConfigurationManagers.clear();
		}
	}

	@Override
	public ISadlModelGetter getSadlModelGetter(String format) throws TranslationException, IOException {
		if (format == null) {
			format = getRepoType();
		}
		// is the format the same as the repoType?
		if (format != null && repoType != null && !format.equals(repoType)) {
			if (SadlPersistenceFormat.validateSadlFormat(format)) {
				return getNewSadlModelGetterForFormat(format);
			}
			else {
				throw new TranslationException("Unsupported persistence format: " + format);
			}
		}
		else if (sadlModelGetter == null) {
			if (sadlModelGetterPutter != null) {
				// since the putter extends the getter, we don't need to create a new getter.
				sadlModelGetter = sadlModelGetterPutter;
			}
			else if (SadlPersistenceFormat.validateSadlFormat(format)) {
				sadlModelGetter = getNewSadlModelGetterForFormat(format);
			}
			else {
				throw new TranslationException("Unsupported persistence format: " + format);
			}
		}
		return sadlModelGetter;
	}
	
	/**
	 * Method to create a new SadlModelGetter for this format
	 * @param format
	 * @return
	 * @throws TranslationException
	 * @throws IOException
	 */
	private ISadlModelGetter getNewSadlModelGetterForFormat(String format) throws TranslationException, IOException {
		if (SadlPersistenceFormat.getRDFFormat(format) != SadlPersistenceFormat.TDB_PseudoFormat &&
				SadlPersistenceFormat.getRDFFormat(format) != SadlPersistenceFormat.SEMTK_PseudoFormat) {
			return new SadlJenaFileGetter(this, format);
		}
		else if (format.equals(SadlPersistenceFormat.JENA_TDB_FORMAT)) {
			return new SadlJenaTDBGetter(this, format);
		}
		else if (format.equals(SadlPersistenceFormat.SEMTK_FORMAT)) {
			return new SadlJenaSemTKGetter(this, format);
		}
		else {
			throw new TranslationException("Persistence format " + format + " not yet implemented");
		}
	}

	@Override
	public ISadlModelGetterPutter getSadlModelGetterPutter(String format) throws TranslationException, IOException {
		if (format != null && repoType != null && !format.equals(repoType)) {
			if (SadlPersistenceFormat.validateSadlFormat(format)) {
				return getNewSadlModelGetterPutterForFormat(format);
			}
			else {
				throw new TranslationException("Unsupported persistence format: " + format);
			}
		}
		else if (sadlModelGetterPutter == null) {
			if (SadlPersistenceFormat.validateSadlFormat(format)) {
				sadlModelGetterPutter = getNewSadlModelGetterPutterForFormat(format);
			}
			else {
				throw new TranslationException("Unsupported persistence format: " + format);
			}
		}
		return sadlModelGetterPutter;
	}

	/**
	 * Method to create a new SadlModelGetterPutter for this format
	 * @param format
	 * @return
	 * @throws TranslationException
	 * @throws IOException
	 */
	private ISadlModelGetterPutter getNewSadlModelGetterPutterForFormat(String format) throws TranslationException, IOException {
		if (SadlPersistenceFormat.getRDFFormat(format) != SadlPersistenceFormat.TDB_PseudoFormat &&
				SadlPersistenceFormat.getRDFFormat(format) != SadlPersistenceFormat.SEMTK_PseudoFormat) {
			return new SadlJenaFileGetterPutter(this, format);
		}
		else if (format.equals(SadlPersistenceFormat.JENA_TDB_FORMAT)) {
			return new SadlJenaTDBGetterPutter(this, format);
		}
		else if (format.equals(SadlPersistenceFormat.SEMTK_FORMAT)) {
			return new SadlJenaSemTKGetterPutter(this, format);
		}
		else {
			throw new TranslationException("Persistence format " + format + " not yet implemented");
		}
	}

	@Override
	public String getRepoType() {
		if (repoType == null) {
			// look at the mappings and see what type is present there
			HashMap<String, String> mps = getMappings();
			if (mps != null) {
				Collection<String> altUrls = mps.values();
				for (String url: altUrls) {
					String fmt = SadlPersistenceFormat.getSadlPersistenceFormatFromFilename(url);
					if (SadlPersistenceFormat.validateSadlFormat(fmt)) {
						repoType = fmt;
						break;
					}
				}
			}
			else {
				repoType = SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT;	// default assumed
			}
		}
		return repoType;
	}

	@Override
	public void setRepoType(String repoType) {
		this.repoType = repoType;
	}

}
