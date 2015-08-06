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
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;

import org.apache.xerces.util.XMLChar;
import org.pojava.datetime.DateTimeConfig;
//import org.eclipse.emf.common.util.URI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.utils.SadlUtils;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.ontology.OntDocumentManager.ReadFailureHandler;
import com.hp.hpl.jena.query.Dataset;
import com.hp.hpl.jena.rdf.model.Bag;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.FileManager;
import com.hp.hpl.jena.util.FileUtils;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * This is a general purpose configuration manager suitable for model deployment environments.
 * This class handles two kinds of configuration information.
 * 1) Configuration information such as Reasoner/Translator pair selection and configuration
 * 2) Mappings of model namespaces to actual URLs for the translated model. 
 *    This is the Jena ont-policy.rdf file, although it may not be useful
 *    for all translations.
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
	protected static final String ONT_MANAGER_ALT_URL = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#altURL";
	protected static final String ONT_MANAGER_PUBLIC_URI = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#publicURI";
	protected static final String ONT_MANAGER_PREFIX = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#prefix";
	protected static final String ONT_MANAGER_ONTOLOGY_SPEC = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#OntologySpec";
	
	private static final String DEFAULT_TRANSLATOR = "com.ge.research.sadl.jena.translator.JenaTranslatorPlugin";
//	private static final String DEFAULT_TRANSLATOR = "com.ge.research.sadl.geonly.jena.JenaOptimizingTranslator";
	private static final String DEFAULT_REASONER = "com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin";
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
	private ITranslator translator = null;
	private IReasoner reasoner = null;
	protected OntDocumentManager jenaDocumentMgr;
	
	private ISadlJenaModelGetter modelGetter = null;
	
	protected String repoType = null;

	protected RDFNode sadlNode = null;
	protected Property createdBy;
	protected Property altUrlProp;
	protected Property publicUrlProp;
	protected Property prefixProp;
	protected RDFNode createdBySadlLiteral;

	private String readError;
	
	private boolean inferenceCanceled = false;

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
		repoType = _repoType;
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
				String url = (String)iterator.next();
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
	
	protected void loadMappingFile() throws ConfigurationException {
		if (getModelFolderPath() != null) {
			String modelFolderPathname = getModelFolderPath().getAbsolutePath();
			String mappingFilename = modelFolderPathname + File.separator + ONT_POLICY_RDF;
			File mappingFile = new File(mappingFilename);
			if (mappingFile.exists()) {
				// load mapping info from file
				setMappingModel(ModelFactory.createDefaultModel()) ;
			    InputStream in = FileManager.get().open(mappingFilename);
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
		else if (getModelFolderUrl() != null) {
			setMappingModel(ModelFactory.createDefaultModel()) ;
			getMappingModel().read(getModelFolderUrl() + "/" + ONT_POLICY_RDF);
		}
		setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
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
		            	if (val.equals(IConfigurationManager.dmyOrderDMY)) {
		            		DateTimeConfig.getGlobalDefault().setDmyOrder(true);
		            	}
		            	else {
		            		DateTimeConfig.getGlobalDefault().setDmyOrder(false);
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
		else if (getModelFolderUrl() != null) {
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
		return null;
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
			otherReasoner = (IReasoner) getClassInstance(reasonerClassName);
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
		IReasoner reasonerInst = getReasonerInstance();
		Resource reasonerCategory = getConfigModel().getResource(CONFIG_NAMESPACE + reasonerInst.getConfigurationCategory());
 		StmtIterator sitr = getConfigModel().listStatements(reasonerCategory, 
				getConfigModel().getProperty(pTRANSLATOR_CLASSNAME), (RDFNode)null);
        if (sitr.hasNext()) { 
        	RDFNode clsnmnode = sitr.nextStatement().getObject();
        	if (clsnmnode instanceof Literal) {
        		return ((Literal)clsnmnode).getValue().toString();
        	}
        }
        ITranslator translator = getTranslator();
		if (translator != null) {
			return translator.getClass().getCanonicalName();
		}
		throw new ConfigurationException("Unable to get current translator for unknown reason.");
	}

    /**
     * This method converts a configuration path filename to a Jena policy file to an
     * InputStream and a uri string
     * @param configFilename
     *
     * @return -- an Object[2] array: 0th element is an InputStream, 1st element is a URI string used for guessing the language and for writing back to disk
     */
    protected static Object[] findInputStream(String configFilename) {
        FileManager fm = new FileManager();
        fm.addLocatorFile();
        fm.addLocatorURL();
        fm.addLocatorClassLoader( fm.getClass().getClassLoader() );

        InputStream in = null ;
        String uri = null;
        StringTokenizer pathElems = new StringTokenizer( configFilename, FileManager.PATH_DELIMITER );
        while (in == null && pathElems.hasMoreTokens()) {
            uri = pathElems.nextToken();
            in = fm.openNoMap( uri );
        }
        Object[] ret = new Object[2];
        ret[0] = in;
        ret[1] = uri;
        return ret;
    }

	protected void setTranslator(ITranslator translator) {
		this.translator = translator;
	}

	protected void setReasoner(IReasoner reasoner) {
		this.reasoner = reasoner;
	}
	
	private void initializeTranslator() throws ConfigurationException {
		translator = getTranslatorInstance();
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

	private ITranslator getTranslatorInstance() throws ConfigurationException {
		String translatorClassName = null;
		ITranslator translatorClass = null;
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
		}
		if (translatorClassName == null) {
			translatorClassName = DEFAULT_TRANSLATOR;
		}
		return getTranslatorInstanceByClass(translatorClassName);
	}
	
	private ITranslator getTranslatorInstanceByClass(String translatorClassName) throws ConfigurationException {
		ITranslator translatorClass = null;
		try {
			translatorClass = (ITranslator) getClassInstance(translatorClassName);
			if (translatorClass == null) {
				throw new ConfigurationException("Unable to instantiate Translator '" + translatorClassName + "'");
			}
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
		String reasonerClassName = null;
//		IReasoner reasonerClass = null;
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
			reasoner = (IReasoner) getClassInstance(reasonerClassName);
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
	public Object getClassInstance(String className)
			throws InstantiationException, IllegalAccessException,
			ClassNotFoundException {
		return this.getClass().getClassLoader().loadClass(className).newInstance();
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
    public List<ImportMapping> loadImportedModel(Ontology importingOntology, OntModel importingModel, String publicImportUri, String altImportUrl) throws ConfigurationException {
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
    	if (altImportUrl == null) {
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
   		getModelGetter().configureToModel(importingModel);
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
    
	private List<ImportMapping> addImports(List<ImportMapping> mappings, String modname, OntModel m) {
		StmtIterator sitr = m.listStatements((Resource)null, OWL.imports, (RDFNode)null);
		if (sitr.hasNext()) {
			List<String> importNames = new ArrayList<String>();
			while (sitr.hasNext()) {
				Statement s = sitr.nextStatement();
				RDFNode obj = s.getObject();
				if (obj.isResource()) {
					String submodelname = ((Resource)obj).getURI();
					importNames.add(submodelname);
				}
			}
			for (int i = 0; i < importNames.size(); i++) {
				String submodelname = importNames.get(i);
				OntModel om = getModelGetter().getOntModel(submodelname, null, null);
				ImportMapping im = new ImportMapping(submodelname, null, null);
				mappings.add(im);
				mappings = addImports(mappings, submodelname, om);
			}
		}
		return mappings;
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
            com.hp.hpl.jena.rdf.model.Resource subj = s.getSubject();	
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
	            	//  1) if the mapping is of type "file:" and the file exists assume it is correct
	            	//	2) else if their is a file of that name in the same folder as the policy file assume that file is the correct one
	            	//	3) else if there is a sibling folder to the folder of the policy file that contains a file of that name assume it is the correct one
	            	if (strAltv.startsWith(IConfigurationManager.FILE_SHORT_PREFIX)) {
			            StringTokenizer st1 = new StringTokenizer(strAltv, "/");
			            String lastToken = null;
			 			while(st1.hasMoreTokens()) {
			 				lastToken = fileName;
			            	fileName = st1.nextToken();
			 			}
			 			String testName = strAltv;
		 				try {
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
			return globalPrefixes.get(uri);
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
    public static String fileNameToFileUrl(String fileName) {
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
    public static String fileUrlToFileName(String urlstr) throws MalformedURLException {
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
	public static String addHashToNonTerminatedNamespace(String inputNamespace) {
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
								newItem.addNameValuePair(new NameValuePair(name.getLocalName(), valObj));
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
										newItem.addNameValuePair(new NameValuePair(name.getLocalName(), valObj));
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
		newCatHier[newCatHier.length - 1] = ((Resource)value).getLocalName();
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
		if (mappings != null) {
			if (mappings.containsKey(publicUri)) {
				return mappings.get(publicUri);
			}
		}
		String alt = getJenaDocumentMgr().doAltURLMapping(publicUri);
		if (alt != null) {
			return alt;
		}
		
		throw new ConfigurationException("PublicURI '" + publicUri + "' not found in mappings.");
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
			String owlEquiv = sadlFile.substring(0, sadlFile.indexOf('.')) + SadlUtils.getOwlFileExtensionWithPrefix();
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
			}
			else {
				setJenaDocumentMgr(OntDocumentManager.getInstance());
			}
		}
		return jenaDocumentMgr;
	}
	
	private void setJenaDocumentMgr(OntDocumentManager jenaDocumentMgr) {
		this.jenaDocumentMgr = jenaDocumentMgr;
	}
	
	public boolean setTranslatorClassName(String translatorClassName)
			throws ConfigurationException {
		return true;
	}
	
	public boolean setReasonerClassName(String reasonerClassName) {
		return true;
	}

	public void setReadError(String message) {
		readError = message;
	}

	public String getTdbFolder() throws IOException {
		return getModelFolder() + "/TDB";
	}

	public ISadlJenaModelGetter getModelGetter() {
		return modelGetter;
	}
	public void setModelGetter(ISadlJenaModelGetter modelGetter) {
		this.modelGetter = modelGetter;
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
			cloneReasonerInstance = (IReasoner) getClassInstance(reasonerClassName);
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
}
