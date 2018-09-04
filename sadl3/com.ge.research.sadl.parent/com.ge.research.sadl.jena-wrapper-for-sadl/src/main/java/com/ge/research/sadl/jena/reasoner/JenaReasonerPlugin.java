/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.jena.reasoner;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.activation.DataSource;

import org.apache.jena.atlas.web.HttpException;
//import org.apache.jena.larq.IndexBuilderString;
//import org.apache.jena.larq.IndexLARQ;
//import org.apache.jena.larq.LARQ;
//import org.apache.lucene.index.IndexReader;
//import org.apache.lucene.index.IndexReader.FieldOption;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.CancellableBuiltin;
import com.ge.research.sadl.jena.reasoner.builtin.TypedBaseBuiltin;
import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin;
import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin.TranslationTarget;
import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.FunctionSignature;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InferenceCanceledException;
import com.ge.research.sadl.reasoner.InvalidDerivationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.Reasoner;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.RuleNotFoundException;
import com.ge.research.sadl.reasoner.SadlJenaModelGetter;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.reasoner.utils.StringDataSource;
import com.hp.hpl.jena.datatypes.DatatypeFormatException;
import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.datatypes.xsd.XSDDuration;
import com.hp.hpl.jena.graph.Graph;
import com.hp.hpl.jena.graph.Node_Literal;
import com.hp.hpl.jena.graph.Node_URI;
import com.hp.hpl.jena.graph.Node_Variable;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntDocumentManager.ReadFailureHandler;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.query.Dataset;
import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.ModelGetter;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.RDFReader;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.reasoner.Derivation;
import com.hp.hpl.jena.reasoner.InfGraph;
import com.hp.hpl.jena.reasoner.TriplePattern;
import com.hp.hpl.jena.reasoner.ValidityReport;
import com.hp.hpl.jena.reasoner.ValidityReport.Report;
import com.hp.hpl.jena.reasoner.rulesys.Builtin;
import com.hp.hpl.jena.reasoner.rulesys.BuiltinRegistry;
import com.hp.hpl.jena.reasoner.rulesys.ClauseEntry;
import com.hp.hpl.jena.reasoner.rulesys.Functor;
import com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner;
import com.hp.hpl.jena.reasoner.rulesys.Node_RuleVariable;
import com.hp.hpl.jena.reasoner.rulesys.Rule;
import com.hp.hpl.jena.reasoner.rulesys.Rule.ParserException;
import com.hp.hpl.jena.reasoner.rulesys.RuleDerivation;
import com.hp.hpl.jena.reasoner.rulesys.builtins.Product;
import com.hp.hpl.jena.shared.RulesetNotFoundException;
import com.hp.hpl.jena.sparql.syntax.Template;
import com.hp.hpl.jena.update.UpdateAction;
import com.hp.hpl.jena.update.UpdateFactory;
import com.hp.hpl.jena.update.UpdateRequest;
import com.hp.hpl.jena.util.FileManager;
import com.hp.hpl.jena.util.PrintUtil;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

/**
 * This class implements the IReasoner interface (by extending Reasoner)
 * for the Jena reasoner/rule engine.
 * 
 * The overall strategy is as follows.
 * 1. Initialization includes the model folder name and the public URI of the tbox entry point. 
 *    These are used to create the schema OntModel. It has a model spec of OWL_MEM so that no inferencing will occur.
 * 2. When instance data is received it is placed in the data OntModel, which also has a model spec of OWL_MEM. The
 * 	  schema model is added to the data model. This allows operations like finding namespaces for query preparation 
 *    to occur efficiently without doing any inference.
 * 3. When a question is asked the inference model (an InfModel) is created by creating a new OntModel with the specified
 * 	  model spec (from configuration, user-set) and binding it, with the data model, to the reasoner.    
 * 4. The configuration manager is responsible for setting up mappings, etc.
 * 
 * $Author: crapo $ 
 * $Revision: 1.19 $ Last modified on   $Date: 2015/09/28 15:19:32 $
 */
public class JenaReasonerPlugin extends Reasoner{
    private static final String DEFAULT_TRANSLATOR_CLASSNAME = "com.ge.research.sadl.jena.translator.JenaTranslatorPlugin";
	protected static final Logger logger = LoggerFactory.getLogger(JenaReasonerPlugin.class);
	public static String ReasonerFamily="Jena-Based";
	public static final String version = "$Revision: 1.19 $";
	private static String ReasonerCategory = "Jena";
	public static final String pModelSpec = "pModelSpec";
	public static final String pTimeOut = "pTimeOut";
	public static final String pRuleMode = "pRuleMode";
	public static final String pOWLTranslation = "pOWLTranslation";
	public static final String pTransitiveClosureCaching = "pTransitiveClosureCaching";
	public static final String pTrace = "pTrace";
	public static final String pUseLuceneIndexer = "pUseLuceneIndexer";
	public static final String pLuceneIndexerClass = "pLuceneIndexerClass";
	public static final String pDerivationLogging = "pDerivationLogging";

	protected static final String OWL_MEM_MICRO_RULE = "OWL_MEM_MICRO_RULE";
	protected static final String OWL_MEM_MINI_RULE = "OWL_MEM_MINI_RULE";
	protected static final String OWL_MEM = "OWL_MEM";
	protected static final String OWL_MEM_RDFS = "OWL_MEM_RDFS";
	protected static final String OWL_MEM_RULE = "OWL_MEM_RULE";
	protected static final String OWL_MEM_TRANS = "OWL_MEM_TRANS";
	protected static final String OWL_LITE_MEM_TRANS = "OWL_LITE_MEM_TRANS";
	protected static final String OWL_LITE_MEM = "OWL_LITE_MEM";
	protected static final String OWL_LITE_MEM_RULE = "OWL_LITE_MEM_RULES";
	protected static final String OWL_DL_MEM_RDFS = "OWL_DL_MEM_RDFS";
	protected static final String OWL_DL_MEM_RULE = "OWL_DL_MEM_RULE";
	protected static final String OWL_LITE_MEM_RDFS = "OWL_LITE_MEM_RDFS";
	protected static final String OWL_DL_MEM_TRANS = "OWL_DL_MEM_TRANS";
	protected static final String OWL_DL_MEM = "OWL_DL_MEM";
	protected static final String RDFS_MEM = "RDFS_MEM";
	protected static final String RDFS_MEM_TRANS = "RDFS_MEM_TRANS";
	protected static final String RDFS_MEM_RDFS = "RDFS_MEM_RDFS";
	
	protected static final String DERIVATION_NONE = "None";
	protected static final String DERIVATION_SHALLOW = "Shallow";
	protected static final String DERIVATION_DEEP = "Deep";
	
	public static final String TIMING_LOAD_MODEL="LoadModelTime";
	public static final String TIMING_LOAD_RULES="LoadRulesTime";
	public static final String TIMING_PREPARE_INFMODEL = "PrepareInfModelTime";
	public static final String TIMING_PREPARE_QUERY="PrepareQuery";
	public static final String TIMING_EXECUTE_QUERY = "ExecuteQueryTime";
	
	protected Boolean[] booleanOptions = {true, false};
	
	protected boolean collectTimingInfo = false;
	protected List<ReasonerTiming> timingInfo = null;
	
	
	protected final String folderNameSeparator = "/";		// the path separator for paths of model resources

	protected IConfigurationManager configurationMgr;
	protected List<ImportMapping> imports = null;
	protected List<String> ruleFilesLoaded;
	protected List<Rule> ruleList;
	protected OntModelSpec modelSpec;
	protected String tbox;
	protected OntModel schemaModel;
	protected boolean schemaModelIsCachedInferredModel = false;
	protected String aboxActualUrl = null;	// if instance data has been loaded from a single URL source, this remembers it
	protected int dataModelSourceCount = 0; // number of sources of data
	protected OntModel dataModel;
	protected GenericRuleReasoner reasoner = null;
	protected GenericRuleReasoner preBoundReasoner = null;
	protected Model infModel;	
	protected Dataset infDataset = null;
	protected boolean newInputFlag = false;
	protected boolean initialized = false;

	private boolean explanationsEnabled;
	
	private String luceneIndexerClass = null;
	
//	private FileAppender traceAppender = null;
	private String outputFormat = "N-TRIPLE";
	private String modelName;
	protected String instDataNS;
	private long tboxLoadTime = 0L;
	private boolean derivationLogging = false;
	private long queryTimeout = -1L;  // Query timeout, -1 means no timeout
	
//	// repo stuff
	private String repoType = null;
	protected List<ConfigurationItem> preferences = null;
	private OntModel tboxModelWithSpec;
	private List<ModelError> newErrors = null;
	
	public JenaReasonerPlugin() {
		// these will have been loaded by the translator and added to the configuration if they are needed
//		String pkg = "com.ge.research.sadl.jena.reasoner.builtin.";
//		addBuiltin("abs", pkg + "Abs");
//		addBuiltin("average", pkg + "Average");
//		addBuiltin("ceiling", pkg + "Ceiling");
//		addBuiltin("floor", pkg + "Floor");
//		addBuiltin("max", pkg + "Max");
//		addBuiltin("min", pkg + "Min");
//		addBuiltin("noSubjectsOtherThan", pkg + "NoSubjectsOtherThan");
//		addBuiltin("notOnlyValue", pkg + "NotOnlyValue");
//		addBuiltin("noUnknownValues", pkg + "NoUnknownValues");
//		addBuiltin("noValuesOtherThan", pkg + "NoValuesOtherThan");
//		addBuiltin("pow", pkg + "Pow");
//		addBuiltin("print", pkg + "Print");
//		addBuiltin("product", pkg + "Product");
//		addBuiltin("sqrt", pkg + "Sqrt");
//		addBuiltin("subtractDates", pkg + "SubtractDates");
//		addBuiltin("sum", pkg + "Sum");
	}
	
	/**
	 * Method used by translators that need the OntModel with import closure for translation
	 * 
	 * @return
	 */
	public OntModel getSchemaModel() {
		return schemaModel;
	}
	
	/**
	 * Method to return a list of all rules loaded by the reasoner
	 * 
	 * @return
	 */
	public List<Rule> getLoadedRules() {
		return ruleList;
	}
	
	/**
	 * Method to set the ConfigurationManager. If not set, a new one will be created.
	 * 
	 * @param configMgr
	 * @throws ConfigurationException 
	 */
	public void setConfigurationManager(IConfigurationManager configMgr) throws ConfigurationException {
//		if ((configMgr instanceof IConfigurationManagerForEditing)) {
//			((IConfigurationManagerForEditing) configMgr).setReasonerClassName(this.getClass().getCanonicalName());
//		}
		configurationMgr = configMgr;
	}
	

	public int initializeReasoner(String folderName, String modelName, String _repoType) throws ReasonerNotFoundException, ConfigurationException {
		return initializeReasoner(folderName, modelName, null, _repoType);		
	}
	
	public GenericRuleReasoner getReasonerOnlyWhenNeeded() throws ConfigurationException {
		if (reasoner != null) {
			return reasoner;
		}
		
		this.ruleFilesLoaded = new ArrayList<String>();
		this.ruleList = new ArrayList<Rule>();
		
		try {
			if (!configurationMgr.getModelGetter().modelExists(getModelName(), tbox)) {
				if (tbox.equals(getModelName())) {
					throw new ConfigurationException("The model '" + getModelName() + "' does not have a mapping and was not found.");
				}
				else {
					throw new ConfigurationException("The model with actual URL '" + tbox + "' and name '" + getModelName() + "' does not appear to exist.");
				}
			}
		} catch (MalformedURLException e) {
			throw new ConfigurationException("The actual file URL '" + tbox + "' for model '" + getModelName() + "' is not well-formed.");
		}

		String derval = getStringConfigurationValue(preferences , pDerivationLogging, DERIVATION_NONE);
		derivationLogging = (derval != null && !derval.equals(DERIVATION_NONE));
		modelSpec = getModelSpec(preferences);	// get this for later use when creating InfModel

		logger.debug("JenaReasonerPlugin.initializeReasoner, tbox = "+tbox);
		try {
			if (!tbox.startsWith("file:") && !tbox.startsWith("http:")) {
				//assume local file
				SadlUtils su = new SadlUtils();
				tbox = su.fileNameToFileUrl(tbox);
				logger.debug("JenaReasonerPlugin.initializeReasoner, modified tbox = "+tbox);
			}

			String format = repoType;
			if (!validateFormat(format)) {
				throw new ConfigurationException("Format '" + format + "' is not supported by reasoner '" + getConfigurationCategory() + "'.");
			}
			if (format.equals(IConfigurationManager.JENA_TDB)) {
				schemaModel = configurationMgr.getModelGetter().getOntModel(getModelName(), tbox, format);	
				schemaModel.getDocumentManager().setProcessImports(true);
				schemaModel.loadImports();
			}
			else {
				if (tbox.endsWith(".TDB/")) {
					// this is a cached inferred TDB model
					schemaModel = configurationMgr.getModelGetter().getOntModel(getModelName(), tbox, format);
					schemaModelIsCachedInferredModel = true;
					return null;
				}
				else {
					schemaModel = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null));
					ReadFailureHandler rfHandler = new SadlReadFailureHandler(logger);
					schemaModel.getDocumentManager().setProcessImports(true);
					schemaModel.getDocumentManager().setReadFailureHandler(rfHandler );
					schemaModel.getSpecification().setImportModelGetter((ModelGetter) configurationMgr.getModelGetter());
					schemaModel.read(tbox, format);
				}
			}
		} catch (Exception e1) {
			e1.printStackTrace();
		}	
		
		if (logger.isDebugEnabled()) {
			logger.debug("schemaModel '" + getModelName() + "' with  tbox '" + tbox + "' loaded");
			dumpModelToLogger(schemaModel);
		}
		loadImports();

		logger.debug("JenaReasonerPlugin.initializeReasoner, imports size = " + (imports == null ? 0 : imports.size()));

		long t2 = System.currentTimeMillis();
		loadRules(schemaModel, getModelName());
		logger.debug("JenaReasonerPluging.initialize, size of ruleList = "+ruleList.size());
		reasoner = new GenericRuleReasoner(ruleList);
		reasoner.setDerivationLogging(derivationLogging);
		logger.debug("JenaReasonerPluging.initialize, size of ruleList from reasoner = "+reasoner.getRules().size());
		reasoner.setMode(getRuleMode(preferences));
		long t3 = System.currentTimeMillis();
		if (collectTimingInfo) {
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_MODEL, "load ontology model", t2 - tboxLoadTime));
			int numRules = ruleList.size();
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_RULES, "load model " + numRules + " rules", t3 - t2));
		}

		long t4;
		if (collectTimingInfo) {
			t4 = System.currentTimeMillis();
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_RULES, "bind schema to reasoner", t4 - t3));			
		}
		boolean transitiveClosure = getBooleanConfigurationValue(preferences, pTransitiveClosureCaching, false);
		reasoner.setTransitiveClosureCaching(transitiveClosure);
		reasoner.setOWLTranslation(getBooleanConfigurationValue(preferences, pOWLTranslation, false));
		boolean bTrace = getBooleanConfigurationValue(preferences, pTrace, false);
		reasoner.setTraceOn(bTrace);
		if (bTrace) {
//			traceAppender = new FileAppender();
			// configure the appender here, with file location, etc
			File tboxfile = null;
			try {
				SadlUtils su = new SadlUtils();
				tboxfile = new File(su.fileUrlToFileName(tbox));
			} catch (MalformedURLException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
//			if (tboxfile != null && tboxfile.exists()) {
//				File modelfolder = tboxfile.getParentFile();
//				try {
//					traceAppender.setFile(modelfolder.getCanonicalPath() + File.separator + "Temp" + File.separator + "trace.log");
//					traceAppender.setImmediateFlush(false);
//					traceAppender.activateOptions();
//				} catch (IOException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//					traceAppender = null;
//				}
//			}
//			else {
//				traceAppender = null;
//			}
		}
		else {
//			traceAppender = null;
		}
		if (getBooleanConfigurationValue(preferences, pUseLuceneIndexer, false)) {
			luceneIndexerClass = getStringConfigurationValue(preferences, pLuceneIndexerClass, "com.ge.research.sadl.jena.reasoner.LuceneModelIndexerImpl");
		}
		
		String strTimeOut = getStringConfigurationValue(preferences, pTimeOut, "-1");
		try {
			queryTimeout = Long.parseLong(strTimeOut.trim());
		}
		catch (NumberFormatException e) {
			String msg = "Invalid timeout value '" + strTimeOut + "'";
			logger.error(msg); addError(new ModelError(msg, ErrorType.ERROR));

		}
		return reasoner;
	}


	private boolean validateFormat(String format) {
		if (format == null ||
				(!format.equals(IConfigurationManager.JENA_TDB) &&
						!format.equals(IConfigurationManager.N3_FORMAT) &&
						!format.equals(IConfigurationManager.N_TRIPLE_FORMAT) &&
						!format.equals(IConfigurationManager.RDF_XML_ABBREV_FORMAT) &&
						!format.equals(IConfigurationManager.RDF_XML_FORMAT))) {
			return false;
		}
		return true;
	}

	public int initializeReasoner(String folderName, String _modelName, 
			List<ConfigurationItem> _preferences, String _repoType) throws ReasonerNotFoundException, ConfigurationException {
		preferences = _preferences;
		repoType = _repoType;
		setModelName(_modelName);
		
		if (timingInfo == null) {
			timingInfo = new ArrayList<ReasonerTiming>();
		}
		else {
			timingInfo.clear();
		}
		
		tboxLoadTime = System.currentTimeMillis();

		if (configurationMgr == null) {
		//	Get the correct Mappings from the policy file
			OntDocumentManager mgr = OntDocumentManager.getInstance();
			mgr.reset();
			mgr.setProcessImports(true);
			configurationMgr = ConfigurationManagerFactory.getConfigurationManager(folderName, repoType);
		}


		//Get the real tbox and rule file path
		tbox = configurationMgr.getAltUrlFromPublicUri(getModelName());
		
		if (tbox == null) {
			throw new ConfigurationException("No mapping to an actual URL found for model '" + getModelName() + "'.");
		}	
		String format = repoType;
		try {
			String tdbFolder = configurationMgr.getTdbFolder();
			if (configurationMgr.getModelGetter() == null) {
				configurationMgr.setModelGetter(new SadlJenaModelGetter(configurationMgr, tdbFolder));
			}
			format = configurationMgr.getModelGetter().getFormat();
			if (!format.equals(IConfigurationManager.JENA_TDB)) {
				String ext = tbox.substring(tbox.lastIndexOf('.'));
				format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;	// this will create a reader that will handle either RDF/XML or RDF/XML-ABBREV 
				if (ext.equalsIgnoreCase(".n3")) {
					format = "N3";
				}
				else if (ext.equalsIgnoreCase(".ntriple") || ext.equalsIgnoreCase(".nt")) {
					format = "N-TRIPLE";
				}
				configurationMgr.getModelGetter().setFormat(format);
			}
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		initialized = true;
		
		return 1;
	}
	
	private void dumpModelToLogger(OntModel model) {
		ByteArrayOutputStream os = new ByteArrayOutputStream(); 
		model.write(os); 
		try {
			String aString = new String(os.toByteArray(),"UTF-8");
			logger.debug(aString);
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}

	private void loadImports() {
		if (configurationMgr != null) {
			try {
				imports = configurationMgr.loadImportedModel(schemaModel.getOntology(modelName), 
								schemaModel, modelName, null);
			} catch (Throwable t) {
				// TODO Auto-generated catch block
				t.printStackTrace();
			}
		}
		
	}


	public int initializeReasoner(URI modelFile, String modelName,
			List<ConfigurationItem> preferences, String _repoType) throws ReasonerNotFoundException {
		ruleList = null;
		try {
			initializeReasoner(modelFile.toString(), modelName, preferences, _repoType);
		} catch (ConfigurationException e) {
			e.printStackTrace();
		}
		return 0;
	}


	public int initializeReasoner(URI modelFile, String modelName, String _repoType)
			throws ReasonerNotFoundException {
		
		try {
			initializeReasoner(modelFile.toString(), modelName, _repoType);
		} catch (ConfigurationException e) {
			e.printStackTrace();
		}
		return 0;
	}


	public boolean loadRules(String ruleFileName) throws IOException {
		if (ruleFileName != null) {
			try {
				InputStream in = configurationMgr.getJenaDocumentMgr().getFileManager().open(ruleFileName);
				if (in != null) {
				    try {
				    	InputStreamReader isr = new InputStreamReader(in);
				    	BufferedReader br = new BufferedReader(isr);
						List<Rule> rules = Rule.parseRules(Rule.rulesParserFromReader(br));
						if (rules != null) {
							ruleList.addAll(rules);
							newInputFlag = true;
							return true;
						}
				    } catch (ParserException e) {
				    	String msg = "Error reading rule file '" + ruleFileName + "': " + e.getMessage();
				    	logger.error(msg);
				    	addError(new ModelError(msg, ErrorType.ERROR));
				    }
				    finally {
				    	in.close();
				    }
				}
			}
			catch (RulesetNotFoundException e) {
				// ok if not found
				return false;
			}
			catch (HttpException e) {
				// ok if not found
				return false;
			}
		}		
//		dataModelSourceCount++;
		return false;
	}
	

	public boolean loadRules(URI ruleFileName) throws IOException {
		if (ruleFileName != null) {
			ruleList.addAll(Rule.rulesFromURL(ruleFileName.toString()));
			newInputFlag  = true;
			dataModelSourceCount++;
			return true;
		}
		//TODO this needs to handle the case where there are no rules
		else 
			return false;
	}


	public boolean addRules(List<String> rules) {
		for(String f:rules)
			ruleList.add(Rule.parseRule(f));	
		if (preBoundReasoner != null) {
			reasoner = preBoundReasoner;
			preBoundReasoner = null;
		}
		try {
			if (getReasonerOnlyWhenNeeded() != null) {
				getReasonerOnlyWhenNeeded().setRules(ruleList);
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		newInputFlag = true;
		dataModelSourceCount++;
		return true;
	}
	

	public boolean addRule(String rule) {
		Rule newRule = Rule.parseRule(rule);
		try {
			deleteRule(newRule.getName());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ruleList.add(newRule);
		if (preBoundReasoner != null) {
			reasoner = preBoundReasoner;
			preBoundReasoner = null;
		}
		try {
			if (getReasonerOnlyWhenNeeded() != null) {
				getReasonerOnlyWhenNeeded().setRules(ruleList);
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		newInputFlag = true;
		dataModelSourceCount++;
		return true;
	}
		

	public boolean deleteRule(String ruleName) throws RuleNotFoundException {
		try {
			getReasonerOnlyWhenNeeded();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		for (int i=0; i<ruleList.size();i++){			
			Rule r  = ruleList.get(i);
			String rName = new String(r.getName());
			if(rName.equals(ruleName)){
				ruleList.remove(r);
				if (preBoundReasoner != null) {
					reasoner = preBoundReasoner;
					preBoundReasoner = null;
				}
				try {
					if (getReasonerOnlyWhenNeeded() != null) {
						getReasonerOnlyWhenNeeded().setRules(ruleList);
					}
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				newInputFlag = true;
				return true;
			}			
		}
		dataModelSourceCount++;
		return false;
	}


	public boolean loadInstanceData(String instanceDatafile) throws IOException, ConfigurationException {		
		if (!instanceDatafile.startsWith("file:") && !instanceDatafile.startsWith("http:")) {
			try {
				SadlUtils su = new SadlUtils();
				instanceDatafile = su.fileNameToFileUrl(instanceDatafile);
			} catch (URISyntaxException e) {
				throw new IOException(e);
			}		
		}
		getReasonerOnlyWhenNeeded();
		initializeDataModel();
		dataModel.add(dataModel.getDocumentManager().getFileManager().loadModel(instanceDatafile));
		addModelNamespaceToJenaMapAsEmptyPrefix(dataModel);
		newInputFlag = true;
		dataModelSourceCount++;
		if (dataModelSourceCount == 1) {
			aboxActualUrl = instanceDatafile;
		}
		return true;
	}


	public boolean loadInstanceData(URI instanceDatafile) throws IOException, ConfigurationException {
		initializeDataModel();
		dataModel.add(FileManager.get().loadModel(instanceDatafile.toString()));
		addModelNamespaceToJenaMapAsEmptyPrefix(dataModel);
		newInputFlag = true;
		dataModelSourceCount++;
		if (dataModelSourceCount == 1) {
			aboxActualUrl = instanceDatafile.toString();
		}
		return true;
	}
	
	public boolean loadInstanceData(OntModel model) throws ConfigurationException {
		getReasonerOnlyWhenNeeded();
		if (dataModel == null) {
			initializeDataModel();
		}
		dataModel.add(model);
		dataModelSourceCount++;
		newInputFlag = true;
		return true;
	}


	public boolean loadInstanceData(InputStream is, String format) throws IOException, ConfigurationException {		
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(is));
			String base = null;
			initializeDataModel();
			OntModel newModel = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null));
			if (format != null) {
				RDFReader reader = newModel.getReader(format);
				reader.read(newModel, is, base);
			}
			else {
				RDFReader reader = newModel.getReader();
				reader.read(newModel, is, base);
			}
			dataModel.add(newModel);
			in.close();
			addModelNamespaceToJenaMapAsEmptyPrefix(dataModel);
			newInputFlag = true;
			dataModelSourceCount++;
			return true;			
		} catch (IOException e) {
			e.printStackTrace();
		}		
		return false;
	}
	
	private boolean addModelNamespaceToJenaMapAsEmptyPrefix(OntModel abmodel) {
		String instDataBaseUri = getBaseUriOfModel(abmodel);
		if (instDataBaseUri != null) {
			setInstanceDataNS(instDataBaseUri + "#");
			abmodel.setNsPrefix("", getInstanceDataNS());
			return true;
		}
		return false;
	}
	
	private String getBaseUriOfModel(OntModel model) {
		String modelBaseUri = null;
		Set<String> importuris = model.listImportedOntologyURIs(true);
		ExtendedIterator<Ontology> ontItr = model.listOntologies();
		if (ontItr.hasNext()) {
			while (ontItr.hasNext()) {
				Ontology ont = ontItr.next();
				if (modelBaseUri == null) {
					modelBaseUri = ont.getURI();	// first is default in case imports are circular
				}
				if (!importuris.contains(ont.getURI())) {
					modelBaseUri = ont.getURI();
					break;
				}
			}
		}
		return modelBaseUri;
	}


	public boolean addTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		getReasonerOnlyWhenNeeded();
		initializeDataModel();
		Statement s = null;
		try {
			s = prepareStatement(sub, pred, obj);
			dataModel.add(s);
		} catch (InvalidNameException e) {
			throw new TripleNotFoundException("Unable to prepare triple (" + sub + ", " + pred + ", " + obj + "): " + e.getMessage());
		} catch (MalformedURLException e) {
			throw new TripleNotFoundException("Unable to prepare triple (" + sub + ", " + pred + ", " + obj + "): " + e.getMessage());
		}
		newInputFlag = true;
		dataModelSourceCount++;
		return true;
	}

	protected void initializeDataModel() throws ConfigurationException {
		if (dataModel == null) {
			if (schemaModel == null) {
				getReasonerOnlyWhenNeeded();
			}
			dataModel = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null));
			String instNS = getInstanceDataNS();
			if (instNS != null) {
				Resource importOnt = dataModel.getResource(getModelName());
				dataModel.createOntology(getInstanceModelName()).addImport(importOnt);
				dataModel.getDocumentManager().setProcessImports(true);
				dataModel.loadImports();
				dataModel.getDocumentManager().setProcessImports(false);
			}
			else if (schemaModel != null) {
				dataModel.add(schemaModel);
			}
			infModel = null;
		}
	}


	public boolean deleteTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		try {
			getReasonerOnlyWhenNeeded();
			if (dataModel != null) {
				RDFNode[] spo = prepareSubjectPredicateObject(sub, pred, obj);
				if (spo != null) {
					StmtIterator stI = dataModel.listStatements((Resource)spo[0], (Property)spo[1], spo[2]);
					if (stI.hasNext()) {
						List<Statement> stmtsToRemove = new ArrayList<Statement>();			
						while (stI.hasNext()) {
							Statement stmt = stI.nextStatement();
							stmtsToRemove.add(stmt);
						}
						stI.close();
						for (int i = 0; i < stmtsToRemove.size(); i++) {
							dataModel.remove(stmtsToRemove.get(i));
						}
						newInputFlag = true;
						dataModelSourceCount++;
						return true;
					}
				}
				else {
					return false;
				}
			}
		} catch (InvalidNameException e) {
			throw new TripleNotFoundException("Unable to prepare triple (" + sub + ", " + pred + ", " + obj + "): " + e.getMessage());
		} catch (MalformedURLException e) {
			throw new TripleNotFoundException("Unable to prepare triple (" + sub + ", " + pred + ", " + obj + "): " + e.getMessage());
		}
		
		return false;
	}


	public void updateTriple(String oldSub, String oldPred, String oldObj,
			String newSub, String newPred, String newObj)
			throws TripleNotFoundException, ConfigurationException {
		this.deleteTriple(oldSub, oldPred, oldObj);
		this.addTriple(newSub, newPred, newObj);
		dataModelSourceCount++;
		newInputFlag = true;
		
	}


	public List<ModelError> checkModelValidity() {
		List<ModelError> results = null;
		try {
			getReasonerOnlyWhenNeeded();
			generateTboxModelWithSpec();
			if (tboxModelWithSpec != null) {
				try {
					ValidityReport report = tboxModelWithSpec.validate();
					if (report == null) {
						try {
							prepareInfModel();
						} catch (ConfigurationException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						if (infModel != null) {
							if (infModel instanceof InfModel) {
								report = ((InfModel) infModel).validate();
							}
							else {
								results = new ArrayList<ModelError>();							
								results.add(new ModelError("Unable to validate; model is not an InfModel", ErrorType.WARNING));
							}
						}
					}
					if (report != null) {
						if (report.isClean()) {
							return null;
						}
						Iterator<Report> rptitr = report.getReports();
						results = new ArrayList<ModelError>();
						while (rptitr.hasNext()) {
							Report rpt = rptitr.next();
							if (rpt.isError()) {
								results.add(new ModelError(rpt.toString(), ErrorType.ERROR));
							}
							else {
								results.add(new ModelError(rpt.toString(), ErrorType.WARNING));
							}
						}
					}
					else {
						results = new ArrayList<ModelError>(1);
						results.add(new ModelError("Failed to complete validity check.", ErrorType.ERROR));
					}
				}
				catch (DatatypeFormatException e) {
					if (results == null) {
						results = new ArrayList<ModelError>();
					}
					results.add(new ModelError("Exception while validating model: " + e.getLocalizedMessage(), ErrorType.ERROR));
				}
				catch (Throwable t) {
					t.printStackTrace();
					if (results == null) {
						results = new ArrayList<ModelError>();
					}
					results.add(new ModelError("Exception while validating model: " + t.getLocalizedMessage(), ErrorType.ERROR));
				}
			}
			else {
				results = new ArrayList<ModelError>(1);
				results.add(new ModelError("Failed to obtain an inferred model on which to do a validity check.", ErrorType.ERROR));
			}
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		return results;
	}


	public DataSource construct(String constructQuery)
			throws QueryParseException, QueryCancelledException {
		startTrace();
		QueryExecution qexec = null;		
		try {
			prepareInfModel();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		Model results = null;
		try {
			long t1 = System.currentTimeMillis();
			qexec = QueryExecutionFactory.create(QueryFactory.create(constructQuery, Syntax.syntaxARQ), this.infModel);
			qexec.setTimeout(queryTimeout);
			results = qexec.execConstruct();
			StmtIterator sitr = results.listStatements(null, RDF.type, OWL.Ontology);
			while (sitr.hasNext()) {
				Statement s = sitr.nextStatement();
				Resource r = s.getSubject();
				r.addProperty(RDFS.comment, results.createTypedLiteral("This model is output of " + this.getClass().getName()));
			}
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			results.write(out, getOutputFormat());
			String data = out.toString();
			StringDataSource ds = new StringDataSource(data, "text/plain");
			if (getOutputFormat().equals("N-TRIPLE") || getOutputFormat().equals("N3")) {
				ds.setName(getOutputFormat());
			}
			else {
				ds.setName("OWL");
			}
			if (collectTimingInfo) {
				long t2 = System.currentTimeMillis();
				timingInfo.add(new ReasonerTiming(TIMING_EXECUTE_QUERY, constructQuery, t2 - t1));
			}
			return ds;
		} 
		catch (com.hp.hpl.jena.query.QueryCancelledException e) {
			e.printStackTrace();
			logger.error("query timed out with Exception: " + e.getMessage());
			throw new QueryCancelledException("Construct Query '" + constructQuery + "' timed out: " + e.getLocalizedMessage());
		}
		catch (Exception e) {
			e.printStackTrace();
			logger.error("query failed with Exception: " + e.getMessage());
			throw new QueryParseException("Construct Query '" + constructQuery + "' failed: " + e.getLocalizedMessage());
		}
		finally { 
			if (qexec != null) qexec.close();
			endTrace();
		}
	}

	public ResultSet ask(String askQuery) throws QueryParseException, QueryCancelledException {
		boolean cancelled = false;
		ResultSet rs = null;
//		synchronized(ReasonerFamily) {
		try {
			startTrace();
			QueryExecution qexec = null;		
			com.hp.hpl.jena.query.ResultSet results = null;		
			prepareInfModel();
			try {
				String qstr = handleNamedQueryByName(askQuery);
				if (qstr != null) {
					askQuery = qstr;
				}
			} catch (InvalidNameException e1) {
				throw new QueryParseException(e1.getMessage());
			}
			try {
				long t1 = System.currentTimeMillis();
	//			IndexLARQ index = null;
	//			if (askQuery.contains("http://jena.hpl.hp.com/ARQ/property#textMatch")) {
	//				// this query uses Lucene
	//				if (luceneIndexerClass != null) {
	//					ILuceneModelIndexer indexer = (ILuceneModelIndexer) Class.forName(luceneIndexerClass).newInstance();
	////					OntModel om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM, infModel);
	//					indexer.setModel(dataModel != null ? dataModel : schemaModel); //indexer.setModel(om);
	//					IndexBuilderString larqBuilder = indexer.buildModelIndex();
	//					index = larqBuilder.getIndex();
	//
	//				}
	//				else {
	//					FieldOption fo = IndexReader.FieldOption.ALL;
	//					IndexBuilderString larqBuilder = new IndexBuilderString();
	//					larqBuilder.indexStatements(this.infModel.listStatements());
	//	//				larqBuilder.indexStatement(s);
	//					larqBuilder.closeWriter();
	//					index = larqBuilder.getIndex();
	//				}
	//			}
				if (askQuery.startsWith("delete") || askQuery.startsWith("insert")) {
					UpdateRequest urequest = UpdateFactory.create(askQuery);
					if (infDataset != null) {
						UpdateAction.execute(urequest, infDataset);
					}
					else {
						UpdateAction.execute(urequest, this.infModel);
					}
				}
				else {
					if (infDataset != null) {
						qexec = QueryExecutionFactory.create(QueryFactory.create(askQuery, Syntax.syntaxARQ),infDataset);
					}
					else {
						qexec = QueryExecutionFactory.create(QueryFactory.create(askQuery, Syntax.syntaxARQ), this.infModel);
					}
		//			if (index != null) {
		//				LARQ.setDefaultIndex(qexec.getContext(), index);
		//			}
					qexec.setTimeout(queryTimeout);
					if (askQuery.trim().substring(0, 3).equals("ask")) {	
						boolean askResult = qexec.execAsk();
						String[] columnName = new String[1];
						columnName[0] = "ask";
						Object array[][] = new Object[1][1];
						array[0][0] = askResult;
						rs = new ResultSet(columnName, array);
					}
					else if (askQuery.trim().substring(0, 9).equals("construct")) {
						Model constructModel = qexec.execConstruct();
						if (constructModel != null) {
							StmtIterator sitr = constructModel.listStatements();
							if (sitr.hasNext()) {
								String[] columnName = new String[3];
								Query q = qexec.getQuery();
								Template template = q.getConstructTemplate();
								Triple triple0 = template.getBGP().get(0);
	//							columnName[0] = qexec.getQuery().getProjectVars().get(0).getVarName();  //"s";
	//							columnName[1] = qexec.getQuery().getProjectVars().get(1).getVarName(); //"p";
	//							columnName[2] = qexec.getQuery().getProjectVars().get(2).getVarName(); //"o";
								com.hp.hpl.jena.graph.Node subj = triple0.getSubject();
								com.hp.hpl.jena.graph.Node pred = triple0.getPredicate();
								com.hp.hpl.jena.graph.Node obj = triple0.getObject();
								
								columnName[0] = subj.isVariable() ? subj.getName() : subj.getLocalName();
								columnName[1] = pred.isVariable() ? pred.getName() : pred.getLocalName();
								columnName[2] = obj.isVariable() ? obj.getName() : obj.getLocalName();
								List<Object[]> dataList = new ArrayList<Object[]>();
								while (sitr.hasNext()) {
									Statement stmt = sitr.nextStatement();
									Object[] row = new Object[3];
									row[0] = stmt.getSubject().toString();
									row[1] = stmt.getPredicate().toString();
									RDFNode val = stmt.getObject();
									if (val instanceof Resource) {
										row[2] = ((Resource)val).toString();
									}
									else if (val instanceof Literal) {
										row[2] = ((Literal)val).getValue();
									}
									else {
										row[2] = val.toString();
									}
									dataList.add(row);
								}
								Object[][] data = new Object[dataList.size()][3];
								for (int r = 0; r < dataList.size(); r++) {
									for (int c = 0; c < 3; c++) {
										data[r][c] = ((Object[]) dataList.get(r))[c];
									}
								}
								rs = new ResultSet(columnName, data);
							}
						}
					}
					else {
						results = qexec.execSelect();
						rs = convertFromJenaResultSetToReasonerResultSet(results);
					}
					if (collectTimingInfo) {
						long t2 = System.currentTimeMillis();
						timingInfo.add(new ReasonerTiming(TIMING_EXECUTE_QUERY, "execute query (" + askQuery + ")", t2 - t1));
					}
				}
			}
			catch (com.hp.hpl.jena.query.QueryCancelledException e) {
				rs = null;
				cancelled = true;
				throw new QueryCancelledException("Query timed out (" + queryTimeout + " seconds): '" + askQuery + "'\n");
			}
			catch (InferenceCanceledException e) {
				rs = null;
				throw e;
			}
			catch (Exception e) {
				rs = null;
				e.printStackTrace();
				logger.error("query failed with Exception: " + e.getMessage());
				throw new QueryParseException("Query '" + askQuery + "' failed: " + e.getLocalizedMessage(), e);
			}
			finally { if (!cancelled && qexec != null) qexec.close();	}
			endTrace();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		finally {
		}
//		}
		return rs;
	}

	protected ResultSet convertFromJenaResultSetToReasonerResultSet(com.hp.hpl.jena.query.ResultSet results) {
		if (!results.hasNext()) {
			return null;
		}
		ArrayList<ArrayList<Object>> o = new ArrayList<ArrayList<Object>>();
		List<String> queryVars = results.getResultVars();
		String[] columnName = new String[queryVars.size()];
		columnName = queryVars.toArray(columnName);
		while (results.hasNext()) {
			QuerySolution soln = results.next();
			ArrayList<Object> temp = new ArrayList<Object>();
			for (int j = 0; j < columnName.length; j++) {
				RDFNode n = soln.get(columnName[j]);
				if (n != null && n.isLiteral()) {
					Object val = ((Literal)n).getValue();
					if (val instanceof XSDDateTime) {
						temp.add(((XSDDateTime)val).asCalendar().getTime());
					} 
					else if (val instanceof XSDDuration) {
						temp.add(((XSDDuration)val).toString());
					}
					else {
						temp.add(val);
					}
				}
				else if (n != null && n.isResource()) {
					if (!((Resource)n).isAnon()){
						temp.add(((Resource)n).getURI());
					}
					else {
						temp.add(n.toString() + "(blank node)");
					}
				}
				else {
					temp.add(n == null? n : n.toString());	// for queries with OPTIONAL n can be null
				}
			}
			o.add(temp);
		}

		Object array[][] = new Object[o.size()][columnName.length];
		for(int i=0; i<o.size(); i++)
			array[i] = (o.get(i)).toArray(new Object[columnName.length]);
		
		ResultSet rs = new ResultSet(columnName, array);
		return rs;		
	}
	

	public ResultSet ask(String sub, String pred, String obj)
			throws TripleNotFoundException {
		startTrace();
		try {
			prepareInfModel();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		long t1 = 0L;
		ReasonerTiming rt = null;
		if (collectTimingInfo) {
			rt = new ReasonerTiming(TIMING_EXECUTE_QUERY, "ask(" + sub + "," + pred + "," + obj + ")", 0);
			t1 = System.currentTimeMillis();
		}
		int i = 0;
		boolean bIncludeSubject = false;
		boolean bIncludePredicate = false;
		boolean bIncludeObject = false;
		int colcount = 0;
		ArrayList<ArrayList<Object>> result = new ArrayList<ArrayList<Object>>();
		Resource r = null;
		if (sub != null) {
			r = ResourceFactory.createResource(sub);
		}
		else {
			bIncludeSubject = true;
			colcount++;
		}
		Property p = null;
		if (pred != null) {
			p = ResourceFactory.createProperty(pred);
		}
		else {
			bIncludePredicate = true;
			colcount++;
		}
		RDFNode n = null;
		if (obj != null) {
			Object objVal = null;
			if (!obj.startsWith("http://")) {
				objVal = xsdStringToObject(obj);
				if (objVal != null) {
					if (dataModel != null) {
						n = dataModel.createTypedLiteral(objVal);
					}
					else {
						n = infModel.createTypedLiteral(objVal);
					}
				}
			}
			if (n == null) {
				n = ResourceFactory.createResource(obj);
			}
		}
		else {
			bIncludeObject = true;
			colcount++;
		}
		StmtIterator stI = this.infModel.listStatements(r, p, n);
		if (!stI.hasNext()) {
			return null;
		}
		while(stI.hasNext()){			
			Statement s = stI.next();
			result.add(new ArrayList<Object>());
			if (sub == null) {
				result.get(i).add(s.getSubject().getURI());
			}
			if (pred == null) {
				result.get(i).add(s.getPredicate().getURI());
			}
			if (obj == null) {
				RDFNode objval = s.getObject();
				if (objval != null && objval.isLiteral()) {
					try {
						result.get(i).add(((Literal)objval).getValue());
					}
					catch (Throwable t) {
						System.err.println("Error (" + t.getMessage() + ") converting literal value to result set");
						result.get(i).add(((Literal)objval).getLexicalForm());
					}
				}
				else if (objval != null && objval.isResource() && !((Resource)objval).isAnon()){
					result.get(i).add(((Resource)objval).getURI());
				}
				else {
					result.get(i).add(objval);	// for queries with OPTIONAL n can be null
				}
			}
			i++;
		}
		
		Object[][] array = new Object[result.size()][result.get(0).size()];
		for(int j=0; j< result.size(); j++)
			array[j] = result.get(j).toArray();
		final String[] columnName = new String[colcount]; // {"Subject", "Predicate", "Object"};
		int colnameindex = 0;
		if (bIncludeSubject) {
			columnName[colnameindex++] = "Subject";
		}
		if (bIncludePredicate) {
			columnName[colnameindex++] = "Predicate";
		}
		if (bIncludeObject) {
			columnName[colnameindex++] = "Object";
		}
		ResultSet rs = new ResultSet(columnName, array);
		endTrace();
		if (collectTimingInfo && rt != null) {
			long t2 = System.currentTimeMillis();
			rt.setMilliseconds(t2 - t1);
			timingInfo.add(rt);
		}
		return rs;
	}
	

	public List<Explanation> explain(String rulename) {
		startTrace();
		try {
			if (getReasonerOnlyWhenNeeded() != null) {
				getReasonerOnlyWhenNeeded().setDerivationLogging(true);
				List<Rule> rules = getReasonerOnlyWhenNeeded().getRules();
				for (int i = 0; i < rules.size(); i++) {
					String ruleName = rules.get(i).getName();
					if (ruleName != null && ruleName.equals(rulename)) {
						return explainRule(rules.get(i), null);
					}
				}
				List<Explanation> explanations = new ArrayList<Explanation>();
				Explanation expl = new Explanation(null, "Failed to get explanation for rule '" + rulename + "'. Rule not in loaded rule set.");
				explanations.add(expl);
				endTrace();
				return explanations;
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	protected List<Explanation> explainRule(Rule rule, List<Explanation> explains) {
		if (explains == null) {
			explains = new ArrayList<Explanation>();
		}
		ClauseEntry[] premises = rule.getBody();
		int startingIndex = (explains.size() > 0) ? explains.size() : 0;
		int testingThruPremise = 0;	// start with first premise only
		int resultsFound = 0;
		List<String> premisesAsStrings = new ArrayList<String>();
		do {
			String sparqlSelect = "select ";
			String filterString = null;
			List<String> selectVars = new ArrayList<String>();
			String sparqlWhere = " where {";
			int whereCnt = 0;
			for (int pi = 0; premises != null && pi <= testingThruPremise && pi < premises.length; pi++) {
				if (premises[pi] instanceof TriplePattern) {
					if (whereCnt++ > 0) {
						sparqlWhere += " . ";
					}
					String tripleStr = generateTripleStringWithVars(selectVars, (TriplePattern)premises[pi]);
					sparqlWhere += tripleStr;
					if (pi == testingThruPremise) {
						GraphPatternElement gpe = tripleElementFromTriplePattern((TriplePattern)premises[pi]);
						Explanation exp = new Explanation(gpe);
						exp.setPatternPrefix("Rule " + rule.getName() + ": Premise " + (pi + 1) + " of " + premises.length + ": ");
						explains.add(exp);
					}
				}
				else if (premises[pi] instanceof Functor) {
					String functorName = ((Functor)premises[pi]).getName();
					com.hp.hpl.jena.graph.Node[] args = ((Functor)premises[pi]).getArgs();
					String functorMsg = functorName + "(";
					for (int ai = 0; args != null && ai < args.length; ai++) {
						functorMsg += nodeShortString(args[ai]) + (ai < (args.length - 1) ? ", " : "");
					}
					functorMsg += ")";
					int existingFilterStringLength = (filterString != null) ? filterString.length() : 0;
					filterString = functorToFilter((Functor)premises[pi], filterString);
					if (pi == testingThruPremise) {
						BuiltinElement be = new BuiltinElement();
						be.setFuncName(functorName);
						for (int ai = 0; ai < args.length; ai++) {
							be.addArgument(graphNodeToSadlNode(args[ai]));							
						}
						Explanation exp = new Explanation(be);
						if (filterString != null && filterString.length() > existingFilterStringLength) {
							exp.setPatternPrefix("Rule " + rule.getName() + ": Premise " + (pi + 1) + " of " + premises.length + ": ");
						}
						else {
							exp.setPatternPrefix("Rule " + rule.getName() + ": Premise " + (pi + 1) + " of " + premises.length + " ignored:");
						}
						explains.add(exp);
					}
				}
				else {
					if (pi == testingThruPremise) {
						Explanation exp = new Explanation(null, "Unhandled premise type: " + premises[pi].getClass().getCanonicalName());
						exp.setPatternPrefix("Rule " + rule.getName() + ": Premise " + (pi + 1) + " of " + premises.length + ": ");
						explains.add(exp);
					}
				}
			}
			if (selectVars != null && selectVars.size() > 0) {
				for (int vi = 0; vi < selectVars.size(); vi++) {
					sparqlSelect += selectVars.get(vi) + " ";
				}
				String q = sparqlSelect + sparqlWhere + (filterString != null ? (" . FILTER(" + filterString + ")") : "") + "}";
				try {
					ResultSet rs = processRuleQuery(rule, premisesAsStrings, q);
					if (rs == null || rs.getRowCount() == 0) {
						resultsFound = 0;
						List<String> explanations = new ArrayList<String>();
						explanations.add("Premises through " + (testingThruPremise + 1) + " had no matches.");
						explanations.add("(SPARQL Query equivalent: " + q + ")");
						explains.get(startingIndex + testingThruPremise).setExplanations(explanations);
					}
					else {
						resultsFound = rs.getRowCount();
						List<String> explanations = new ArrayList<String>();
						explanations.add("Premises through " + (testingThruPremise + 1) + " had " + resultsFound + " matches.");
						explanations.add("(SPARQL Query: " + q + ")");
						String varNames = "";
						String[] header = rs.getColumnNames();
						for (int i = 0; i < header.length; i++) {
							if (i > 0) varNames += ", ";
							varNames += header[i];
						}
						explanations.add(varNames);
						for (int i = 0; i < rs.getRowCount(); i++) {
							String rowStr = "";
							for (int j = 0; j < rs.getColumnCount(); j++) {
								if (j > 0) rowStr += ", ";
								rowStr += rs.getResultAt(i, j).toString();
							}
							explanations.add(rowStr);
						}
						explains.get(startingIndex + testingThruPremise).setExplanations(explanations);
					}
				} catch (QueryCancelledException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (QueryParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			else {
				ClauseEntry clause = premises[testingThruPremise];
				if (clause instanceof TriplePattern) {
					ExtendedIterator<Triple> eitr = infModel.getGraph().find(((TriplePattern)clause).getSubject(), 
								((TriplePattern)clause).getPredicate(),
								((TriplePattern)clause).getObject());
					if (eitr.hasNext()) {
						List<String> explanations = new ArrayList<String>();
						explanations.add("Premise " + (testingThruPremise + 1) + " has matches.");
						while (eitr.hasNext()) {
							explanations.add(eitr.next().toString());
							resultsFound++;
						}
						explains.get(startingIndex + testingThruPremise).setExplanations(explanations);
					}
					else {
						List<String> explanations = new ArrayList<String>();
						explanations.add("Premises through " + (testingThruPremise + 1) + " had no matches.");
						explains.get(startingIndex + testingThruPremise).setExplanations(explanations);
					}
				}
			}
			testingThruPremise++;
		} while (resultsFound > 0 && testingThruPremise < premises.length);
		return explains;		
	}

	protected String functorToFilter(Functor functor, String oldFilterString) {
		String filterString = null;
		String functorName = functor.getName();
		com.hp.hpl.jena.graph.Node[] args = functor.getArgs();
		if (functorName.equals("equal")) {
			if (filterString == null && args.length > 1) {
				filterString = nodeToFilterString(args[0]);
				filterString += " = ";
				filterString += nodeToFilterString(args[1]);
			}
		}
		else if (functorName.equals("notEqual")) {
			if (filterString == null && args.length > 1) {
				filterString = nodeToFilterString(args[0]);
				filterString += " != ";
				filterString += nodeToFilterString(args[1]);
			}
		}
		else if (functorName.equals("ge")) {
			if (filterString == null && args.length > 1) {
				filterString = nodeToFilterString(args[0]);
				filterString += " >= ";
				filterString += nodeToFilterString(args[1]);
			}
			
		}
		else if (functorName.equals("greaterThan")) {
			if (filterString == null && args.length > 1) {
				filterString = nodeToFilterString(args[0]);
				filterString += " > ";
				filterString += nodeToFilterString(args[1]);
			}
		}
		else if (functorName.equals("noValue")) {
			if (filterString == null && args.length > 1) {
				filterString = "NOT EXISTS { ";
				filterString += nodeToFilterString(args[0]);
				filterString += " ";
				filterString += nodeToFilterString(args[1]);
				filterString += " ";
				if (args.length == 3) {
					filterString += nodeToFilterString(args[2]) + " }";
				}
				else {
					// make up a new variable
					filterString += "?unspecified_value }";
				}
			}
		}
		if (filterString == null) {
			return oldFilterString;
		}
		else if (oldFilterString == null) {
			return filterString;
		}
		else {
			return oldFilterString + " && " + filterString;
		}
	}
	
	protected String nodeToFilterString(com.hp.hpl.jena.graph.Node node) {
		if (node instanceof Node_Literal) {
			return ((Node_Literal)node).getLiteralLexicalForm();
		}
		else if (node instanceof Node_URI) {
			return "<" + ((Node_URI)node).getURI() + ">";
		}
		else if (node.getName() != null) {
			return node.getName();
		}
		return node.toString();
	}

	/**
	 * Convert a Jena graph TriplePattern to a SADL model TripleElement
	 * @param tp
	 * @return
	 */
	protected TripleElement tripleElementFromTriplePattern(TriplePattern tp) {
		TripleElement te = new TripleElement();
		te.setSubject(graphNodeToSadlNode(tp.asTriple().getSubject()));
		te.setPredicate(graphNodeToSadlNode(tp.asTriple().getPredicate()));
		te.setObject(graphNodeToSadlNode(tp.asTriple().getObject()));
		return te;
	}
	
	/**
	 * Convert a Jena graph Node to a SADL model Node
	 * @param node
	 * @return
	 */
	protected Node graphNodeToSadlNode(com.hp.hpl.jena.graph.Node node) {
		if (node instanceof Node_Variable) {
			return new VariableNode(((Node_Variable)node).getName().substring(1));
		}
		else if (node instanceof Node_URI) {
			return new NamedNode(((Node_URI)node).getURI());
		}
		else if (node instanceof Node_Literal){
			com.ge.research.sadl.model.gp.Literal lit = new com.ge.research.sadl.model.gp.Literal();
			lit.setValue(((Node_Literal)node).getLiteral().getValue());
			return lit;
		}
		else {
			return new NamedNode(node.toString());
		}
	}

	protected String tripleShortString(TriplePattern pattern) {
		String tripleStr = nodeShortString(pattern.getSubject()) + " ";
		tripleStr += nodeShortString(pattern.getPredicate()) + " ";
		tripleStr += nodeShortString(pattern.getObject()) + " ";
		return tripleStr;
	}
	
	protected String nodeShortString(com.hp.hpl.jena.graph.Node n) {
		if (n instanceof Node_RuleVariable) {
			return((Node_RuleVariable)n).toString();
		}
		else if (n instanceof Node_URI) {
			return n.getLocalName();
		}
		else {
			return "<" + n.toString() + "> ";
		}
	}

	protected String generateTripleStringWithVars(List<String> selectVars, TriplePattern pattern) {
		String tripleStr = "";
		com.hp.hpl.jena.graph.Node s = pattern.getSubject();
		if (s instanceof Node_RuleVariable) {
			String vn = ((Node_RuleVariable)s).toString();
			if (!selectVars.contains(vn)) {
				selectVars.add(vn);
			}
			tripleStr += vn + " ";
		}
		else {
			tripleStr += "<" + s.toString() + "> ";
		}
		com.hp.hpl.jena.graph.Node p = pattern.getPredicate();
		if (p instanceof Node_RuleVariable) {
			String vn = ((Node_RuleVariable)p).toString();
			if (!selectVars.contains(vn)) {
				selectVars.add(vn);
			}
			tripleStr += vn + " ";
		}
		else {
			tripleStr += "<" + p.toString() + "> ";
		}
		com.hp.hpl.jena.graph.Node o = pattern.getObject();
		if (o instanceof Node_RuleVariable) {
			String vn = ((Node_RuleVariable)o).toString();
			if (!selectVars.contains(vn)) {
				selectVars.add(vn);
			}
			tripleStr += vn + " ";
		}
		else if (o instanceof Node_URI){
			tripleStr += "<" + o.toString() + "> ";
		}
		else if (o instanceof Node_Literal){
			Object objVal = ((Node_Literal)o).getLiteralValue();
			tripleStr += JenaTranslatorPlugin.literalValueToString(objVal, TranslationTarget.QUERY_TRIPLE);
		}
		return tripleStr;
	}

	protected ResultSet processRuleQuery(com.hp.hpl.jena.reasoner.rulesys.Rule rule, List<String> premisesAsStrings, String q) throws QueryParseException, QueryCancelledException, InvalidNameException, ConfigurationException {
		logger.debug("Explanation executing query: " + q);
		q = prepareQuery(q);
		ResultSet rs = ask(q);
		if (rs != null) {
			int numResults = rs.getRowCount();
			String[] headers = rs.getColumnNames();
			String headerStr = "          ";
			for (int hi = 0; hi < headers.length; hi++) {
				if (hi > 0) headerStr += ", ";
				headerStr += "?" + headers[hi].toString();
			}
			for (int row = 0; row < numResults; row++) {
				String rowStr = "          ";
				for (int col = 0; col < rs.getColumnCount(); col++) {
					if (col > 0) rowStr += ", ";
					Object o = rs.getResultAt(row, col);
					if (o instanceof com.hp.hpl.jena.graph.Node) {
						rowStr += nodeShortString((com.hp.hpl.jena.graph.Node)o);
					}
					else if (o instanceof String && ((String)o).indexOf('#') > 0) {
						rowStr += ((String)o).substring(((String)o).indexOf('#') + 1);
					}
					else {
						rowStr += o.toString();
					}
				}
			}
		}
		return rs;
	}


	public List<Explanation> explain(List<GraphPatternElement> patterns) {
		startTrace();
		try {
			if (getReasonerOnlyWhenNeeded() != null) {
				getReasonerOnlyWhenNeeded().setDerivationLogging(true);
				prepareInfModel();
				List<Explanation> explanations = new ArrayList<Explanation>();
				for (int i = 0; patterns != null && i < patterns.size(); i++) {
					GraphPatternElement gpe = patterns.get(i);
					explanations = explainGraphPatternElement(explanations, gpe);
				}
				endTrace();
				return explanations;
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	protected List<Explanation> explainGraphPatternElement(List<Explanation> explanations, GraphPatternElement gpe) {
		if (gpe instanceof Junction) {
			explanations = explainGraphPatternElement(explanations, (GraphPatternElement) ((Junction)gpe).getLhs());
			explanations = explainGraphPatternElement(explanations, (GraphPatternElement) ((Junction)gpe).getRhs());
		}
		else if (gpe instanceof TripleElement) {
			Resource subj = null;
			Property prop = null;
			RDFNode obj = null;
			Node sn = ((TripleElement)gpe).getSubject();
			Node pn = ((TripleElement)gpe).getPredicate();
			Node on = ((TripleElement)gpe).getObject();
			
			if (sn instanceof NamedNode) {
				if(((NamedNode)sn).getNodeType() != null && ((NamedNode)sn).getNodeType().equals(NodeType.InstanceNode)) {
					subj = infModel.getResource(((NamedNode)sn).toFullyQualifiedString());
				}
			}
			if (pn instanceof RDFTypeNode) {
				prop = RDF.type;
			}
			else if (pn instanceof NamedNode) {
				if(((NamedNode)pn).getNodeType() != null && 
						(((NamedNode)pn).getNodeType().equals(NodeType.PropertyNode) ||
								((NamedNode)pn).getNodeType().equals(NodeType.ObjectProperty) ||
								((NamedNode)pn).getNodeType().equals(NodeType.DataTypeProperty))) {	
					prop = schemaModel.getOntProperty(((NamedNode)pn).toFullyQualifiedString());
					if (prop == null) {
						prop = infModel.getProperty(((NamedNode)pn).toFullyQualifiedString());
					}
				}
			}
			if (on instanceof NamedNode) {
				if(((NamedNode)on).getNodeType() != null && !((NamedNode)on).getNodeType().equals(NodeType.VariableNode)) {  // any type except variable
					obj = infModel.getResource(((NamedNode)on).toFullyQualifiedString());
				}
			}
			else if (on instanceof com.ge.research.sadl.model.gp.Literal){
				if (prop.canAs(OntProperty.class)) {
					try {
						obj = SadlUtils.getLiteralMatchingDataPropertyRange(schemaModel, prop.as(OntProperty.class), ((com.ge.research.sadl.model.gp.Literal)on).getValue());
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				if (obj == null) {
					obj = schemaModel.createTypedLiteral(((com.ge.research.sadl.model.gp.Literal)on).getValue());
				}
			}
			else {
				// ok to not have a value
			}
			StmtIterator stI = this.infModel.listStatements(subj, prop, obj);
			if (stI.hasNext()) {
				while (stI.hasNext()) {
					Statement stmt = stI.nextStatement();
					Iterator<Derivation> itr =  getDerivation(stmt.asTriple());
					TripleElement tpl = null;
					if (subj == null || prop == null || obj == null) {
						tpl = new TripleElement();
						NamedNode snn = new NamedNode(stmt.getSubject().toString());
						snn.setNamespace(stmt.getSubject().getNameSpace());
						tpl.setSubject(snn);
						NamedNode pnn = new NamedNode(stmt.getPredicate().getLocalName());
						pnn.setNamespace(stmt.getPredicate().getNameSpace());
						tpl.setPredicate(pnn);
						RDFNode onode = stmt.getObject();
						if (onode instanceof Resource) {
							NamedNode onn = new NamedNode(((Resource)onode).toString());
							onn.setNamespace(((Resource)onode).getNameSpace());
							tpl.setObject(onn);
						}
						else if (onode instanceof Literal){
							com.ge.research.sadl.model.gp.Literal ol = new com.ge.research.sadl.model.gp.Literal();
							ol.setValue(((Literal)onode).getValue());
							tpl.setObject(ol);
						}
					}
					else {
						tpl = (TripleElement) gpe;
					}
					if (itr != null && itr.hasNext()) {
						HashMap<String, Derivation> dvs = new HashMap<String, Derivation>();
						while (itr.hasNext()) {
							Derivation dv = itr.next();
							dvs.put(dv.toString(), dv);
						}
						Iterator<Derivation> ditr = dvs.values().iterator();
						while (ditr.hasNext()) {
							Derivation dv = ditr.next();
							Explanation expl = new Explanation(tpl, deepDerivationTrace(dv, true, 4, 0, true));
							expl.setPatternPrefix("Derivation of ");
							explanations.add(expl);
						}
					}
					else {			
						Explanation expl = new Explanation(tpl, "Statement is true in model but no derivation available.");
						expl.setPatternPrefix("Derivation of ");
						explanations.add(expl);
					}
				}
			}
			else {
				// no matches: look for rules that might infer the desired statement
				List<Rule> matchingRules = findRulesInferringStatement(sn, pn,on);
				for (int j = 0; matchingRules != null && j < matchingRules.size(); j++) {
					Explanation expl = new Explanation(gpe, "Statement not found but might be inferred by rule '" + matchingRules.get(j).getName() + "'.\n");
					expl.setPatternPrefix("Possible inference of ");
					explanations.add(expl);
					explanations = explainRule(matchingRules.get(j), explanations);
				}
			}
		}
		return explanations;
	}

	protected List<Rule> findRulesInferringStatement(Node sn, Node pn, Node on) {
		String snUri = null;
		// if the subject is a variable leave snUri null
		if (sn instanceof NamedNode && !((NamedNode)sn).getNodeType().equals(NodeType.VariableNode)) {
			snUri = ((NamedNode)sn).toFullyQualifiedString();
		}
		String pnUri = null;
		// if the predicate is a variable leave pnUri null
		if (pn instanceof NamedNode && !((NamedNode)pn).getNodeType().equals(NodeType.VariableNode)) {
			pnUri = ((NamedNode)pn).toFullyQualifiedString();
		}
		String onUri = null;
		// if the object is a variable leave onUri null
		if (on instanceof NamedNode && !((NamedNode)on).getNodeType().equals(NodeType.VariableNode)) {
			onUri = ((NamedNode)on).toFullyQualifiedString();
		}
		List<Rule> allRules;
		try {
			if (getReasonerOnlyWhenNeeded() != null) {
				allRules = getReasonerOnlyWhenNeeded().getRules();
				List<Rule> matchingRules = null;
				for (int i = 0; allRules != null && i < allRules.size(); i++) {
					Rule rl = allRules.get(i);
					ClauseEntry[] hdelmts = rl.getHead();
					for (int j = 0; hdelmts != null && j < hdelmts.length; j++) {
						ClauseEntry cls = hdelmts[j];
						if (cls instanceof TriplePattern) {
							// at this time we only consider triple patterns
							com.hp.hpl.jena.graph.Node sjn = ((TriplePattern)cls).getSubject();
							com.hp.hpl.jena.graph.Node pjn = ((TriplePattern)cls).getPredicate();
							com.hp.hpl.jena.graph.Node ojn = ((TriplePattern)cls).getObject();
							
							if ((snUri != null && sjn instanceof Node_URI && ((Node_URI)sjn).getURI().equals(snUri)) ||
									sjn instanceof Node_Variable || snUri == null) {
								// if the subject is an exact match or if the rule triple pattern subject is a variable or 
								//	the subject to be matched is a variable
								if ((pnUri != null && pjn instanceof Node_URI && ((Node_URI)pjn).getURI().equals(pnUri)) ||
									(pnUri == null && pjn instanceof Node_Variable)) {
									// if the predicate is an exact match or if the pattern and rule triple are both variables
									if ((onUri == null) ||
											(pnUri != null && ojn instanceof Node_URI && ((Node_URI)ojn).getURI().equals(onUri))) {
										// if the pattern object to be matched is a variable or
										//	it is an exact match or
										if (matchingRules == null) {
											matchingRules = new ArrayList<Rule>();
										}
										matchingRules.add(rl);
										break;
									}
								}
							}
						}
					}
				}
				return ((matchingRules != null && matchingRules.size() > 0) ? matchingRules : null);
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	protected Iterator<Derivation> getDerivation(Triple match) {
		return ((InfGraph) infModel.getGraph()).getDerivation(match);
	}

	protected StringBuilder indent(StringBuilder sb, int level) {
		for (int i = 0; i < level; i++) {
			sb.append(" ");
		}
		return sb;
	}
	
	protected String deepDerivationTrace(Derivation d, boolean bindings, int indent, int level, boolean includeValue) {
		if (d instanceof RuleDerivation) {
			StringBuilder sb = new StringBuilder();
			if (bindings) {
				sb = indent(sb, indent + 4);
				sb.append((level > 0 ? "which " : "") + "was concluded by: " + d.toString() + "\n");
			}
			int margin = indent + 12;
			List<Triple> matches = ((RuleDerivation) d).getMatches();
			if (matches != null && matches.size() > 0) {
				sb = indent(sb, indent + 8);
				sb.append("based on matching conditions:\n");
				for (int i = 0; i < matches.size(); i++) {
					Triple match = matches.get(i);
					Iterator<Derivation> derivations = getDerivation(match);
				    if (derivations == null || !derivations.hasNext()) {
				        sb = indent(sb, indent + 12);
				        if (match == null) {
				            // A primitive
				        	ClauseEntry term = ((RuleDerivation) d).getRule().getBodyElement(i);
				        	if (term instanceof Functor) {
				        		sb.append(((Functor)term).getName() + "(");
				        		com.hp.hpl.jena.graph.Node[] args = ((Functor)term).getArgs();
				        		for (int j = 0; j < args.length; j++) {
				        			String nstr;
				        			if (args[j].isURI()) {
				        				nstr = args[j].getLocalName();
				        			}
				        			else {
				        				nstr = args[j].toString();
				        			}
				        			if (nstr.startsWith("?")) {
				        				nstr = nstr.substring(1);
				        			}
				        			if (j > 0) {
				        				sb.append(", ");
				        			}
				        			sb.append(nstr);
				        		}
				        		sb.append(")\n");
				        	} else {
				        		sb.append("call to built in");
				        	}
				        } else {
				        	sb.append(tripleToString(match));
				        	sb.append("\n");
				        }
				    } else {
				        sb = indent(sb, indent + 12);
			        	sb.append(tripleToString(match));
				        if (derivations.hasNext()) {
				        	HashMap<String, Derivation> dvs = new HashMap<String, Derivation>();
					    	while (derivations.hasNext()) {
						    	Derivation derivation = derivations.next();
						    	dvs.put(derivation.toString(), derivation);
					    	}
					    	Iterator<Derivation> ditr = dvs.values().iterator();
					    	while (ditr.hasNext()) {
						    	Derivation derivation = ditr.next();
//					    	if (!derivationAlreadyShown(derivation, seen, out, margin)) {
	//					    	if (seen != null && derivation instanceof RuleDerivation && seen.contains(derivation)) {
	//					    		PrintUtil.printIndent(out, margin + 4);
	////					    		out.println(tripleToString(match) + " - already shown");
	//					    		out.println("- explanation already shown");
	//					    	} else {
	//					    		if (seen != null && !seen.contains(derivation)) {
	//						    		if (seen == null) {
	//						    			seen = new HashSet();
	//						    		}
	//						    		seen.add(derivation);
	//					    		}
						    		sb.append("\n");
						    		sb.append(deepDerivationTrace(derivation, bindings, margin, level + 4, includeValue));
	//					    	}
					    	}
				        }
				    }
				}
			}
			return sb.toString();
		}
		else {
			return d.toString();
		}
	}
	
	protected String tripleToString(Triple t) {
		StringBuffer sb = new StringBuffer();
		sb.append(tripleSubjectToString(t.getSubject()));
		sb.append(" ");
		sb.append(t.getPredicate().getLocalName());
		sb.append(" ");
		com.hp.hpl.jena.graph.Node n = t.getObject();
		sb.append(tripleValueToString(n));
		return sb.toString();
	}

	protected String tripleSubjectToString(com.hp.hpl.jena.graph.Node s) {
		if (s.isURI()) {
			return s.getLocalName();
		}
		else {
			return s.toString();
		}
	}

	public String tripleValueToString(com.hp.hpl.jena.graph.Node n) {
		StringBuffer sb = new StringBuffer();
		if (n.isLiteral()) {
			Object val = n.getLiteralValue();
			if (val instanceof String) {
				sb.append("\"");
				sb.append(n.getLiteralLexicalForm());
				sb.append("\"");
			}
			else {
				sb.append(n.getLiteralLexicalForm());
			}
		}
		else if (n.isURI()) {
			sb.append(n.getLocalName());
		}
		else {
			sb.append(n.toString());
		}
		return sb.toString();
	}
		
	protected void prepareInfModel() throws ConfigurationException {
		getReasonerOnlyWhenNeeded();
		if (infModel == null || newInputFlag == true) {
			if (schemaModelIsCachedInferredModel) {
				infModel = schemaModel;
			}
			else {
				long t1 = System.currentTimeMillis();
				generateTboxModelWithSpec();
				logger.debug("In prepareInfModel, modelSpec: "+modelSpec.toString());
				logger.debug("In prepareInfModel, reasoner rule count: "+getReasonerOnlyWhenNeeded().getRules().size());
				infModel = ModelFactory.createInfModel(reasoner, tboxModelWithSpec);
//		        InfGraph graph = reasoner.bind(tboxModelWithSpec.getGraph());
//		        infModel = new InfModelImpl(graph);

				synchronized(ReasonerFamily) {
					infModel.size();	// this forces instantiation of the inference model
					if (collectTimingInfo) {
						long t2 = System.currentTimeMillis();
						timingInfo.add(new ReasonerTiming(TIMING_PREPARE_INFMODEL, "prepare inference model", t2 - t1));
					}
				}
			}
		}
		else if(newInputFlag == true) {
			logger.debug("In prepareInfModel, reusing infModel with newInputFlag is true");
			if (infModel instanceof InfModel) {
				synchronized(ReasonerFamily) {
					logger.debug("In prepareInfModel, reusing infModel, rebinding existing infModel");
					((InfModel) infModel).rebind();
					infModel.size();	// force re-instantiation?
				}
			}
		} else {
			logger.debug("In prepareInfModel, reusing infModel without any changes, newInputFlag is false");
		}
		newInputFlag = false;			
	}

	private void generateTboxModelWithSpec() {
		if (schemaModelIsCachedInferredModel) {
			// don't need a model spec, new OntModel; use the model on the TDB Dataset directly
			if (dataModel != null) {
				tboxModelWithSpec = dataModel;
			}
			else {
				tboxModelWithSpec = schemaModel;
			}
		}
		else {
			// use the data to create a new OntModel with the specified model spec
			if (dataModel != null) {
				tboxModelWithSpec = ModelFactory.createOntologyModel(modelSpec, dataModel);
			}
			else {
				tboxModelWithSpec = ModelFactory.createOntologyModel(modelSpec, schemaModel);
			}

		}
	}

	protected void loadRules(OntModel m, String modelName) {
		try {	
			String altUrl = configurationMgr.getAltUrlFromPublicUri(modelName);

			if (altUrl == null) {
				throw new ConfigurationException("Model URI '" + modelName + "' not found in mappings!");
			}

			if (altUrl != null) {
				String altFN = new SadlUtils().fileUrlToFileName(altUrl);
				String rulefn = altFN.substring(0, altFN.lastIndexOf(".")) + ".rules";
				if (!ruleFilesLoaded.contains(rulefn)) {
					if (loadRules(rulefn)) {
						ruleFilesLoaded.add(rulefn);
					}
				}
			}
			
			if (imports != null) {
				for (int i = 0; i < imports.size(); i++) {
					ImportMapping impMap = imports.get(i);
					String impUri = impMap.getPublicURI();
					altUrl = impMap.getActualURL();
					if (altUrl == null) {
						altUrl = configurationMgr.getAltUrlFromPublicUri(impUri);
					}
					if (altUrl != null) {
						String rulefn = altUrl.substring(0, altUrl.lastIndexOf(".")) + ".rules";
						if (!ruleFilesLoaded.contains(rulefn)) {
							if (loadRules(rulefn)) {
								ruleFilesLoaded.add(rulefn);
							}
						}
					}
				}
			}
		} catch (Throwable e) {
			// TODO Auto-generated catch block
//			e.printStackTrace();
			addError(new ModelError(e.getMessage(), ErrorType.ERROR));
		}
	}
	
	protected static synchronized boolean addBuiltin(String name, String fullyQualifiedClassName, IConfigurationManager configMgr) {
		name = name.trim();
		fullyQualifiedClassName = fullyQualifiedClassName.trim();
		if (fullyQualifiedClassName.substring(0, fullyQualifiedClassName.lastIndexOf('.')).equals(Product.class.getPackage().toString())) {
			// don't need to register Jena builtins
			return false;
		}
		
		try {
//			Class<?> builtinCls = Class.forName(fullyQualifiedClassName, true, Thread.currentThread().getContextClassLoader());
			Class<?> builtinCls = Class.forName(fullyQualifiedClassName);
			Constructor<?> c = builtinCls.getConstructor();
			Object javaInstance = c.newInstance();
			if (javaInstance instanceof Builtin) {
				BuiltinRegistry.theRegistry.register((Builtin) javaInstance);
				if (javaInstance instanceof CancellableBuiltin) {
					((CancellableBuiltin)javaInstance).setConfigMgr(configMgr);
				}
			}
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		}
		return true;
	}


	public boolean configure(ConfigurationItem configItem) {
		String category = configItem.getLeafCategory();
		if (category == null) {
			return false;
		}
		List<NameValuePair> nvpList = configItem.getNameValuePairs();

		if (IConfigurationManager.BuiltinCategory.equals(category)) {
			Object nameObj = configItem.getNamedValue("name");
			if (nameObj != null) {
				String name = nameObj.toString();
				Object clssObj = configItem.getNamedValue("class");
				if (clssObj != null) {
					String clss = clssObj.toString();
					return addBuiltin(name, clss, configurationMgr);
				}
			}
			return false;
		}
		else {
			if (configuration == null) {
				configuration = new HashMap<String, Object>();
			}
			for (int j = 0; nvpList != null && j < nvpList.size(); j++) {
				NameValuePair nvp = nvpList.get(j);
				String name = nvp.getName();
				Object value = nvp.getValue();
				configuration.put(name, value);
			}
			return true;
		}
	}
	
	/**
	 * This is specific to addTriple
	 * @param sub
	 * @param pred
	 * @param obj
	 * @return
	 * @throws InvalidNameException
	 * @throws MalformedURLException 
	 */
	protected Statement prepareStatement(String sub, String pred, String obj) throws InvalidNameException, MalformedURLException {
		RDFNode[] spo = prepareSubjectPredicateObject(sub, pred, obj);
		if (spo != null && spo.length == 3) {
			Resource r = (Resource) spo[0];
			Property p = (Property) spo[1];
			RDFNode n = spo[2];
			Statement s = null;
			if (r == null) {
				throw new InvalidNameException("Not able to resolve triple subject '" + sub + "'.");
			}
			else if (p == null) {
				throw new InvalidNameException("Not able to resolve triple predicate '" + pred + "'.");			
			}
			else if (n == null) {
				if (schemaModel != null) {
					schemaModel.write(System.out, "N-TRIPLE");
				}
				if (dataModel != null) {
					dataModel.write(System.out, "N-TRIPLE");
				}
				throw new InvalidNameException("Not able to resolve triple object '" + obj + "'.");
			}
			else {
				s = ResourceFactory.createStatement(r, p, n);
			}
			return s;
		}
		throw new InvalidNameException("Unexpected error resolving triple <" + sub + ", " + pred + ", " + obj + ">");
	}

	private RDFNode[] prepareSubjectPredicateObject(String sub, String pred,
			String obj) throws InvalidNameException, MalformedURLException {
		Property p = null;
		if (pred != null) {
			p = ResourceFactory.createProperty(pred);
		}
		
		RDFNode n = null;
		if (obj != null) {
			if (obj.startsWith("http://") && obj.contains("#")) {
				// this looks like a URI -- this would fail if there were a string value assigned that looked like a URI...
				n = getOntResource(obj);
				if (n == null) {
					throw new InvalidNameException("Resource name '" + obj + "' not found in models.");
				}
			}
			else {
				if (p.equals(RDF.type)) {
					// this is an invalid object
					throw new InvalidNameException("'" + obj + "' is not a valid class name.");
				}
				else if (p.canAs(OntProperty.class) && ((OntProperty)p.as(OntProperty.class)).isObjectProperty()) {
					// this is an invalid object
					throw new InvalidNameException("'" + obj + "' is not a valid value for property '" + pred + "'");
				}
				Object objVal = xsdStringToObject(obj);
				if (objVal != null) {
					n = schemaModel.createTypedLiteral(objVal);
				}
				else {
					n = ResourceFactory.createResource(obj);
				}
			}
		}
		
		Resource r = null;
		if (sub != null) {
			r = getOntResource(sub);
			if (r == null) {
				if (dataModel != null && p != null && p.equals(RDF.type) && n.canAs(OntClass.class)) {
					r = dataModel.createIndividual(sub, n.as(OntClass.class));
				}
				else {
					r = ResourceFactory.createResource(sub);
				}
			}
		}
		RDFNode[] spo = new RDFNode[3];
		spo[0] = r;
		spo[1] = p;
		spo[2] = n;
		return spo;
	}

	private Resource getOntResource(String uri) throws MalformedURLException {
		Resource r = null;
		if (schemaModel.getOntClass(uri) != null || schemaModel.getIndividual(uri) != null) {
			r = schemaModel.getResource(uri);
		}
		else if (dataModel != null && (dataModel.getOntClass(uri) != null || dataModel.getIndividual(uri) != null)) {
			r = dataModel.getResource(uri);
		}
		if (r == null) {
			// look in imports
			if (imports != null) {
				for (int i = 0; i < imports.size(); i++) {
					ImportMapping im = imports.get(i);
					if (im != null) {
//						OntModel impModel = im.getModel();
//						if (impModel != null) {
//							r = impModel.getOntResource(uri);
//							if (r != null) {
//								break;
//							}
//						}
					}
				}
				// so we've failed to here; check to see if the t-box is stale (
				if (tboxLoadTime > 0) {
					SadlUtils su = new SadlUtils();
					for (int i = 0; i < imports.size(); i++) {
						ImportMapping im = imports.get(i);
						if (im != null) {
							String actualUrl = im.getActualURL();
							if (actualUrl.startsWith("file:")) {
								File impFile = new File(su.fileUrlToFileName(actualUrl));
								if (impFile.exists()) {
									if (impFile.lastModified() > tboxLoadTime) {
										// reload the file
//										OntModel m = im.getModel();
//										m.remove(m);
//										m.read(actualUrl);
									}
								}
							}
						}
					}
					for (int i = 0; i < imports.size(); i++) {
						ImportMapping im = imports.get(i);
						if (im != null) {
//							OntModel impModel = im.getModel();
//							if (impModel != null) {
//								r = impModel.getOntResource(uri);
//								if (r != null) {
//									break;
//								}
//							}
						}
					}
				}
			}
		}
		return r;
	}
	
	/**
	 * This method returns the category (name) of this specific reasoner.
	 * This is a "root" category name, which will have sub-categories.
	 */
	

	public String getConfigurationCategory() {
		return ReasonerCategory;
	}


	public Map<String, ConfigurationOption> getReasonerConfigurationOptions() {
		Map<String, ConfigurationOption> map = new HashMap<String, ConfigurationOption>();
		String[] categoryHierarchy = {ReasonerCategory};
		String[] ruleModeOptions = {GenericRuleReasoner.HYBRID.toString(), 
				GenericRuleReasoner.FORWARD.toString(), 
				GenericRuleReasoner.BACKWARD.toString(),
				GenericRuleReasoner.FORWARD_RETE.toString()};
		String[] modelSpecOptions = {OWL_MEM, OWL_MEM_RDFS, OWL_MEM_TRANS, OWL_MEM_RULE, OWL_MEM_MICRO_RULE, OWL_MEM_MINI_RULE,
				OWL_DL_MEM, OWL_DL_MEM_RDFS, OWL_DL_MEM_TRANS, OWL_DL_MEM_RULE, OWL_LITE_MEM, OWL_LITE_MEM_TRANS, OWL_LITE_MEM_RDFS,
				OWL_LITE_MEM_RULE, RDFS_MEM, RDFS_MEM_TRANS, RDFS_MEM_RDFS};
		String[] derivationOptions = {DERIVATION_NONE, DERIVATION_SHALLOW, DERIVATION_DEEP};
		
		map.put(pModelSpec, 
				new ConfigurationOption(categoryHierarchy, pModelSpec, "Jena ontology model specification",
						OWL_DL_MEM_RDFS, modelSpecOptions));
		map.put(pTimeOut, 
				new ConfigurationOption(categoryHierarchy, pTimeOut, "Query timeout (seconds, -1 for no limit)", "-1", null));
		map.put(pRuleMode, 
				new ConfigurationOption(categoryHierarchy, pRuleMode, "Jena reasoner mode", GenericRuleReasoner.HYBRID.toString(), ruleModeOptions ));
		map.put(pOWLTranslation, 
				new ConfigurationOption(categoryHierarchy, pOWLTranslation , "Translate some OWL constructs (intersection) to rules", false, booleanOptions));
		map.put(pTransitiveClosureCaching, 
				new ConfigurationOption(categoryHierarchy, pTransitiveClosureCaching  , "Cache transitive inferences to improve performance (may not work with all rules sets)", false, booleanOptions));		
		map.put(pDerivationLogging, 
				new ConfigurationOption(categoryHierarchy, pDerivationLogging, "Track and display derivations", DERIVATION_NONE, derivationOptions));
		map.put(pTrace, 
				new ConfigurationOption(categoryHierarchy, pTrace, "Log rule activity to startup console (run with -console option)", false, booleanOptions));
		map.put(pUseLuceneIndexer, 
				new ConfigurationOption(categoryHierarchy, pUseLuceneIndexer, "Use Custom Lucene Indexer", false, booleanOptions));
		return map;
	}

	protected String getDerivationLevel() {
		return getStringConfigurationValue(null, pDerivationLogging, DERIVATION_NONE);
	}

	protected OntModelSpec getModelSpec(List<ConfigurationItem> preferences) {
		Object modelSpecID = findPreference(preferences, pModelSpec);
		if (modelSpecID != null) {
			configure(findConfigurationItem(preferences, pModelSpec));
		}
		if (modelSpecID == null && configuration != null) {
			modelSpecID = configuration.get(pModelSpec);
		}
		if (modelSpecID != null) {
			OntModelSpec ms = new OntModelSpec(getModelSpec(modelSpecID.toString()));
			ms.setDocumentManager(configurationMgr.getJenaDocumentMgr());
			return ms;
		}
		OntModelSpec ms = new OntModelSpec(OntModelSpec.OWL_MEM);
		ms.setDocumentManager(configurationMgr.getJenaDocumentMgr());
		return ms;
	}

	protected OntModelSpec getModelSpec(String modelSpecID) {
		if (modelSpecID.equals(OWL_DL_MEM)) {
			return OntModelSpec.OWL_DL_MEM;
		}
		else if (modelSpecID.equals(OWL_DL_MEM_RDFS)) {
			return OntModelSpec.OWL_DL_MEM_RDFS_INF;
		}
		else if (modelSpecID.equals(OWL_DL_MEM_RULE)) {
			return OntModelSpec.OWL_DL_MEM_RULE_INF;
		}
		else if (modelSpecID.equals(OWL_DL_MEM_TRANS)) {
			return OntModelSpec.OWL_DL_MEM_TRANS_INF;
		}
		else if (modelSpecID.equals(OWL_LITE_MEM)) {
			return OntModelSpec.OWL_LITE_MEM;
		}
		else if (modelSpecID.equals(OWL_LITE_MEM_RDFS)) {
			return OntModelSpec.OWL_LITE_MEM_RDFS_INF;
		}
		else if (modelSpecID.equals(OWL_LITE_MEM_RULE)) {
			return OntModelSpec.OWL_LITE_MEM_RULES_INF;
		}
		else if (modelSpecID.equals(OWL_LITE_MEM_TRANS)) {
			return OntModelSpec.OWL_LITE_MEM_TRANS_INF;
		}
		else if (modelSpecID.equals(OWL_MEM)) {
			return OntModelSpec.OWL_MEM;
		}
		else if (modelSpecID.equals(OWL_MEM_MICRO_RULE)) {
			return OntModelSpec.OWL_MEM_MICRO_RULE_INF;
		}
		else if (modelSpecID.equals(OWL_MEM_MINI_RULE)) {
			return OntModelSpec.OWL_MEM_MINI_RULE_INF;
		}
		else if (modelSpecID.equals(OWL_MEM_RDFS)) {
			return OntModelSpec.OWL_MEM_RDFS_INF;
		}
		else if (modelSpecID.equals(OWL_MEM_RULE)) {
			return OntModelSpec.OWL_MEM_RULE_INF;
		}
		else if (modelSpecID.equals(OWL_MEM_TRANS)) {
			return OntModelSpec.OWL_MEM_TRANS_INF;
		}
		else if (modelSpecID.equals(RDFS_MEM)) {
			return OntModelSpec.RDFS_MEM;
		}
		else if (modelSpecID.equals(RDFS_MEM_RDFS)) {
			return OntModelSpec.RDFS_MEM_RDFS_INF;
		}
		else if (modelSpecID.equals(RDFS_MEM_TRANS)) {
			return OntModelSpec.RDFS_MEM_TRANS_INF;
		}
		return OntModelSpec.OWL_MEM;
	}

	protected com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner.RuleMode getRuleMode(List<ConfigurationItem> preferences) throws ConfigurationException {
		Object ruleModeID = findPreference(preferences, pRuleMode);
		if (ruleModeID != null) {
			configure(findConfigurationItem(preferences, pRuleMode));
		}
		if (ruleModeID == null && configuration != null) {
			ruleModeID = configuration.get(pRuleMode);
		}
		if (ruleModeID != null) {
			return getRuleMode(ruleModeID.toString());
		}
		return GenericRuleReasoner.HYBRID;
	}
	
	
	protected com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner.RuleMode getRuleMode(String ruleModeID) throws ConfigurationException {
		if (ruleModeID.equals(GenericRuleReasoner.BACKWARD.toString())) {
			return GenericRuleReasoner.BACKWARD;
		}
		else if (ruleModeID.equals(GenericRuleReasoner.FORWARD.toString())) {
			return GenericRuleReasoner.FORWARD;
		} 
		else if (ruleModeID.equals(GenericRuleReasoner.FORWARD_RETE.toString())) {
			return GenericRuleReasoner.FORWARD_RETE;
		}
		else if (ruleModeID.equals(GenericRuleReasoner.HYBRID.toString())) {
			return GenericRuleReasoner.HYBRID;
		}
		else {
			throw new ConfigurationException("Invalid Jena Reasoner mode: " + ruleModeID);
		}				
	}


	public void enableExplanation(boolean bVal) {
		if (reasoner != null) {
			try {
				if (getReasonerOnlyWhenNeeded() != null) {
					getReasonerOnlyWhenNeeded().setDerivationLogging(bVal);
				}
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			explanationsEnabled = true;
		}
	}
	

	public boolean isExplanationEnabled() {
		return !getDerivationLevel().equals(DERIVATION_NONE);
	}


	public boolean saveInferredModel(String filename, String modelname, boolean deductionsOnly) throws FileNotFoundException {
		try {
			prepareInfModel();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		if (infModel != null) {
			OntModel m;
			if (deductionsOnly && infModel instanceof InfModel) {
				m = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null), ((InfModel) infModel).getDeductionsModel());
			}
			else {
				m = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null), infModel);
			}
			String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;	
		    FileOutputStream fps = new FileOutputStream(filename);
	        RDFWriter rdfw = m.getWriter(format);
	        rdfw.write(m, fps, modelname);
	        try {
				fps.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return false;
	}

// works 6/3/2014 12:48 p.m.
	public boolean reset() {
//		if (infModel != null) {
//			System.out.println("Before rebind, infModel size is: " + infModel.size());
//		}
		if (!initialized) {
			return false;
		}
//		if (dataModel != null) {
//			System.out.println("Before removeAll, dataModel size is: " + dataModel.size());
//			dataModel.getBaseModel().removeAll();
//			System.out.println("Before removeAll, tboxModelWithSpec size is: " + tboxModelWithSpec.size());
//			tboxModelWithSpec.removeAll();
//			System.out.println("After basemodel removeAll, dataModel size is: " + dataModel.size());
////			dataModel.removeAll();
////			System.out.println("After removeAll, dataModel size is: " + dataModel.size());
//		}
		infModel = null;
		dataModel = null;
		tboxModelWithSpec = null;
//		prepareInfModel();
		if (infModel != null && infModel instanceof InfModel) {
			((InfModel)infModel).rebind();
//			System.out.println("After rebind, infModel size is: " + infModel.size());
		}
		// what else do we need to do?
		return true;
	}
	
//	public boolean reset() {
//		if (infModel != null) {
//			System.out.println("On reset, infModel size is: " + infModel.size());
//		}
//		if (!initialized) {
//			return false;
//		}
//		if (dataModel != null) {
//			System.out.println("Before removeAll, dataModel size is: " + dataModel.size());
//			dataModel.removeAll();
//			System.out.println("After removeAll, dataModel size is: " + dataModel.size());
//		}
//		infModel = null;
//		dataModel = null;
////		prepareInfModel();
////		if (infModel != null) {
////			infModel.rebind();
////		}
//		// what else do we need to do?
//		return true;
//	}	

	public String objectValueToStringValue(Object objValue, String predicate) throws ConfigurationException {
		getReasonerOnlyWhenNeeded();
		if (schemaModel != null) {
			OntProperty pred = null;
			Property nonOntPred = null;
			if (predicate != null) {
				pred = schemaModel.getOntProperty(predicate);
				if (pred == null) {
					nonOntPred = schemaModel.getProperty(predicate);
					if (nonOntPred != null) {
						String msg = "Found predicate but it isn't an OntProperty";
						logger.debug(msg); addError(new ModelError(msg, ErrorType.ERROR));
					}
				}
			}
			
			RDFNode val = null;
			if (pred != null && pred.isDatatypeProperty()) {
				if (pred.getRange() != null) {
					try {
						val = SadlUtils.getLiteralMatchingDataPropertyRange(schemaModel, pred, objValue);
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				else {
					val = schemaModel.createTypedLiteral(objValue);
				}
			}
			else {
				if (pred == null && objValue instanceof String && !(((String)objValue).startsWith("http://" ) && ((String)objValue).contains("#"))) {
					val = schemaModel.createTypedLiteral(objValue);
				}
				else {
					val = schemaModel.getIndividual(objValue.toString());
				}
			}
			if (val != null) {
				return val.toString();
			}
		}
		return objValue.toString();
	}


	public String prepareQuery(String query) throws InvalidNameException, ConfigurationException {
		getReasonerOnlyWhenNeeded();
		OntModel model = null;
		if (dataModel != null) {
			model = dataModel;
		}
		else if (schemaModel != null) {
			model = schemaModel;
		}
		if (model != null) {
			ReasonerTiming rt = null;
			long t1 = 0L;
			if (collectTimingInfo) {
				rt = new ReasonerTiming(TIMING_PREPARE_QUERY, "prepare query (" + query + ")", 0);		// do this now to pick up query text before preparation
				t1 = System.currentTimeMillis();
			}
			if (configurationMgr != null) {
//				ITranslator translator = configurationMgr.getTranslatorForReasoner(ReasonerCategory);
//				if (translator == null) {
//					translator = configurationMgr.getTranslatorForReasoner(this);
//				}
				ITranslator translator = configurationMgr.getTranslatorForReasoner(this);
				if (translator != null) {
					translator.setConfigurationManager(configurationMgr);
					query = translator.prepareQuery(model, query);
					if (collectTimingInfo) {
						long t2 = System.currentTimeMillis();
						rt.setMilliseconds(t2 - t1);
						timingInfo.add(rt);
					}
				}
				else {
					throw new ConfigurationException("Unable to obtain a translator.");
				}
			}
			else {
				throw new ConfigurationException("No ConfigurationManager availalble.");
			}
		}
		return query;
	}


	public String parameterizeQuery(String query, List<Object> values) throws InvalidNameException, ConfigurationException {
		getReasonerOnlyWhenNeeded();
		OntModel model = null;
		if (dataModel != null) {
			model = dataModel;
		}
		else if (schemaModel != null) {
			model = schemaModel;
		}
		if (model != null) {
			query = handleNamedQueryByName(query);
			ReasonerTiming rt = null;
			long t1 = 0L;
			if (collectTimingInfo) {
				rt = new ReasonerTiming(TIMING_PREPARE_QUERY, "prepare query (" + query + ")", 0);		// do this now to pick up query text before preparation
				t1 = System.currentTimeMillis();
			}
			if (configurationMgr != null) {
//				ITranslator translator = configurationMgr.getTranslatorForReasoner(ReasonerCategory);
//				if (translator == null) {
//					translator = configurationMgr.getTranslatorForReasoner(this);
//				}
				ITranslator translator = configurationMgr.getTranslatorForReasoner(this);
				if (translator != null) {
					translator.setConfigurationManager(configurationMgr);
					query = translator.parameterizeQuery(model, query, values);
					if (collectTimingInfo) {
						long t2 = System.currentTimeMillis();
						rt.setMilliseconds(t2 - t1);
						timingInfo.add(rt);
					}
				}
				else {
					throw new ConfigurationException("Unable to obtain a translator.");
				}
			}
			else {
				throw new ConfigurationException("No ConfigurationManager availalble.");
			}
		}
		return query;
	}

	/**
	 * Method to talk a query which is the URI of a named query and return the query string.
	 * @param query
	 * @return
	 * @throws InvalidNameException
	 */
	private String handleNamedQueryByName(String query) throws InvalidNameException {
		OntModel model = null;
		if (dataModel != null) {
			model = dataModel;
		}
		else if (schemaModel != null) {
			model = schemaModel;
		}
		if (model != null) {
			try {
				if (SadlUtils.validateUri(query) == null) {
					Individual inst = model.getIndividual(query);
					if (inst != null) {
						RDFNode idb = inst.getPropertyValue(RDFS.isDefinedBy);
						if (idb instanceof Literal) {
							String qstr = ((Literal)idb).getValue().toString();
							if (qstr.contains("ask") || qstr.contains("select") || qstr.contains("construct") ||
									qstr.contains("insert") || qstr.contains("delete")) {
								query = qstr;
							}
							else {
								throw new InvalidNameException("'" + inst.getURI() + "' appears to be a named query or update but SPARQL string not found.");
							}
						}
					}
				}
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return query;
	}
	
	public String getReasonerFamily() {
		return ReasonerFamily;
	}


	public Class<?> getBuiltinClass() {
		return Builtin.class;
	}


	public BuiltinInfo getBuiltinInfo(Class<?> bcls) {
		try {
			Builtin binst = (Builtin) this.getClass().getClassLoader().loadClass(bcls.getCanonicalName()).newInstance();
			BuiltinInfo binfo = new BuiltinInfo(binst.getName(), bcls.getCanonicalName(), getReasonerFamily(), binst.getArgLength());
			if (binst instanceof TypedBaseBuiltin) {
				binfo.setSignature(((TypedBaseBuiltin)binst).getFunctionSignatureString());
				return binfo;
			}
			else {
				return binfo;
			}
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}				
		return null;
	}


	public List<BuiltinInfo> getImplicitBuiltins() {
		List<BuiltinInfo> implbltins = new ArrayList<BuiltinInfo>();
		String pkg = "com.hp.hpl.jena.reasoner.rulesys.builtins";
		String[] impbuiltinnames = {
		"AddOne", "Bound", "CountLiteralValues", "IsBNode", "IsDType",
		"IsLiteral", "ListContains", "ListEntry", "ListEqual", "ListLength", 
		"ListMapAsObject", "ListMapAsSubject", "ListNotContains", "ListNotEqual", 
		"NotBNode", "NotDType", "NotLiteral", "Now", "Regex", "StrConcat", "Table", 
		"TableAll", "Unbound", "UriConcat"};
		for (int i = 0; i < impbuiltinnames.length; i++) {
			String fqn = pkg + "." + impbuiltinnames[i];
			try {
				Class<?> builtinCls = Class.forName(fqn, true, Thread.currentThread().getContextClassLoader());
				Constructor<?> c = builtinCls.getConstructor();
				Object javaInstance = c.newInstance();
				if (javaInstance instanceof Builtin) {
					BuiltinInfo bi = new BuiltinInfo(((Builtin)javaInstance).getName(), fqn, getReasonerFamily(), ((Builtin)javaInstance).getArgLength());
					implbltins.add(bi);
				}
			} catch (ClassNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (NoSuchMethodException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InstantiationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return implbltins;
	}

	public String[] getBuiltinFunctions(){
		return new String[]{"addOne(decimal)decimal", 
							"bound(string)boolean", 
							"countLiteralValues(string,string)int", 
							"isBNode(string)boolean", 
							"isDType(string)boolean", 
							"isLiteral(string)boolean",
							"listContains(string,string)boolean", 
							"listEntry(string,int)string", 
							"listEqual(string,string)boolean", 
							"listLength(string)int", 
							"listMapAsObject(string,string,string)boolean", 
							"listMapAsSubject(string,string,string)boolean", 
							"listNotContains(string,string)boolean", 
							"listNotEqual(string,string)boolean", 
							"notBNode(string)boolean", 
							"notBType(string)boolean", 
							"notDType(string)boolean", 
							"notLiteral(string)boolean", 
							"now()dateTime", 
							"regex(string,string)string", 
							"strConcat(string,string)string", 
							"uriConcat(string,string)string",
							"unbound(string)string"};
	}
	
	@Override
	public List<FunctionSignature> getImplicitBuiltinSignatures() {
		List<FunctionSignature> fsList = new ArrayList<FunctionSignature>();
		for(String s : getBuiltinFunctions()){
			int openParenLoc = s.indexOf("(");
			String localName;
			if (openParenLoc > 0) {
				localName = s.substring(0,openParenLoc).trim();
			}
			else {
				localName = s.trim();
			}
			if (localName.startsWith("^")) {
				localName = localName.substring(1);
			}
			fsList.add(new FunctionSignature(s,	"com.hp.hpl.jena.reasoner.rulesys.builtins" + "#" + localName));
		}
		return fsList;
	}

	public DataSource getDerivations() throws InvalidDerivationException, ConfigurationException {
		getReasonerOnlyWhenNeeded();
		if (getDerivationLevel().equals(DERIVATION_NONE)){
			return null;
		}
		try {
			prepareInfModel();
			StmtIterator sitr;
			if (infModel instanceof InfModel) {
				sitr = ((InfModel) infModel).getDeductionsModel().listStatements();
			}
			else {
				sitr = infModel.listStatements();
			}
			if (sitr.hasNext()) {
				StringWriter swriter = new StringWriter();
				PrintWriter out = new PrintWriter(swriter);
				out.println("Derivations from instance data combined with model '" + tbox + "', " + now() + "\n");
				writeStatementDerivations(out, null, sitr);
				String derivations = swriter.toString();
				out.close();
				StringDataSource ds = new StringDataSource(derivations, "text/plain");
				ds.setName("Derivations");
				return ds;
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
		return null;
	}

	protected void writeStatementDerivations(PrintWriter out, HashSet<Derivation> seen, StmtIterator sitr) throws InvalidDerivationException {
		while (sitr.hasNext()) {
			Statement s = sitr.nextStatement();
			out.println(tripleToString(s));
//			out.println(s.getSubject().getURI() + " " + s.getPredicate().getURI() + " " + objVal);
			if (!getDerivationLevel().equals(DERIVATION_NONE)) {
				int cnt = 0;
				//Iterator itr = infModel.getDerivation(s);
				Iterator<Derivation> itr = getDerivation(s.asTriple());
				if (itr != null) {
					while (itr.hasNext()) {
						Derivation d = (Derivation) itr.next();
						if (getDerivationLevel().equals(DERIVATION_SHALLOW)) {
							printShallowDerivationTrace(infModel.getGraph(), d, out, 0, 0, false);
						}
						else {
							if (!derivationAlreadyShown(d, seen, out, 0)) {
								// must be DERIVATION_DEEP
								if (seen == null) {
									seen = new HashSet<Derivation>();
								}
								printDeepDerivationTrace(infModel.getGraph(), d, out, true, 0, 0, seen, false);
							}
						}
						cnt++;
					}
					if (cnt > 0) {
						out.print("\n");
					}
				}
			}
		}
	}
   
	public void printShallowDerivationTrace(Graph graph, Derivation d, PrintWriter out, int indent, int level, boolean includeValue) throws InvalidDerivationException {
		if (includeValue) {
			out.print(derivationValueToString(d));
		}
		out.println(shallowDerivationToString(d, indent, level, includeValue));	
	}
	
	public String shallowDerivationToString(Derivation d, int indent, int level, boolean includeValue) throws InvalidDerivationException {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < indent; i++) {
			sb.append(" ");
		}
		if (includeValue) {
			sb.append(derivationValueToString(d));
			sb.append(" ");
		}
		sb.append("  set by ");
		sb.append(d.toString());
		sb.append("\n");
		return sb.toString();
	}

	public void printDeepDerivationTrace(Graph infGraph, Derivation d, PrintWriter out, boolean bindings, int indent, 
			int level, HashSet<Derivation> seen, boolean includeValue) throws InvalidDerivationException {
		if (d instanceof RuleDerivation) {
			if (bindings) {
				PrintUtil.printIndent(out, indent + 2);
				out.println((level > 0 ? "which " : "") + "was concluded by: " + d.toString() + "\n");
			}
			int margin = indent + 4;
			List<Triple> matches = ((RuleDerivation) d).getMatches();
			if (matches != null && matches.size() > 0) {
				PrintUtil.printIndent(out, indent + 2);
				out.println("based on matching conditions:\n");
				for (int i = 0; i < matches.size(); i++) {
					Triple match = matches.get(i);
					Iterator<Derivation> derivations = getDerivation(match);
				    if (derivations == null || !derivations.hasNext()) {
				        PrintUtil.printIndent(out, indent + 4);
				        if (match == null) {
				            // A primitive
				        	ClauseEntry term = ((RuleDerivation) d).getRule().getBodyElement(i);
				        	if (term instanceof Functor) {
				        		out.println(((Functor)term).getName() + "()");
				        	} else {
				        		out.println("call to built in");
				        	}
				        } else {
				        	out.println(tripleToString(match));
				        }
				    } else {
				        PrintUtil.printIndent(out, indent + 4);
			        	out.println(tripleToString(match));
				    	while (derivations.hasNext()) {
					    	Derivation derivation = (Derivation)derivations.next();
					    	if (!derivationAlreadyShown(derivation, seen, out, margin)) {
					    		if (seen != null && !seen.contains(derivation)) {
						    		seen.add(derivation);
					    		}
					    		printDeepDerivationTrace(infGraph, derivation, out, bindings, margin, level + 1, seen, includeValue);
					    	}
				    	}
				    }
				}
			}
		}
		else {
			PrintUtil.printIndent(out, indent + 2);
			out.println(d.toString());
		}
	}
	
	public boolean derivationAlreadyShown(Derivation derivation, HashSet<Derivation> seen, PrintWriter out, int margin) {
    	if (seen != null && derivation instanceof RuleDerivation && seen.contains(derivation)) {
    		PrintUtil.printIndent(out, margin + 4);
    		out.println("- explanation already shown");
    		return true;
    	} 
		return false;
	}

	public String tripleToString(Statement t) {
		StringBuffer sb = new StringBuffer();
		sb.append(tripleSubjectToString(t.getSubject()));
		sb.append(" ");
		Property p = t.getPredicate();
		if (p.isProperty()) {
			sb.append(p.getLocalName());
		}
		else {
			sb.append("[unexpected non-property]: " + p.toString());
		}
		sb.append(" ");
		RDFNode n = t.getObject();
		sb.append(tripleValueToString(n));
		return sb.toString();
	}
	
	public String tripleSubjectToString(Resource s) {
		if (!s.isAnon()) {
			return s.getLocalName();
		}
		else {
			return s.toString();
		}
	}

	public String tripleValueToString(RDFNode n) {
		StringBuffer sb = new StringBuffer();
		if (n.canAs(Literal.class)) {
			Object val = ((Literal)n.as(Literal.class)).getValue();
			if (val instanceof String) {
				sb.append("\"");
				sb.append(val.toString());
				sb.append("\"");
			}
			else {
				sb.append(val.toString());
			}
		}
		else if (n.canAs(Resource.class)){
			if (n.isAnon()) {
//				sb.append("<blank node>");
				sb.append(n.toString());
			}
			else {
				sb.append(((Resource)n.as(Resource.class)).getLocalName());
			}
		}
		else {
			sb.append(n.toString());
		}
		return sb.toString();
	}

	public String derivationValueToString(Derivation d) throws InvalidDerivationException {
		if (d instanceof RuleDerivation) {
			return tripleValueToString(((RuleDerivation)d).getConclusion().getObject());
		}
		throw new InvalidDerivationException("Unexpected Derivation type: " + d.toString());
	}
	
	/**
	 * Call to begin tracing
	 * @return
	 */
	private boolean startTrace() {
//		if (traceAppender != null) {
//			LoggerFactory lf;
//				Logger logger = logger.getRootLogger();
//				logger.addAppender(traceAppender);
//		    LoggerContext lc = (LoggerContext) LoggerFactory.getILoggerFactory();
//		    
//		    try {
//		      JoranConfigurator configurator = new JoranConfigurator();
//		      configurator.setContext(lc);
//		      // the context was probably already configured by default configuration 
//		      // rules
//		      lc.reset(); 
//		      configurator.doConfigure(args[0]);
//		    } catch (JoranException je) {
//		       je.printStackTrace();
//		    }


//			return true;
//		}
		return false;
	}
	
	/**
	 * Call to end tracing
	 * 
	 * @return
	 */
	private boolean endTrace() {
//		if (traceAppender != null) {
//			// logger.removeAppender(appender);
//			return true;
//		}
		return false;
	}


	public List<ReasonerTiming> getTimingInformation() {
		return timingInfo;
	}


	public boolean collectTimingInformation(boolean bCollect) {
		boolean oldVal = collectTimingInfo;
		collectTimingInfo = bCollect;
		return oldVal;
	}


	public String getReasonerVersion() {
		return version;
	}

	public void setOutputFormat(String outputFmt) {
		if (outputFmt != null &&
				(outputFmt.equals(IConfigurationManager.N3_FORMAT) ||
						outputFmt.equals(IConfigurationManager.N_TRIPLE_FORMAT) ||
						outputFmt.equals(IConfigurationManager.RDF_XML_ABBREV_FORMAT) ||
						outputFmt.equals(IConfigurationManager.RDF_XML_FORMAT))) {
			this.outputFormat = outputFmt;
		}
	}

	public String getOutputFormat() {
		return outputFormat;
	}

	protected void setModelName(String modelName) {
		this.modelName = modelName;
	}

	protected String getModelName() {
		return modelName;
	}

	protected void setInstanceDataNS(String instDataNS) {
		this.instDataNS = instDataNS;
	}

	protected String getInstanceDataNS() {
		return instDataNS;
	}
	
	protected String getInstanceModelName() {
		if (instDataNS != null) {
			if (instDataNS.endsWith("#")) {
				return instDataNS.substring(0, instDataNS.length() - 1);
			}
			return instDataNS;
		}
		return null;
	}


	public void setInstanceDataNamespace(String ns) {
		setInstanceDataNS(ns);
	}


	public String getInstanceDataNamespace() {
		return getInstanceDataNS();
	}

	public void setModelInputFormat(String owlModelFormat) {
		// TODO Auto-generated method stub
		
	}
	
	public void setQueryTimeout(long timeout) {
		logger.info("Setting query timeout to "+timeout+" ms.");
		this.queryTimeout = timeout;
	}

	public Model getInferredModel(boolean deductionsOnly) throws ConfigurationException {
		prepareInfModel();
		if (deductionsOnly && infModel instanceof InfModel) {
			return ((InfModel)infModel).getDeductionsModel();
		}
		return infModel;
	}

	public boolean clearCache() throws InvalidNameException {
		return true;
	}

	@Override
	public List<ModelError> getErrors() {
		List<ModelError> returning = newErrors;
		newErrors = null;
		return returning;
	}

	private void addError(ModelError newError) {
		if (newErrors == null) {
			newErrors = new ArrayList<ModelError>();
		}
		newErrors.add(newError);
	}

	@Override
	public boolean isInitialized() {
		return initialized;
	}

	@Override
	public String getDefaultTranslatorClassName() {
		return DEFAULT_TRANSLATOR_CLASSNAME;
	}

	@Override
	public boolean loadInstanceData(Object model) throws ConfigurationException {
		// TODO Auto-generated method stub
		return false;
	}

}
