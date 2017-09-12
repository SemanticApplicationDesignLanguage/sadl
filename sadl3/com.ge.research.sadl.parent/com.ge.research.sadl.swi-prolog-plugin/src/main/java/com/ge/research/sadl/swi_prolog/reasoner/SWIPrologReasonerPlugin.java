package com.ge.research.sadl.swi_prolog.reasoner;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.ConnectException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.gp.FunctionSignature;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.IConfigurationManager;
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
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.swi_prolog.plinterface.SWIPrologServiceInterface;
import com.ge.research.sadl.swi_prolog.translator.SWIPrologTranslatorPlugin;
//import com.ge.research.sadl.swi_prolog.plinterface.SWIPrologInterface;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.OWL;

public class SWIPrologReasonerPlugin extends Reasoner {
    protected static final Logger logger = LoggerFactory.getLogger(SWIPrologReasonerPlugin.class);
	public static String ReasonerFamily="SWI-Prolog-Based";
	private static String ReasonerCategory = "SWI-Prolog-Reasoner";

	private String translatorPrologFolder;
	private String portNumber = null;
	private SWIPrologServiceInterface prologServiceInstance;
	private String plUrl;
	private IConfigurationManager configMgr;
	private List<ModelError> newErrors = null;
	private boolean initialized = false;
	
	public SWIPrologReasonerPlugin() {
		logger.debug("Creating new " + this.getClass().getName() + " reasoner.");
	}

	@Override
	public int initializeReasoner(String KBIdentifier, String modelName,
			List<ConfigurationItem> preferences, String repoType)
			throws ReasonerNotFoundException, ConfigurationException {
		if (preferences != null) {
			for (ConfigurationItem config: preferences){
				for (NameValuePair pair: config.getNameValuePairs()){
					System.out.println(pair.getName());
					System.out.println(pair.getValue());
				}
			}
		}
		return initializeReasoner(KBIdentifier, modelName, repoType);
	}

	@Override
	public int initializeReasoner(String KBIdentifier, String modelName,
			String repoType) throws ReasonerNotFoundException,
			ConfigurationException {
		
		//System.out.println("KB identifier is: " + KBIdentifier);
		//System.out.println("Model name: " + modelName);
		
		String port = "5000"; //preferences.get(RequirementsPreference.P_PROLOG_SERVICE_URL);
		if (getPortNumber() == null) {
			setPortNumber(port);
		}
		String url = "http://localhost:" + port + "/result";
		if (url == null || url.isEmpty()) {
			throw new ConfigurationException("Reasoning by service call requires that a service URL be set in preferences.");
		}
		
		setPlUrl(url);
		
		// Step 1: create prolog instance
		SWIPrologServiceInterface pl = new SWIPrologServiceInterface();
		
		// initialize Prolog service
		String errMsg = prepareService(pl, url, "true");
		if (errMsg != null) {
			System.err.println(errMsg);
			return -1;
		}
		
		setPrologServiceInstance(pl);
		
		String modelAltUrl = getConfigMgr().getAltUrlFromPublicUri(modelName);
//		File plFilesFolder = new File(KBIdentifier);
//		File[] files = plFilesFolder.listFiles(); 
		/*
		Map<String, String> envMap = System.getenv();
		SortedMap<String, String> sortedEnvMap = new TreeMap<String, String>(envMap);
		Set<String> keySet = sortedEnvMap.keySet();
		for (String key : keySet) {
			String value = envMap.get(key);
			System.out.println("[" + key + "] " + value);
		}*/
		
		StringBuffer sbLoad = new StringBuffer();
		sbLoad.append(":- load_pl_file('");
		String owlFile = null;
		try {
			owlFile = new SadlUtils().fileUrlToFileName(modelAltUrl);
			sbLoad.append(createDerivedFilename(owlFile, "pl"));
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		sbLoad.append("').\n");
		sbLoad.append(":- load_rdf_file('");
		sbLoad.append(owlFile);
		sbLoad.append("').\n");
		
		StringBuffer sbUnload = new StringBuffer();
		pl.clearPlRules();
		if (sbLoad.length() > 0) {
			pl.addPlRules(sbLoad.toString());
		}
		try {
			System.out.println(pl.runPlQueryNoArgs(url, "true", true));
			pl.clearPlRules();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		//SWIPrologInterface.initProlog(KBIdentifier);
		// to get configuration options
		//String derval = getStringConfigurationValue(preferences , plImport, null);
		setInitialized(true);
		return 0;
	}

	@Override
	public int initializeReasoner(URI KBIdentifier, String modelName,
			List<ConfigurationItem> preferences, String repoType)
			throws ReasonerNotFoundException, ConfigurationException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int initializeReasoner(URI KBIdentifier, String modelName,
			String repoType) throws ReasonerNotFoundException,
			ConfigurationException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void setConfigurationManager(IConfigurationManager configMgr)
			throws ConfigurationException {
		this.setConfigMgr(configMgr);
	}

	@Override
	public boolean reset() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean loadRules(String ruleFileName) throws IOException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean loadRules(URI ruleFileName) throws IOException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addRule(String rule) {
		try {
			if (getPrologServiceInstance().addPlRules(":- load_into_db_beginning((" + rule + ")).")) {
				if (getPrologServiceInstance().runPlQueryNoArgs(getPlUrl(), "true", true)) {
					getPrologServiceInstance().clearPlRules();
					return true;
				}
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			
		}
		getPrologServiceInstance().clearPlRules();
		return false;
	}

	@Override
	public boolean deleteRule(String rule) throws RuleNotFoundException {
		try {
			if (getPrologServiceInstance().addPlRules(":- retract_once((" + rule + ")).")) {
				if (getPrologServiceInstance().runPlQueryNoArgs(getPlUrl(), "true", true)) {
					getPrologServiceInstance().clearPlRules();
					return true;
				}
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		getPrologServiceInstance().clearPlRules();
		return false;
	}

	@Override
	public void setInstanceDataNamespace(String ns) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getInstanceDataNamespace() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean loadInstanceData(String instanceDatafile)
			throws IOException, ConfigurationException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean loadInstanceData(URI instanceDatafile) throws IOException,
			ConfigurationException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean loadInstanceData(InputStream is, String format)
			throws IOException, ConfigurationException {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean loadInstanceData(OntModel model)
			throws ConfigurationException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addRules(List<String> rules) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public boolean deleteTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public void updateTriple(String oldSub, String oldPred, String oldObj,
			String newSub, String newPred, String newObj)
			throws TripleNotFoundException, ConfigurationException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public ResultSet ask(String askQuery) throws QueryParseException,
			QueryCancelledException {
		if (askQuery != null) {
			String[] querySplit = askQuery.split("\\s+");
			List<String> vars = new ArrayList<String>();
			boolean varStart = false;
			for (int index=0; index<querySplit.length; index++){
				if (querySplit[index].toLowerCase().equals("select")){
					varStart = true;
					continue;
				}
				
				if (querySplit[index].toLowerCase().equals("where")){
					varStart = false;
					continue;
				}
				
				if (varStart)
					vars.add(removeLeadingQuestion(querySplit[index]));
			}
			
			int whereIndex = askQuery.indexOf(" where ");
			String plQuery;
			if (whereIndex > 0) {
				plQuery = askQuery.substring(whereIndex + 7);
			}
			else {
				plQuery = askQuery;
			}
			try {
				return prologQueryToResultSet(plQuery, vars);
			} catch (Exception e) {
				addError(new ModelError("Error processing query '" + plQuery + "': " + e.getMessage(), ErrorType.ERROR));
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return null;
	}

	private ResultSet prologQueryToResultSet(String plQuery, List<String> vars) throws Exception {
		List<Hashtable> result = null;
		Object[][] returnset = null;
		
		if (vars != null && vars.size() > 0) {
			result = getPrologServiceInstance().runPlQueryMultipleArgs(getPlUrl(), plQuery, vars, true);
			if (result != null && result.size() > 0) {
				returnset = new Object[result.size()][vars.size()];
				int row = 0; 
				for (Hashtable hTable: result){
					int col = 0;
					for (String var: vars){ 
						returnset[row][col] = hTable.get(var).toString();
						col += 1;
					}
					row += 1;
				}	
			}
			String[] varArray = vars.toArray(new String[vars.size()]) ;
			getPrologServiceInstance().clearPlRules();
			return returnset != null ? new ResultSet(varArray, returnset) : null;
		}
		else {
			boolean r = getPrologServiceInstance().runPlQueryNoArgs(getPlUrl(), plQuery, true);
			String[] colHeaders = new String[1];
			colHeaders[0] = "X";
			returnset = new Object[1][1];
			returnset[0][0] = r;
			getPrologServiceInstance().clearPlRules();
			return new ResultSet(colHeaders, returnset);
		}
	}

	private String removeLeadingQuestion(String var) {
		if (var.startsWith("?")) {
			return var.substring(1);
		}
		return var;
	}

	@Override
	public String prepareQuery(String query) throws InvalidNameException,
			ConfigurationException {
		// TODO Auto-generated method stub
		return query;
	}

	@Override
	public ResultSet ask(String sub, String pred, String obj)
			throws TripleNotFoundException {
		List<String> args = new ArrayList<String>();
		StringBuilder query = new StringBuilder("holds(");
		if (pred != null) {
			query.append("'");
			query.append(SWIPrologTranslatorPlugin.hostToLowercase(pred));
			query.append("'");
		}
		else {
			query.append("P");
			args.add("P");
		}
		query.append(",");
		if (sub != null) {
			query.append("'");
			query.append(SWIPrologTranslatorPlugin.hostToLowercase(sub));
			query.append("'");
		}
		else {
			query.append("S");
			args.add("S");
		}
		query.append(",");
		if (obj != null) {
			query.append("'");
			query.append(SWIPrologTranslatorPlugin.hostToLowercase(obj));
			query.append("'");
		}
		else {
			query.append("O");
			args.add("O");
		}
		query.append(")");
		try {
			return prologQueryToResultSet(query.toString(), args);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public boolean configure(ConfigurationItem configItem) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public DataSource construct(String constructQuery)
			throws QueryParseException, QueryCancelledException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getReasonerFamily() {
		return ReasonerFamily;
	}

	@Override
	public String getReasonerVersion() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean collectTimingInformation(boolean bCollect) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<ReasonerTiming> getTimingInformation() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Class<?> getBuiltinClass() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getConfigurationCategory() {
		return ReasonerCategory;
	}

	@Override
	public Map<String, ConfigurationOption> getReasonerConfigurationOptions() {
		Map<String, ConfigurationOption> map = new HashMap<String, ConfigurationOption>();
		String[] categoryHierarchy = {ReasonerCategory};
		String plImport = "plImport";
		map.put(plImport, 
				new ConfigurationOption(categoryHierarchy, plImport, "Prolog file to import", "", null));
		
		String plArgs = "plArgs";
		map.put(plArgs, 
				new ConfigurationOption(categoryHierarchy, plArgs, "Prolog runtime arguments", "", null));
		String plHostl = "plHost";
		map.put(plHostl, new ConfigurationOption(categoryHierarchy, plHostl, "Prolog HTTP Service Host", "http://localhost", null));
		String plPort = "plPort";
		map.put(plPort, new ConfigurationOption(categoryHierarchy, plPort, "Prolog HTTP Service Port", "5000", null));
		return map;
	}

	@Override
	public void enableExplanation(boolean bVal) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean isExplanationEnabled() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<Explanation> explain(String rulename) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DataSource getDerivations() throws InvalidDerivationException,
			ConfigurationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<ModelError> checkModelValidity() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Explanation> explain(List<GraphPatternElement> patterns) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean saveInferredModel(String filename, String modelname,
			boolean deductionsOnly) throws FileNotFoundException {
		// TODO Auto-generated method stub
		return false;
	}

	public Model getInferredModel(boolean deductionsOnly)
			throws ConfigurationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String objectValueToStringValue(Object objValue, String predicate)
			throws ConfigurationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<BuiltinInfo> getImplicitBuiltins() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<FunctionSignature> getImplicitBuiltinSignatures() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public BuiltinInfo getBuiltinInfo(Class<?> trans) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setOutputFormat(String outputFmt) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setModelInputFormat(String owlModelFormat) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean clearCache() throws InvalidNameException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<ModelError> getErrors() {
		List<ModelError> returning = newErrors;
		newErrors = null;
		return returning;
	}

	public String prepareService(SWIPrologServiceInterface pl, String url, String query) {
		// Step 1: create prolog instance
		if (pl == null) {
			pl = new SWIPrologServiceInterface();
		}
		
		
		// Step 2: is the service running?
		boolean isRunning = true;
		try {
			boolean retValue = pl.runPlQueryNoArgs(url, query, true);
		}
		catch (ConnectException e) {
			isRunning = false;
		} catch (Exception e) {
			// TODO Auto-generated catch block
//			e.printStackTrace();
		}
		
		// Step 3: kill existing SWI-Prolog service
		if (isRunning) {
			try {
				Runtime.getRuntime().exec("taskkill /F /IM swipl-win.exe");
				// must wait a brief period of time or the killed process will respond to the query below
				while (isRunning) {
					try {
						boolean retValue = pl.runPlQueryNoArgs(url, query, true);
					}
					catch (ConnectException e) {
						isRunning = false;
					} catch (Exception e) {
						// TODO Auto-generated catch block
//						e.printStackTrace();
					}
				}
			} catch (IOException e2) {
				e2.printStackTrace();
			}
		}
		
		// process is now running so now clear the temp folderprolog-service-temp
        String tmpfolder;
		try {
			tmpfolder = getTranslatorPrologFolder();
	        File tmpFolderFile = new File(tmpfolder);
	        File[] files = tmpFolderFile.listFiles();
	    	for (int j = 0; files != null && j < files.length; j++) {
	    		files[j].delete();
	    	}
	    	tmpFolderFile.delete();
		} catch (IOException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
    	
		boolean serviceOK = false;
		int errorNumber = 0;
		while (!serviceOK) {
			// Step 3: execute query
			try {
				boolean retValue = pl.runPlQueryNoArgs(url, query, true);
				serviceOK = true;
			}
			catch (ConnectException e) {
				if (errorNumber == 0) {
					try {
						// Step 4: if query failed for the first time, start Service
						Runtime.getRuntime().exec("cmd /c start /min " + getPrologCommandLine());
					} catch (IOException e1) {
						e1.printStackTrace();
						return e1.getMessage();
					} catch (TranslationException e1) {
						e1.printStackTrace();
						return e1.getMessage();
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
				return e.getMessage();
			}
			errorNumber++;
			if (errorNumber > 1000) {
				return "Failed to prepare service after 1000 iterations.";
			}
		}
		return null;
	}
	
	public String getPrologCommandLine() throws TranslationException, IOException {
		String batchFile = getTranslatorPrologFolder() + "/run-prolog-service.bat";
		
		// write the file
		File bf = new File(batchFile);
		if (bf.exists() && !bf.canWrite()) {
			throw new TranslationException("Can't create batch file '" + batchFile + "'; not writable.");
		}
		else {
			bf.getParentFile().mkdirs();
		}
		
		String runServiceFile = getConfigMgr().getModelFolder() + "/" + SWIPrologTranslatorPlugin.SWI_RUN_PROLOG_SERVICE_PL;
//		String contents = "start /min swipl-win.exe -s " + runServiceFile + "\nexit\n";
		String contents = "start /min swipl-win.exe --traditional -s " + runServiceFile + "\nexit\n";
		SadlUtils su = new SadlUtils();
		su.stringToFile(bf, contents, false);
		
		// create temp folder for SWI-Prolog service
		String plConfigFile = getTranslatorPrologFolder() + "/prolog-service-config/prolog-service-config.pl";
		File pltf = new File(plConfigFile);
		if (pltf.exists() && !pltf.canWrite()) {
			throw new TranslationException("Can't create Prolog output file container'" + plConfigFile + "'; not writable.");
		}
		if (!pltf.getParentFile().exists()) {
			pltf.getParentFile().mkdirs();
		}
//		String tempFolder = getTranslatorPrologFolder() + "/temp";
//		File tmp = new File(tempFolder);
//		if (!tmp.exists() && !tmp.mkdirs()) {
//			throw new TranslationException("Unable to create temp folder '" + tempFolder + "'");
//		}
		String port = getPortNumber();
		contents = "tmp_dir('" + getTranslatorPrologFolder().replace('\\', '/') + "/').\nport_number(" + port + ").";
		su.stringToFile(pltf, contents, false);
		return batchFile;
	}

	public void setPortNumber(String port) {
		portNumber = port;
	}

	public String getPortNumber() {
		return portNumber;
	}

	private SWIPrologServiceInterface getPrologServiceInstance() {
		return prologServiceInstance;
	}

	private void setPrologServiceInstance(SWIPrologServiceInterface prologServiceInstance) {
		this.prologServiceInstance = prologServiceInstance;
	}

	private String getPlUrl() {
		return plUrl;
	}

	private void setPlUrl(String plurl) {
		this.plUrl = plurl;
	}

	public String getTranslatorPrologFolder() throws IOException {
		if (translatorPrologFolder == null) {
			String pfp = getConfigMgr().getModelFolder() + "/temp";
			File pfpf = new File(pfp);
			pfpf.mkdirs();
			setTranslatorPrologFolder(pfp);
		}
		return translatorPrologFolder;
	}

	public void setTranslatorPrologFolder(String translatorPrologFolder) {
		this.translatorPrologFolder = translatorPrologFolder;
	}

	/**
	 * Method to find all of the imports, direct and indirect, for a given Owl model in the OwlModels folder
	 * @param folder
	 * @param owlFile
	 * @return
	 * @throws URISyntaxException
	 * @throws MalformedURLException
	 */
	protected List<String> importedOwlFiles(String folder, String owlFile)
			throws URISyntaxException, MalformedURLException {
		SadlUtils su = new SadlUtils();
		owlFile = su.fileNameToFileUrl(owlFile);
		//				OntModel m = OntDocumentManager.getInstance().getOntology(owlFile, OntModelSpec.OWL_MEM);
		OntModel m = getConfigMgr().getJenaDocumentMgr().getOntology(owlFile, OntModelSpec.OWL_MEM);
		getConfigMgr().getJenaDocumentMgr().setProcessImports(true);
		m.loadImports();
		StmtIterator sitr = m.listStatements((Resource)null, OWL.imports, (RDFNode)null);
		if (sitr.hasNext()) {
			List<String> imports = new ArrayList<String>();
			while (sitr.hasNext()) {
				Statement stmt = sitr.nextStatement();
				RDFNode obj = stmt.getObject();
				if (obj instanceof Resource) {
					//							String fn = su.fileUrlToFileName(OntDocumentManager.getInstance().doAltURLMapping(((Resource)obj).toString()));
					String fn = su.fileUrlToFileName(getConfigMgr().getJenaDocumentMgr().doAltURLMapping(((Resource)obj).toString()));
					if (!imports.contains(fn)) {
						imports.add(fn);
					}
				}
			}
			return imports;
		}
		return null;
	}

	/**
	 * Load all OWL files imported, directly or indirectly, by a requirements file given the OWL file of the requirements file
	 * @param folder
	 * @param owlFile
	 * @return
	 * @throws Exception 
	 */
	protected List<String> loadOwlFilesImports(String folder, String owlFile)
			throws Exception {
		File fldr = new File(folder);
		if (!fldr.exists()) {
			throw new TranslationException("Folder for which to load all OWL files (" + folder + ") doesn't exist.");
		}
		else if (!fldr.isDirectory()) {
			throw new TranslationException("Location for which to load all OWL files (" + folder + ") isn't a folder.");
		}

		List<String> files = importedOwlFiles(folder, owlFile);

		if (files != null) {
			for (int i = 0; i < files.size(); i++) {
				File f = new File(new SadlUtils().fileUrlToFileName(files.get(i)));
				if (f.exists()) {
					getPrologServiceInstance().runPlQueryNoArgs(getPlUrl(), "unload_rdf_file('" + f.getCanonicalPath().replace('\\', '/') + "')", true);
					getPrologServiceInstance().runPlQueryNoArgs(getPlUrl(), "load_rdf_file('" + f.getCanonicalPath().replace('\\', '/') + "')", true);
				}
				else {
					throw new TranslationException("OWL file '" + f.getCanonicalPath() + "' does not exist, can't be loaded.");
				}
			}
		}
		return files;
	}

	private IConfigurationManager getConfigMgr() {
		return configMgr;
	}

	private void setConfigMgr(IConfigurationManager configMgr) {
		this.configMgr = configMgr;
	}

	protected String createDerivedFilename(String filename, String newext) {
		int lastDot = filename.lastIndexOf('.');
		if (lastDot > 0) {
			return filename.substring(0, lastDot + 1) + newext;
		}
		return filename + "." + newext;
	}
	
	private void addError(ModelError newError) {
		if (newErrors  == null) {
			newErrors = new ArrayList<ModelError>();
		}
		newErrors.add(newError);
	}

	@Override
	public boolean isInitialized() {
		// TODO Auto-generated method stub
		return false;
	}

	protected void setInitialized(boolean initialized) {
		this.initialized = initialized;
	}

	@Override
	public String getDefaultTranslatorClassName() {
		return "com.ge.research.sadl.swi_prolog.translator.SWIPrologTranslatorPlugin";
	}

	@Override
	public boolean loadInstanceData(Object model) throws ConfigurationException {
		throw new ConfigurationException("Method not supported in this reasoner.");
	}

}
