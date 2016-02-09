package com.ge.research.sadl.prolog.reasoner;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
//import java.net.URL;
//import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoMoreSolutionException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Theory;

import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.prolog.fileinterface.FileInterface;
import com.ge.research.sadl.prolog.ontologyinterface.OntologyInterface;
import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidDerivationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.Reasoner;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.RuleNotFoundException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.Model;

public class PrologReasonerPlugin extends Reasoner {
    protected static final Logger logger = LoggerFactory.getLogger(PrologReasonerPlugin.class);
	private static final String TIMING_INITIALIZE = "Initialize";
	private static final String TIMING_RDF_TO_PROLOG = "RDFtoProlog";
	private String TIMING_QUERYPREP = "PrologQueryPrep";
	private String TIMING_QUERYEXEC = "PrologQueryExecution";
	public static String ReasonerFamily="tu-Prolog-Based";
	private static String ReasonerCategory = "tu-Prolog_Reasoner";
	private Prolog plengine = null;
	private OntologyInterface ontointerface = new OntologyInterface();
	
	private String runInitPred = "";
	@SuppressWarnings("unused")
	private String plImport = "";
	@SuppressWarnings("unused")
	private String plArgs = "";

	protected boolean collectTimingInfo = false;
	protected List<ReasonerTiming> timingInfo = null;
	private String kbIdentifier = null;

	private List<ModelError> newErrors = null;
	
	@Override
	public int initializeReasoner(String KBIdentifier, String modelName,
			List<ConfigurationItem> preferences, String repoType)
			throws ReasonerNotFoundException, ConfigurationException {
		// TODO Auto-generated method stub
		if (preferences != null) {
			for (ConfigurationItem config: preferences){
				for (NameValuePair pair: config.getNameValuePairs()){
					if (pair.getName().equals("plImport"))
						plImport = (String) pair.getValue();
					if (pair.getName().equals("plArgs"))
						plArgs = (String) pair.getValue();
					if (pair.getName().equals("runInitPred"))
						runInitPred = (String) pair.getValue();
				}
			}
		}
		return initializeReasoner(KBIdentifier,modelName,repoType);
	}

	@Override
	public int initializeReasoner(String KBIdentifier, String modelName,
			String repoType) throws ReasonerNotFoundException,
			ConfigurationException {
		
		//System.out.println("KB identifier is: " + KBIdentifier);
		//System.out.println("Model name: " + modelName);
		//System.out.println("Repo type: " + repoType);
		
		kbIdentifier = KBIdentifier;
		if (collectTimingInfo) {
			if (timingInfo == null) {
				timingInfo = new ArrayList<ReasonerTiming>();
			}
			else {
				timingInfo.clear();
			}
		}
		
		try {
			long t1 = System.currentTimeMillis();
			ontointerface.initializeOntologyInterface(KBIdentifier, modelName, repoType);
			if (collectTimingInfo) {
				long t2 = System.currentTimeMillis();
				timingInfo.add(new ReasonerTiming(TIMING_INITIALIZE, "load ontology model", t2 - t1));
			}
		} catch (QueryParseException | QueryCancelledException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		return 1;
	}
	
	private boolean ignoreFile(String file){
		if (runInitPred != null && !runInitPred.equals("") && file.equals("init.pl"))
			return true;
		
		return false;
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
		if (ontointerface != null) {
			ontointerface.setConfigMgr(configMgr);
		}
		
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
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean deleteRule(String ruleName) throws RuleNotFoundException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setInstanceDataNamespace(String ns) {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			ontointerface.getJenareasoner().setInstanceDataNamespace(ns);
		}
	}

	@Override
	public String getInstanceDataNamespace() {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().getInstanceDataNamespace();
		}
		return null;
	}

	@Override
	public boolean loadInstanceData(String instanceDatafile)
			throws IOException, ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().loadInstanceData(instanceDatafile);
		}
		return false;
	}

	@Override
	public boolean loadInstanceData(URI instanceDatafile) throws IOException,
			ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().loadInstanceData(instanceDatafile);
		}
		return false;
	}

	@Override
	public boolean loadInstanceData(InputStream is, String format)
			throws IOException, ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().loadInstanceData(is, format);
		}
		return false;
	}

	@Override
	public boolean loadInstanceData(OntModel model)
			throws ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().loadInstanceData(model);
		}
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
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().addTriple(sub, pred, obj);
		}
		return false;
	}

	@Override
	public boolean deleteTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().deleteTriple(sub, pred, obj);
		}
		return false;
	}

	@Override
	public void updateTriple(String oldSub, String oldPred, String oldObj,
			String newSub, String newPred, String newObj)
			throws TripleNotFoundException, ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			ontointerface.getJenareasoner().updateTriple(oldSub, oldPred, oldObj, newSub, newPred, newObj);
		}
	}

	@Override
	public ResultSet ask(String askQuery) throws QueryParseException,
			QueryCancelledException {
		// if not prolog query, invoke other reasoner(s)
		if (!isPrologQuery(askQuery)){
			return ontointerface.runNonPrologQuery(askQuery);
		}
		
		long t0 = System.currentTimeMillis();
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
			
			if (varStart) {
				String[] varSplit = querySplit[index].split(",+");
				for (int i=0; i<varSplit.length; ++i){
					vars.add(varSplit[i]);
				}
			}
		}
		
		int whereIndex = askQuery.indexOf(" where ");
		String plQuery = whereIndex >= 0 ? askQuery.substring(whereIndex + 7) : askQuery;
		if (!plQuery.endsWith(".")) {
			plQuery = plQuery + ".";
		}
		long t1 = System.currentTimeMillis();
		if (collectTimingInfo) {
			timingInfo.add(new ReasonerTiming(TIMING_QUERYPREP, "prepare query", t1 - t0));
		}
		int solution_count = 0;
		List<String> solution_list = new ArrayList<String>();
		try {
			System.out.println("Query: " + plQuery);
			SolveInfo solution = getPlengine().solve(plQuery);
			while (solution.isSuccess()) {
				solution_count += 1;
				for (String var: vars){
					solution_list.add(solution.getVarValue(var).toString());
				}
				//System.out.println(solution.getBindingVars());
				//System.out.println(solution.getSolution().toString());
				if (getPlengine().hasOpenAlternatives())
					solution = getPlengine().solveNext();
				else
					break;
			}
		} catch (MalformedGoalException | NoSolutionException | NoMoreSolutionException e) {
			System.err.println("Error: " + (e.getMessage() != null ? e.getMessage() : "") + " executing query '" + plQuery + "'");
			e.printStackTrace();
		}
		if (collectTimingInfo) {
			long t2 = System.currentTimeMillis();
			timingInfo.add(new ReasonerTiming(TIMING_QUERYEXEC, "execute query", t1 - t0));
		}
		//List<Var> plvars = new ArrayList<Var>();
		//if (solution.isSuccess())
			//plvars = solution.getBindingVars();
		if (solution_count > 0){
			Object[][] returnset = new Object[solution_count][vars.size()];
			int row = 0; int col = 0;
			for (String solution_val : solution_list){
				returnset[row][col] = solution_val;
				col += 1;
				if (col == vars.size()){
					row += 1;
					col = 0;
				}
			}

			//return ontointerface.queryAndWrite(askQuery, "", "");
			return new ResultSet(returnset);
		}
		
		return null;
		
		
	}

	@Override
	public String prepareQuery(String query) throws InvalidNameException,
			ConfigurationException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			IReasoner reasoner = ontointerface.getJenareasoner();
			String pQuery = reasoner.prepareQuery(query);
			List<ModelError> errors = reasoner.getErrors();
			if (errors != null) {
				for (int i = 0; i < errors.size(); i++) {
					addError(errors.get(i));
				}
			}
			return pQuery;
		}
		return query;
	}

	@Override
	public ResultSet ask(String sub, String pred, String obj)
			throws TripleNotFoundException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().ask(sub, pred, obj);
		}
		return null;
	}

	@Override
	public boolean configure(ConfigurationItem configItem) {
		// TODO Auto-generated method stub
		
		for (NameValuePair pair: configItem.getNameValuePairs()){
				if (pair.getName().equals("plImport"))
					plImport = (String) pair.getValue();
				if (pair.getName().equals("plArgs"))
					plArgs = (String) pair.getValue();
				if (pair.getName().equals("runInitPred"))
					runInitPred = (String) pair.getValue();
			}
		
		return false;
	}

	@Override
	public DataSource construct(String constructQuery)
			throws QueryParseException, QueryCancelledException {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			return ontointerface.getJenareasoner().construct(constructQuery);
		}
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
		boolean oldVal = collectTimingInfo;
		collectTimingInfo = bCollect;
		return oldVal;
	}

	@Override
	public List<ReasonerTiming> getTimingInformation() {
		return timingInfo;
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
		
		String runInitPred = "runInitPred";
		map.put(runInitPred, 
				new ConfigurationOption(categoryHierarchy, runInitPred, "Semi-colon separated list of facts to compute during initialization (ex: holds('subClassOf',X,Y);instanceOf(X,Y))", "", null));

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

	@Override
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
	public BuiltinInfo getBuiltinInfo(Class<?> trans) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setOutputFormat(String outputFmt) {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			ontointerface.getJenareasoner().setOutputFormat(outputFmt);
		}
	}

	@Override
	public void setModelInputFormat(String owlModelFormat) {
		if (ontointerface != null && ontointerface.getJenareasoner() != null) {
			ontointerface.getJenareasoner().setModelInputFormat(owlModelFormat);
		}
	}

	@Override
	public boolean clearCache() throws InvalidNameException {
		// TODO Auto-generated method stub
		return false;
	}
	
	private boolean isPrologQuery(String query){
		int num_single_quotes = 0;
		int num_double_quotes = 0;
		if (!query.contains("select") && !query.contains("construct") && !query.contains("ask")) {
			return true;
		}
		String[] querySplit = query.trim().split("\\s+");
//		if (querySplit.length < 4)
//			return false;
		if (querySplit[0].toLowerCase().equals("select")){
			for (int index = 0; index < query.length(); index++)
			{
				if (String.valueOf(query.charAt(index)).equals("'") && !String.valueOf(query.charAt(index-1)).equals("\\")){ 
					num_single_quotes += 1;
					continue;
				}
			
				if (String.valueOf(query.charAt(index)).equals("\"") && !String.valueOf(query.charAt(index-1)).equals("\\")){
					num_double_quotes += 1;
					continue;
				}
				
				if (String.valueOf(query.charAt(index)).equals("{")){
					if ((num_single_quotes % 2 == 0) && (num_double_quotes % 2 == 0))
						return false;
				}
				
			}
			
			return true;
		}
			
		return false;
	}

	private Prolog getPlengine() throws QueryParseException, QueryCancelledException {
		if (plengine == null) {
			long t1 = System.currentTimeMillis();
			if (ontointerface.preparePrologFiles(kbIdentifier) == 0) {
				System.err.println("Error encountered preparing Prolog input files");
			}
			if (collectTimingInfo) {
				long t2 = System.currentTimeMillis();
				timingInfo.add(new ReasonerTiming(TIMING_RDF_TO_PROLOG, "prepare Prolog input files", t2 - t1));
			}
			plengine = new Prolog();
			plengine.clearTheory();
			File plFilesFolder = new File(kbIdentifier );
			
			if (runInitPred != null && !runInitPred.equals("")){
				String rdfFile = kbIdentifier + File.separator + "rdf.pl";
				String initFile = kbIdentifier + File.separator + "init.pl";

				try {
					System.out.println("Loading " + rdfFile);
					plengine.addTheory(new Theory(new FileInputStream(rdfFile)));
					System.out.println("Loading " + initFile);
					plengine.addTheory(new Theory(new FileInputStream(initFile)));
				} catch (InvalidTheoryException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				} catch (FileNotFoundException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}

				/*
			Prolog tempplengine = plengine; 

			try {
				System.out.println("Loading " + initFile);
				tempplengine.addTheory(new Theory(new FileInputStream(initFile)));
			} catch (InvalidTheoryException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (FileNotFoundException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}*/

				// compute initialization predicates
				String[] atoms = runInitPred.split(";");
				for (int index=0; index<atoms.length; index++){
					try {
						System.out.println("Running Query: " + atoms[index]);
						SolveInfo solution = plengine.solve(atoms[index] + ".");
						while (solution.isSuccess()) {
							FileInterface.writeFile(rdfFile, solution.getSolution().toString() + ".\n", true);
							//try {
							//plengine.addTheory(new Theory(solution.getSolution().toString() + "."));
							//} catch (InvalidTheoryException e) {
							// TODO Auto-generated catch block
							//e.printStackTrace();
							//}
							//System.out.println(solution.getSolution());
							//System.out.println(solution.getBindingVars());
							//System.out.println(solution.getSolution().toString());
							if (plengine.hasOpenAlternatives())
								solution = plengine.solveNext();
							else
								break;
						}
					} catch (MalformedGoalException | NoSolutionException | NoMoreSolutionException e) {
						// TODO Auto-generated catch block
						System.err.println("Error: " + e.getMessage());
						e.printStackTrace();
					}
				}
				plengine.clearTheory();
			}
			/*
			Map<String, String> envMap = System.getenv();
			SortedMap<String, String> sortedEnvMap = new TreeMap<String, String>(envMap);
			Set<String> keySet = sortedEnvMap.keySet();
			for (String key : keySet) {
				String value = envMap.get(key);
				System.out.println("[" + key + "] " + value);
			}*/
			File[] files = plFilesFolder.listFiles(); 
			// first load prolog files and then owl/rdf files
			for (int i = 0; i < files.length; i++){
				if (files[i].getName().endsWith(".pl")){
					if (!ignoreFile(files[i].getName())){
						System.out.println("Loading " + files[i].getAbsolutePath());
						try {
							plengine.addTheory(new Theory(new FileInputStream(files[i].getAbsolutePath())));
						} catch (InvalidTheoryException e) {
							// TODO Auto-generated catch block
							System.err.println("Syntax error: " + e.getMessage());
						} catch (FileNotFoundException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
			}
			
			// to get configuration options
			//String derval = getStringConfigurationValue(preferences , plImport, null);
		}
		return plengine;
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
}
