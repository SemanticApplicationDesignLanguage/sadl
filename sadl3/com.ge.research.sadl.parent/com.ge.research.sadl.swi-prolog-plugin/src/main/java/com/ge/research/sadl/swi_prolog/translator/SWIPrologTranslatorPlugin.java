package com.ge.research.sadl.swi_prolog.translator;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.NegatedExistentialQuantifier;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.FunctionNotSupportedException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.swi_prolog.fileinterface.FileInterface;
//import com.ge.research.sadl.swi_prolog.plinterface.SWIPrologInterface;
import com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

public class SWIPrologTranslatorPlugin implements ITranslator {
    public static final String SWI_RUN_PROLOG_SERVICE_PL = "swi-run-prolog-service.pl";

	public static final String SWI_CUSTOM_PREDICATES_PL = "swi-custom-predicates.pl";

	public static final String SWI_PROLOG_SERVICE_PL = "swi-prolog-service.pl";

	public static final String SWI_STANDARD_DECLARATIONS_PL = "swi-standard-declarations.pl";

	protected static final Logger logger = LoggerFactory.getLogger(SWIPrologTranslatorPlugin.class);

    private static final String TranslatorCategory = "SWI-Prolog_Translator";

	private static final Object DEFAULTS_URI = "http://research.ge.com/Acuity/defaults.owl";
    protected List<ModelError> errors = null;
    private boolean saveRuleFileAfterModelSave = true;
    private List<String> importOrder;
	protected IConfigurationManager configurationMgr;
    private List<String> builtinList = Arrays.asList("printhead","derive","rdf","length","sub_string","sub_atom", "equal");
    private List<String> functionListArity1 = Arrays.asList("abs", "sign", "random", "random_float", "round", "integer", 
    		"float", "float_fractional_part", "float_integer_part", "truncate", "floor", "ceiling", "sqrt", "sin", "cos", 
    		"tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asingh", "acosh", "atanh", "log", "log10", "exp");
    private List<String> functionListArity2 = Arrays.asList("atan2", "copysign", "div", "max", "min");
    private List<String> validatedBuiltins = null;
    private List<String> invalidatedBuiltins = null;
	private Rule ruleInTranslation = null;
	private Query queryInTranslation = null;
	private int nextQueryVarNum = 1;

	private String modelName = null;
	private OntModel theModel = null;
    private Map<String, String> prefixes = new HashMap<String, String>();

	private int prologOnlyVarCnt = 0;

	private String translationFolder;

	private IReasoner reasoner;
	
    private enum RulePart {PREMISE, CONCLUSION, NOT_A_RULE}
    private enum SpecialBuiltin {NOVALUE, NOVALUESPECIFIC, NOTONLY, ONLY, ISKNOWN}
	public enum TranslationTarget {RULE_TRIPLE, RULE_BUILTIN, QUERY_TRIPLE, QUERY_FILTER}
	
	public SWIPrologTranslatorPlugin() {
		logger.debug("Creating new '" + this.getClass().getName() + "' translator.");
	}
	
	@Override
	public String getConfigurationCategory() {
		return TranslatorCategory;
	}

	@Override
	public void setConfigurationManager(IConfigurationManager configMgr) throws ConfigurationException {
		configurationMgr = configMgr;
		try {
			assureRequiredPrologFiles(configMgr.getModelFolderPath().getCanonicalPath());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			throw new ConfigurationException("Failed to create required Prolog files", e);
		}
	}

	@Override
	public List<ModelError> translateAndSaveModel(OntModel model,
			String translationFolder, String modelName, List<String> orderedImports, String saveFilename)
			throws TranslationException, IOException, URISyntaxException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<ModelError> translateAndSaveModel(OntModel model,
			List<Rule> ruleList, String translationFolder, String modelName,
			List<String> orderedImports, String saveFilename) throws TranslationException, IOException,
			URISyntaxException {
		
		setTheModel(model);
		setModelName(modelName);
		this.setTranslationFolder(translationFolder);
		if (errors != null) {
			errors.clear();
		}
		if (model == null) {
			return addError("Cannot save model in file '" + saveFilename + "' as it has no model.", ErrorType.ERROR);
		}
		if (modelName == null) {
			return addError("Cannot save model in file '" + saveFilename + "' as it has no name.", ErrorType.ERROR);
		}
		
		setImportOrder(orderedImports);
		//for (String plimport: orderedImports)
			//System.out.println(plimport);
		

        if (saveRuleFileAfterModelSave) {
    		String ruleFilename = createDerivedFilename(saveFilename, "pl");
    		String fullyQualifiedRulesFilename = translationFolder + File.separator + ruleFilename;
			// clear the file
    		FileInterface.writeFile(fullyQualifiedRulesFilename, "", false);
//			FileInterface.writeFile(fullyQualifiedRulesFilename, ":- rdf_load('" + saveFilename + "').\n", true);
			
			for (int i = 0; orderedImports != null && i < orderedImports.size(); i++) {
				String impUri = orderedImports.get(i).toString();
				if (impUri.equals(DEFAULTS_URI)) {
					continue;
				}
				try {
					String impUrl = configurationMgr.getAltUrlFromPublicUri(impUri);
					String impName = impUrl.substring(impUrl.lastIndexOf('/') + 1);
					String plImpName = createDerivedFilename(impName, "pl");
					StringBuilder sb = new StringBuilder();
					sb.append(":- rdf_load('");
					sb.append(impName);
					sb.append("').\n:- consult('");
					sb.append(plImpName);
					sb.append("').\n");
					FileInterface.writeFile(fullyQualifiedRulesFilename, sb.toString(), true);
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
			}
			       
			if (ruleList != null){
				for (Rule rule: ruleList){
					//System.out.println(rule.toString());
//					String ruleStr = toString(rule.getRuleName(),rule.getIfs(),rule.getThens());
					String ruleStr = translateRule(model, rule);
					FileInterface.writeFile(fullyQualifiedRulesFilename, ruleStr.trim() + "\n", true);
				}
			}
        }
		
		//saveRuleFileAfterModelSave = false;	// reset
		return (errors != null && errors.size() > 0) ? errors : null;
	}

	private void assureRequiredPrologFiles(String translationFolder) {
		String standardDecls = translationFolder + File.separator + SWI_STANDARD_DECLARATIONS_PL;
		File f = new File(standardDecls);
		if (!f.exists()){
			FileInterface.writeFile(standardDecls, getStandardDeclarations(), false);
		}
		String prologService = translationFolder + File.separator + SWI_PROLOG_SERVICE_PL;
		f = new File(prologService);
		if (!f.exists()) {
			FileInterface.writeFile(prologService, getService(), false);
		}
		String custom = translationFolder + File.separator + SWI_CUSTOM_PREDICATES_PL;
		File cpf = new File(custom);
		if (!cpf.exists()) {
			FileInterface.writeFile(custom, "% custom predicates and initialization for SWI-Prolog should be defined here\n\n", false);
		}
		String runService = translationFolder + File.separator + SWI_RUN_PROLOG_SERVICE_PL;
		f = new File(runService);
		if (!f.exists()) {
			FileInterface.writeFile(runService, getRunService(), false);
		}
	}

	@Override
	public List<ModelError> translateAndSaveModelWithOtherStructure(
			OntModel model, Object otherStructure, String translationFolder,
			String modelName, List<String> orderedImports, String saveFilename) throws TranslationException,
			IOException, URISyntaxException {
	
		throw new TranslationException("This translator (" + this.getClass().getCanonicalName() + ") does not translate other knowledge structures.");
	}

	@Override
	public String translateRule(OntModel model, Rule rule)
			throws TranslationException {
		setRuleInTranslation(rule);

		List<GraphPatternElement> thens = rule.getThens();
		if (thens != null) {
			// expand all rule head conjunctions to list form first
			thens = headJunctionsToList(thens);
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < thens.size(); i++) {
				StringBuilder thisThen = new StringBuilder();
				GraphPatternElement then = thens.get(i);
				List<GraphPatternElement> oneThens = new ArrayList<GraphPatternElement>();
				oneThens.add(then);
				String thenStr = graphPatternElementsToPrologRuleString(oneThens, RulePart.CONCLUSION);
				if (thenStr != null) {
					thisThen.append(thenStr);
					List<GraphPatternElement> givens = rule.getGivens();
					List<GraphPatternElement> ifs = rule.getIfs();
					if ((givens != null && givens.size() > 0) || (ifs != null && ifs.size() > 0)) {
						thisThen.append(" :- ");
					}
					String givenStr = graphPatternElementsToPrologRuleString(givens, RulePart.PREMISE);
					if (givenStr != null) {
						thisThen.append(givenStr);
					}
					String ifStr = graphPatternElementsToPrologRuleString(ifs, RulePart.PREMISE);
					if (ifStr != null) {
						if (givenStr != null) {
							thisThen.append(", ");
						}
						thisThen.append(ifStr);
					}
					if (thisThen.length() > 0) {
						thisThen.append(". \n");
						// check for valid rule here (so it is one rule at a time, not all rules together)
						try {
							String ruleStr = thisThen.toString().trim();
							if (ruleStr.endsWith(".")) {
								ruleStr = ruleStr.substring(0, ruleStr.length() - 1);
							}
							if (getReasoner().addRule(ruleStr)) {
								getReasoner().deleteRule(ruleStr);
							}
													
						}
						catch( Exception e) {
							addError(this.getModelName() + ": " + e.getMessage(), ErrorType.ERROR);
						}
					}
					sb.append(thisThen.toString());
				}
			}
			setRuleInTranslation(null);
			return sb.toString();
		}
		setRuleInTranslation(null);
		return null;
	}

	private List<GraphPatternElement> headJunctionsToList(
			List<GraphPatternElement> thens) throws TranslationException {
		for (int i = 0; thens != null && i < thens.size(); i++) {
			GraphPatternElement then = thens.get(i);
			// if it's a Junction expand; otherwise nothing is needed
			if (then instanceof Junction) {
				thens = thenJunctionToList(thens, i, (Junction) then);
			}
		}
		return thens;
	}
	
	private List<GraphPatternElement> thenJunctionToList(
			List<GraphPatternElement> thens, int index, GraphPatternElement then) throws TranslationException {
		if (then instanceof Junction) {
			if (((Junction) then).getJunctionType().equals(JunctionType.Disj)) {
				throw new TranslationException("Disjunction in rule head not supported: " + then.toFullyQualifiedString());
			}
			else {
				Object lhs = ((Junction)then).getLhs();
				Object rhs = ((Junction)then).getRhs();
				if (!(lhs instanceof GraphPatternElement) || !(rhs instanceof GraphPatternElement)) {
					throw new TranslationException("Conjunction in rule head has elements that were not GraphPatternElements: " + then.toFullyQualifiedString());
				}
				if (!(lhs instanceof Junction) && !(rhs instanceof Junction)) {
					// this is a bottom so replace this element with the two non-Junctions, keep the same order
					thens.set(index, (GraphPatternElement) rhs);
					thens.add(index, (GraphPatternElement) lhs);
				}
				else {
					// we aren't to the bottom yet or we're on our way back up
					thens = thenJunctionToList(thens, index, (GraphPatternElement) lhs);
					thens = thenJunctionToList(thens, index, (GraphPatternElement) rhs);
					if (!(lhs instanceof Junction)) {
						thens.add(index, (GraphPatternElement) lhs);
					}
					if (!(rhs instanceof Junction)) {
						thens.add(index, (GraphPatternElement) rhs);
					}
				}
			}
		}
		return thens;
	}

	@Override
	public String translateQuery(OntModel model, Query query)
			throws TranslationException, InvalidNameException {
		setTheModel(model);
		
		StringBuilder sb = new StringBuilder();
		List<VariableNode> vars = query.getVariables();
		List<GraphPatternElement> patterns = query.getPatterns();
		String test1 = graphPatternElementsToPrologRuleString(patterns, RulePart.NOT_A_RULE);
		if (patterns != null) {
			for (int i = 0; i < patterns.size(); i++) {
				GraphPatternElement gpe = patterns.get(i);
				String test2 = graphPatternElementToPrologRuleString(gpe, RulePart.NOT_A_RULE);
				if (i > 0) {
					sb.append(",");
				}
				sb.append(test2);
			}
//			sb.append(".");
		}

		if (vars != null && vars.size() > 0) {
			List<String> prologVars = new ArrayList<String>();
			StringBuilder sb2 = new StringBuilder("select ");
			for (int i = 0; i < vars.size(); i++) {
				prologVars.add(sadlVariableToPrologVariable(vars.get(i).getName()));
				sb2.append(prologVars.get(i));
				sb2.append(" ");
			}
			sb2.append("where ");
			sb.insert(0, sb2.toString());
		}
		return sb.toString();
		//System.out.println(query.toString());
		
//		return query.toFullyQualifiedString().replace("and", ",").replace("!=", "\\==");
	}

	@Override
	public String getReasonerFamily() {
		return SWIPrologReasonerPlugin.ReasonerFamily;
	}

	@Override
	public Map<String, ConfigurationOption> getTranslatorConfigurationOptions() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean configure(ConfigurationItem configItem) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean configure(List<ConfigurationItem> configItems) {
		// TODO Auto-generated method stub
		return false;
	}

	public String prepareQuery(OntModel model, String queryStr)	throws InvalidNameException {
		// TODO Auto-generated method stub
		return null;
	}

	public List<ConfigurationItem> discoverOptimalConfiguration(
			String translationFolder, String modelName,
			IConfigurationManager configMgr, List<Query> queries)
			throws FunctionNotSupportedException, ConfigurationException,
			TranslationException {
		// TODO Auto-generated method stub
		return null;
	}

	public String translateAndSaveRules(OntModel model, List<Rule> ruleList,
			String modelName) throws TranslationException {
		setTheModel(model);
		setModelName(modelName);
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<ModelError> validateRule(Rule rule) {
		// TODO Auto-generated method stub
		return null;
	}

	public String toString(String ruleName, List<GraphPatternElement> ifs, List<GraphPatternElement> thens) {
		StringBuilder sb = new StringBuilder("");
		//sb.append(ruleName);
		//sb.append(": ");
		
		for (int i = 0; thens != null && i < thens.size(); i++) {
			//GraphPatternElement gpe = thens.get(i);
			sb.append(formatConstruct(thens.get(i).toFullyQualifiedString()));
		}
		
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			if (i == 0) {
				sb.append(" :- ");				
			}
			if (i > 0) sb.append(" , ");
			String ifsStr = ifs.get(i).toFullyQualifiedString();
			// find all negated variables
			int sIndex = ifsStr.indexOf("(not(", 0);
			int prevIndex = 0;
			while(sIndex != -1){
				int eIndex = ifsStr.indexOf(")", sIndex + 5);
				String var = ifsStr.substring(sIndex+5, eIndex);
				String negStr = ifsStr.substring(sIndex,eIndex+2);
				String tempStr = ifsStr.substring(0,sIndex);
				//System.out.println(tempStr);
				//int prevHoldsIndex = tempStr.lastIndexOf("(holds(");
				int prevHoldsIndex = getPrevPredIndex(tempStr);
				//System.out.println(prevHoldsIndex);
				String firstStr = ifsStr.substring(0,prevHoldsIndex+1);
				String secStr = ifsStr.substring(prevHoldsIndex+1,ifsStr.length());
				String newStr = (firstStr + "\\+" + secStr).replace(negStr,"").replace(","+var+")", ")");
				ifsStr = newStr;
				sIndex = ifsStr.indexOf("(not(", 0);
			}
			
			sb.append(formatConstruct(ifsStr));
			//sb.append(ifsStr.replace(") and (",") , (").replace(" and ","").replaceAll("\\([\\(]+","(").replace("(holds(", "holds(").replace("(\\+holds(", "\\+holds(").replaceAll("\\)[\\)]+",")"));
			
		}
		sb.append(".");
		return sb.toString();
	}
	
	protected int getPrevPredIndex(String str){
		int index = -1;
		for (String builtin: builtinList){
			int lastIndex = str.lastIndexOf("(" + builtin + "(");
			if ( lastIndex > index)
				index = lastIndex;
		}
		
		return index;
	}
	
	protected String formatConstruct(String construct){
		//TODO need to change this once intermediate format is fixed
		String temp = construct.replace(") and (",") , (").replace(" and ","").replaceAll("\\([\\(]+","(").replaceAll("\\)[\\)]+",")");
		
		for (String builtin: builtinList){
			temp = temp.replace("(" + builtin + "(", builtin + "(").replace("(\\+" + builtin + "(", "\\+" + builtin + "(");
		}
		
		// put URIs in quotes
		int sIndex = temp.indexOf("http://", 0);

		while(sIndex != -1){
			int eIndex = getURIEIndex(temp,sIndex);
			String uri = temp.substring(sIndex, eIndex);
			String prevStr = temp.substring(0,sIndex);
			String nextStr = temp.substring(eIndex);
			String newStr = (prevStr + "'" + uri + "'" + nextStr);
			temp = newStr;
			sIndex = temp.indexOf("http://", eIndex);
		}
		return temp.replace("\"'", "'").replace("'\"", "'").replace("\"", "'");
	}
	
	protected int getURIEIndex(String str, int sIndex){
		char[] strArray = str.toCharArray();
		for (int index=sIndex+5; index < strArray.length; index++){
			if (Character.isLetter(strArray[index]) || Character.isDigit(strArray[index]))
				continue;
			if (strArray[index] == '/' || strArray[index] == '#' || strArray[index] == '.' || strArray[index] == '-' || strArray[index] == '_')
				continue;
			
			return index;
		}
		
		return str.length();
	}
	
	protected List<ModelError> addError(String msg, ErrorType level) {
		if (errors == null) {
			errors = new ArrayList<ModelError>();
		}
		errors.add(new ModelError(msg, level));
		return errors;
	}
	
	protected String createDerivedFilename(String filename, String newext) {
		int lastDot = filename.lastIndexOf('.');
		if (lastDot > 0) {
			return filename.substring(0, lastDot + 1) + newext;
		}
		return filename + "." + newext;
	}
	
	protected String getStandardDeclarations(){
		StringBuilder sb = new StringBuilder("% loading semantic web libraries\n");
		sb.append(":- use_module(library(semweb/rdf_db)).\n");
		sb.append(":- use_module(library(semweb/rdfs)).\n");
		sb.append(":- rdf_load(library(semweb/rdfs)).\n\n");
		
		sb.append("%:- style_check(-atom).\n\n");
		
		sb.append("% defining properties of predicates\n");

		sb.append("% properties of holds and derive are fixed as below\n");
		sb.append(":- dynamic holds/5.\n");
		sb.append(":- dynamic holds/4.\n");
		sb.append(":- dynamic holds/3.\n");
		sb.append(":- dynamic holds/2.\n");
		sb.append(":- dynamic holds/1.\n\n");
		
		sb.append(":- multifile holds/5.\n");
		sb.append(":- multifile holds/4.\n");
		sb.append(":- multifile holds/3.\n");
		sb.append(":- multifile holds/2.\n");
		sb.append(":- multifile holds/1.\n\n");
		
		sb.append(":- discontiguous holds/5.\n");
		sb.append(":- discontiguous holds/4.\n");
		sb.append(":- discontiguous holds/3.\n");
		sb.append(":- discontiguous holds/2.\n");
		sb.append(":- discontiguous holds/1.\n\n");
		
		sb.append(":- dynamic derive/5.\n");
		sb.append(":- dynamic derive/4.\n");
		sb.append(":- dynamic derive/3.\n");
		sb.append(":- dynamic derive/2.\n");
		sb.append(":- dynamic derive/1.\n\n");
		
		sb.append(":- multifile derive/5.\n");
		sb.append(":- multifile derive/4.\n");
		sb.append(":- multifile derive/3.\n");
		sb.append(":- multifile derive/2.\n");
		sb.append(":- multifile derive/1.\n\n");
		
		sb.append(":- discontiguous derive/5.\n");
		sb.append(":- discontiguous derive/4.\n");
		sb.append(":- discontiguous derive/3.\n");
		sb.append(":- discontiguous derive/2.\n");
		sb.append(":- discontiguous derive/1.\n\n");
		    
		return sb.toString();
		    
	}
	
	private String getRunService() {
		StringBuilder sb = new StringBuilder("% start Prolog Service\n");
		sb.append(":- style_check(-singleton).\n");
		sb.append(":- consult('swi-standard-declarations.pl').\n\n\n");

		sb.append("load_pl_file(X) :- consult(X).\n");
		sb.append("unload_pl_file(X) :- unload_file(X).\n\n");

		sb.append("load_rdf_file(X) :- rdf_load(X).\n");
		sb.append("unload_rdf_file(X) :- rdf_unload(X).\n\n");

		sb.append("load_into_db(X) :- assertz(X).\n");
		sb.append("load_into_db_beginning(X) :- asserta(X).\n\n");
		
		sb.append("retract_all(X) :- retractall(X).\n\n\n");
		sb.append("retract_once(X) :- once(retract(X)).\n\n\n");

		sb.append("holds(P,S,O) :- rdf(S,P,O).\n\n\n");
		
		sb.append("pred_defined(Pred) :-  \n");
		sb.append("    findall(X,(predicate_property(X1,visible),term_to_atom(X1,X)),XL),\n");
		sb.append("    member(E,XL),\n");
		sb.append("    concat(Pred,'(',PredStr),\n");
		sb.append("    sub_string(E,0,L,A,PredStr), !.\n\n\n");
		
		sb.append(":- consult('swi-custom-predicates.pl').\n\n\n");

		sb.append("%%%%%%%%%%%%%%%\n");
		sb.append("% start service\n");
		sb.append("%%%%%%%%%%%%%%%\n");
		sb.append(":- consult('swi-prolog-service.pl').\n");
		sb.append(":- query:port_number(X), server(X), !.\n");

		return sb.toString();
	}

	private String getService() {
		StringBuilder sb = new StringBuilder("% script to start Prolog service\n");
		sb.append(":- module(query, [ server/1]).\n\n");

		sb.append(":- use_module(library(http/httpd)).\n");
		sb.append(":- use_module(library(http/thread_httpd)).\n");
		sb.append(":- use_module(library(http/http_dispatch)).\n");
		sb.append(":- use_module(library(http/http_parameters)).\n");
		sb.append(":- use_module(library(http/http_mime_plugin)).\n");
		sb.append(":- use_module(library(http/http_client)).\n");
		sb.append(":- use_module(library(http/html_write)).\n");
		sb.append(":- use_module(library(lists)).\n");
		sb.append(":- use_module(library(http/js_write)).\n");
		sb.append(":- use_module(library(sgml)).\n");
		sb.append(":- use_module(library(http/http_session)).\n");
		sb.append(":- use_module(library(thread_pool)).\n");
		sb.append(":- use_module(library(broadcast)).\n");
		sb.append(":- use_module(library(http/json)).\n");
		sb.append(":- use_module(library(http/json_convert)).\n");
		sb.append(":- use_module(library(http/http_json)).\n\n\n");


		sb.append(":- thread_pool_create(compute, 3,\n");
		sb.append("                      [ local(20000), global(100000), trail(50000),\n");
		sb.append("                        backlog(5)\n");
		sb.append("                      ]).\n\n\n");


		sb.append(":- http_handler(root(reasoning_server), reasoning_server, [spawn(compute)]).\n");
		sb.append(":- http_handler(root(result),result,[spawn(compute)]).\n");
		sb.append(":- http_handler(root(predefinedResult),predefinedResult,[spawn(compute)]).\n");
		sb.append(":- http_handler(root(image),image,[spawn(compute)]).\n\n");

		sb.append(":- listen(http_session(end(SessionId, Peer)),\n");
		sb.append("          end_session(SessionId)).\n\n\n");
		          

		sb.append(":- consult('temp/prolog-service-config/prolog-service-config.pl').\n\n");
		          
		sb.append("end_session(SessionId) :- tmp_dir(Dir),\n");
		sb.append("                          atomic_list_concat([Dir,'temp-',SessionId,'.pl'],'',File),\n");
		sb.append("                          unload_file(File),\n");
		sb.append("                          (exists_file(File) -> delete_file(File)).\n\n");
		                          
		sb.append("get_session_file(File) :- tmp_dir(Dir),\n");
		sb.append("                          http_session_id(SessionId),\n");
		sb.append("                          atomic_list_concat([Dir,'temp-',SessionId,'.pl'],'',File).\n\n");
		                          
		sb.append("get_session_file(SessionId,File) :- tmp_dir(Dir),\n");
		sb.append("                                    atomic_list_concat([Dir,'temp-',SessionId,'.pl'],'',File).\n\n\n");
		                          
		          
		sb.append("%:- http_set_session_options([path(reasoning_server)]).\n\n");

		sb.append("server(Port) :-\n");
		sb.append("        http_server(http_dispatch, [port(Port)]).\n\n");


		sb.append("%response(Request) :-  thread_create(result(Request),Id,[detached(true)]).\n");
		sb.append("% result(Request) :- write(user_output,'result request received\\n'), (   memberchk(method(post), Request),\n");
		sb.append("%             http_read_data(Request, Parts, [form_data(mime)]),\n");
		sb.append("%             member(mime(Attributes, Data, []), Parts)\n");
		sb.append("%         ->  % process file here; this demo just prints the info gathered\n");
		sb.append("%             reply(Request,Data)\n");
		sb.append("%         ;   throw(http_reply(bad_request(bad_data)))\n");
		sb.append("%         ).\n\n");
		        
		sb.append("result(Request) :- write(user_output,'result request received\\n'), (   (memberchk(method(post), Request),\n");
		sb.append("            http_read_data(Request, Data, []), write(user_output,Data), write(user_output,'\\n'))\n");
		sb.append("            %member(mime(Attributes, Data, []), Parts)\n");
		sb.append("        ->  % process file here; this demo just prints the info gathered\n");
		sb.append("            (reply(Request,Data))\n");
		sb.append("        ;   (throw(http_reply(bad_request(bad_data))))\n");
		sb.append("        ).\n\n");
		        
		sb.append("predefinedResult(Request) :- write(user_output,'Predefined query invoked\\n'), (   memberchk(method(post), Request),\n");
		sb.append("            http_read_data(Request, Data, []), write(user_output,Data), getCorrespondingQuery(Data,Query)\n");
		sb.append("            %member(mime(Attributes, Data, []), Parts)\n");
		sb.append("        ->  % process file here; this demo just prints the info gathered\n");
		sb.append("            reply(Request,Query)\n");
		sb.append("        ;   throw(http_reply(bad_request(bad_data)))\n");
		sb.append("        ).\n\n");

		sb.append("handle_json_request(Request) :-\n");
		sb.append("      %write(user_output,Request),\n");
		sb.append("      write(user_output,'reading request\\n'),\n");
		sb.append("      http_read_json(Request, JSONIn),\n");
		sb.append("      write(user_output,JSONIn),\n");
		sb.append("      json_to_prolog(JSONIn, PrologIn),\n");
		sb.append("%       <compute>(PrologIn, PrologOut),           % application body\n");
		sb.append("      prolog_to_json(PrologIn, JSONOut),\n");
		sb.append("      reply_json(JSONOut).\n\n");
		      
		sb.append("reasoning_server(Request) :-\n");
		sb.append("  write(user_output,'reasoning server request received\\n'),\n");
		sb.append("%   writeq(user_output,Request),\n");
		sb.append("  write(user_output,'Request written\\n'),\n");
		sb.append("  handle_json_request(Request).\n\n");
		                   
		sb.append("reply(Request,[query=Query]) :- http_session_id(SessionId),\n");
		sb.append("                      get_session_file(SessionId,File),\n");
		sb.append("                      open(File,write,Stream),\n");
		sb.append("                      atomic_list_concat([':- module(\\'',SessionId,'\\',[]).'],'',Module),\n");
		sb.append("                      write(Stream,Module),nl(Stream),\n");
		sb.append("                      %term_to_atom(Data,DataT),\n");
		sb.append("                      %sub_string(DataT,7,Len,1,Query),\n");
		sb.append("                      write(user_output,'writing query \\n'),\n");
		sb.append("                      write(user_output,Query),\n");
		sb.append("                      write(user_output,'\\n'),\n");
		sb.append("                      %sub_string(Query,1,Len1,1,QueryToWrite),\n");
		sb.append("                       write(Stream,Query),\n");
		sb.append("                       nl(Stream),\n");
		sb.append("                       close(Stream),\n");
		sb.append("                       consult(File),\n");
		sb.append("                       %atomic_list_concat(['\\'',SessionId,'\\''],'',SessionIdStr),\n");
		sb.append("                       findall(L,SessionId:qresult(L),LL),\n");
		sb.append("                       %writeq(user_output,LL), write(user_output,'\\n'),\n");
		sb.append("                       %qresult(LL),\n");
		sb.append("                       SessionId:targetVar(Var),\n");
		sb.append("                       list_results(Request,Var,LL).\n");
		sb.append("                       %close(LogStream).\n\n\n");
		                       
		                       
		sb.append(":- multifile prolog:message//1.\n\n");

		sb.append("prolog:message(bad_data) -->\n");
		sb.append("        [ 'Please enter a query.'\n");
		sb.append("        ].\n\n\n");




		sb.append("%       Create a table of all available modules with their source-file\n\n");

		sb.append("list_results(Request,Var,RL) :-\n");
		sb.append("        reply_html_page(title('Query Results'),\n");
		sb.append("                        [ h1('Query Results'),\n");
		sb.append("                          table([class=table2],[ \\header(Var)\n");
		sb.append("                                | \\results(RL)\n");
		sb.append("                                ])\n");
		sb.append("                        ]).\n\n\n");


		sb.append("header(Var) -->\n");
		sb.append("            html(tr([\\head_elements(Var)])).\n\n");

		sb.append("head_elements([]) --> [].\n");
		sb.append("head_elements([H|T]) -->\n");
		sb.append("      html(th(H)), head_elements(T).\n\n\n\n");




		sb.append("results([]) --> [].\n");
		sb.append("results([H|T]) -->\n");
		sb.append("            %{write(user_output,'writing '),write(user_output,H),write(user_output,'\\n')},\n");
		sb.append("            html(tr([\\result_elements(H)])),\n");
		sb.append("            results(T).\n\n\n");



		sb.append("result_elements([]) --> [].\n");
		sb.append("result_elements([H|T]) -->\n");
		sb.append("                   %{write(user_output,'writing '),write(user_output,H),write(user_output,'\\n')},\n");
		sb.append("                   html(td(H)),result_elements(T).\n");
		return sb.toString();
	}

	public void setImportOrder(List<String> orderedImports){
		importOrder = orderedImports;
	}
	
	public List<String> getImportOrder(){
		return importOrder;
	}

	private String graphPatternElementsToPrologRuleString(List<GraphPatternElement> elements, RulePart rulePart) throws TranslationException {
		int cnt = 0;
		if (elements != null && elements.size() > 0) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; elements != null && i < elements.size(); i++) {
				if (cnt > 0) sb.append(", ");
				SpecialBuiltin spb = processSpecialBuiltins(elements, i);	// check for special handling required for some built-ins
				if (spb != null) {
					// get the triple in question
					TripleElement trel = null;
					if (elements.get(i) instanceof TripleElement) {
						trel = (TripleElement)elements.get(i);
					}
					else {
						logger.error("Unhandled graph pattern element detected as special builtin: " + elements.get(i).toString());
					}
					
					// translate based on type of spb
					if (spb.equals(SpecialBuiltin.NOVALUE)) {
						sb.append(createNoValue(trel, TranslationTarget.RULE_BUILTIN));
					}
					else if (spb.equals(SpecialBuiltin.ISKNOWN)) {
						sb.append(graphPatternElementToPrologRuleString(trel, rulePart));
						sb.append(", ");
						sb.append("bound(" + nodeToString(trel.getObject(), TranslationTarget.RULE_BUILTIN) + ")");
					}
					else  {
						if (spb.equals(SpecialBuiltin.NOVALUESPECIFIC)) {
							sb.append(createNoValueSpecific(trel, TranslationTarget.RULE_BUILTIN));
						}
						else if (spb.equals(SpecialBuiltin.NOTONLY)) {
							sb.append(createNotOnly(trel, TranslationTarget.RULE_BUILTIN));
						}
						else if (spb.equals(SpecialBuiltin.ONLY)) {
							sb.append(createOnly((TripleElement)elements.get(i), TranslationTarget.RULE_BUILTIN));
						}
						else {
							logger.error("Unhandled special builtin: " + elements.toString());
						}
					}
				}
				else {
					sb.append(graphPatternElementToPrologRuleString(elements.get(i), rulePart));
				}
				cnt++;
			}
			return sb.toString();
		}
		return null;
	}

	/**
	 * Convert GraphPatternElement to String in the context of a Rule
	 * 
	 * @param gpe
	 * @return
	 * @throws TranslationException
	 */
		private String graphPatternElementToPrologRuleString(GraphPatternElement gpe, RulePart rulePart) throws TranslationException {
			StringBuilder sb = null;
			if (gpe instanceof TripleElement) {
				if (!((TripleElement)gpe).getModifierType().equals(TripleModifierType.None)) {
					sb = new StringBuilder();
					TripleModifierType type = ((TripleElement)gpe).getModifierType();
					if (type.equals(TripleModifierType.Not)) {
						sb.append("\\+");
						sb.append(tripleElementToRawPrologString((TripleElement) gpe, TranslationTarget.RULE_TRIPLE, rulePart, null));
					}
					else {
						throw new TranslationException("Unhandled triple pattern: " + gpe.toFullyQualifiedString());
					}
//					else if (type.equals(TripleModifierType.Only)) {
//						sb.append("notOnlyValue(");
//					}
//					else {
//						sb.append("noValueOtherThan(");
//					}
//					sb.append(nodeToString(((TripleElement)gpe).getSubject(),TranslationTarget.RULE_BUILTIN));
//					sb.append(", ");
//					Node pn = ((TripleElement)gpe).getPredicate();
//					checkPredicateSpecial(pn);
//					sb.append(nodeToString(pn, TranslationTarget.RULE_BUILTIN));
//					if (!(((TripleElement)gpe).getObject() instanceof KnownNode)) {
//						sb.append(", ");
//						sb.append(nodeToString(((TripleElement)gpe).getObject(), TranslationTarget.RULE_BUILTIN));
//					}
//					sb.append(")");
				}
				else {
					sb = tripleElementToRawPrologString((TripleElement) gpe, TranslationTarget.RULE_TRIPLE, rulePart, null);  // 2/16/2011 false); 
				}
			}
			else if (gpe instanceof BuiltinElement) {
				sb = new StringBuilder();
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				String biname = builtinTypeToString((BuiltinElement)gpe);
				if (((BuiltinElement)gpe).getFuncType().equals(BuiltinType.UserAdded)) {
					String err = validateBuiltin((BuiltinElement)gpe, args);
					if (err != null) {
						addError(getModelName() + ": Built-in '" + biname + "/" + args.size() + "' is not found in the loaded Prolog predicates", ErrorType.ERROR);
					}
					sb.append(biname);
					sb.append("(");
					for (int i = 0; args != null && i < args.size(); i++) {
						if (i > 0) sb.append(", ");
						sb.append(nodeToString(args.get(i), TranslationTarget.RULE_BUILTIN));
					}
					sb.append(")");
				}
				else if (((BuiltinElement)gpe).getFuncType().equals(BuiltinType.BuiltinFunction)) {
					int arity = functionListArity1.contains(biname) ? 1 : 2;
					if (arity >= args.size()) {
						addError("Built-in '" + biname + "' doesn't appear to have the expected number of arguments", ErrorType.ERROR);
					}
					sb.append(nodeToString(args.get(args.size() - 1), TranslationTarget.RULE_BUILTIN));
					sb.append(" is ");
					sb.append(narityToBinary(biname, args, 0, args.size() - 1));
				}
				else if (((BuiltinElement)gpe).getArguments().size() > 2) {
					sb.append(nodeToString(args.get(((BuiltinElement)gpe).getArguments().size() - 1), TranslationTarget.RULE_BUILTIN));
					sb.append(" is ");
					if (((BuiltinElement)gpe).getArguments().size() == 3) {
						sb.append(nodeToString(args.get(0), TranslationTarget.RULE_BUILTIN));
						sb.append(" ");
						sb.append(biname);
						sb.append(" ");
						sb.append(nodeToString(args.get(1), TranslationTarget.RULE_BUILTIN));
					}
					else {
//						throw new TranslationException("Unhandled BuiltinElement: " + gpe.toFullyQualifiedString());
						addError("Unhandled BuiltinElement: " + gpe.toFullyQualifiedString(), ErrorType.ERROR);

					}
				}
				else if (((BuiltinElement)gpe).getFuncType().equals(BuiltinType.Equal)) {
					Node arg0 = args.get(0);
					Node arg1 = args.get(1);
					if (arg1 instanceof NamedNode) {
						if (((NamedNode)arg1).getNodeType().equals(NodeType.ClassNode)) {
							sb.append("rdf( ");
							sb.append("http://www.w3.org/1999/02/22-rdf-syntax-ns#type', ");
							sb.append(nodeToString(arg0, TranslationTarget.RULE_BUILTIN));
							sb.append(", ");
							sb.append(nodeToString(arg1, TranslationTarget.RULE_BUILTIN));
							sb.append(")");
						}
						else {
							sb.append(nodeToString(arg0, TranslationTarget.RULE_BUILTIN));
							sb.append(" == ");
							sb.append(nodeToString(arg1, TranslationTarget.RULE_BUILTIN));
						}
					}
					else if (arg1 instanceof Literal) {
						sb.append(nodeToString(arg0, TranslationTarget.RULE_BUILTIN));
						sb.append(" == ");
						sb.append(nodeToString(arg1, TranslationTarget.RULE_BUILTIN));
					}
					else {
						addError("BuiltinType is equal but not the expected variable and class ", ErrorType.ERROR);
					}
				}
				else if (((BuiltinElement)gpe).getArguments().size() == 2) {
					sb.append(nodeToString(args.get(0), TranslationTarget.RULE_BUILTIN));
					sb.append(" ");
					sb.append(biname);
					sb.append(" ");
					sb.append(nodeToString(args.get(1), TranslationTarget.RULE_BUILTIN));
				}
				else if (((BuiltinElement)gpe).getFuncType().equals(BuiltinType.Not)) {
					sb.append("\\+");
					sb.append(" ");
					Node arg = args.get(0);
					if (arg instanceof ProxyNode) {
						Object pfor = ((ProxyNode)arg).getProxyFor();
						if (pfor instanceof GraphPatternElement) {
							sb.append(graphPatternElementToPrologRuleString((GraphPatternElement)pfor, rulePart));
						}
						else {
							throw new TranslationException("Non-graph element proxy-for in ProxyNode '" + arg.toFullyQualifiedString() + "'");
						}
					}
					else {
						sb.append(nodeToString(args.get(0), TranslationTarget.RULE_BUILTIN));
					}
				}
				else {
//					throw new TranslationException("Unary BuiltinElement: " + gpe.toFullyQualifiedString());
					addError("Unhandled Unary BuiltinElement: " + gpe.toFullyQualifiedString(), ErrorType.ERROR);

				}
			}
			else if (gpe instanceof Junction) {
				sb = new StringBuilder();
				String joiner;
				JunctionType jtype = ((Junction)gpe).getJunctionType();
				if (jtype.equals(JunctionType.Conj)) {
					joiner = ", ";
				}
				else {
					joiner = "; ";
				}
				Object lhs = ((Junction)gpe).getLhs();
				if (lhs instanceof List<?>) {
					sb.append(graphPatternElementsToPrologRuleString((List<GraphPatternElement>)lhs, rulePart));
				}
				else if (lhs instanceof GraphPatternElement) {
					sb.append(graphPatternElementToPrologRuleString((GraphPatternElement) lhs, rulePart));
				}
				else {
					throw new TranslationException("Unexpected junction lhs type: " + lhs.getClass());
				}
				
				sb.append(joiner);
				
				Object rhs = ((Junction)gpe).getRhs();
				if (rhs instanceof List<?>) {
					sb.append(graphPatternElementsToPrologRuleString((List<GraphPatternElement>)rhs, rulePart));
				}
				else if (rhs instanceof GraphPatternElement) {
					sb.append(graphPatternElementToPrologRuleString((GraphPatternElement) rhs, rulePart));
				}
				else {
					throw new TranslationException("Unexpected junction rhs type: " + rhs.getClass());					
				}
			}
			else if (gpe instanceof NegatedExistentialQuantifier) {
				throw new TranslationException("Negated existential quantification not yet suppported.");
			}
			else {
				throw new TranslationException("GraphPatternElement '" + gpe.toString() + "' cannot be translated to a Prolog rule.");
			}
			return sb.toString();
		}
		
		private Object narityToBinary(String biname, List<Node> args, int istart, int iend) throws TranslationException {
			StringBuilder sb = new StringBuilder(biname);
			sb.append("(");
			for (int i = istart; args != null && i < istart + 2; i++) {
				if (i > istart) sb.append(", ");
				if (i > istart && i < iend - 1) {
					sb.append(narityToBinary(biname, args, istart + 1, iend));
				}
				else {
					sb.append(nodeToString(args.get(i), TranslationTarget.RULE_BUILTIN));
				}
			}
			sb.append(")");
			return sb.toString();
		}

		/**
		 * Method to convert a TripleElement to a Prolog String without delimiters
		 * 
		 * @param gpe
		 * @param sbfilter 
		 * @return
		 * @throws TranslationException
		 */
		private StringBuilder tripleElementToRawPrologString(TripleElement gpe, TranslationTarget target, RulePart rulePart, StringBuilder sbfilter) throws TranslationException {
			StringBuilder sb = new StringBuilder();
			Node subj = gpe.getSubject();
			Node pred = gpe.getPredicate();
			checkPredicateSpecial(pred);
			Node obj = gpe.getObject();
			boolean moveObjectToEqualityTest = false;
			if (target.equals(TranslationTarget.RULE_TRIPLE)) {
				sb.insert(0, "holds(");
			}
			sb.append(nodeToString(pred, target));
			sb.append(", ");
			sb.append(nodeToString(subj, target));
			sb.append(", ");
			String newVar = null;
			if (rulePart.equals(RulePart.PREMISE) && target.equals(TranslationTarget.RULE_TRIPLE) && tripleHasDecimalObject(gpe)) {
				// this would be a triple match on a float or double value, which is not reliable
				//	move the object to a separate equality test
				moveObjectToEqualityTest = true;
				newVar = "PV" + getNewVariableForRule();
				if (obj instanceof VariableNode && isDatatypePropertyWithXSDRange(gpe)) {
					String littype = getNewPrologOnlyVariableForRule();
					String litval = getNewPrologOnlyVariableForRule();
					sb.append("literal(type(");
					sb.append(littype);
					sb.append(",");
					sb.append(litval);
					sb.append("))),");
					if (tripleHasNumericObject(gpe)) {
						sb.append(" atom_number(");
						sb.append(litval);
						sb.append(",");
					}
				}
				sb.append(newVar);
			}
			else {
				if (rulePart.equals(RulePart.NOT_A_RULE)) {
					if (ITranslator.isKnownNode(obj)) {
						newVar = "PV" + getNewVariableForQuery();
						if (gpe.getModifierType().equals(TripleModifierType.Not)) {
							sb.append(newVar);
							sb.insert(0, "OPTIONAL {");
							sb.append("}");
							if (sbfilter != null) {
								if (sbfilter.length() > 0) {
									sbfilter.append(" && ");
								}
								sbfilter.append("!bound(");
								sbfilter.append(newVar);
								sbfilter.append(")");
							}
						}
						else {
							sb.append(newVar);
						}
					}
					else if (tripleHasDecimalObject(gpe)) {
						newVar = "PV" + getNewVariableForQuery();
							sb.append(newVar);
							if (sbfilter != null) {
							if (sbfilter.length() > 0) {
								sbfilter.append(" && ");
							}
							sbfilter.append(newVar);
							if (gpe.getModifierType().equals(TripleModifierType.Not)) {
								sbfilter.append(" != ");
							}
							else {
								sbfilter.append(" = ");
							}
							sbfilter.append(nodeToString(obj, TranslationTarget.RULE_BUILTIN));
						}
						else {
							sb.append(newVar);
						}
					}
					else {
						sb.append(nodeToString(obj, target));
					}
				}
				else {
					if (rulePart.equals(RulePart.PREMISE) && target.equals(TranslationTarget.RULE_TRIPLE) && obj instanceof VariableNode && isDatatypePropertyWithXSDRange(gpe)) {
						String littype = getNewPrologOnlyVariableForRule();
						String litval = getNewPrologOnlyVariableForRule();
						sb.append("literal(type(");
						sb.append(littype);
						sb.append(",");
						sb.append(litval);
						sb.append("))),");
						if (tripleHasNumericObject(gpe)) {
							sb.append(" atom_number(");
							sb.append(litval);
							sb.append(",");
						}
					}
					sb.append(nodeToString(obj, target));
				}
			}
			if (target.equals(TranslationTarget.RULE_TRIPLE)) {
				sb.append(")");
			}
			else {
				// this is a query
				if (gpe.getModifierType() != null && gpe.getModifierType().equals(TripleModifierType.Not)) {
					// this is negation--translate into a filter on !exits
					sb.insert(0, "!EXISTS { ");
					sb.append(" }");
					sbfilter.append(sb);
					sb.setLength(0);
				}
			}
			if (moveObjectToEqualityTest) {
				// now add the equality test. (this is only for rules)
				sb.append(", ");
				sb.append(newVar);
				sb.append(" == ");
				sb.append(nodeToString(obj, TranslationTarget.RULE_BUILTIN));
			}
			return sb;
		}
		
	/**
	 * Look for special built-ins and if found process them, modifying the element list as needed.
	 * @param elements
	 * @param index
	 * @return
	 */
	private SpecialBuiltin processSpecialBuiltins(List<GraphPatternElement> elements, int index) {
		if (elements.get(index) instanceof TripleElement) {
			if (!((TripleElement)elements.get(index)).getModifierType().equals(TripleModifierType.None)) {
				TripleElement trel = (TripleElement)elements.get(index);
				if (trel.getModifierType().equals(TripleModifierType.Not)) {
					if (ITranslator.isKnownNode(trel.getObject())) {
						return SpecialBuiltin.NOVALUE;
					}
					else {
						return SpecialBuiltin.NOVALUESPECIFIC;
					}
				}
				else if (trel.getModifierType().equals(TripleModifierType.NotOnly)) {
					return SpecialBuiltin.NOTONLY;
				}
				else if (trel.getModifierType().equals(TripleModifierType.Only)) {
					return SpecialBuiltin.ONLY;
				}
			}
			else if (elements.size() > (index + 1) && elements.get(index + 1) instanceof BuiltinElement) {
				// these special builtins will be of the form:
				//		x predicate y, op(y,z)
				//  or in other words, the first argument of the operation will be the object of the triple
				//	(is that restrictive enough??)
				BuiltinElement be = (BuiltinElement) elements.get(index + 1);
				BuiltinType bt = be.getFuncType();
				List<Node> args = be.getArguments();
				
				Node biarg1 = (args.size() > 0) ? args.get(0) : null;			// builtin 0th argument node
				Node trobj = ((TripleElement)elements.get(index)).getObject();	// triple object node
				if (biarg1 != null && trobj != null && biarg1 instanceof NamedNode && trobj instanceof NamedNode
						&& ((NamedNode)biarg1).getName().equals(((NamedNode)trobj).getName())) {	
					if (bt.equals(BuiltinType.NotEqual) && args.size() == 2) {
						Node arg2 = args.get(1);
						if (ITranslator.isKnownNode(arg2)) {
							// this case: (x pred y), !=(y, known)
							// 	just drop the i+1 builtin
							elements.remove(index + 1);
							return SpecialBuiltin.NOVALUE;
						}	
						else {
							// this case: (x pred y), !=(y, z)
							//	move the z to the object of the triple and drop the i+1 builtin
							if (args.size() > 1) {
								Node biarg2 = args.get(1);
								Node trsubj = ((TripleElement)elements.get(index)).getSubject();
								if (biarg2 instanceof NamedNode && !(biarg2 instanceof VariableNode) && trsubj instanceof NamedNode &&
										!(((NamedNode)biarg2).getName().equals(((NamedNode)trsubj).getName()))) {
									((TripleElement)elements.get(index)).setObject(args.get(1));
									elements.remove(index + 1);
									return SpecialBuiltin.NOVALUESPECIFIC;
								}
							}
						}
					}
					else if (bt.equals(BuiltinType.NotOnly)) {
						((TripleElement)elements.get(index)).setObject(args.get(1));
						elements.remove(index + 1);
						return SpecialBuiltin.NOTONLY;
					}
					else if (bt.equals(BuiltinType.Only)) {
						((TripleElement)elements.get(index)).setObject(args.get(1));
						elements.remove(index + 1);
						return SpecialBuiltin.ONLY;
					}
				}
			}
			else if (ITranslator.isKnownNode(((TripleElement)elements.get(index)).getObject())) {
				Node var = new VariableNode("v" + System.currentTimeMillis());
				((TripleElement)elements.get(index)).setObject(var);
				return SpecialBuiltin.ISKNOWN;
			}
		}
		return null;
	}
	
	private String builtinTypeToString(BuiltinElement bin) throws TranslationException {
		BuiltinType ftype = bin.getFuncType();
		String builtinName = null;
		if (ftype.equals(BuiltinType.Divide)) {
			builtinName = "/";
		}
		else if (ftype.equals(BuiltinType.Equal)) {
			builtinName = "==";
		}
		else if (ftype.equals(BuiltinType.GT)) {
			builtinName = ">";
		}
		else if (ftype.equals(BuiltinType.GTE)) {
			builtinName = ">=";
		}
		else if (ftype.equals(BuiltinType.LT)) {
			builtinName = "<";
		}
		else if (ftype.equals(BuiltinType.LTE)) {
			builtinName = "=<";
		}
		else if (ftype.equals(BuiltinType.Minus)) {
			builtinName = "-";
		}
		else if (ftype.equals(BuiltinType.Modulus)) {
			builtinName = "mod";
		}
		else if (ftype.equals(BuiltinType.Multiply)) {
			builtinName = "*";
		}
		else if (ftype.equals(BuiltinType.Negative)) {
			builtinName = "-";
		}
		else if (ftype.equals(BuiltinType.Not)) {
			builtinName = "\\+";
		}
		else if (ftype.equals(BuiltinType.NotEqual)) {
			builtinName = "\\==";
		}
		else if (ftype.equals(BuiltinType.NotOnly)) {
			throw new TranslationException("notOnlyValue not supported in Prolog");
		}
		else if (ftype.equals(BuiltinType.Only)) {
			throw new TranslationException("noValuesOtherThan not supported in Prolog");
		}
		else if (ftype.equals(BuiltinType.Plus)) {
			builtinName = "+";
		}
		else if (ftype.equals(BuiltinType.Power)) {
			builtinName = "**";
		}
		else if (ftype.equals(BuiltinType.Assign)) {
			builtinName = "is";
		}
		else if (isBuiltinFunction(bin.getFuncName())) {
			builtinName = bin.getFuncName();
			bin.setFuncType(BuiltinType.BuiltinFunction);
		}
		else if (ftype.equals(BuiltinType.UserAdded)) {
			// this is good
			builtinName = bin.getFuncName();
		}
		else {
			logger.warn("Something went wrong finding/loading Builtin '" + bin.getFuncName() + "' of type '" + ftype + "'");
			addError(getModelName() + ": Found reference to unknown built-in '" + bin.getFuncName() + "' of type '" + ftype + "'", ErrorType.WARNING);
//			throw new TranslationException("Unable to resolve built-in of type '" + ftype + "'");
			return bin.getFuncName();
		}

		return builtinName;
	}
	
	String validateBuiltin(BuiltinElement bin, List<Node> args) {
		if (functionListArity1.contains(bin.getFuncName()) || functionListArity2.contains(bin.getFuncName())) {	
			return null;
		}
		String validLookupName = bin.getFuncName() + (args != null ? args.size() : 0);
		if (validatedBuiltins != null && validatedBuiltins.contains(validLookupName)) {
			return null;
		}
		if (invalidatedBuiltins != null && invalidatedBuiltins.contains(validLookupName)) {
			return "'" + bin.getFuncName() + "/" + args.size() + "' not found in loaded Prolog functors.";
		}
		// call Prolog to find out if the predicate is defined
		String query = "select X where current_functor(" + bin.getFuncName() + ",X)";
		try {
			ResultSet rs = getReasoner().ask(query);
			while (rs != null && rs.hasNext()) {
				Object[] row = rs.next();
				if (row != null && row[0] != null) {
					if (Integer.parseInt(row[0].toString()) == args.size()) {
						if (validatedBuiltins == null) {
							validatedBuiltins = new ArrayList<String>();
						}
						validatedBuiltins.add(validLookupName);
						return null;
					}
				}
			}
			if (invalidatedBuiltins == null) {
				invalidatedBuiltins = new ArrayList<String>();
			}
			invalidatedBuiltins.add(validLookupName);
			return "'" + bin.getFuncName() + "/" + args.size() + "' not found in loaded Prolog functors.";
		} catch (Exception e) {
			return e.getMessage();
		} 
	}
	
	private String sadlVariableToPrologVariable(String varName) {
		return "PV" + varName;
	}
	
	private String nodeToString(Node node, TranslationTarget target) throws TranslationException {
		if (node instanceof NamedNode) {
			NodeType ntype = ((NamedNode)node).getNodeType();
			if (ntype.equals(NodeType.VariableNode)) {
				// double-check this; if a concept was declared after reference in a rule or query 
				//	it may have been parsed as a variable but actually be a defined concept 
				OntResource r = getTheModel().getOntResource(getModelName() + "#" + ((NamedNode)node).getName());
				if (r == null) {
					return sadlVariableToPrologVariable(((NamedNode)node).getName());
				}
				// it appears that at time of parsing of the rule or query the named concept was not defined but
				//	was subsequently. Warn user of this apparent error: concepts must be defined before they are 
				//	used in a rule or query.
				String msg = "The concept '" + ((NamedNode)node).getName() + "' ";
				if (ruleInTranslation != null) {
					msg += "in rule '" + ruleInTranslation.getRuleName() + "' ";
				}
				msg += " in model '" + getModelName() + "' is used before it is defined. Please define the concept before referencing it in a query or rule.";
				addError(msg, ErrorType.ERROR);
				logger.error(msg);
			}
			if (node instanceof RDFTypeNode) {
				return "'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'";
			}
			else {
				String nts;
				if (((NamedNode)node).getNamespace() != null) {
					String prefix = ((NamedNode)node).getPrefix();
					if (prefix != null) {
						registerPrefix(prefix, ((NamedNode)node).getNamespace());
					}
					else {
						// this must be the default namespace
						((NamedNode)node).setPrefix("");
					} 
					nts = ((NamedNode)node).getNamespace() + ((NamedNode)node).getName();
				}
				else {
					nts = ((NamedNode)node).getName();
				}
				if (nts.equals("http://www.w3.org/2000/01/rdf-schema#subClassOf")) {
					return "'http://www.w3.org/2000/01/rdf-schema#subClassOf'";
				}
				else {
					return "'" + hostToLowercase(nts) + "'";
				}
			}
		}
		else if (node instanceof Literal) {
			Object litObj = ((Literal)node).getValue();
			if (((Literal)node).getOriginalText() != null) {
				return singleQuoteString(((Literal)node).getOriginalText());
			}
			return singleQuoteString(literalValueToString(litObj, target));
		}
		else if (ITranslator.isKnownNode(node)) {
			return "PV" + getNewVariableForRule();
		}
		else if (node == null) {
			throw new TranslationException("Encountered null node in nodeToString; this indicates incorrect intermediate form and should not happen");
		}
		else {
			throw new TranslationException("Nnode '" + node.toString() + "' cannot be translated to Prolog format.");
		}
	}
	
	public static String hostToLowercase(String uri) {
		boolean uppercaseInHost = false;
		if (!uri.startsWith("http://")) {
			return uri;
		}
		int hostStart = 7; 	// http://*
		int hostEnd = 0;
		for (int i = hostStart; i < uri.length(); i++) {
			char ch = uri.charAt(i);
			if (ch == '/' || ch == '#') {
				hostEnd = i;
				break;
			}
			if (Character.isUpperCase(ch)) {
				uppercaseInHost = true;
			}
		}
		if (uppercaseInHost) {
			if (hostEnd < hostStart) {
				hostEnd = uri.length();
			}
			String host = uri.substring(hostStart, hostEnd).toLowerCase();
			return uri.substring(0, hostStart) + host + uri.substring(hostEnd);
		}
		return uri;
	}
	
	private String singleQuoteString(String strval) {
		String trimmed = strval.trim();
		if (trimmed.startsWith("\"")) {
			trimmed = "'" + trimmed.substring(1);
		}
//		else if (!trimmed.startsWith("'")){
//			trimmed = "'" + trimmed;
//		}
		if (trimmed.endsWith("\"")) {
			trimmed = trimmed.substring(0, trimmed.length() - 1) + "'";
		}
//		else if (!trimmed.endsWith("'")){
//			trimmed = trimmed + "'";
//		}
		return trimmed;
	}

	public static synchronized String literalValueToString(Object litObj, TranslationTarget target) {
		if (litObj instanceof String) {
			litObj = "'" + litObj + "'";
		}
		if (litObj instanceof String) {
			return (String)litObj;
		}
		else if (litObj instanceof Boolean) {
			if (target.equals(TranslationTarget.QUERY_TRIPLE) || target.equals(TranslationTarget.QUERY_FILTER)) {
				return "'" + litObj.toString().trim() + "'^^<http://www.w3.org/2001/XMLSchema#boolean>";
			}
			else {
				return "'" + litObj.toString().trim() + "'^^http://www.w3.org/2001/XMLSchema#boolean";
			}
		}
		else if (litObj instanceof Long) {
			if (target.equals(TranslationTarget.QUERY_FILTER) || target.equals(TranslationTarget.RULE_BUILTIN)) {
				return litObj.toString().trim();
			}
			else if (target.equals(TranslationTarget.QUERY_TRIPLE)) {
				return "'" + litObj.toString().trim() + "'^^<http://www.w3.org/2001/XMLSchema#long>";
			}
			else {
				return "'" + litObj.toString().trim() + "'^^http://www.w3.org/2001/XMLSchema#long";					
			}
		}
		else if (litObj instanceof Integer) {
			if (target.equals(TranslationTarget.QUERY_FILTER) || target.equals(TranslationTarget.RULE_BUILTIN)) {
				return litObj.toString().trim();
			}
			else if (target.equals(TranslationTarget.QUERY_TRIPLE)) {
				return "'" + litObj.toString().trim() + "'^^<http://www.w3.org/2001/XMLSchema#int>";
			}
			else {
				return "'" + litObj.toString().trim() + "'^^http://www.w3.org/2001/XMLSchema#int";
			}
		}
		else if (litObj instanceof Double) {
			if (target.equals(TranslationTarget.QUERY_FILTER) || target.equals(TranslationTarget.RULE_BUILTIN)) {
				return litObj.toString().trim();
			}
			else if (target.equals(TranslationTarget.QUERY_TRIPLE)) {
				return "'" + litObj.toString().trim() + "'^^<http://www.w3.org/2001/XMLSchema#double>";
			}
			else {
				return "'" + litObj.toString().trim() + "'^^http://www.w3.org/2001/XMLSchema#double";
			}
		}
		else if (litObj instanceof Float) {
			if (target.equals(TranslationTarget.QUERY_FILTER) || target.equals(TranslationTarget.RULE_BUILTIN)) {
				return litObj.toString().trim();
			}
			else if (target.equals(TranslationTarget.QUERY_TRIPLE)) {
				return "'" + litObj.toString().trim() + "'^^<http://www.w3.org/2001/XMLSchema#float>";
			}
			else {
				return "'" + litObj.toString().trim() + "'^^http://www.w3.org/2001/XMLSchema#float";
			}
		}
		else {
			return litObj.toString();
		}

	}
	
	private void checkPredicateSpecial(Node predNode) {
		if (predNode instanceof NamedNode) {
			if (((NamedNode)predNode).getNamespace() == null && 
					((NamedNode)predNode).getName() != null && 
					((NamedNode)predNode).getName().equals(RDFS.comment.getLocalName()) ) {
				((NamedNode)predNode).setNamespace(RDFS.getURI());
			}
		}
	}

	private String createNoValue(TripleElement trel, TranslationTarget target) throws TranslationException {
		Node arg1 = trel.getSubject();
		Node arg2 = trel.getPredicate();
		return "noValue(" + nodeToString(arg1, target) + ", " + nodeToString(arg2, target) + ")";
	}

	private Object createNoValueSpecific(TripleElement trel, TranslationTarget target) throws TranslationException {
		Node arg1 = trel.getSubject();
		Node arg2 = trel.getPredicate();
		Node arg3 = trel.getObject();
		return "noValue(" + nodeToString(arg1, target) + ", " + nodeToString(arg2, target) + ", " + nodeToString(arg3, target) + ")";
	}

	private Object createOnly(TripleElement trel, TranslationTarget target) throws TranslationException {
		Node arg1 = trel.getSubject();
		Node arg2 = trel.getPredicate();
		Node arg3 = trel.getObject();
		return "noValuesOtherThan(" + nodeToString(arg1, target) + ", " + nodeToString(arg2, target) + ", " + nodeToString(arg3, target) + ")";
	}

	private Object createNotOnly(TripleElement trel, TranslationTarget target) throws TranslationException {
		Node arg1 = trel.getSubject();
		Node arg2 = trel.getPredicate();
		Node arg3 = trel.getObject();
		return "notOnlyValue(" + nodeToString(arg1, target) + ", " + nodeToString(arg2, target) + ", " + nodeToString(arg3, target) + ")";
	}


	private void setRuleInTranslation(Rule ruleInTranslation) {
		this.ruleInTranslation = ruleInTranslation;
	}

	private Rule getRuleInTranslation() {
		return ruleInTranslation;
	}

	private Query getQueryInTranslation() {
		return queryInTranslation ;
	}

	private void setQueryInTranslation(Query queryInTranslation) {
		this.queryInTranslation = queryInTranslation;
	}

	private boolean isDatatypePropertyWithXSDRange(TripleElement gpe) {
		Node pred = gpe.getPredicate();
		OntProperty prop = getTheModel().getOntProperty(((NamedNode)pred).toFullyQualifiedString());
		if (prop != null && prop.isDatatypeProperty()) {
			Resource rng = prop.getRange();
			if(rng.getNameSpace().equals(XSD.getURI())) {
				return true;
			}
		}
		return false;
	}

	private boolean tripleHasDecimalObject(TripleElement gpe) {
		Node pred = gpe.getPredicate();
		Node obj = gpe.getObject();
		if (!(obj instanceof NamedNode) && pred instanceof NamedNode && ((NamedNode)pred).getNamespace() != null) {
			OntProperty prop = getTheModel().getOntProperty(((NamedNode)pred).toFullyQualifiedString());
			if (prop != null && prop.isDatatypeProperty()) {
				Resource rng = prop.getRange();
				if (rng.toString().contains("double") || rng.toString().contains("float")) {
					return true;
				}
			}
		}
		if (obj instanceof Literal) {
			Object objval = ((Literal)obj).getValue();
			if (objval instanceof Double || objval instanceof Float) {
				return true;
			}
		}
		return false;
	}
	
	private boolean tripleHasNumericObject(TripleElement gpe) {
		Node pred = gpe.getPredicate();
		OntProperty prop = getTheModel().getOntProperty(((NamedNode)pred).toFullyQualifiedString());
		if (prop != null && prop.isDatatypeProperty()) {
			Resource rng = prop.getRange();
			if (rng.toString().contains("double") || rng.toString().contains("float") || rng.toString().contains("int") || rng.toString().contains("long") || rng.toString().contains("decimal")) {
				return true;
			}
		}
		return false;
	}
	
	private String getNewVariableForQuery() {
		int cntr = nextQueryVarNum;
		Query query = getQueryInTranslation();
		String querystr = query.toString();
		String varName;
		do {
			varName = "v" + cntr++;
		} while (querystr.indexOf(varName) > 0);
		nextQueryVarNum = cntr;
		return varName;
	}
	
	private String getNewVariableForRule() {
		int cntr = 1;
		Rule rule = getRuleInTranslation();
		if (rule != null) {
			String rulestr = rule.toString();
			String varName;
			do {
				varName = "v" + cntr++;
			} while (rulestr.indexOf(varName) > 0);
			return varName;
		}
		else {
			return "v1";
		}
	}
	
	private String getNewPrologOnlyVariableForRule() {
		return "PV" + prologOnlyVarCnt ++;
	}
	
	private void setTheModel(OntModel theModel) {
		this.theModel  = theModel;
	}

	private OntModel getTheModel() {
		return theModel;
	}

	private String getModelName() {
		return modelName ;
	}

	private void setModelName(String modelName) {
		this.modelName = modelName;
	}

	private void registerPrefix(String prefix, String namespace) {
		if (prefix == null) {
			logger.error("Prefix is null in registerPrefix");
		}
		if (!prefixes.containsKey(prefix)) {
			prefixes.put(prefix, namespace);
		}
	}

	private String getTranslationFolder() {
		return translationFolder;
	}

	private void setTranslationFolder(String translationFolder) {
		this.translationFolder = translationFolder;
	}

	private IReasoner getReasoner() throws ConfigurationException, ReasonerNotFoundException {
		if (reasoner == null) {
			reasoner = configurationMgr.getReasoner();
		}
		if (!reasoner.isInitialized()) {
			reasoner.setConfigurationManager(configurationMgr);
			int iStatus = reasoner.initializeReasoner(
					getTranslationFolder(), modelName, IConfigurationManager.RDF_XML_ABBREV_FORMAT);
		}
		return reasoner;
	}

	@Override
	public String translateEquation(OntModel model, Equation equation) throws TranslationException {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public String getBuiltinFunctionModel(){
		StringBuilder sb = new StringBuilder();
		sb.append("uri \"");
		sb.append(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI);
		sb.append("\" alias ");
		sb.append(SadlConstants.SADL_BUILTIN_FUNCTIONS_ALIAS);
		sb.append(".\n\n");
		
		return sb.toString();
	}
	
	//This method existed before built-in function type-checking was added to the interface.
	@Override
	public boolean isBuiltinFunction(String name) {
		if (functionListArity1.contains(name)) {
			return true;
		}
		if (functionListArity2.contains(name)) {
			return true;
		}
		return false;
	}
	
	@Override
	public Enum isBuiltinFunctionTypeCheckingAvailable(){
		return SadlConstants.SADL_BUILTIN_FUNCTIONS_TYPE_CHECKING_AVAILABILITY.NAME_ONLY;
	}

	@Override
	public String getLocalFragmentNamespace(String arg0) throws ConfigurationException {
		throw new ConfigurationException("getLocalFragmentNamespace not implemented in this translator");
	}

	
	/**
	 * Returns supported data types for the project to show a warning for RCE that 
	 * a declared data type within .sreq files may not be compatible with downstream projects 
	 * @return supportedDataTypes string list of the projects supported data types
	 */
	@Override
	public List<String> getSupportedDataTypes() {
		List<String> supportedDataTypes = new ArrayList<String>();
		
		// Add support primitive types here as strings
		// ie. supportedDataTypes.add("int");
		// ie. supportedDataTypes.add("string");
		
		return supportedDataTypes;
	}

}
