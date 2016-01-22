package com.ge.research.sadl.prolog.translator;

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

import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.Prolog;
import alice.tuprolog.Theory;

import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NegatedExistentialQuantifier;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.prolog.fileinterface.FileInterface;
import com.ge.research.sadl.prolog.reasoner.PrologReasonerPlugin;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.FunctionNotSupportedException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.TranslationException;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.vocabulary.RDFS;

public class PrologTranslatorPlugin implements ITranslator {
    protected static final Logger logger = LoggerFactory.getLogger(PrologTranslatorPlugin.class);

    private static final String TranslatorCategory = "tu-Prolog_Translator";
    protected List<ModelError> errors = null;
    private boolean saveRuleFileAfterModelSave = true;
    private List<String> importOrder;
	protected IConfigurationManager configurationMgr;
    private List<String> builtinList = Arrays.asList("printhead","print","derive",
    		"holds","rdf","length","sub_string","sub_atom","equal");
	private Rule ruleInTranslation = null;
	private Query queryInTranslation = null;
	private int nextQueryVarNum = 1;

	private String modelName = null;
	private OntModel theModel = null;
    private Map<String, String> prefixes = new HashMap<String, String>();
	
    private enum RulePart {PREMISE, CONCLUSION, NOT_A_RULE}
    private enum SpecialBuiltin {NOVALUE, NOVALUESPECIFIC, NOTONLY, ONLY, ISKNOWN}
	public enum TranslationTarget {RULE_TRIPLE, RULE_BUILTIN, QUERY_TRIPLE, QUERY_FILTER}

	@Override
	public String getConfigurationCategory() {
		return TranslatorCategory;
	}

	@Override
	public void setConfigurationManager(IConfigurationManager configMgr) throws ConfigurationException {
//		if ((configMgr instanceof IConfigurationManagerForEditing)) {
//			((IConfigurationManagerForEditing) configMgr).setTranslatorClassName(this.getClass().getCanonicalName());
//		}
		configurationMgr = configMgr;
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
		Prolog plengine = new Prolog();
		
        if (saveRuleFileAfterModelSave) {
			String ruleFilename = createDerivedFilename(saveFilename, "pl");
			String fullyQualifiedRulesFilename = translationFolder + File.separator + ruleFilename;
			// clear the file
			FileInterface.writeFile(fullyQualifiedRulesFilename, "", false);
			//FileInterface.writeFile(fullyQualifiedRulesFilename, getHeaders(), true);
			
        
			if (ruleList != null){
				for (Rule rule: ruleList){
					//System.out.println(rule.toString());
//					String ruleStr = toString(rule.getRuleName(),rule.getIfs(),rule.getThens());
					String ruleStr = translateRule(model, rule);
					try{
						plengine.addTheory(new Theory(ruleStr.substring(0, ruleStr.length()-1)));
//						System.out.println(ruleStr);
						FileInterface.writeFile(fullyQualifiedRulesFilename, ruleStr + "\n", true);
					}catch (InvalidTheoryException e) {
					    System.err.println("Invalid Prolog rule: " + e.getMessage());
					}
				}
			}
        }
        
        plengine.clearTheory();
		
		//saveRuleFileAfterModelSave = false;	// reset
		return (errors != null && errors.size() > 0) ? errors : null;
	}

	@Override
	public List<ModelError> translateAndSaveModelWithOtherStructure(
			OntModel model, Object otherStructure, String translationFolder,
			String modelName, List<String> orderedImports, String saveFilename) throws TranslationException,
			IOException, URISyntaxException {
		// TODO Auto-generated method stub
		return null;
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
				GraphPatternElement then = thens.get(i);
				List<GraphPatternElement> oneThens = new ArrayList<GraphPatternElement>();
				oneThens.add(then);
				String thenStr = graphPatternElementsToPrologRuleString(oneThens, RulePart.CONCLUSION);
				if (thenStr != null) {
					sb.append(thenStr);
					List<GraphPatternElement> givens = rule.getGivens();
					List<GraphPatternElement> ifs = rule.getIfs();
					if ((givens != null && givens.size() > 0) || (ifs != null && ifs.size() > 0)) {
						sb.append(" :- ");
					}
					String givenStr = graphPatternElementsToPrologRuleString(givens, RulePart.PREMISE);
					if (givenStr != null) {
						sb.append(givenStr);
					}
					String ifStr = graphPatternElementsToPrologRuleString(ifs, RulePart.PREMISE);
					if (ifStr != null) {
						if (givenStr != null) {
							sb.append(", ");
						}
						sb.append(ifStr);
					}
					if (sb.length() > 0) {
						sb.append(".\n\n");
					}
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
				thens = thenJunctionToList(thens, index, (GraphPatternElement) lhs);
				thens = thenJunctionToList(thens, index, (GraphPatternElement) rhs);
				thens.set(index, (GraphPatternElement) lhs);
				thens.add(index, (GraphPatternElement) rhs);
			}
		}
		return thens;
	}

	@Override
	public String translateQuery(OntModel model, Query query)
			throws TranslationException, InvalidNameException {
		setTheModel(model);
		setModelName(modelName);
		// TODO Auto-generated method stub
		//System.out.println(query.toString());
		
		return query.toFullyQualifiedString().replace("and", ",").replace("!=", "\\==");
	}

	@Override
	public String getReasonerFamily() {
		return PrologReasonerPlugin.ReasonerFamily;
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

	@Override
	public String prepareQuery(OntModel model, String queryStr)
			throws InvalidNameException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<ConfigurationItem> discoverOptimalConfiguration(
			String translationFolder, String modelName,
			IConfigurationManager configMgr, List<Query> queries)
			throws FunctionNotSupportedException, ConfigurationException,
			TranslationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
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
//			int prevIndex = 0;
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
	
	protected String getHeaders(){
		String toWrite = "% defining properties of predicates\n";

		toWrite += "% properties of holds and derive are fixed as below\n";
		toWrite += ":- dynamic holds/5.\n";
		toWrite += ":- dynamic holds/4.\n";
		toWrite += ":- dynamic holds/3.\n";
		toWrite += ":- dynamic holds/2.\n";
		toWrite += ":- dynamic holds/1.\n\n";
		
		toWrite += ":- multifile holds/5.\n";
		toWrite += ":- multifile holds/4.\n";
		toWrite += ":- multifile holds/3.\n";
		toWrite += ":- multifile holds/2.\n";
		toWrite += ":- multifile holds/1.\n\n";
		
		toWrite += ":- discontiguous holds/5.\n";
		toWrite += ":- discontiguous holds/4.\n";
		toWrite += ":- discontiguous holds/3.\n";
		toWrite += ":- discontiguous holds/2.\n";
		toWrite += ":- discontiguous holds/1.\n\n";
		
		toWrite += ":- dynamic derive/5.\n";
		toWrite += ":- dynamic derive/4.\n";
		toWrite += ":- dynamic derive/3.\n";
		toWrite += ":- dynamic derive/2.\n";
		toWrite += ":- dynamic derive/1.\n\n";
		
		toWrite += ":- multifile derive/5.\n";
		toWrite += ":- multifile derive/4.\n";
		toWrite += ":- multifile derive/3.\n";
		toWrite += ":- multifile derive/2.\n";
		toWrite += ":- multifile derive/1.\n\n";
		
		toWrite += ":- discontiguous derive/5.\n";
		toWrite += ":- discontiguous derive/4.\n";
		toWrite += ":- discontiguous derive/3.\n";
		toWrite += ":- discontiguous derive/2.\n";
		toWrite += ":- discontiguous derive/1.\n\n";
		    
		return toWrite;
		    
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
					sb.append(biname);
					sb.append("(");
					for (int i = 0; args != null && i < args.size(); i++) {
						if (i > 0) sb.append(", ");
						sb.append(nodeToString(args.get(i), TranslationTarget.RULE_BUILTIN));
					}
					sb.append(")");
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
					sb.append("(holds ");
					Node arg0 = args.get(0);
					Node arg1 = args.get(1);
					if (arg1 instanceof NamedNode && ((NamedNode)arg1).getNodeType().equals(NodeType.ClassNode)) {
						sb.append("'_instanceOf_' ");
						sb.append(nodeToString(arg0, TranslationTarget.RULE_BUILTIN));
						sb.append(" ");
						sb.append(nodeToString(arg0, TranslationTarget.RULE_BUILTIN));
					}
					else {
						addError("BuiltinType is equal but the expected variable and class ", ErrorType.ERROR);
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
				newVar = getNewVariableForRule();
				sb.append("PV");
				sb.append(newVar);
			}
			else {
				if (rulePart.equals(RulePart.NOT_A_RULE)) {
					if (obj instanceof KnownNode) {
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
				sb.append(" equal(?");
				sb.append(newVar);
				sb.append(", ");
				sb.append(nodeToString(obj, TranslationTarget.RULE_BUILTIN));
				sb.append(")");
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
					if (trel.getObject() instanceof KnownNode) {
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
						if (arg2 instanceof KnownNode) {
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
			else if (((TripleElement)elements.get(index)).getObject() instanceof KnownNode) {
				Node var = new VariableNode("v" + System.currentTimeMillis());
				((TripleElement)elements.get(index)).setObject(var);
				return SpecialBuiltin.ISKNOWN;
			}
		}
		return null;
	}
	
	/**
	 * Converts a built-in element to a string representation.
	 * @param bin
	 * @return String
	 */
	private String builtinTypeToString(BuiltinElement bin) throws TranslationException {
		BuiltinType ftype = bin.getFuncType();
		String builtinName = null;
		if (ftype.equals(BuiltinType.Divide)) {
//			return "quotient";
			builtinName = "/";
		}
		else if (ftype.equals(BuiltinType.Equal)) {
//			return "equal";
			builtinName = "==";
		}
		else if (ftype.equals(BuiltinType.GT)) {
//			return "greaterThan";
			builtinName = ">";
		}
		else if (ftype.equals(BuiltinType.GTE)) {
//			return "ge";
			builtinName = ">=";
		}
		else if (ftype.equals(BuiltinType.LT)) {
//			return "lessThan";
			builtinName = "<";
		}
		else if (ftype.equals(BuiltinType.LTE)) {
//			return "le";
			builtinName = "=<";
		}
		else if (ftype.equals(BuiltinType.Minus)) {
//			return "difference";
			builtinName = "-";
		}
		else if (ftype.equals(BuiltinType.Modulus)) {
//			return "mod";
			builtinName = "mod";
		}
		else if (ftype.equals(BuiltinType.Multiply)) {
//			return "product";
			builtinName = "*";
		}
		else if (ftype.equals(BuiltinType.Negative)) {
//			return "negative";
			builtinName = "-";
		}
		else if (ftype.equals(BuiltinType.Not)) {
//			return "noValue";
			builtinName = "\\+";
		}
		else if (ftype.equals(BuiltinType.NotEqual)) {
//			return "notEqual";
			builtinName = "\\==";
		}
		else if (ftype.equals(BuiltinType.NotOnly)) {
//			return "notOnlyValue";
//			builtinName = "notOnlyValue");
			throw new TranslationException("notOnlyValue not supported in Prolog");
		}
		else if (ftype.equals(BuiltinType.Only)) {
//			return "noValuesOtherThan";
//			builtinName = "noValuesOtherThan");
			throw new TranslationException("noValuesOtherThan not supported in Prolog");
		}
		else if (ftype.equals(BuiltinType.Plus)) {
//			return "sum";
			builtinName = "+";
		}
		else if (ftype.equals(BuiltinType.Power)) {
//			return "pow";			
			builtinName = "**";
		}
		else if (ftype.equals(BuiltinType.Assign)) {
//			return "assign";
			builtinName = "is";
		}
		else if (ftype.equals(BuiltinType.UserAdded) && bin.getFuncName().equals("holds")) {
// 			this is good
            builtinName = "holds";
        }
        else if (ftype.equals(BuiltinType.UserAdded) &&bin.getFuncName().equals("rdf")) {
            builtinName = "rdf";
        }
        else if (ftype.equals(BuiltinType.UserAdded) &&bin.getFuncName().equals("equal")) {
            builtinName = "equal";
        }
		else {
            
			logger.warn("Something went wrong finding/loading Builtin '" + bin.getFuncName() + "' of type '" + ftype + "'");
            addError("Found reference to unknown built-in '" + bin.getFuncName() + "' of type '" + ftype + "'", ErrorType.WARNING);
//          throw new TranslationException("Unable to resolve built-in of type '" + ftype + "'");
            return bin.getFuncName();
		}

		return builtinName;
	}
	
	private String nodeToString(Node node, TranslationTarget target) throws TranslationException {
		if (node instanceof NamedNode) {
			NodeType ntype = ((NamedNode)node).getNodeType();
			if (ntype.equals(NodeType.VariableNode)) {
				// double-check this; if a concept was declared after reference in a rule or query 
				//	it may have been parsed as a variable but actually be a defined concept 
				OntResource r = getTheModel().getOntResource(getModelName() + "#" + ((NamedNode)node).getName());
				if (r == null) {
					return "PV" + ((NamedNode)node).getName();
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
				return "'_instanceOf_'";
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
					return "'_subClassOf_'";
				}
				else {
					return "'" + nts + "'";
				}
			}
		}
		else if (node instanceof Literal) {
			Object litObj = ((Literal)node).getValue();
			if (((Literal)node).getOriginalText() != null) {
				return ((Literal)node).getOriginalText();
			}
			return literalValueToString(litObj, target);
		}
		else if (node instanceof KnownNode) {
			return "PV" + getNewVariableForRule();
		}
		else if (node == null) {
			throw new TranslationException("Encountered null node in nodeToString; this indicates incorrect intermediate form and should not happen");
		}
		else {
			throw new TranslationException("Nnode '" + node.toString() + "' cannot be translated to Prolog format.");
		}
	}

	public static synchronized String literalValueToString(Object litObj, TranslationTarget target) {
		if (litObj instanceof String) {
			litObj = "\"" + litObj + "\"";
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

}
