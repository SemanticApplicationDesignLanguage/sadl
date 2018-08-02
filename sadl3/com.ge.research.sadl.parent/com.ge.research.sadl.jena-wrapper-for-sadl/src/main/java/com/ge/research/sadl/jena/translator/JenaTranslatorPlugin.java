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

package com.ge.research.sadl.jena.translator;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import javax.naming.directory.ModificationItem;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.TypedBaseBuiltin;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.FunctionSignature;
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
import com.ge.research.sadl.model.gp.Query.Order;
import com.ge.research.sadl.model.gp.Query.OrderingPair;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.ConfigurationType;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.FunctionNotSupportedException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.reasoner.rulesys.Builtin;
import com.hp.hpl.jena.reasoner.rulesys.BuiltinRegistry;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

public class JenaTranslatorPlugin implements ITranslator {
    private static final String THERE_EXISTS = "thereExists";

	protected static final Logger logger = LoggerFactory.getLogger(JenaTranslatorPlugin.class);
    
    private static final String TranslatorCategory = "Basic_Jena_Translator";
    
	private static final String ReasonerFamily="Jena-Based";

	protected IConfigurationManager configurationMgr;

	public enum TranslationTarget {RULE_TRIPLE, RULE_BUILTIN, QUERY_TRIPLE, QUERY_FILTER}
    
    private enum SpecialBuiltin {NOVALUE, NOVALUESPECIFIC, NOTONLY, ONLY, ISKNOWN, THEREEXISTS}
    
    private enum RulePart {PREMISE, CONCLUSION, NOT_A_RULE}
    
    private Map<String, String> prefixes = new HashMap<String, String>();
	
	private boolean saveRuleFileAfterModelSave = true; 	// unless set to false because an explicit list of rules are

	private Rule ruleInTranslation = null;
	private Query queryInTranslation = null;
	private int nextQueryVarNum = 1;

	private OntModel theModel;
	private String modelName;

	protected List<ModelError> errors = null;
														// provided, we need to look for rule files in
														// imported models and if there are any create a rule file
														// for this model so that the imported rule files will be loaded
	
	/**
	 * The null argument constructor
	 */
	public JenaTranslatorPlugin() {
		
	}
	

	public List<ModelError> translateAndSaveModel(OntModel model, String translationFolder,
			String modelName, List<String> orderedImports, String saveFilename) throws TranslationException, IOException, URISyntaxException {
		
		// Jena models have been saved to the OwlModels folder by the ModelManager prior to calling the translator. For Jena
		//	reasoners, no additional saving of OWL models is need so we can continue on to rule translation and saving.
		if (errors != null) {
			errors.clear();
		}
		if (model == null) {
			return addError("Cannot save model in file '" + saveFilename + "' as it has no model.");
		}
		if (modelName == null) {
			return addError("Cannot save model in file '" + saveFilename + "' as it has no name.");
		}
		
        if (saveRuleFileAfterModelSave) {
			String ruleFilename = createDerivedFilename(saveFilename, "rules");
			String fullyQualifiedRulesFilename = translationFolder + File.separator + ruleFilename;
	        translateAndSaveRules(model, null, modelName, fullyQualifiedRulesFilename);
        }
        saveRuleFileAfterModelSave = false;	// reset
		return (errors != null && errors.size() > 0) ? errors : null;
    }


	public List<ModelError> translateAndSaveModel(OntModel model, List<Rule> ruleList,
			String translationFolder, String modelName, List<String> orderedImports, String saveFilename)
			throws TranslationException, IOException, URISyntaxException {
		if (errors != null) {
			errors.clear();
		}
		saveRuleFileAfterModelSave = false;
		// a Jena model simply writes out the OWL file
		translateAndSaveModel(model, translationFolder, modelName, orderedImports, saveFilename);
		
		String ruleFilename = createDerivedFilename(saveFilename, "rules");
		String fullyQualifiedRulesFilename = translationFolder + File.separator + ruleFilename;
		if (ruleList != null && ruleList.size() > 0) {
			translateAndSaveRules(model, ruleList, modelName, fullyQualifiedRulesFilename);
		}
		else {
			// there isn't a rules file but make sure there isn't an old one around that needs to be deleted
			File oldRuleFile = new File(fullyQualifiedRulesFilename);
			if (oldRuleFile.exists() && oldRuleFile.isFile()) {
				try {
					oldRuleFile.delete();
				}
				catch (Exception e) {
					addError("Failed to delete old rules file '" + fullyQualifiedRulesFilename + "'.");
					logger.error("Failed to delete old rule file '" + fullyQualifiedRulesFilename + "': " + e.getLocalizedMessage());
				}
			}
		}
		return (errors != null && errors.size() > 0) ? errors : null;
	}


	public String translateRule(OntModel model, Rule rule)
			throws TranslationException {
		setRuleInTranslation(rule);
		boolean translateToBackwardRule = false;
		StringBuilder sb = new StringBuilder();
		
		// put annotations (if any) in the rule
		if (rule.getAnnotations() != null) {
			Iterator<String[]> annItr = rule.getAnnotations().iterator();
			sb.append("#/**\n");
			while (annItr.hasNext()) {
				String[] annNVP = annItr.next();
				sb.append("# * @");
				sb.append(annNVP[0]);
				sb.append("\t");
				String val = annNVP[1];
				String linesep =System.lineSeparator();
				String[] valLines = val.split(linesep);
				for (int i = 0; i < valLines.length; i++) {
					if (i > 0) {
						sb.append("# * ");
					}
					sb.append(valLines[i]);
					sb.append("\n");
				}
			}
			sb.append("# */\n");
		}
		
		if (translateToBackwardRule) {
			sb.append("[");
			sb.append(rule.getRuleName());
			sb.append(": ");
			List<GraphPatternElement> thens = rule.getThens();
			String thenStr = graphPatternElementsToJenaRuleString(thens, RulePart.CONCLUSION);
			if (thenStr != null) {
				sb.append(thenStr);
			}
			sb.append(" <- ");
			List<GraphPatternElement> givens = rule.getGivens();
			String givenStr = graphPatternElementsToJenaRuleString(givens, RulePart.PREMISE);
			if (givenStr != null) {
				sb.append(givenStr);
			}
			List<GraphPatternElement> ifs = rule.getIfs();
			String ifStr = graphPatternElementsToJenaRuleString(ifs, RulePart.PREMISE);
			if (ifStr != null) {
				if (givenStr != null) {
					sb.append(", ");
				}
				sb.append(ifStr);
			}
			sb.append("]");
		}
		else {
			sb.append("[");
			sb.append(rule.getRuleName());
			sb.append(": ");
			List<GraphPatternElement> givens = rule.getGivens();
			String givenStr = graphPatternElementsToJenaRuleString(givens, RulePart.PREMISE);
			if (givenStr != null) {
				sb.append(givenStr);
			}
			List<GraphPatternElement> ifs = rule.getIfs();
			String ifStr = graphPatternElementsToJenaRuleString(ifs, RulePart.PREMISE);
			if (ifStr != null) {
				if (givenStr != null) {
					sb.append(", ");
				}
				sb.append(ifStr);
			}
			sb.append(" -> ");
			List<GraphPatternElement> thens = rule.getThens();
			String thenStr = graphPatternElementsToJenaRuleString(thens, RulePart.CONCLUSION);
			if (thenStr != null) {
				sb.append(thenStr);
			}
			sb.append("]");
		}
		setRuleInTranslation(null);
		return (sb.length() > 0 ? sb.toString() : null);
	}
	
	private String graphPatternElementsToJenaRuleString(List<GraphPatternElement> elements, RulePart rulePart) throws TranslationException {
		if (elements != null && elements.size() > 0) {
			StringBuilder sb = new StringBuilder();
			int idx = 0;
			while(idx < elements.size()) {
				if (idx > 0) sb.append(", ");
				SpecialBuiltin spb = processSpecialBuiltins(elements, idx);	// check for special handling required for some built-ins
				if (spb != null) {
					// get the triple in question
					TripleElement trel = null;
					if (elements.get(idx) instanceof TripleElement) {
						trel = (TripleElement)elements.get(idx);
					}
					else if (elements.get(idx) instanceof BuiltinElement && ((BuiltinElement)elements.get(idx)).getFuncName().equals(THERE_EXISTS)) {
						if (((BuiltinElement)elements.get(idx)).getArguments() == null || ((BuiltinElement)elements.get(idx)).getArguments().size() != 1) {
							logger.error("Function 'thereExists' should have one and only one argument");
							addError("Function 'thereExists' should have one and only one argument.");						
							return sb.toString();
						}
						if (!(((BuiltinElement)elements.get(idx)).getArguments().get(0) instanceof VariableNode)) {
							logger.error("Function 'thereExists' should have a variable as argument.");
							addError("Function 'thereExists' should have a variable as argument.");	
							return sb.toString();
						}
						VariableNode thereExistsVar = (VariableNode) ((BuiltinElement)elements.get(idx)).getArguments().get(0);
						if (thereExistsVar.getType() == null) {
							logger.error("Function 'thereExists' variable must have a type");
							addError("Function 'thereExists' variable must have a type.");			
							return sb.toString();
						}
						BuiltinElement bi = new BuiltinElement();
						bi.setFuncName("getInstance");
						bi.addArgument(thereExistsVar.getType());
						elements.set(idx, bi);
						int restIdx = idx + 1;
						while (restIdx < elements.size()) {
							// these should all be TripleElement graph patterns 
							GraphPatternElement gpe = elements.get(restIdx);
							if (!(gpe instanceof TripleElement) || !((TripleElement)gpe).getSubject().equals(thereExistsVar)) {
								logger.error("Found end of 'thereExists' with something after");
								sb.append(graphPatternElementToJenaRuleString(elements.get(idx), rulePart));
								idx++;
								break;
							}
							TripleElement tgpe = (TripleElement) gpe;
							if (tgpe.getSubject().equals(thereExistsVar) && tgpe.getPredicate().equals(new RDFTypeNode()) && tgpe.getObject().equals(thereExistsVar.getType())) {
								elements.remove(restIdx);
							}
							else if (tgpe.getSubject().equals(thereExistsVar)) {
								// add arguments for property and then value
								bi.addArgument(tgpe.getPredicate());
								bi.addArgument(tgpe.getObject());
								elements.remove(restIdx);
							}
							else if (tgpe.getObject().equals(thereExistsVar)) {
								// add arguments for subject and then property
								bi.addArgument(tgpe.getSubject());
								bi.addArgument(tgpe.getPredicate());
								elements.remove(restIdx);
							}
							else {
								restIdx++;
							}
						}
						// we now want to process this as if it were this in the first place
						// (except remove extra comma)
						if (sb.length() > 0) {
							int lastComma = sb.lastIndexOf(",");
							if (lastComma > 0) {
								int diff = sb.length() - lastComma;
								if (diff > 0 &&diff <= 2) {
									for (int i = 0; i < diff; i++) {
										sb.deleteCharAt(lastComma);
									}
								}
							}
						}
						continue;
					}
					else {
						logger.error("Unhandled graph pattern element detected as special builtin: " + elements.get(idx).toString());
					}
					
					// translate based on type of spb
					if (spb.equals(SpecialBuiltin.NOVALUE)) {
						sb.append(createNoValue(trel, TranslationTarget.RULE_BUILTIN));
					}
					else if (spb.equals(SpecialBuiltin.ISKNOWN)) {
						sb.append(graphPatternElementToJenaRuleString(trel, rulePart));
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
							sb.append(createOnly((TripleElement)elements.get(idx), TranslationTarget.RULE_BUILTIN));
						}
						else {
							logger.error("Unhandled special builtin: " + elements.toString());
						}
					}
				}
				else {
					sb.append(graphPatternElementToJenaRuleString(elements.get(idx), rulePart));
				}
				idx++;
			}
			return sb.toString();
		}
		return null;
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
		else if (elements.get(index) instanceof BuiltinElement && ((BuiltinElement)elements.get(index)).getFuncName().equals(THERE_EXISTS)) {
			return SpecialBuiltin.THEREEXISTS;
		}
		return null;
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


	public String translateQuery(OntModel model, Query query)
			throws TranslationException, InvalidNameException {
		boolean isEval = false;
		setTheModel(model);
		setModelName(modelName);
		if (query == null) {
			throw new TranslationException("Invalid query: query is null!");			
		}
		if (query.getPatterns() != null) {
			GraphPatternElement gpe1 = query.getPatterns().get(0);
			if (gpe1 instanceof Junction) {
				Object gperhs = ((Junction)gpe1).getRhs();
				if (gperhs instanceof ProxyNode) gperhs = ((ProxyNode)gperhs).getProxyFor();
				if (gperhs instanceof BuiltinElement && ((BuiltinElement)gperhs).getFuncName().equalsIgnoreCase("eval")) {
					isEval = true;
					Object gpelhs = ((Junction)gpe1).getLhs();
					if (gpelhs instanceof ProxyNode) gpelhs = ((ProxyNode)gpelhs).getProxyFor();
					if (gpelhs instanceof GraphPatternElement) {
						query.getPatterns().set(0, (GraphPatternElement) gpelhs);
						query.setToBeEvaluated(true);
					}
				}
			}
		}
		if (query.getSparqlQueryString() != null) {
			return prepareQuery(model, query.getSparqlQueryString());
		}
		if (query.getKeyword() == null) {
			if (query.getVariables() == null) {
				query.setKeyword("ask");
			}
			else {
				query.setKeyword("select");
			}
		}
		if (!query.getKeyword().equals("ask") && query.getVariables() == null) {
			throw new TranslationException("Invalid query (" + query.toString() + "): must be a valid structure with specified variable(s).");
		}
		setQueryInTranslation(query);
		StringBuilder sbmain = new StringBuilder();
		StringBuilder sbfilter = new StringBuilder();
		sbmain.append(query.getKeyword());		
		sbmain.append(" ");
		if (query.getKeyword().equalsIgnoreCase("construct")) {
			sbmain.append("{");
		}
		if (query.isDistinct()) {
			sbmain.append("distinct ");
		}
		
		List<VariableNode> vars = query.getVariables();
		List<GraphPatternElement> elements = query.getPatterns();
		if (vars != null && vars.size() > 0) {
			for (int i = 0; i < vars.size(); i++) {
				if (i > 0) sbmain.append(" ");
				sbmain.append("?" + vars.get(i).getName());
			}
		}
		else {
			sbmain.append("*");
		}
		if (query.getKeyword().equalsIgnoreCase("construct")) {
			sbmain.append("} ");
		}
		sbmain.append(" where {");
		
		int tripleCtr = 0; 
		int builtinCtr = 0;
		for (int i = 0; elements != null && i < elements.size(); i++) {
			GraphPatternElement gpe = elements.get(i);
			// need to handle or, and
			if (gpe instanceof Junction) {
				if (tripleCtr++ > 0) sbmain.append(" . ");
				String junctionStr = junctionToQueryString((Junction)gpe, sbfilter);
				sbmain.append(junctionStr);
			}
			else if (gpe instanceof TripleElement) {
				if (tripleCtr++ > 0) sbmain.append(" . ");
				String jenaStr = graphPatternElementToJenaQueryString(gpe, sbfilter, TranslationTarget.QUERY_TRIPLE, RulePart.NOT_A_RULE);
				sbmain.append(jenaStr);
			}
			else if (gpe instanceof BuiltinElement) {
//				if (builtinCtr++ > 0) {
//					sbfilter.append(" && ");
//				}
//				else {
//					sbfilter.append("FILTER (");
//				}
//				sbfilter.append(graphPatternElementToJenaQueryString(gpe, sbfilter, TranslationTarget.QUERY_FILTER));
				// the filter string will be added in the method
				graphPatternElementToJenaQueryString(gpe, sbfilter, TranslationTarget.QUERY_FILTER, RulePart.NOT_A_RULE);
			}
		}
		if (sbfilter.length() > 0) {
			sbfilter.insert(0, "FILTER ("); sbfilter.append(")");
			if (!sbmain.toString().trim().endsWith(".")) {
				sbmain.append(" . ");
			}
			sbmain.append(sbfilter.toString());
		}
		sbmain.append("}");
		if (query.getOrderBy() != null) {
			List<OrderingPair> ops = query.getOrderBy();
			if (ops.size() > 0) {
				sbmain.append(" order by");
				for (int i = 0; i < ops.size(); i++) {
					sbmain.append(" ");
					OrderingPair op = ops.get(i);
					boolean explicitOrder = false;
					if (op.getOrder() != null && op.getOrder().equals(Order.DESC)) {
						sbmain.append("DESC(");
						explicitOrder = true;
					}
					sbmain.append("?");
					sbmain.append(op.getVariable());
					if (explicitOrder) {
						sbmain.append(")");
					}
				}
			}
		}
		return prepareQuery(model, sbmain.toString());
	}
	
	/**
	 * Convert a junction to a query string. Filter stuff goes to the sbfilter StringBuilder, triple stuff gets returned.
	 * 
	 * @param gpe
	 * @param sbfilter
	 * @return
	 * @throws TranslationException
	 */
	private String junctionToQueryString(Junction gpe, StringBuilder sbfilter) throws TranslationException {
		// We have a junction, could be one of
		//  1. triple junction filter, e.g., ... x prop y and y < 3
		//	2. filter junction triple, e.g., ... y < 3 and x prop z
		//  3. filter junction filter, e.g., ... x > 0 and x < 3
		//  4. triple junction triple, e.g., ... x prop1 y and y prop2 z
		JunctionType jtype = ((Junction)gpe).getJunctionType();
		boolean lhsFilter = false;
		boolean rhsFilter = false;
		Object lhsobj = ((Junction)gpe).getLhs();
		if (lhsobj instanceof ProxyNode) lhsobj = ((ProxyNode)lhsobj).getProxyFor();
		Object rhsobj = ((Junction)gpe).getRhs();
		if (rhsobj instanceof ProxyNode) rhsobj = ((ProxyNode)rhsobj).getProxyFor();
		if (lhsobj instanceof BuiltinElement) {
			lhsFilter = true;
		}
		if (rhsobj instanceof BuiltinElement) {
			rhsFilter = true;
		}

		StringBuilder sbjunct = new StringBuilder();
		String connector = null;
		boolean wrapInCurleyBrackets = false;
		if (lhsFilter || rhsFilter) {
			if (lhsFilter && rhsFilter) {
				// this is a junction within the filter, case 3
				connector = junctionToFilterString(jtype);
				sbfilter.append("(");
				graphPatternElementToJenaQueryString((GraphPatternElement)lhsobj, sbfilter, TranslationTarget.QUERY_FILTER, RulePart.NOT_A_RULE);
				sbfilter.append(connector);
				graphPatternElementToJenaQueryString((GraphPatternElement) rhsobj, sbfilter, TranslationTarget.QUERY_FILTER, RulePart.NOT_A_RULE);									
				sbfilter.append(")");
			}
			else {
				// Note: junctions between a triple and a built-in filter are ignored, cases 1 & 2
				if (lhsFilter) {
					graphPatternElementToJenaQueryString((GraphPatternElement)lhsobj, sbfilter, TranslationTarget.QUERY_FILTER, RulePart.NOT_A_RULE);
					if (rhsobj instanceof BuiltinElement) {
						sbfilter.append(junctionToFilterString(jtype));
					}
					sbjunct.append(graphPatternElementToJenaQueryString((GraphPatternElement) rhsobj, sbfilter, TranslationTarget.QUERY_TRIPLE, RulePart.NOT_A_RULE));					
				}
				else {	// rhsFilter
					sbjunct.append(graphPatternElementToJenaQueryString((GraphPatternElement) lhsobj, sbfilter, TranslationTarget.QUERY_TRIPLE, RulePart.NOT_A_RULE));
					if (lhsobj instanceof BuiltinElement) {
						sbfilter.append(junctionToFilterString(jtype));
					}
					graphPatternElementToJenaQueryString((GraphPatternElement) rhsobj, sbfilter, TranslationTarget.QUERY_FILTER, RulePart.NOT_A_RULE);
				}
			}
		}
		else {
			// this is a junction between triple patterns, case 4
			if (jtype.equals(JunctionType.Conj)) { // (and)
				connector = " . ";
			}
			else {
				// must be Disj (or)
				wrapInCurleyBrackets = true;
				connector = "} UNION {";
			}
			if (wrapInCurleyBrackets) {
				sbjunct.append("{");
			}			
			sbjunct.append(graphPatternElementToJenaQueryString((GraphPatternElement) lhsobj, sbfilter, TranslationTarget.QUERY_TRIPLE, RulePart.NOT_A_RULE));
			sbjunct.append(connector);
			sbjunct.append(graphPatternElementToJenaQueryString((GraphPatternElement) rhsobj, sbfilter, TranslationTarget.QUERY_TRIPLE, RulePart.NOT_A_RULE));					
			if (wrapInCurleyBrackets) {
				sbjunct.append("}");
			}
		}
		return sbjunct.toString();
	}

	private String junctionToFilterString(JunctionType jtype) {
		if (jtype.equals(JunctionType.Conj)) { // and
			return " && ";
		}
		else {
			// must be Disj (or)
			return " || ";
		}
	}

	private String builtinToFilterFunctionString(BuiltinElement gpe) throws TranslationException {
		List<Node> args = gpe.getArguments();
		if (args.size() < 2) {
			throw new TranslationException("Filter '" + gpe.getFuncName() + "' must take two arguments.");
		}
		if (gpe.getFuncType().equals(BuiltinType.Equal)) {
			gpe.setFuncName("=");		// there are several possibilities here that all map to "=" in SPARQL
		}
		if (gpe.getFuncType().equals(BuiltinType.Not)) {
			gpe.setFuncName("!=");
		}
		switch(gpe.getFuncType()) {
		case Equal:
		case GT:
		case GTE:
		case LT:
		case LTE:
		case NotEqual:
		case Not:
			String filter = nodeToString(args.get(0), TranslationTarget.QUERY_FILTER) + " " + gpe.getFuncName() + " " + nodeToString(args.get(1), TranslationTarget.QUERY_FILTER);
			return filter;  // nodeToString(args.get(0), TranslationTarget.QUERY_FILTER) + " " + gpe.getFuncName() + " " + nodeToString(args.get(1), TranslationTarget.QUERY_FILTER);
		default:
			throw new TranslationException("Unhandled filter type: " + gpe.getFuncName());
		}
	}

	private boolean translateAndSaveRules(OntModel model, List<Rule> ruleList, String modelName, String filename) throws TranslationException, IOException {
		if (ruleList == null || ruleList.size() < 1) {
			throw new TranslationException("No rules provided to rule translation.");
		}
		if (prefixes != null) {
			prefixes.clear();    // don't keep around prefixes from another rule file generation
		}
		// Open file and output header and imports
		File ruleFile = new File(filename);
		if (ruleFile.exists()) {
			boolean success = ruleFile.delete();
			if (!success) {
				addError("Failed to delete old rules file '" + filename + "'.");
				logger.error("Failed to delete old rule file '" + filename + "'.");
				if (ruleList == null || ruleList.size() == 0) {
					setTheModel(null);
					setModelName(null);
					return false;   
				}
				// else don't return--maybe we can open it for output anyway
			}
		}
		String jenaRules = translateAndSaveRules(model, ruleList, modelName) ;
		SadlUtils su = new SadlUtils();
		su.stringToFile(ruleFile, jenaRules, true);
		return (errors== null || errors.size() == 0) ? true : false;
	}
	
	/**
	 * Save the 
	 * @param model
	 * @param ruleList
	 * @param modelName
	 * @return
	 * @throws TranslationException
	 */
	public String translateAndSaveRules(OntModel model, List<Rule> ruleList, String modelName) throws TranslationException {
		if (ruleList == null) {
			return null;
		}
		setTheModel(model);
		setModelName(modelName);
		StringBuilder contents = new StringBuilder();
		contents.append("# Jena Rules file generated by SADL IDE -- Do not edit! Edit the SADL model and regenerate.\n");
		contents.append("#  Created from SADL model '" + modelName + "'\n\n");
		
		StringBuilder ruleContent = new StringBuilder();
		for (int i = 0; i < ruleList.size(); i++) {
			Rule rule = ruleList.get(i);
			ruleContent.append(translateRule(model, rule));
			ruleContent.append("\n");
		}
		
		// now add prefix info to rule file output
		Iterator<String> itr2 = prefixes.keySet().iterator();
		while (itr2.hasNext()) {
			String prefix = itr2.next();
			String ns = prefixes.get(prefix);
			contents.append("@prefix ");
			contents.append(prefix);
			contents.append(": <");
			contents.append(ns);
			contents.append(">\n");
		}
		contents.append("\n");
		
		// Because rule files are loaded for each sub-model, there is no need to put in includes
		if (ruleContent.length() > 0) {
			contents.append(ruleContent);
		}
		setTheModel(null);
		setModelName(null);
		return contents.toString();
	}
	
	public String modelNsToRuleNs(String modelNs) {
		return modelNs + ".rules";
	}

/**
 * Convert GraphPatternElement to String in the context of a Rule
 * 
 * @param gpe
 * @return
 * @throws TranslationException
 */
	private String graphPatternElementToJenaRuleString(GraphPatternElement gpe, RulePart rulePart) throws TranslationException {
		StringBuilder sb = null;
		if (gpe instanceof TripleElement) {
			if (!((TripleElement)gpe).getModifierType().equals(TripleModifierType.None) && !((TripleElement)gpe).getModifierType().equals(TripleModifierType.Assignment)) {
				sb = new StringBuilder();
				TripleModifierType type = ((TripleElement)gpe).getModifierType();
				if (type.equals(TripleModifierType.Not)) {
					sb.append("noValue(");
				}
				else if (type.equals(TripleModifierType.Only)) {
					sb.append("noValueOtherThan(");
				}
				else if (type.equals(TripleModifierType.NotOnly)) {
					sb.append("notOnlyValue(");
				}
				sb.append(nodeToString(((TripleElement)gpe).getSubject(),TranslationTarget.RULE_BUILTIN));
				sb.append(", ");
				Node pn = ((TripleElement)gpe).getPredicate();
				checkPredicateSpecial(pn);
				sb.append(nodeToString(pn, TranslationTarget.RULE_BUILTIN));
				if (!ITranslator.isKnownNode((((TripleElement)gpe).getObject()))) {
					sb.append(", ");
					sb.append(nodeToString(((TripleElement)gpe).getObject(), TranslationTarget.RULE_BUILTIN));
				}
				sb.append(")");
			}
			else {
				sb = tripleElementToRawJenaString((TripleElement) gpe, TranslationTarget.RULE_TRIPLE, rulePart, null);  // 2/16/2011 false); 
			}
		}
		else if (gpe instanceof BuiltinElement) {
			sb = new StringBuilder();
			List<Node> args = ((BuiltinElement)gpe).getArguments();
			if ((((BuiltinElement)gpe).getFuncName().equals("is") || ((BuiltinElement)gpe).getFuncName().equals("assign"))
					&& ((BuiltinElement)gpe).getFuncType().equals(BuiltinType.Assign)) {
				sb.append("assign(");
				sb.append(nodeToString(args.get(1), TranslationTarget.RULE_BUILTIN));
				sb.append(",");
				sb.append(nodeToString(args.get(0), TranslationTarget.RULE_BUILTIN));
			}
			else {
				sb.append(builtinTypeToString((BuiltinElement)gpe));
				sb.append("(");
				for (int i = 0; args != null && i < args.size(); i++) {
					Node arg = args.get(i);
					if (i > 0) sb.append(", ");
					if (arg instanceof ProxyNode) {
						Object pfor = ((ProxyNode)arg).getProxyFor();
						if (pfor instanceof GraphPatternElement) {
							sb.append(graphPatternElementToJenaRuleString((GraphPatternElement) pfor, rulePart));
						}
						else {
							throw new TranslationException("Non-graph element proxy-for in ProxyNode '" + arg.toFullyQualifiedString() + "'");
						}
					}
					else {
						sb.append(nodeToString(arg, TranslationTarget.RULE_BUILTIN));
					}
				}
			}
			sb.append(")");
		}
		else if (gpe instanceof Junction) {
			sb = new StringBuilder();
			JunctionType jtype = ((Junction)gpe).getJunctionType();
			if (jtype.equals(JunctionType.Conj)) {
				Object lhs = ((Junction)gpe).getLhs();
				if (lhs instanceof List<?>) {
					sb.append(graphPatternElementsToJenaRuleString((List<GraphPatternElement>)lhs, rulePart));
				}
				else if (lhs instanceof GraphPatternElement) {
					sb.append(graphPatternElementToJenaRuleString((GraphPatternElement) lhs, rulePart));
				}
				else {
					throw new TranslationException("Unexpected junction lhs type: " + lhs.getClass());
				}
				Object rhs = ((Junction)gpe).getRhs();
				if (rhs instanceof List<?>) {
					sb.append(", ");
					sb.append(graphPatternElementsToJenaRuleString((List<GraphPatternElement>)rhs, rulePart));
				}
				else if (rhs instanceof GraphPatternElement) {
					sb.append(", ");
					sb.append(graphPatternElementToJenaRuleString((GraphPatternElement) rhs, rulePart));
				}
				else {
					throw new TranslationException("Unexpected junction rhs type: " + rhs.getClass());					
				}
			}
			else {
				System.err.println("Encountered unhandled OR in rule '" + ruleInTranslation.getRuleName() + "'");
//				throw new TranslationException("Jena rules do not currently support disjunction (OR).");
			}
		}
		else if (gpe instanceof NegatedExistentialQuantifier) {
			throw new TranslationException("Existential quantification with negation is not supported by the Jena reasoner.");
		}
		else {
			throw new TranslationException("GraphPatternElement '" + gpe.toString() + "' cannot be translated to Jena rule.");
		}
		return sb.toString();
	}
	
	private String graphPatternElementToJenaQueryString(GraphPatternElement gpe, StringBuilder sbfilter, 
			TranslationTarget target, RulePart rulePart) throws TranslationException {
		if (gpe instanceof TripleElement) {
			StringBuilder sb = tripleElementToRawJenaString((TripleElement) gpe, target, rulePart, sbfilter);
			return sb.toString();
		}
		else if (gpe instanceof Junction) {
			String junctStr = junctionToQueryString((Junction)gpe, sbfilter);
			return junctStr;
		}
		else if (gpe instanceof BuiltinElement) {
			String bistr = builtinToFilterFunctionString((BuiltinElement)gpe);
			if (sbfilter.length() > 0) {
				String tmp = sbfilter.toString().trim();
				if (!tmp.endsWith("(") && !tmp.endsWith("&&") && !tmp.endsWith("||")) {
					sbfilter.append(" && ");
				}
			}
			sbfilter.append(bistr);
			return null;
		}
		else {
			throw new TranslationException("GraphPatternElement '" + gpe.toString() + "' not yet handled in Jena query.");			
		}
	}
	
	/**
	 * Method to convert a TripleElement to a Jena String without delimiters
	 * 
	 * @param gpe
	 * @param sbfilter 
	 * @return
	 * @throws TranslationException
	 */
	private StringBuilder tripleElementToRawJenaString(TripleElement gpe, TranslationTarget target, RulePart rulePart, StringBuilder sbfilter) throws TranslationException {
		StringBuilder sb = new StringBuilder();
		Node subj = gpe.getSubject();
		Node pred = gpe.getPredicate();
		checkPredicateSpecial(pred);
		Node obj = gpe.getObject();
		boolean moveObjectToEqualityTest = false;
		if (target.equals(TranslationTarget.RULE_TRIPLE)) {
			sb.insert(0, '(');
		}
		sb.append(nodeToString(subj, target));
		sb.append(" ");
		sb.append(nodeToString(pred, target));
		sb.append(" ");
		String newVar = null;
		if (rulePart.equals(RulePart.PREMISE) && target.equals(TranslationTarget.RULE_TRIPLE) && tripleHasDecimalObject(gpe)) {
			// this would be a triple match on a float or double value, which is not reliable
			//	move the object to a separate equality test
			moveObjectToEqualityTest = true;
			newVar = getNewVariableForRule();
			sb.append("?");
			sb.append(newVar);
		}
		else {
			if (rulePart.equals(RulePart.NOT_A_RULE)) {
				if (ITranslator.isKnownNode(obj)) {
					newVar = "?" + getNewVariableForQuery();
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
					newVar = "?" + getNewVariableForQuery();
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
	
	private void checkPredicateSpecial(Node predNode) {
		if (predNode instanceof NamedNode) {
			if (((NamedNode)predNode).getNamespace() == null && 
					((NamedNode)predNode).getName() != null && 
					((NamedNode)predNode).getName().equals(RDFS.comment.getLocalName()) ) {
				((NamedNode)predNode).setNamespace(RDFS.getURI());
			}
		}
	}

	private Object builtinTypeToString(BuiltinElement bin) throws TranslationException {
		BuiltinType ftype = bin.getFuncType();
		if (ftype.equals(BuiltinType.Divide)) {
//			return "quotient";
			bin.setFuncName("quotient");
		}
		else if (ftype.equals(BuiltinType.Equal)) {
//			return "equal";
			bin.setFuncName("equal");
		}
		else if (ftype.equals(BuiltinType.GT)) {
//			return "greaterThan";
			bin.setFuncName("greaterThan");
		}
		else if (ftype.equals(BuiltinType.GTE)) {
//			return "ge";
			bin.setFuncName("ge");
		}
		else if (ftype.equals(BuiltinType.LT)) {
//			return "lessThan";
			bin.setFuncName("lessThan");
		}
		else if (ftype.equals(BuiltinType.LTE)) {
//			return "le";
			bin.setFuncName("le");
		}
		else if (ftype.equals(BuiltinType.Minus)) {
//			return "difference";
			bin.setFuncName("difference");
		}
		else if (ftype.equals(BuiltinType.Modulus)) {
//			return "mod";
			bin.setFuncName("mod");
		}
		else if (ftype.equals(BuiltinType.Multiply)) {
//			return "product";
			bin.setFuncName("product");
		}
		else if (ftype.equals(BuiltinType.Negative)) {
//			return "negative";
			bin.setFuncName("negative");
		}
		else if (ftype.equals(BuiltinType.Not)) {
//			return "noValue";
			bin.setFuncName("noValue");
		}
		else if (ftype.equals(BuiltinType.NotEqual)) {
//			return "notEqual";
			bin.setFuncName("notEqual");
		}
		else if (ftype.equals(BuiltinType.NotOnly)) {
//			return "notOnlyValue";
			bin.setFuncName("notOnlyValue");
		}
		else if (ftype.equals(BuiltinType.Only)) {
//			return "noValuesOtherThan";
			bin.setFuncName("noValuesOtherThan");
		}
		else if (ftype.equals(BuiltinType.Plus)) {
//			return "sum";
			bin.setFuncName("sum");
		}
		else if (ftype.equals(BuiltinType.Power)) {
//			return "pow";			
			bin.setFuncName("pow");
		}
		else if (ftype.equals(BuiltinType.Assign)) {
//			return "assign";
			bin.setFuncName("assign");
		}

		String builtinName = bin.getFuncName();

		// Note: the order here allows any built-in which overrides the ones in Jena to be picked up preferentially
		//	see if it is known to the ConfigurationManager or if we can find it in the services registry
		boolean status = findOrAddBuiltin(builtinName);
		if (!status) {
			// if not see if it is one already registered
			Builtin bltin = BuiltinRegistry.theRegistry.getImplementation(builtinName);
			if (bltin == null) {
				logger.error("Something went wrong finding/loading Builtin '" + builtinName + "'");
				addError("Unable to resolve built-in '" + builtinName + "'");
			}
		}
		return builtinName;
	}
	
	private boolean findOrAddBuiltin(String builtinName) {
		int cnt = 0;
		// is it known to the ConfigurationManager?
		String[] categories = new String[2];
		try {
			categories[0] = configurationMgr.getReasoner().getReasonerFamily();
			categories[1] = IConfigurationManager.BuiltinCategory;
			List<ConfigurationItem> knownBuiltins = configurationMgr.getConfiguration(categories, false);
			for (int i = 0; knownBuiltins != null && i < knownBuiltins.size(); i++) {
				ConfigurationItem item = knownBuiltins.get(i);
				Object itemName = item.getNamedValue("name");
				if (itemName != null && itemName instanceof String && ((String)itemName).equals(builtinName)) {
					logger.debug("Built-in '" + builtinName + "' found in configuration.");
					return true;
				}
			}
		} catch (ConfigurationException e) {
			// this is ok--new ones won't be found
//			e.printStackTrace();
//			logger.error("Unable to find Builtin '" + builtinName + "' in current configuration: " + e.getLocalizedMessage());
		}

		// Use ServiceLoader to find an implementation of Builtin that has this name
		ServiceLoader<Builtin> serviceLoader = ServiceLoader.load(Builtin.class);
		if( serviceLoader != null ){
			logger.debug("ServiceLoader is OK");
			for( Iterator<Builtin> itr = serviceLoader.iterator(); itr.hasNext() ; ){
				try {
					Builtin bltin = itr.next();
					cnt++;
					if (bltin.getName().equals(builtinName)) {
						String clsname = bltin.getClass().getCanonicalName();
						// TODO is there a reasonable check here?
						if (1 > 0) {
							if (configurationMgr instanceof IConfigurationManagerForEditing) {
								ConfigurationItem newItem = new ConfigurationItem(categories);
								newItem.addNameValuePair(newItem.new NameValuePair("name", builtinName, ConfigurationType.Bag));
								newItem.addNameValuePair(newItem.new NameValuePair("class", clsname, ConfigurationType.Bag));
								((IConfigurationManagerForEditing) configurationMgr).addConfiguration(newItem);
								((IConfigurationManagerForEditing) configurationMgr).saveConfiguration();
								logger.info("Built-in '" + builtinName + "' found in service registry and added to configuration.");
							}
						}
						else {
							logger.info("Built-in '" + builtinName + "' found in service registry.");
						}
						BuiltinRegistry.theRegistry.register(builtinName, bltin);
						return true;
					}
				}
				catch (Throwable t) {
					t.printStackTrace();
					logger.error(t.getLocalizedMessage());
				}
					
			}
		} else {
			logger.debug("ServiceLoader is null");
		}
		logger.debug("Failed to find Builtin with name '" + builtinName + "' after examining " + cnt + " registered Builtins.");
		
		return false;
	}

	private String nodeToString(Node node, TranslationTarget target) throws TranslationException {
		if (node instanceof ConstantNode) {
			Literal litval = constantToLiteral((ConstantNode)node);
			return literalValueToString(litval, target);
		}
		else if (node instanceof NamedNode) {
			NodeType ntype = ((NamedNode)node).getNodeType();
			if (ntype == null) {
				String msg = "Node '" + node.toFullyQualifiedString() + "' has a null node type.";
				addError(msg);
				logger.error(msg);
			}
			else if (ntype.equals(NodeType.VariableNode)) {
				// double-check this; if a concept was declared after reference in a rule or query 
				//	it may have been parsed as a variable but actually be a defined concept 
				OntResource r = getTheModel().getOntResource(getModelName() + "#" + ((NamedNode)node).getName());
				if (r == null) {
					return "?" + ((NamedNode)node).getName();
				}
				// it appears that at time of parsing of the rule or query the named concept was not defined but
				//	was subsequently. Warn user of this apparent error: concepts must be defined before they are 
				//	used in a rule or query.
				String msg = "The concept '" + ((NamedNode)node).getName() + "' ";
				if (ruleInTranslation != null) {
					msg += "in rule '" + ruleInTranslation.getRuleName() + "' ";
				}
				msg += " in model '" + getModelName() + "' is used before it is defined. Please define the concept before referencing it in a query or rule.";
				addError(msg);
				logger.error(msg);
			}
			if (node instanceof RDFTypeNode) {
//				registerPrefix("rdf", "");	I don't think these need an explicit prefix awc 11/23/2010
				if (target.equals(TranslationTarget.QUERY_TRIPLE) || target.equals(TranslationTarget.QUERY_FILTER)) {
					return "<rdf:type>";
				}
				else {
					return "rdf:type";
				}
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
				if (target.equals(TranslationTarget.QUERY_TRIPLE) || target.equals(TranslationTarget.QUERY_FILTER)) {
					return "<" + nts + ">";
				}
				else {
					return nts;
				}
			}
		}
		else if (node instanceof Literal) {
			Object litObj = ((Literal)node).getValue();
			return literalValueToString(litObj, target);
		}
		else if (ITranslator.isKnownNode(node)) {
			return "?" + getNewVariableForRule();
		}
		else if (node == null) {
			throw new TranslationException("Encountered null node in nodeToString; this indicates incorrect intermediate form and should not happen");
		}
		else {
			throw new TranslationException("Nnode '" + node.toString() + "' cannot be translated to Jena format.");
		}
	}

	private Literal constantToLiteral(ConstantNode node) throws TranslationException {
		if (node.getName().equals("PI")) {
			Literal lit = new Literal();
			lit.setValue(Math.PI);
			lit.setOriginalText(node.getName());
			return lit;
		}
		else if (node.getName().equals("e")) {
			Literal lit = new Literal();
			lit.setValue(Math.E);
			lit.setOriginalText(node.getName());
			return lit;
		}
		throw new TranslationException("Unknown constant '" + node.getName() + "' cannot be translated");
	}


	public static synchronized String literalValueToString(Object litObj, TranslationTarget target) {
		if (litObj instanceof String) {
			if (target.equals(TranslationTarget.RULE_BUILTIN)) {
				return "'" + litObj.toString().trim() + "'^^http://www.w3.org/2001/XMLSchema#string";
			}
			else {
				litObj = "\"" + litObj + "\"";
				return (String)litObj;
			}
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
	
	private void registerPrefix(String prefix, String namespace) {
		if (prefix == null) {
			logger.error("Prefix is null in registerPrefix");
		}
		if (!prefixes.containsKey(prefix)) {
			prefixes.put(prefix, namespace);
		}
	}

	protected String createDerivedFilename(String filename, String newext) {
		int lastDot = filename.lastIndexOf('.');
		if (lastDot > 0) {
			return filename.substring(0, lastDot + 1) + newext;
		}
		return filename + "." + newext;
	}

	/**
	 * 	Method to prepare a query by expanding the URIs of concepts to be complete URIs
	 * 
	 * @param model
	 * @param q
	 * @return
	 * @throws InvalidNameException
	 */
	public String prepareQuery(OntModel model, String q) throws InvalidNameException {
		int openBracket = q.indexOf('<');
		if (openBracket > 0) {
			int closeBracket = q.indexOf('>', openBracket);
			if (closeBracket <= openBracket) {
				// this could be a comparison in a FILTER...
				return q;
			}
			String before = q.substring(0, openBracket + 1);
			String url = q.substring(openBracket + 1, closeBracket);
			String rest = q.substring(closeBracket);
			rest = prepareQuery(model, rest);
			if (url.indexOf('#') > 0) {
				return before + url + rest;
			}
			else if (url.indexOf(':') > 0) {
				url = expandPrefixedUrl(model, url);
			}
			else if (isValidLocalName(url)){
				url = findNameNs(model, url);
			}
			return before + url + rest;
		}
		return q;
	}

	protected boolean isValidLocalName(String name) {
		if (name == null || name.indexOf(" ") >= 0 || name.indexOf("?") >= 0) {
			return false;
		}
		return true;
	}

	/**
	 * This method takes an OntModel and a concept name and tries to find the concept in the model or
	 * one of the models imported by the model.
	 * 
	 * @param model -- the OntModel at the root of the search
	 * @param name -- the concept name
	 * @return -- the fuly-qualified name of the concept as found in some model
	 * 
	 * @throws InvalidNameException -- the concept was not found
	 */
	public static synchronized String findNameNs(OntModel model, String name) throws InvalidNameException {
		String uri = findConceptInSomeModel(model, name);
		if (uri != null) {
			return uri;
		}
		Iterator<String> impitr = model.listImportedOntologyURIs(true).iterator();
		while (impitr.hasNext()) {
			String impuri = impitr.next();
			if (!impuri.endsWith("#")) {
				impuri += "#";
			}
			impuri = getUriInModel(model, impuri, name);
			if (impuri != null) {
				logger.debug("found concept with URI '" + impuri + "'");
				return impuri;
			}
		}
		ExtendedIterator<Ontology> oitr = model.listOntologies();
		while (oitr.hasNext()) {
			Ontology onto = oitr.next();
			if (onto != null) {
				ExtendedIterator<OntResource> importsItr = onto.listImports();
				while (importsItr.hasNext()) {
					OntResource or = importsItr.next();
					String ns = or.getURI();
					if (!ns.endsWith("#")) {
						ns = ns + "#";
					}
					String muri = getUriInModel(model, or.getURI(), name);
					if (muri != null) {
						logger.debug("found concept with URI '" + muri + "'");
						return muri;
					}
				}
				// try this ontology--maybe it wasn't in the map used by findConceptInSomeModel
				String muri = getUriInModel(model, onto.getURI() + "#", name);
				if (muri != null) {
					logger.debug("found concept with URI '" + muri + "'");
					return muri;
				}
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug("Failed to find '" + name + "' in any model.");
			ByteArrayOutputStream sos = new ByteArrayOutputStream();
			model.write(sos);
			logger.debug(sos.toString());
		}
		throw new InvalidNameException("'" + name + "' not found in any model.");
	}

	private static synchronized String findConceptInSomeModel(OntModel model, String name) {
		Map<String, String> map = model.getNsPrefixMap();
		Iterator<String> uriitr = map.values().iterator();
		while (uriitr.hasNext()) {
			String ns = uriitr.next();
			String uri = getUriInModel(model, ns, name);
			if (uri != null) {
				logger.debug("found concept with URI '" + uri + "'");
				return uri;
			}
		}
		logger.debug("did not find concept with name '" + name + "'");
		return null;
	}
	
	private static synchronized String getUriInModel(OntModel model, String ns, String name) {
		Resource r = model.getAnnotationProperty(ns + name);
        if (r != null) {
            return r.getURI();
        }
        r = model.getDatatypeProperty(ns + name);
        if (r != null) {
            return r.getURI();
        }
        r = model.getObjectProperty(ns + name);
        if (r != null) {
            return r.getURI();
        }
        r = model.getOntClass(ns + name);
        if (r != null) {
            return r.getURI();
        }
        r = model.getIndividual(ns + name);
        if (r != null) {
            return r.getURI();
        }
        if (RDF.type.getURI().equals(ns + name)) {
        	return RDF.type.getURI();
        }
        return null;
	}

	protected String expandPrefixedUrl(OntModel model, String name) {
		String prefix = name.substring(0, name.indexOf(':'));
		String lname = name.substring(name.indexOf(':') + 1);
		String ns = model.getNsPrefixURI(prefix);
		if (ns != null) {
			if (ns.endsWith("#")) {
				name = ns + lname;
			}
			else {
				name = ns + "#" + lname;
			}
		}
		return name;
	}
	
	protected List<ModelError> addError(String msg) {
		if (errors == null) {
			errors = new ArrayList<ModelError>();
		}
		errors.add(new ModelError(msg, ErrorType.ERROR));
		return errors;
	}

	protected List<ModelError> addError(String msg, ErrorType errType) {
		if (errors == null) {
			errors = new ArrayList<ModelError>();
		}
		errors.add(new ModelError(msg, errType));
		return errors;
	}

	protected List<ModelError> addError(ModelError err) {
		if (errors == null) {
			errors = new ArrayList<ModelError>();
		}
		errors.add(err);
		return errors;
	}

	public String getReasonerFamily() {
		return ReasonerFamily;
	}


	public String getConfigurationCategory() {
		return TranslatorCategory;
	}

	private void setRuleInTranslation(Rule ruleInTranslation) {
		this.ruleInTranslation = ruleInTranslation;
	}

	private Rule getRuleInTranslation() {
		return ruleInTranslation;
	}

	private void setTheModel(OntModel theModel) {
		this.theModel = theModel;
	}

	private OntModel getTheModel() {
		return theModel;
	}


	public Map<String, ConfigurationOption> getTranslatorConfigurationOptions() {
		// This translator doesn't have any configuration items
		return null;
	}


	public boolean configure(ConfigurationItem configItem) {
		// This translator doesn't have any configuration items
		return false;
	}


	public boolean configure(List<ConfigurationItem> configItems) {
		// This translator doesn't have any configuration items
		return false;
	}


	public List<ConfigurationItem> discoverOptimalConfiguration(
			String translationFolder, String modelName, IConfigurationManager configMgr, List<Query> queries) throws FunctionNotSupportedException, ConfigurationException, TranslationException {
		throw new FunctionNotSupportedException(this.getClass().getCanonicalName() + " does not support discovery of optimal configuration.");
	}

	private Query getQueryInTranslation() {
		return queryInTranslation;
	}

	private void setQueryInTranslation(Query queryInTranslation) {
		this.queryInTranslation = queryInTranslation;
	}


	public void setConfigurationManager(IConfigurationManager configMgr) throws ConfigurationException {
//		if ((configMgr instanceof IConfigurationManagerForEditing)) {
//			((IConfigurationManagerForEditing) configMgr).setTranslatorClassName(this.getClass().getCanonicalName());
//		}
		configurationMgr = configMgr;
	}


	private String getModelName() {
		return modelName;
	}


	private void setModelName(String modelName) {
		this.modelName = modelName;
	}


	@Override
	public List<ModelError> translateAndSaveModelWithOtherStructure(
			OntModel model, Object otherStructure, String translationFolder,
			String modelName, List<String> orderedImports, String saveFilename) throws TranslationException,
			IOException, URISyntaxException {
		if (otherStructure instanceof List<?>) {
			OntModel eqModel = null;	// get model
			// remove all equations in this namespace
			boolean equationWarningGiven = false;
			for (Object os: (List<?>)otherStructure) {
				if (os instanceof Equation) {
					if (((Equation)os).getBody() != null && !equationWarningGiven) {
						// only warn for equations, not externals
						addError(new ModelError(this.getClass().getCanonicalName() + " does not currently translate equations", ErrorType.ERROR));
						// add equations
//						System.out.println("Jena translator ready to save equation '" + os.toString() + "'");
						equationWarningGiven = true;
					}
				}
			}
			// save eqModel
		}
		return (errors != null && errors.size() > 0) ? errors : null;
	}

	public List<ModelError> validateRule(com.ge.research.sadl.model.gp.Rule rule) {
		List<ModelError> errors = null;
		
		// conclusion binding tests
		List<GraphPatternElement> thens = rule.getThens();
		for (int i = 0; thens != null && i < thens.size(); i++) {
			GraphPatternElement gpe = thens.get(i);
			if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				if (args == null) {
					ModelError me = new ModelError("Built-in '" + ((BuiltinElement)gpe).getFuncName() + 
							"' with no arguments not legal in rule conclusion", ErrorType.ERROR);
					if (errors == null) {
						errors = new ArrayList<ModelError>();
					}
					errors.add(me);
				}
				else {
					for (int j = 0; j < args.size(); j++) {
						Node arg = args.get(j);
						if (arg instanceof VariableNode) {
							if (!variableIsBoundInOtherElement(rule.getGivens(), 0, gpe, false, false, arg) && 
									!variableIsBoundInOtherElement(rule.getIfs(), 0, gpe, false, false, arg)) {
								ModelError me = new ModelError("Conclusion built-in '" + ((BuiltinElement)gpe).getFuncName() + 
										"', variable argument '" + arg.toString() + "' is not bound in rule premises", ErrorType.ERROR);
								if (errors == null) {
									errors = new ArrayList<ModelError>();
								}
								errors.add(me);
							}
						}
					}
				}
			}
			else if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getSubject() instanceof VariableNode && 
						!variableIsBoundInOtherElement(rule.getGivens(), 0, gpe, false, false, ((TripleElement)gpe).getSubject())
						&& !variableIsBoundInOtherElement(rule.getIfs(), 0, gpe, false, false, ((TripleElement)gpe).getSubject())) {
					ModelError me = new ModelError("Subject of conclusion triple '" + gpe.toString() + 
							"' is not bound in rule premises", ErrorType.ERROR);
					addError(me);
				}
				if (((TripleElement)gpe).getObject() instanceof VariableNode && 
						!variableIsBoundInOtherElement(rule.getGivens(), 0, gpe, false, false, ((TripleElement)gpe).getObject())
						&& !variableIsBoundInOtherElement(rule.getIfs(), 0, gpe, false, false, ((TripleElement)gpe).getObject())) {
					ModelError me = new ModelError("Object of conclusion triple '" + gpe.toString() + 
							"' is not bound in rule premises", ErrorType.ERROR);
					if (errors == null) {
						errors = new ArrayList<ModelError>();
					}
					errors.add(me);
				}
			}
		}
		return errors;
	}
	
	private Map<String, NamedNode> getTypedVars(com.ge.research.sadl.model.gp.Rule rule) {
		Map<String, NamedNode> results = getTypedVars(rule.getGivens());
		Map<String, NamedNode> moreResults = getTypedVars(rule.getIfs());
		if (moreResults != null) {
			if (results == null) {
				results = moreResults;
			}
			else {
				results.putAll(moreResults);
			}
		}
		moreResults = getTypedVars(rule.getThens());
		if (moreResults != null) {
			if (results == null) {
				results = moreResults;
			}
			else {
				results.putAll(moreResults);
			}
		}
		return results;
	}
	
	private Map<String, NamedNode> getTypedVars(List<GraphPatternElement> gpes) {
		Map<String, NamedNode> results = null;
		for (int i = 0; gpes != null && i < gpes.size(); i++) {
			GraphPatternElement gpe = gpes.get(i);
			if (gpe instanceof TripleElement && 
					(((TripleElement)gpe).getModifierType() == null || 
					((TripleElement)gpe).getModifierType().equals(TripleModifierType.None) ||
					((TripleElement)gpe).getModifierType().equals(TripleModifierType.Not)) &&
					((TripleElement)gpe).getSubject() instanceof VariableNode &&
					((TripleElement)gpe).getPredicate() instanceof RDFTypeNode &&
					((TripleElement)gpe).getObject() instanceof NamedNode) {
				if (results == null) {
					results = new HashMap<String, NamedNode>();
				}
				String varName = ((VariableNode)((TripleElement)gpe).getSubject()).getName();
				NamedNode varType = (NamedNode) ((TripleElement)gpe).getObject();
				if (results.containsKey(varName)) {
					NamedNode nn = results.get(varName);
					if (!nn.equals(varType) && !(nn instanceof VariableNode || varType instanceof VariableNode)) {
						addError(new ModelError("Variable '" + varName + "' is typed more than once in the rule.", ErrorType.WARNING));
					}
				}
				results.put(varName, varType);
			}
			else if (gpe instanceof Junction) {
				Object lobj = ((Junction)gpe).getLhs();
				Object robj = ((Junction)gpe).getRhs();
				if (lobj instanceof GraphPatternElement || robj instanceof GraphPatternElement) {
					List<GraphPatternElement> junctgpes = new ArrayList<GraphPatternElement>();
					if (lobj instanceof GraphPatternElement) {
						junctgpes.add((GraphPatternElement) lobj);
					}
					if (robj instanceof GraphPatternElement) {
						junctgpes.add((GraphPatternElement) robj);
					}
					if (results == null) {
						results = getTypedVars(junctgpes);
					}
					else {
						Map<String, NamedNode> moreresults = getTypedVars(junctgpes);
						if (moreresults != null) {
							results.putAll(moreresults);
						}
					}
				}
			}
		}
		return results;
	}

	/**
	 * This method checks the list of GraphPatternElements to see if the specified variable is bound in these elements
	 * 
	 * @param gpes - list of GraphPatternElements to check
	 * @param startingIndex - where to start in the list
	 * @param gp - the element in which this variable appears 
	 * @param boundIfEqual - use the current element for test?
	 * @param matchMustBeAfter - must the binding be after the current element
	 * @param v - the variable Node being checked
	 * @return - true if the variable is bound else false
	 */
	public boolean variableIsBoundInOtherElement(List<GraphPatternElement> gpes, int startingIndex, GraphPatternElement gp, 
			boolean boundIfEqual, boolean matchMustBeAfter, Node v) {
		boolean reachedSame = false;
		for (int i = startingIndex; gpes != null && i < gpes.size(); i++) {
			GraphPatternElement gpe = gpes.get(i);
			while (gpe != null) {
				boolean same = gp == null ? false : gp.equals(gpe);
				if (same) {
					reachedSame = true;
				}
				boolean okToTest = false;
				if (matchMustBeAfter && reachedSame && !same) {
					okToTest = true;
				}
				if (!matchMustBeAfter && (!same || (same && boundIfEqual))) {
					okToTest = true;
				}
				if (okToTest && variableIsBound(gpe, v)) {
					return true;
				}
				gpe = gpe.getNext();
			}
		}
		return false;
	}
	
	private boolean variableIsBound(GraphPatternElement gpe, Node v) {
		if (gpe instanceof TripleElement) {
			if ((((TripleElement)gpe).getSubject() != null &&((TripleElement)gpe).getSubject().equals(v)) || 
					(((TripleElement)gpe).getObject() != null && ((TripleElement)gpe).getObject().equals(v))) {
				return true;
			}
		}
		else if (gpe instanceof BuiltinElement) {
			List<Node> args = ((BuiltinElement)gpe).getArguments();
			// TODO built-ins can actually have more than the last argument as output, but we can only tell this
			//	if we have special knowledge of the builtin. Current SADL grammar doesn't allow this to occur.
			if (args != null && args.get(args.size() - 1) != null && args.get(args.size() - 1).equals(v)) {
				return true;
			}
		}
		else if (gpe instanceof Junction) {
			Object lhsobj = ((Junction)gpe).getLhs();
			if (lhsobj instanceof GraphPatternElement && variableIsBound((GraphPatternElement)lhsobj, v)) {
				return true;
			}
			else if (lhsobj instanceof VariableNode && ((VariableNode)lhsobj).equals(v)) {
				return true;
			}
			Object rhsobj = ((Junction)gpe).getRhs();
			if (rhsobj instanceof GraphPatternElement && variableIsBound((GraphPatternElement)rhsobj, v)) {
				return true;
			}
			else if (rhsobj instanceof VariableNode && ((VariableNode)rhsobj).equals(v)) {
				return true;
			}
		}
		return false;
	}


	@Override
	public String translateEquation(OntModel model, Equation equation) throws TranslationException {
		throw new TranslationException("Equation translation not yet implemented in " + this.getClass().getCanonicalName());
	}
	
	public List<FunctionSignature> getBuiltinFunctionSignatures() throws ConfigurationException{
		return configurationMgr.getReasoner().getImplicitBuiltinSignatures();
	}
	
	@Override
	public String getBuiltinFunctionModel(){
		StringBuilder sb = new StringBuilder();
		sb.append("uri \"");
		sb.append(IReasoner.SADL_BUILTIN_FUNCTIONS_URI);
		sb.append("\" alias ");
		sb.append(IReasoner.SADL_BUILTIN_FUNCTIONS_ALIAS);
		sb.append(".\n\n");
		
		try {
			List<FunctionSignature> bfsigs = getBuiltinFunctionSignatures();
			for(FunctionSignature fs : bfsigs){
				sb.append(fs.FunctionSignatureToSadlModelFormat());
				sb.append("\n\n");
			}
			
			if (configurationMgr instanceof IConfigurationManagerForEditing) {
				IReasoner reasonerInst = null;
				try {
					reasonerInst = configurationMgr.getReasoner();
					Class<?> bcls = reasonerInst.getBuiltinClass();
					 ServiceLoader<?> serviceLoader = ServiceLoader.load(bcls);

					if (serviceLoader != null) {
						for ( Iterator<?> itr = serviceLoader.iterator(); itr.hasNext();) {
							try {
								Object trans = itr.next();
								BuiltinInfo binfo = reasonerInst.getBuiltinInfo(trans
										.getClass());
								if (trans != null) {
									if (trans instanceof TypedBaseBuiltin) {
										FunctionSignature fs = new FunctionSignature(((TypedBaseBuiltin)trans).getFunctionSignatureString(), binfo.getUri());
										sb.append(fs.FunctionSignatureToSadlModelFormat());
										sb.append("\n\n");
									} else {
										String untypedFctSignature = binfo.getName() + "(--)--";
										FunctionSignature fs = new FunctionSignature(untypedFctSignature, binfo.getUri());
										sb.append(fs.FunctionSignatureToSadlModelFormat());
										sb.append("\n\n");
									}
								}
							} catch (Throwable t) {
								t.printStackTrace();
							}
						}
					}

				} catch (ConfigurationException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}

				try {
					List<BuiltinInfo> builtins = ((IConfigurationManagerForEditing)configurationMgr).getAvailableBuiltinsForCurrentReasoner();
					for (int i = 0; builtins != null && i < builtins.size(); i++) {
						BuiltinInfo bi = builtins.get(i);
						if (!builtinInSignatureList(bfsigs, bi)) {
							sb.append("External ");
							sb.append(bi.getName());
							sb.append("(");		
							sb.append("--");
	//						for(int i = 0; i < bi..parameterTypes.length; i++){
	//							if(!this.parameterTypes[i].isEmpty()){
	//								sb.append(this.parameterTypes[i].toLowerCase());
	//								sb.append(" X");
	//								if(i != this.parameterTypes.length - 1){
	//									sb.append(", ");
	//								}
	//							}
	//						}
							sb.append(") returns ");
	//						sb.append(this.returnType.toLowerCase());
							sb.append("--");
							sb.append(":\n\""); 
	//						sb.append(this.uri);
							sb.append(bi.getUri());
							sb.append("\".\n\n");
						}
					}
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		} catch (ConfigurationException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}

		return sb.toString();
	}
	
	private boolean builtinInSignatureList(List<FunctionSignature> bfsigs, BuiltinInfo bi) {
		String biName = bi.getName();
		String biNS = bi.getClassName().substring(0, bi.getClassName().lastIndexOf('.'));
		if (bfsigs != null) {
			for (int i = 0; i < bfsigs.size(); i++) {
				FunctionSignature bfsig = bfsigs.get(i);
				if (bfsig.getName().equals(biName) && bfsig.getUri().substring(0, bfsig.getUri().indexOf('#')).equals(biNS)) {
					return true;
				}
			}
		}
		return false;
	}


	@Override
	public boolean isBuiltinFunction(String builtinName){
		// is it known to the ConfigurationManager?
		String[] categories = new String[2];
		try {
			categories[0] = configurationMgr.getReasoner().getReasonerFamily();
			categories[1] = IConfigurationManager.BuiltinCategory;
			List<ConfigurationItem> knownBuiltins = configurationMgr.getConfiguration(categories, false);
			if (knownBuiltins != null) {
				for (ConfigurationItem item : knownBuiltins) {
					Object itemName = item.getNamedValue("name");
					if (itemName != null && itemName instanceof String && ((String)itemName).equals(builtinName)) {
						return true;
					}
				}
			}
		} catch (ConfigurationException e) {
			// this is ok--one more check to go.
		}

		// Use ServiceLoader to find an implementation of Builtin that has this name
		ServiceLoader<Builtin> serviceLoader = ServiceLoader.load(Builtin.class);
		if( serviceLoader != null ){
			for( Iterator<Builtin> itr = serviceLoader.iterator(); itr.hasNext() ; ){
				try {
					Builtin bltin = itr.next();
					if (bltin.getName().equals(builtinName)) {
						return true;
					}
				}
				catch (Throwable t) {
					t.printStackTrace();
					logger.error(t.getLocalizedMessage());
				}
			}
		}
		
		return false;
	}
	
	@Override
	public Enum isBuiltinFunctionTypeCheckingAvailable(){
		return IReasoner.SADL_BUILTIN_FUNCTIONS_TYPE_CHECKING_AVAILABILITY.NAME_ONLY;
	}


	@Override
	public String getLocalFragmentNamespace(String name) throws InvalidNameException, ConfigurationException {
		OntModel model = getTheModel();
		if (model != null) {
			String fqn = findNameNs(model, name);
			if (fqn != null) {
				if (fqn.indexOf('#') > 0) {
					return fqn.substring(0, fqn.indexOf('#'));
				}
				else {
					throw new InvalidNameException("Found fully qualified name '" + fqn + "' but it doesn't appear to be a valid in-namespace concept URI.");
				}
			}
			else {
				throw new InvalidNameException("No URI found in any model for local name '" + name + "'");
			}
		}
		throw new ConfigurationException("No model is identified in the translator, unable to search for '" + name + "'");
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
