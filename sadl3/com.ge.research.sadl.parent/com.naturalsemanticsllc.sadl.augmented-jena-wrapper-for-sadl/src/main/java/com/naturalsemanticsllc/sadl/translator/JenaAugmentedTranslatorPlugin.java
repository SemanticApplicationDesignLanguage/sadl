/************************************************************************
 * Copyright © 2021 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemanticsllc.sadl.translator;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.IntersectionClass;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.UnionClass;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFList;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.utils.ResourceManager;

public class JenaAugmentedTranslatorPlugin extends JenaTranslatorPlugin implements ITranslator {

	@Override
	public String getConfigurationCategory() {
		return "Augmented-" + super.getConfigurationCategory();
//		return super.getConfigurationCategory();
	}

	
	@Override
	public List<ModelError> translateAndSaveModel(OntModel model, List<Rule> ruleList,
			String translationFolder, String modelName, List<String> orderedImports, String saveFilename)
			throws TranslationException, IOException, URISyntaxException {
		if (errors != null) {
			errors.clear();
		}
		saveRuleFileAfterModelSave = false;
		// a Jena model simply writes out the OWL file
		translateAndSaveModel(model, translationFolder, modelName, orderedImports, saveFilename);

		/*
		 * This method differs from its parent in that 
		 * 	a) it looks for rule stages and for default values and creates multiple rule files 
		 * 		identified in their name by stage (.rules-stage<n>)
		 *  b) the stage of the rule and the level of the default are used, with the rule stage
		 *  	having preference when the stage and rule are the same, to create a sequence
		 *  	of files starting with .rules-stage0, and without gaps. The lack of gaps allows
		 *  	easier cleanup of old files. 
		 *  Note that if there are no staged rules and there are no defaults the resulting rule
		 *  	file will be the same as with the parent translator 
		 */
		
		Map<Integer, List<Rule>> rulesByStage = null;
		Integer maxStage = 0;
		
		if (ruleList != null && ruleList.size() > 0) {
			rulesByStage = new HashMap<Integer, List<Rule>>();
			for (Rule rule : ruleList) {
				int stage = rule.getStage();
				if (stage < 0) stage = 0;
				if (!rulesByStage.containsKey(stage)) {
					List<Rule> stageRules = new ArrayList<Rule>();
					rulesByStage.put(stage, stageRules);
				}
				rulesByStage.get(stage).add(rule);
				if (maxStage < stage) {
					maxStage = stage;
				}
			}
		}
			
		String ruleFilename = createDerivedFilename(saveFilename, "rules");
		String fullyQualifiedRulesFilename = translationFolder + File.separator + ruleFilename;
		boolean useStageInName = rulesByStage != null && rulesByStage.size() > 1;

		// Remove old rule filenames
		// 1. any non-staged rule file
		File f1 = new File(fullyQualifiedRulesFilename);
		if (f1.exists()) {
			if (!f1.delete()) {
				addError("Failed to delete old rule file '" + fullyQualifiedRulesFilename + "'");
			}
		}
		// 2. any staged rule files until there are no more
		int stageNum = 0;
		do {
			String stageFQRuleFilename = fullyQualifiedRulesFilename + "-stage" + stageNum;
			File f2 = new File(stageFQRuleFilename);
			if (f2.exists()) {
				if (!f2.delete()) {
					addError("Failed to delete old rule file '" + stageFQRuleFilename + "'");
				}
				stageNum++;
			}
			else {
				stageNum = -1;
			}
		} while (stageNum >= 0);
		
	
		// Save rule file(s), both for rules and any default values, interleaving per the stage and level
		Set<Integer> stages = rulesByStage != null ? rulesByStage.keySet() : null;	
		Map<Integer, List<String>> defvalrulesbylevel = getDefaultValueRules(model);
		Set<Integer> levels = defvalrulesbylevel != null ? defvalrulesbylevel.keySet() : null;
		
		if (stages != null || levels != null) {
			// we have some rules to save
			Integer maxRuleStage = stages != null ? Collections.max(stages) : -1;
			Integer maxDVLevel = levels != null ? Collections.max(levels) : -1;
			
			int stageOrLevel = 0;
			int ruleSuffix = 0;
			SadlUtils su = new SadlUtils();

			do {
				if (stages != null && stages.contains(stageOrLevel)) {
					// we have a stage for this stageOrLevel
					String stageFQRuleFilename;
					if (useStageInName) {
						stageFQRuleFilename = rulesByStage.size() == 1 ? fullyQualifiedRulesFilename : fullyQualifiedRulesFilename + "-stage" + ruleSuffix;
					}
					else {
						stageFQRuleFilename = fullyQualifiedRulesFilename;
					}
					if (rulesByStage.get(stageOrLevel) != null && rulesByStage.get(stageOrLevel).size() > 0) {
						translateAndSaveRules(model, rulesByStage.get(stageOrLevel), modelName, stageFQRuleFilename);
					}
					else {
						addError("A stage without any rules. This should not happen! Please report with description of situation.");
					}
					ruleSuffix++;
				}
				
				if (levels != null && levels.contains(stageOrLevel)) {
					// we have a default value for this stageOrLevel
					List<String> rules = defvalrulesbylevel.get(stageOrLevel);
					if (rules != null) {
						StringBuilder sb = new StringBuilder();
						sb.append("# Jena Rule file for default values, level ");
						sb.append(stageOrLevel);
						sb.append("\n# Created by the JenaAugmentedTranslator\n\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\n");
						for (String rule : rules) {
							sb.append(rule);
							sb.append("\n");
						}
						String levelFQRuleFilename = fullyQualifiedRulesFilename + "-stage" + ruleSuffix;
						File ruleFile = new File(levelFQRuleFilename);
						su.stringToFile(ruleFile, sb.toString(), true);
					}
					ruleSuffix++;
				}
				stageOrLevel++;
			} while (stageOrLevel <= maxRuleStage || stageOrLevel <= maxDVLevel);		
		}
		return (errors != null && errors.size() > 0) ? errors : null;
	}


	/**
	 * Method to generate rules for default values
	 * @param model
	 * @return --  map of default rules by level
	 */
	private Map<Integer, List<String>> getDefaultValueRules(OntModel model) {
		
		StmtIterator stmtitr = model.listStatements(null, RDFS.seeAlso, (RDFNode)null);
		if (stmtitr.hasNext()) {
			String defvaluri = ResourceManager.ACUITY_DEFAULTS_NS + "DefaultValue";
			Property hasDefaultProp = model.getProperty(ResourceManager.ACUITY_DEFAULTS_NS + "hasDefault");
			Property hasLevelProp = model.getProperty(ResourceManager.ACUITY_DEFAULTS_NS + "hasLevel");
			Property appliesToPropChainProp = model.getProperty(ResourceManager.ACUITY_DEFAULTS_NS + "appliesToPropertyChain");
			Property propertyElementProp = model.getProperty(ResourceManager.ACUITY_DEFAULTS_NS + "propertyElement");
			Property nextPropChainElementProp = model.getProperty(ResourceManager.ACUITY_DEFAULTS_NS + "nextPropertyChainElement");
			
			Map<Integer, List<String>> defValRulesByLevel = new HashMap<Integer, List<String>>();
			while (stmtitr.hasNext()) {
				OntClass onClass = null;
				List<Property> propChainList = new ArrayList<Property>();
				RDFNode defval = null;
				int lvl = 0;
				Statement stmt = stmtitr.nextStatement();
				RDFNode obj = stmt.getObject();
				Individual objInst = null;
				if (obj.isResource() && obj.asResource().canAs(Individual.class)) {
					objInst = obj.asResource().as(Individual.class);
					if (objInst.hasRDFType(defvaluri)) {	
						Resource subj = stmt.getSubject();
						if (subj.canAs(OntClass.class)) {
							onClass = subj.as(OntClass.class);
						}
						defval = objInst.getPropertyValue(hasDefaultProp);
						RDFNode lvlnode = objInst.getPropertyValue(hasLevelProp);
						if (lvlnode != null && lvlnode.isLiteral()) {
							lvl = lvlnode.asLiteral().getInt();
						}
						else {
							lvl = 0;
						}
						
						RDFNode atp = objInst.getPropertyValue(appliesToPropChainProp);
						while (atp != null) {
							if (atp.isResource()) {
								Statement pepstmt = atp.asResource().getProperty(propertyElementProp);
								if (pepstmt != null && pepstmt.getObject().isResource()) {
									Resource pep = pepstmt.getObject().asResource();
									if (pep.canAs(Property.class)) {
										propChainList.add(pep.as(Property.class));
									}
								}
								Statement npce = atp.asResource().getProperty(nextPropChainElementProp);
								if (npce != null) {
									atp = npce.getObject();
								}
								else {
									atp = null;
								}
							}
						}
					}
				}
				String rule = getRuleForDefault(model, objInst, onClass, propChainList, defval);
				if (rule != null) {
					if (!defValRulesByLevel.containsKey(lvl)) {
						defValRulesByLevel.put(lvl, new ArrayList<String>());
					}
					defValRulesByLevel.get(lvl).add(rule);
				}
			}
			return defValRulesByLevel;
		}
		return null;
	}


	private String getRuleForDefault(OntModel model, Individual objInst, OntClass onClass, List<Property> propChainList, RDFNode defval) {
		String ruleName = objInst.getURI();
		StringBuilder sb = new StringBuilder("[");
		sb.append(ruleName);
		sb.append(": (");
		sb.append("?i rdf:type ");
		sb.append(onClass.getURI());
		sb.append("), ");
		int vcntr = 0;
		String lastvar = "?i";
		for (int i = 0; i < propChainList.size(); i++) {
			Property p = propChainList.get(i);
			if (i < propChainList.size() - 1) {
				if (i > 0) sb.append(", ");
				sb.append("(");
				sb.append(lastvar);
				sb.append(" ");
				sb.append(p.getURI());
				sb.append(" ");
				String var = "?v" + vcntr++;
				sb.append(var);
				lastvar = var;
				sb.append(")");
			}
			else {
				if (i > 0) sb.append(", ");
				sb.append("noValue(");
				sb.append(lastvar);
				sb.append(",");
				sb.append(p.getURI());
				sb.append(") -> ");
				
				if (defval.isResource() && defval.asResource().isURIResource()) {
					sb.append("(");
					sb.append(lastvar);
					sb.append(" ");
					sb.append(p.getURI());
					sb.append(" ");
					sb.append(defval.asResource().getURI());
					sb.append(")");
				}
				else if (defval.isLiteral()) {
					sb.append("(");
					sb.append(lastvar);
					sb.append(" ");
					sb.append(p.getURI());
					sb.append(" ");
					sb.append(literalToString(model, defval.asLiteral()));
					sb.append(")");
				}
				else if (defval.canAs(Individual.class)){
					// must be a blank node, which may be a structure with property values
					// create a set of arguments to thereExists built-in to create the structure and assign in rule conclusion
					Individual defvalinst = defval.as(Individual.class);
					ExtendedIterator<Resource> typitr = defvalinst.listRDFTypes(true);
					OntClass typecls = null;
					if (typitr.hasNext()) {
						typecls = typitr.next().as(OntClass.class);
					}
					if (typecls != null) {
						sb.append("thereExists(");
						sb.append(typecls.getURI());
						StmtIterator sitr = defvalinst.listProperties();
						while (sitr.hasNext()) {
							Statement stmt = sitr.nextStatement();
							if (!stmt.getPredicate().equals(RDF.type)) {
								sb.append(", ");
								sb.append(stmt.getPredicate().getURI());
								sb.append(", ");
								RDFNode obj = stmt.getObject();
								if (obj.isLiteral()) {
									sb.append(literalToString(model, obj.asLiteral()));
								}
								else {
									sb.append(obj.asResource().getURI());
								}
							}
						}
						sb.append(")");	// closes arguments of thereExists
					}
					else {
						addError(new ModelError("Class of default value individual not found", ErrorType.ERROR));
					}
				}
			}
		}
		sb.append("]");
		
		return sb.toString();
	}
	
	private String literalToString(OntModel model, Literal litval) {
		String dturi = litval.asLiteral().getDatatypeURI();
		boolean forceQuotes = dturi != null ? isRDFDatatypeString(model, dturi, litval) : true;
		if (litval.asLiteral().getDatatypeURI() == null) {
			String lf = litval.asLiteral().getLexicalForm();
			if (forceQuotes || lf.contains(" ") || lf.contains("\"")) {
				String s = litval.asLiteral().getLexicalForm();
				return makeStringDoubleQuoted(s); 
			}
			return litval.asLiteral().getLexicalForm();
		}
		if (forceQuotes || litval.asLiteral().getDatatypeURI().equals(XSD.xstring.getURI())) {
			String s = litval.asLiteral().getLexicalForm();
			if (s.startsWith("\"") && s.endsWith("\"")) {
				s = s.substring(1, s.length() - 2);
			}
			return makeStringDoubleQuoted(s); 
		}
		else {
			return litval.asLiteral().getLexicalForm();
		}

	}
	
	private boolean isRDFDatatypeString(OntModel model, String dturi, RDFNode value) {
		if (dturi.startsWith(XSD.getURI())) {
			// this is a direct type
			if (dturi.equals(XSD.xstring.getURI()) || dturi.equals(XSD.date.getURI()) || 
					dturi.equals(XSD.dateTime.getURI()) || dturi.equals(XSD.time.getURI()) || 
					dturi.equals(XSD.anyURI.getURI())) {
				return true;
			}
			else {
				return false;
			}
		}
		Resource rsrc = model.getResource(dturi);
		// is there an equivalent class?
		List<String> intersection = null;
		List<String> union = null;
		String xsdtype = null;
		RDFNode eqCls = null;
		StmtIterator eqitr = rsrc.listProperties(OWL.equivalentClass);
		if (eqitr.hasNext()) {
			eqCls = eqitr.nextStatement().getObject();
			if (eqCls.canAs(Resource.class)) {
				if (eqCls.canAs(UnionClass.class)) {
					union = new ArrayList<String>();
					RDFList opList = eqCls.as(UnionClass.class).getOperands();
					for (int i = 0; i < opList.size(); i++) {
						RDFNode opnode = opList.get(i);
						union.add(opnode.asResource().getLocalName());
					}
				}
				else if (eqCls.canAs(IntersectionClass.class)) {
					intersection  = new ArrayList<String>();
					RDFList opList = eqCls.as(IntersectionClass.class).getOperands();
					for (int i = 0; i < opList.size(); i++) {
						RDFNode opnode = opList.get(i);
						intersection.add(opnode.asResource().getLocalName());
					}
				}
				else {
					RDFNode odt = eqCls.as(OntResource.class).getPropertyValue(OWL2.onDatatype);
					if (odt != null && odt.canAs(Resource.class)) {
						String ns = odt.as(Resource.class).getNameSpace();
						if (ns.startsWith(XSD.getURI())) {
							xsdtype = odt.as(Resource.class).getLocalName();
						}
					}
				}
			}
		}
		else {
			System.err.println("Unable to determine if RDFDatatype '" + dturi + " has string value.");
		}
		if (xsdtype != null && xsdtype.equals(XSD.xstring.getLocalName())) {
			return true;
		}
		if (union != null) {
			if (union.contains(XSD.xstring.getLocalName())) {
				if (value.asLiteral().getValue() instanceof Number) {
					return false;
				}
				return true;
			}
		}
		return false;
	}
	
	private String makeStringDoubleQuoted(String s) {
		s = s.replace("\"", "\\\"");
		return "\"" + s + "\"";
	}


}
