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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;

import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.utils.ResourceManager;

public class JenaAugmentedTranslatorPlugin extends JenaTranslatorPlugin {

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
		 * This method differs from its parent in that it
		 * 	a) looks for rule stages and creates multiple rule files identified in their name by stage
		 *  b) and furthermore, looks for default values in the OWL model and constructs default value
		 *  	rules in potentially multiple files according to the default value levels
		 */
		
		Map<Integer, List<Rule>> rulesByStage = null;
		
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
			}
		}
				
		String ruleFilename = createDerivedFilename(saveFilename, "rules");
		String fullyQualifiedRulesFilename = translationFolder + File.separator + ruleFilename;
		Integer maxStage = 0;
		if (rulesByStage != null && rulesByStage.size() > 0) {
			Set<Integer> stages = rulesByStage.keySet();
			int i = 0;
			for (Integer stage : stages) {
				if (stage > maxStage) maxStage = stage;
				String stageFQRuleFilename = rulesByStage.size() == 1 ? fullyQualifiedRulesFilename : fullyQualifiedRulesFilename + "-stage" + i++;
				if (rulesByStage.get(stage) != null && rulesByStage.get(stage).size() > 0) {
					translateAndSaveRules(model, rulesByStage.get(stage), modelName, stageFQRuleFilename);
				}
				else {
					// there isn't a rules file but make sure there isn't an old one around that needs to be deleted
					File oldRuleFile = new File(stageFQRuleFilename);
					if (oldRuleFile.exists() && oldRuleFile.isFile()) {
						try {
							oldRuleFile.delete();
						}
						catch (Exception e) {
							addError("Failed to delete old rules file '" + stageFQRuleFilename + "'.");
							logger.error("Failed to delete old rule file '" + stageFQRuleFilename + "': " + e.getLocalizedMessage());
						}
					}
				}
			}
		}
				
		Map<Integer, List<String>> defvalrulesbylevel = getDefaultValueRules(model);
		if (defvalrulesbylevel != null) {
			Set<Integer> levels = defvalrulesbylevel.keySet();
			SadlUtils su = new SadlUtils();
			for (Integer level : levels) {
				List<String> rules = defvalrulesbylevel.get(level);
				if (rules != null) {
					StringBuilder sb = new StringBuilder();
					sb.append("# Jena Rule file for default values, level ");
					sb.append(level);
					sb.append("\nCreated by the JenaAugmentedTranslator\n\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\n");
					for (String rule : rules) {
						sb.append(rule);
						sb.append("\n");
					}
					int fidx = maxStage + level;
					String levelFQRuleFilename = fullyQualifiedRulesFilename + "-stage" + fidx;
					File ruleFile = new File(levelFQRuleFilename);
					su.stringToFile(ruleFile, sb.toString(), true);
				}
			}
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
				String rule = getRuleForDefault(objInst, onClass, propChainList, defval);
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


	private String getRuleForDefault(Individual objInst, OntClass onClass, List<Property> propChainList, RDFNode defval) {
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
				sb.append("(");
				sb.append(lastvar);
				sb.append(" ");
				sb.append(p.getURI());
				sb.append(" ");
				String var = "?v" + vcntr++;
				sb.append(var);
				sb.append(")");
				sb.append("), (");
				sb.append(var);
				sb.append(" ");
				lastvar = var;
			}
			else {
				sb.append("noValue(");
				sb.append(lastvar);
				sb.append(",");
				sb.append(p.getURI());
				sb.append(") -> (");
				
				if (defval.isResource() && defval.asResource().isURIResource()) {
					sb.append(lastvar);
					sb.append(" ");
					sb.append(p.getURI());
					sb.append(defval.asResource().getURI());
				}
				else if (defval.isLiteral()) {
					sb.append(lastvar);
					sb.append(" ");
					sb.append(p.getURI());
					sb.append(defval.asLiteral().getValue().toString());
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
						sb.append(", ");
						StmtIterator sitr = defvalinst.listProperties();
						while (sitr.hasNext()) {
							Statement stmt = sitr.nextStatement();
							if (!stmt.getPredicate().equals(RDF.type)) {
								sb.append(stmt.getPredicate().getURI());
								sb.append(", ");
								RDFNode obj = stmt.getObject();
								if (obj.isLiteral()) {
									sb.append(obj.asLiteral().getValue().toString());
								}
								else {
									sb.append(obj.asResource().getURI());
								}
							}
						}
					}
					else {
						addError(new ModelError("Class of default value individual not found", ErrorType.ERROR));
					}
				}
			}
		}
		sb.append(")]");
		
		return sb.toString();
	}
}