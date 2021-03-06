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
package com.naturalsemanticsllc.sadl.reasoner;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner;

import com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.RuleNotFoundException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;


/*
 * This class differs from its parent in that rules are loaded in stages and when certain operations 
 * occur, namely changing rules or changing the data, rules must also be unloaded to restart the staged
 * sequence of rule application to create the inferred model.
 */
public class JenaAugmentedReasonerPlugin extends JenaReasonerPlugin {

	@Override
	public String getConfigurationCategory() {
		return "Augmented-" + super.getConfigurationCategory();
//		return super.getConfigurationCategory();
	}
	
	@Override
	public GenericRuleReasoner getReasonerOnlyWhenNeeded() throws ConfigurationException {
		return super.getReasonerOnlyWhenNeeded();
	}
		
	@Override
	protected void loadRules(OntModel m, String modelName) {
		super.loadRules(m, modelName);
	}
	
	@Override
	public boolean addRule(String rule) {
		return super.addRule(rule);
	}
	
	@Override
	public boolean deleteRule(String ruleName) throws RuleNotFoundException {
		return super.deleteRule(ruleName);
	}
		
	@Override
	public boolean addRules(List<String> rules) {
		return super.addRules(rules);
	}
	
	@Override
	public boolean addTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		return super.addTriple(sub, pred, obj);
	}
	
	@Override
	public boolean deleteTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		return super.deleteTriple(sub, pred, obj);
	}
	
	@Override
	public boolean loadInstanceData(InputStream is, String format) throws IOException, ConfigurationException {		
		return super.loadInstanceData(is, format);
	}
	
	@Override
	public boolean loadInstanceData(Object model) throws ConfigurationException {
		return super.loadInstanceData(model);
	}
	
	@Override
	public boolean loadInstanceData(OntModel model) throws ConfigurationException {
		return super.loadInstanceData(model);
	}
	
	@Override
	public boolean loadInstanceData(String instanceDatafile) throws IOException, ConfigurationException {		
		return super.loadInstanceData(instanceDatafile);
	}
	
	@Override
	public boolean loadInstanceData(URI instanceDatafile) throws IOException, ConfigurationException {
		return super.loadInstanceData(instanceDatafile);
	}

}
