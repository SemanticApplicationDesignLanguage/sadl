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

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.ontology.OntModel;

import com.ge.research.sadl.jena.translator.JenaTranslatorPlugin;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.reasoner.TranslationException;

public class JenaAugmentedTranslatorPlugin extends JenaTranslatorPlugin {

	@Override
	public String getConfigurationCategory() {
		return "Augmented-" + super.getConfigurationCategory();
	}

	
	@Override
	public List<ModelError> translateAndSaveModel(OntModel model, List<Rule> ruleList,
			String translationFolder, String modelName, List<String> orderedImports, String saveFilename)
			throws TranslationException, IOException, URISyntaxException {
		/*
		 * This method differs from its parent in that it
		 * 	a) looks for rule stages and creates multiple rule files identified in their name by stage
		 *  b) and furthermore, looks for default values in the OWL model and constructs default value
		 *  	rules in potentially multiple files according to the default value levels
		 */
		
		return super.translateAndSaveModel(model, ruleList, translationFolder, modelName, orderedImports, saveFilename);
	}
}
