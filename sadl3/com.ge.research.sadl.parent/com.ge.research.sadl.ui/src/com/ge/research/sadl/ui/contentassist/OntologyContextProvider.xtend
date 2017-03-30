/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.contentassist

import com.ge.research.sadl.processing.ISadlOntologyHelper.Context
import com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.google.common.base.Optional
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.GrammarUtil
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext

import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import static com.ge.research.sadl.sADL.SADLPackage.Literals.*
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlSimpleTypeReference

/**
 * Singleton service converting a Eclipse-based content assist context into
 * an ontology helper context.
 * 
 * @author akos.kitta
 */
class OntologyContextProvider {

	def Optional<Context> getOntologyContext(ContentAssistContext it) {

		for (grammarElement : firstSetGrammarElements?.filter(Assignment)) {
			val ruleName = GrammarUtil.containingParserRule(grammarElement).name;
			val featureName = grammarElement.feature;
			val clazz = currentModel?.eClass;
			val key = '''«ruleName»_«featureName»'''.toString.toUpperCase;

			if (clazz == SADL_PROPERTY_INITIALIZER && key == SADLPROPERTYINITIALIZER_VALUE) {
				val initializer = currentModel as SadlPropertyInitializer;
				val instance = initializer.eContainer as SadlInstance;
				val type = instance.type;
				if (type instanceof SadlSimpleTypeReference) {
					val builder = new ContextBuilder(type.type);
					builder.grammarContextId = SADLPROPERTYINITIALIZER_VALUE;
					builder.restriction = initializer.property;
					return Optional.of(builder.build);
				}
			}

		}

		return Optional.absent;
	}

}
