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
import com.ge.research.sadl.processing.OntModelProvider
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.google.common.base.Optional
import com.google.inject.Singleton
import com.hp.hpl.jena.ontology.OntModel
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.slf4j.LoggerFactory

import static com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder.createWithoutSubject
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import com.ge.research.sadl.sADL.SadlModel
import org.eclipse.xtext.EcoreUtil2

/**
 * Singleton service converting a Eclipse-based content assist context into
 * an ontology helper context.
 * 
 * @author akos.kitta
 */
@Singleton
class OntologyContextProvider {

	private static val LOGGER = LoggerFactory.getLogger(OntologyContextProvider);

	/**
	 * Transforms the content assist context into a ontology helper context and returns with it.
	 * May return with an absent when the transformation is not viable.
	 */
	def Optional<Context> getOntologyContext(ContentAssistContext it) {
		return getOntologyContext(it, ValidationAcceptor.NOOP);
	}

	/**
	 * Transforms the content assist context into a ontology helper context with the given acceptor.
	 */
	def Optional<Context> getOntologyContext(ContentAssistContext it, ValidationAcceptor acceptor) {

		for (grammarElement : firstSetGrammarElements?.filter(Assignment)) {
			val clazz = currentModel?.eClass;
			val key = TO_STRING.apply(grammarElement);

			if (key == SADLPROPERTYINITIALIZER_VALUE) {
				val initializer = currentModel.propertyInitializer;
				val instance = initializer.eContainer as SadlInstance;
				val type = instance.type;
				if (type instanceof SadlSimpleTypeReference) {
					val builder = new ContextBuilder(type.type) => [
						grammarContextId = key;
						addRestriction(initializer.property);
						validationAcceptor = acceptor;
						contextClass = clazz;
					];
					return Optional.of(builder.build);
				}
			} else if (key == SADLPROPERTYINITIALIZER_PROPERTY) {
				val initializer = currentModel.propertyInitializer;
				if (initializer != null) {
					val instance = initializer.eContainer as SadlInstance;
					val type = instance.type;
					if (type instanceof SadlSimpleTypeReference) {
						val builder = new ContextBuilder(type.type) => [
							grammarContextId = key;
							validationAcceptor = acceptor;
							contextClass = clazz;
						];
						return Optional.of(builder.build);
					}
				}
				else {
					if (LOGGER.debugEnabled) {
						LOGGER.warn('''Case handling incomplete: «key» [Class: «clazz.name»]''');
					}
				}
			} else if (ONTOLOGY_INDEPENDENT_CONTEXT_IDS.contains(key)) {
				val builder = createWithoutSubject(currentModel.ontModel) => [
					grammarContextId = key;
					contextClass = clazz;
				];
				return Optional.of(builder.build);
			} else {
				if (LOGGER.debugEnabled) {
					LOGGER.warn('''Unhandled case: «key» [Class: «clazz.name»]''');
				}
			}

		}

		return Optional.absent;
	}

	private def OntModel getOntModel(EObject it) {
		return OntModelProvider.find(eResource);
	}
	
	private def dispatch getPropertyInitializer(SadlModel it) {
		return EcoreUtil2.getAllContentsOfType(it, SadlPropertyInitializer).head;
	}
	
	private def dispatch getPropertyInitializer(SadlPropertyInitializer it) {
		return it;
	}

}
