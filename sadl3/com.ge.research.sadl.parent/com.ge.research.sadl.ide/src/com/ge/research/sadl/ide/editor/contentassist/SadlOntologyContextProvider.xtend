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
package com.ge.research.sadl.ide.editor.contentassist

import com.ge.research.sadl.processing.IModelProcessor
import com.ge.research.sadl.processing.ISadlOntologyHelper.Context
import com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder
import com.ge.research.sadl.processing.OntModelProvider
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.PropOfSubject
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlRangeRestriction
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.sADL.SubjHasProp
import com.google.common.base.Optional
import com.google.inject.Singleton
import com.hp.hpl.jena.ontology.OntModel
import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.slf4j.LoggerFactory

import static com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder.createWithoutSubject
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.QueryStatement

/**
 * Singleton service ontology context provider service for SADL.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlOntologyContextProvider implements IOntologyContextProvider {

	static val LOGGER = LoggerFactory.getLogger(SadlOntologyContextProvider);

	override Optional<Context> getOntologyContext(ContentAssistContext it, IModelProcessor processor) {
		return getOntologyContext(it, processor, ValidationAcceptor.NOOP);
	}

	override Optional<Context> getOntologyContext(ContentAssistContext it, IModelProcessor processor,
		ValidationAcceptor acceptor) {

		for (grammarElement : firstSetGrammarElements?.filter(Assignment)) {
			val clazz = currentModel?.eClass;
			val key = TO_STRING.apply(grammarElement);

			if (key == SADLPROPERTYINITIALIZER_VALUE) {
				val initializer = currentModel.propertyInitializer;
				if (initializer !== null) {
					val instance = initializer.eContainer as SadlInstance;
					val type = instance.type;
					if (type instanceof SadlSimpleTypeReference) {
						val builder = new ContextBuilder(type.type, processor) => [
							grammarContextId = key;
							if (initializer instanceof SadlPropertyInitializer) {
								addRestriction((initializer as SadlPropertyInitializer).property);
							} else if (initializer instanceof SadlResource) {
								addRestriction(initializer)
							}
							validationAcceptor = acceptor;
							contextClass = clazz;
						];
						return Optional.of(builder.build);
					}
				} else {
					// need to do something else to get the type
					// val i = 0;
				}
			} else if (key == SADLPROPERTYINITIALIZER_PROPERTY) {
				val initializer = currentModel.propertyInitializer;
				if (initializer !== null) {
					val instance = initializer.eContainer as SadlInstance;
					val type = instance.type;
					if (type instanceof SadlSimpleTypeReference) {
						// Broken AST. We have type reference to a non-existing type eventually.
						// For instance: `Shape is a class. myShape is a S<|>`
						// TODO: Is it possible (and required at all) to move this logic to the context builder? 
						if (!type.type.eIsProxy) {
							val builder = new ContextBuilder(type.type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					}
				} else {
					if (LOGGER.debugEnabled) {
						LOGGER.warn('''Case handling incomplete: «key» [Class: «clazz.name»]''');
					}
				}
			} else if (key == SADLSTATEMENT_SUPERELEMENT) {
				val initializer = currentModel.getClassOrPropertyInitializer;
				if (initializer !== null) {
					if (initializer instanceof SadlClassOrPropertyDeclaration) {
						val type = (initializer as SadlClassOrPropertyDeclaration).classOrProperty.get(0)
						if (type instanceof SadlResource) {
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					} else if (initializer instanceof SadlRangeRestriction) {
						val type = (initializer as SadlRangeRestriction).classOrPropertyInitializer
						if (type instanceof SadlResource) {
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					}
				}
			} else if (key == SADLPRIMARYTYPEREFERENCE_TYPE) {
				val initializer = currentModel.getClassOrPropertyInitializer;
				if (initializer !== null) {
					if (initializer instanceof SadlClassOrPropertyDeclaration) {
						val type = (initializer as SadlClassOrPropertyDeclaration).classOrProperty.get(0)
//						val instance = initializer.eContainer as SadlClassOrPropertyDeclaration
						if (type instanceof SadlResource) {
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					} else if (initializer instanceof SadlRangeRestriction) {
						val type = (initializer as SadlRangeRestriction).classOrPropertyInitializer
//						val instance = initializer.eContainer as SadlClassOrPropertyDeclaration
						if (type instanceof SadlResource) {
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					}
				}
			} else if (key == SADLPROPERTYRESTRICTION_TYPEONLY) {
				val initializer = currentModel.getClassOrPropertyInitializer;
				if (initializer !== null) {
					if (initializer instanceof SadlClassOrPropertyDeclaration) {
						val type = (initializer as SadlClassOrPropertyDeclaration).classOrProperty.get(0)
//						val instance = initializer.eContainer as SadlClassOrPropertyDeclaration
						if (type instanceof SadlResource) {
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					} else if (initializer instanceof SadlRangeRestriction) {
						val type = (initializer as SadlRangeRestriction).classOrPropertyInitializer
//						val instance = initializer.eContainer as SadlClassOrPropertyDeclaration
						if (type instanceof SadlResource) {
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							];
							return Optional.of(builder.build);
						}
					}
				}
			} else if (key == PROPOFSUBJECT_RIGHT) {
				val initializer = currentModel.getPropOfSubjectInitializer
				if (initializer !== null) {
					val left = (initializer as PropOfSubject).left
					if (left instanceof Name) {
						val type = (left as Name).name
						val builder = new ContextBuilder(type, processor) => [
							grammarContextId = key;
							validationAcceptor = acceptor;
							contextClass = clazz;
						];
						return Optional.of(builder.build);
					}
				}
			} else if (key == PROPOFSUBJECT_PROP) {
				val initializer = currentModel.getSubjHasPropInitializer
				if (initializer !== null) {
					
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

	//--------------getPropertyInitializer--------------------------

	private def dispatch getPropertyInitializer(EObject it) {
		return null;
	}

	private def dispatch getPropertyInitializer(SadlModel it) {
		val spi = EcoreUtil2.getAllContentsOfType(it, SadlPropertyInitializer)
		if (!spi.empty) {
			return (spi as List<SadlPropertyInitializer>).head;
		}
		val spsr = EcoreUtil2.getAllContentsOfType(it, SadlResource)
		if (!spsr.empty) {
			return (spsr as List<SadlResource>).head;
		}
		return null;
	}

	private def dispatch getPropertyInitializer(SadlSimpleTypeReference it) {
		return it;
	}

	private def dispatch getPropertyInitializer(SadlPropertyInitializer it) {
		return it;
	}

	//--------------getClassOrPropertyInitializer--------------------------

	private def dispatch getClassOrPropertyInitializer(EObject it) {
		return null;
	}

	private def dispatch getClassOrPropertyInitializer(SadlInstance it) {
		return it;
	}

	private def dispatch getClassOrPropertyInitializer(SadlSimpleTypeReference it) {
		return it;
	}

	private def dispatch getClassOrPropertyInitializer(SadlClassOrPropertyDeclaration it) {
		return it;
	}

	private def dispatch getClassOrPropertyInitializer(SadlRangeRestriction it) {
		return it;
	}

	private def dispatch getClassOrPropertyInitializer(BinaryOperation it) {
		return it;
	}

	private def dispatch getClassOrPropertyInitializer(SadlModel it) {
		return EcoreUtil2.getAllContentsOfType(it, SadlClassOrPropertyDeclaration).head;
	}

	//--------------getPropOfSubjectInitializer--------------------------

	private def dispatch getPropOfSubjectInitializer(EObject it) {
		return null;
	}

	private def dispatch getPropOfSubjectInitializer(PropOfSubject it) {
		return it;
	}

	private def dispatch getPropOfSubjectInitializer(SadlModel it) {
		return EcoreUtil2.getAllContentsOfType(it, PropOfSubject).head;
	}

	//--------------getSubjHasPropInitializer--------------------------

	protected def dispatch getSubjHasPropInitializer(EObject it) {
		return null;
	}

	protected def dispatch getSubjHasPropInitializer(SubjHasProp it) {
		return it;
	}

	protected def dispatch getSubjHasPropInitializer(SadlModel it) {
		return EcoreUtil2.getAllContentsOfType(it, SubjHasProp).head;
	}

	protected def dispatch getSubjHasPropInitializer(QueryStatement it) {
		return EcoreUtil2.getAllContentsOfType(it, SubjHasProp).head;
	}

}
