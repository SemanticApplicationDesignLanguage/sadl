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
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.ISadlOntologyHelper.Context
import com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder
import com.ge.research.sadl.processing.OntModelProvider
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.PropOfSubject
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlRangeRestriction
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.sADL.SubjHasProp
import com.google.common.base.Optional
import com.google.inject.Inject
import com.google.inject.Singleton
import org.apache.jena.ontology.OntModel
import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.slf4j.LoggerFactory

import static com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder.createWithoutSubject
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import com.ge.research.sadl.sADL.SadlDisjointClasses
import com.ge.research.sadl.sADL.SadlSameAs
import org.eclipse.xtext.nodemodel.impl.LeafNodeWithSyntaxError
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.impl.HiddenLeafNode
import org.eclipse.xtext.nodemodel.impl.CompositeNodeWithSemanticElement
import org.eclipse.emf.common.util.EList
import org.eclipse.xtext.nodemodel.impl.HiddenLeafNodeWithSyntaxError
import org.eclipse.xtext.nodemodel.impl.CompositeNode
import org.eclipse.xtext.nodemodel.impl.AbstractNode
import com.ge.research.sadl.sADL.SadlNecessaryAndSufficient
import com.ge.research.sadl.sADL.SadlPropertyCondition
import com.ge.research.sadl.sADL.SadlHasValueCondition
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlNestedInstance

/**
 * Singleton service ontology context provider service for SADL.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlOntologyContextProvider implements IOntologyContextProvider {

	static val LOGGER = LoggerFactory.getLogger(SadlOntologyContextProvider);

	@Inject
	IOntologyContextProvider ontologyContextProvider;

	@Inject
	IModelProcessorProvider modelProcessorProvider;

	override Optional<Context> getOntologyContext(ContentAssistContext context) {
		val resource = context.currentModel.eResource;
		val processor = modelProcessorProvider.getProcessor(resource);
		val acceptor = new ProposalProviderValidationAcceptor;
		val ontologyContext = ontologyContextProvider.getOntologyContext(context, processor, acceptor).orNull;
		return Optional.fromNullable(ontologyContext);
	}

	override Optional<Context> getOntologyContext(ContentAssistContext it, IModelProcessor processor) {
		return getOntologyContext(it, processor, ValidationAcceptor.NOOP);
	}

	override Optional<Context> getOntologyContext(ContentAssistContext it, IModelProcessor processor,
		ValidationAcceptor acceptor) {

		for (grammarElement : firstSetGrammarElements?.filter(Assignment)) {
			val clazz = currentModel?.eClass;
			val key = TO_STRING.apply(grammarElement);
			if (key == SADLNESTEDINSTANCE_TYPE) {
				if (currentModel instanceof SadlNestedInstance) {
					val cont = (currentModel as SadlNestedInstance).eContainer
					if (cont instanceof SadlPropertyInitializer) {
						val psr = (cont as SadlPropertyInitializer).property
						val builder = new ContextBuilder(psr, processor) => [
							grammarContextId = key
							validationAcceptor = acceptor;
							contextClass = clazz;
						]
						return Optional.of(builder.build)
					}
				}
			}
			else if (key == SADLPROPERTYINITIALIZER_VALUE) {
				// subject should be the class of the statement subject
				// restriction[0] should be the property
				val initializer = currentModel.propertyInitializer;
				if (initializer !== null) {
					val instance = initializer.eContainer as SadlInstance;
					val type = instance.type !== null ? instance.type : getTypeFromPropertyInitializers(instance.propertyInitializers);
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
					else if (type instanceof SadlResource) {
						val builder = new ContextBuilder(type, processor) => [
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
					else {
						if (LOGGER.debugEnabled) {
							LOGGER.warn('''Case handling incomplete: «key» [Class: «clazz.name»]''');
						}
					}
				} else {
					// need to do something else to get the type
					// val i = 0;
				}
			} else if (key == SADLHASVALUECONDITION_RESTRICTION) {
				var SadlResource sr = null;
				if (currentModel instanceof SadlHasValueCondition) {
					val cont = currentModel.eContainer;
					if (cont instanceof SadlPropertyCondition) {
						sr = (cont as SadlPropertyCondition).property;
					}
				}
				else if (currentModel instanceof SadlPropertyCondition) {
					sr = (currentModel as SadlPropertyCondition).property;
				}
				else if (currentModel instanceof SadlProperty) {
					sr = (currentModel as SadlProperty).nameOrRef;
				}
				if (sr !== null) {
					val type = sr;
					val builder = new ContextBuilder(type, processor) => [
						grammarContextId = key;
						validationAcceptor = acceptor;
						contextClass = clazz;
					];
					return Optional.of(builder.build);
				}
			} else if (key == SADLPROPERTYINITIALIZER_PROPERTY) {
				val initializer = currentModel.propertyInitializer;
				if (initializer !== null && initializer.eContainer instanceof SadlInstance) {
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
				} else if (initializer !== null && initializer.eContainer instanceof SadlClassOrPropertyDeclaration) {
					val builder = createWithoutSubject(currentModel.ontModel, processor, currentModel) => [
						grammarContextId = key;
						validationAcceptor = acceptor;
						contextClass = clazz;
					];
					return Optional.of(builder.build);
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
			} else if (key == SADLSTATEMENT_PROPCONDITIONS) {
				val subj = (currentModel as SadlNecessaryAndSufficient).subject;
				if (subj instanceof SadlSimpleTypeReference) {
					val type = (subj as SadlSimpleTypeReference).type;
					if (type instanceof SadlResource) {
						val builder = new ContextBuilder(type, processor) => [
							grammarContextId = key;
							validationAcceptor = acceptor;
							contextClass = clazz;
						];
						return Optional.of(builder.build);
					}
				}
				
//			} else if (key == SADLPROPERTYRESTRICTION_OTHERPROPERTY) {
//				val initializer = currentModel.eContainer;
//				if (initializer !== null) {
//					if (initializer instanceof SadlResource) {
//						
//					}
//				}
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
			} else if (key == SADLSTATEMENT_CLASSES) {
				if (currentModel instanceof SadlDisjointClasses) {
					val djclses = (currentModel as SadlDisjointClasses).classes
					if (djclses.size > 0) {
						for (sr : djclses) {
							val type = sr.name;
							
							val builder = new ContextBuilder(type, processor) => [
								grammarContextId = key;
								validationAcceptor = acceptor;
								contextClass = clazz;
							]
							return Optional.of(builder.build);
						}
					}
				}
			} else if (key == SADLSTATEMENT_CLASSORPROPERTY) {
				if (currentModel instanceof SadlClassOrPropertyDeclaration) {
					val cplst = (currentModel as SadlClassOrPropertyDeclaration).classOrProperty;
					if (cplst.size > 0) {
						for (sr : cplst) {
							val type = sr.name;
							if (type !== null) {
								val builder = new ContextBuilder(type, processor) => [
									grammarContextId = key;
									validationAcceptor = acceptor;
									contextClass = clazz;
								]
								return Optional.of(builder.build);
							}
							else {
								val builder = createWithoutSubject(currentModel.ontModel, processor, currentModel) => [
									grammarContextId = key;
									validationAcceptor = acceptor;
									contextClass = clazz;
								];
								return Optional.of(builder.build);
							}
						}
					}
				}
			} else if (key == SADLSTATEMENT_SAMEAS) {
				if (currentModel instanceof SadlSameAs) {
					val builder = createWithoutSubject(currentModel.ontModel, processor, currentModel) => [
						grammarContextId = key;
						validationAcceptor = acceptor;
						contextClass = clazz;
					];
					return Optional.of(builder.build);
				}
			} else if (key == SADLCARDINALITYCONDITION_CARDINALITY) {
				if (currentModel instanceof SadlModel) {
					// this may be a SadlResource at the start of a line
					val sr = getPrecedingSadlResource(it);
					if (sr.name !== null) {
						val type = sr.name;

						val builder = new ContextBuilder(type, processor) => [
							grammarContextId = key;
							validationAcceptor = acceptor;
							contextClass = clazz;
						]
						return Optional.of(builder.build);
					}
					if (LOGGER.debugEnabled) {
						LOGGER.warn('''Unhandled case: «key» [Class: «clazz.name»]''');
					}
				}			
			} else if (key == SADLSTATEMENT_TYPE) {
				val type = getPrecedingSadlResource(it);
				if (type !== null) {
					val builder = new ContextBuilder(type, processor) => [
						grammarContextId = key;
						validationAcceptor = acceptor;
						contextClass = clazz;
					];
					return Optional.of(builder.build);
				}
				else {
					val builder = createWithoutSubject(currentModel.ontModel, processor, currentModel) => [
						grammarContextId = key;
						validationAcceptor = acceptor;
						contextClass = clazz;
					];
					return Optional.of(builder.build);
				}
			} else if (ONTOLOGY_INDEPENDENT_CONTEXT_IDS.contains(key)) {
				val builder = createWithoutSubject(currentModel.ontModel, processor, currentModel) => [
					grammarContextId = key;
					validationAcceptor = acceptor;
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
		
	def getTypeFromPropertyInitializers(EList<SadlPropertyInitializer> propInitializers) {
		if (propInitializers !== null && 
			propInitializers.size > 0) {
			val piitr = propInitializers.iterator;
			while (piitr.hasNext) {
				val pi = piitr.next
				if (pi instanceof SadlPropertyInitializer) {
					val spi = (pi as SadlPropertyInitializer)
					if (spi.type !== null) {
						return spi.type
					}
					if (pi.eContainer !== null &&
						pi.eContainer instanceof SadlInstance) {
						return (pi.eContainer as SadlInstance).nameOrRef
					}
				}
			}
		}
		return null
	}

	def getPrecedingSadlResource(ContentAssistContext context) {
		if (context.lastCompleteNode instanceof LeafNodeWithSyntaxError &&
			(context.lastCompleteNode as LeafNodeWithSyntaxError).previousSibling instanceof HiddenLeafNode &&
			((context.lastCompleteNode as LeafNodeWithSyntaxError).previousSibling as HiddenLeafNode).previousSibling instanceof CompositeNodeWithSemanticElement) {
			val eobj = (((context.lastCompleteNode as LeafNodeWithSyntaxError).previousSibling as HiddenLeafNode).previousSibling as CompositeNodeWithSemanticElement).semanticElement
			if (eobj instanceof SadlResource) {
				return (eobj as SadlResource)
			}
		}
		if (context.currentNode instanceof HiddenLeafNodeWithSyntaxError) {
			var cn = context.currentNode;
			while (cn !== null) {
				if (cn instanceof AbstractNode) {
					if (cn instanceof CompositeNode) {
						var CompositeNodeWithSemanticElement cnwse = null
						if ((cn as CompositeNode).firstChild instanceof CompositeNodeWithSemanticElement) {
							cnwse = (cn as CompositeNode).firstChild as CompositeNodeWithSemanticElement
						}
						else if ((cn as CompositeNode).firstChild instanceof CompositeNode &&
							((cn as CompositeNode).firstChild as CompositeNode).firstChild instanceof CompositeNodeWithSemanticElement) {
							cnwse = (((cn as CompositeNode).firstChild) as CompositeNode).firstChild as CompositeNodeWithSemanticElement
						}
						if (cnwse !== null) {
							if (cnwse.semanticElement instanceof SadlResource) {
								return (cnwse.semanticElement as SadlResource)
							}
							
						}
					}
					cn = cn.previousSibling;
				}
			}
		}
		return null
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
