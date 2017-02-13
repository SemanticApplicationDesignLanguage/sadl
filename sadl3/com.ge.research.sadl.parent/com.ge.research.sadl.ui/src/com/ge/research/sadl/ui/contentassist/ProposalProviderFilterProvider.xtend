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

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.jena.JenaBasedSadlModelValidator
import com.ge.research.sadl.jena.OntModelProvider
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.SadlModelProcessorProvider.CompositeModelProcessor
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.utils.ImportHelper
import com.google.common.base.Predicate
import com.google.common.base.Predicates
import com.google.common.collect.Iterables
import com.google.inject.Inject
import com.google.inject.Provider
import com.google.inject.Singleton
import com.hp.hpl.jena.ontology.OntResource
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.GrammarUtil
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.validation.Issue

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

import static extension org.eclipse.xtext.EcoreUtil2.*
import org.eclipse.emf.ecore.util.EcoreUtil
import com.ge.research.sadl.sADL.PropOfSubject
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.SadlHasValueCondition
import com.ge.research.sadl.sADL.SadlPropertyCondition

/**
 * Provides filter for removing items from the content proposal.
 * 
 * @author akos.kitta
 */
@Singleton
class ProposalProviderFilterProvider {

	@Inject
	ImportHelper importHelper;

	@Inject
	IModelProcessorProvider processorProvider;

	@Inject
	DeclarationExtensions declarationExtensions;

	@Inject
	Provider<DeclarationExtensions> extensionProvider;

	def Predicate<IEObjectDescription> getCrossReferenceFilter(ContentAssistContext context) {
		val grammarElements = context.firstSetGrammarElements;
		if (grammarElements.nullOrEmpty) {
			return Predicates.alwaysTrue;
		}

		val predicates = <Predicate<IEObjectDescription>>newArrayList();
		for (grammarElement : grammarElements.filter(Assignment)) {
			// 'x is a' => for types from imported resources.						
			val ruleName = GrammarUtil.containingParserRule(grammarElement).name;
			val featureName = grammarElement.feature;
			val currentModel = context.currentModel;
			val clazz = currentModel?.eClass;
			val key = '''«ruleName»_«featureName»'''.toString.toUpperCase;
			if (key == 'SADLPRIMARYTYPEREFERENCE_TYPE') {
				// 'x is a' => for all declared types from current model and the imported (including the transitive) ones.
				predicates.add(currentModel.createPrimaryTypeRefFilter);
			} else if (clazz === SADL_INSTANCE && key == 'SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE') {
				// 'x is a' => for primitive types.
				// XXX: Handled via the keyword computer. Primitive types automatically imported.
			} else if (clazz === SADL_PROPERTY_INITIALIZER && key == 'SADLPROPERTYINITIALIZER_PROPERTY') {
				// 'x is a Rectangle with' => for properties available on type Rectangle.
				predicates.add(currentModel.createPropertyInitializerFilter)
			} else if (clazz === SADL_CLASS_OR_PROPERTY_DECLARATION && key == 'SADLPRIMARYTYPEREFERENCE_TYPE') {
				// 'x is a type of' => all classes and properties.
			} else if (clazz === SADL_CLASS_OR_PROPERTY_DECLARATION && key == 'SADLSTATEMENT_SUPERELEMENT') {
				// 'Person is a class. {Man, Woman} are types of'
				// => for super elements, imported types should be there except the defined types: Man, Woman.
				val predicate = Predicates.and(currentModel.createPrimaryTypeRefFilter, // First get the available types.
				currentModel.createSelfClassOrPropertyFilter); // Then filter out those which we are defining right now.
				predicates.add(predicate);
			} else if (clazz === SADL_RANGE_RESTRICTION) {
				// 'Person is a class described by birth with a single value of type '
				// => allow all primitive and complex, visible types.
				predicates.add(Predicates.alwaysTrue);
			} else if (clazz === TEST_STATEMENT && key == 'PRIMARYEXPRESSION_VALUE') {
				// Test: => properties are allowed
				predicates.add(currentModel.createPropertyFilter);
			} else if (clazz === PROP_OF_SUBJECT && key == 'PRIMARYEXPRESSION_VALUE') {
				// Test: width of => Only types which have property width is allowed here.				
				predicates.add(currentModel.createSubjectOfPropertyFilter);
			} else if (clazz === SADL_NECESSARY_AND_SUFFICIENT && key == 'SADLPROPERTYCONDITION_PROPERTY') {
				// A Person is a Man only if => only properties are available.
				predicates.add(currentModel.createPropertyFilter);
			} else if (clazz === SADL_HAS_VALUE_CONDITION && key == 'SADLRESOURCE_NAME') {
				// A Person is a Man only if gender always has value => instances of Gender type.
				// XXX: filter CA properly based on the OWL model.
				predicates.add(currentModel.createExplicitValueFilter); 
			} else {
				// println('''Unhandled case with class: «clazz» and key: «key»''');
			}
		}
		return if(predicates.nullOrEmpty) Predicates.alwaysFalse else Predicates.or(predicates);
	}
	
	def Predicate<IEObjectDescription> createExplicitValueFilter(EObject currentModel) {
		if (currentModel instanceof SadlHasValueCondition && currentModel.eContainer instanceof SadlPropertyCondition) {
			val property = (currentModel.eContainer as SadlPropertyCondition).property
			if (property instanceof SadlResource) {
				val Predicate<IEObjectDescription> predicate = [
						if (EClass === SADL_RESOURCE) {
							val candidate = EObjectOrProxy as SadlResource;
							return isSadlResourceInDomainOfProperty(property, candidate);
						}
						return false;
					];
					return predicate;
			}
			
		}
		return Predicates.alwaysFalse;
	}
	
	private def Predicate<IEObjectDescription> createSubjectOfPropertyFilter(EObject currentModel) {
		if (currentModel instanceof PropOfSubject) {
			val left = currentModel.left;
			if (left instanceof Name) {
				val resource = left.name;
				if (resource !== null) {
					val Predicate<IEObjectDescription> predicate = [
						if (EClass === SADL_RESOURCE) {
							val candidate = EObjectOrProxy as SadlResource;
							return isSadlResourceInDomainOfProperty(candidate, resource);
						}
						return false;
					];
					return predicate;
				}
			}

		}
		return Predicates.alwaysFalse;
	}
	
	private def createPropertyFilter(EObject currentModel) {
		val Predicate<IEObjectDescription> predicate = [
			if (EClass === SADL_RESOURCE) {
				val candidate = EObjectOrProxy as SadlResource;
				return candidate.eContainer instanceof SadlProperty;
			}
			return false;
		];
		return predicate;
	}

	private def createPropertyInitializerFilter(EObject currentModel) {
		if (currentModel.eContainer instanceof SadlInstance) {
			val instance = currentModel.eContainer as SadlInstance;
			val type = instance.type;
			if (type instanceof SadlSimpleTypeReference) {
				val Predicate<IEObjectDescription> predicate = [
					if (EClass === SADL_RESOURCE) {
						val candidate = EObjectOrProxy as SadlResource;
						if (candidate.eContainer instanceof SadlProperty) {
							return isSadlResourceInDomainOfProperty(type.type, candidate);
						}
					}
					return false;
				];
				return predicate;
			}
		}
		return Predicates.alwaysFalse;
	}

	private def boolean isSadlResourceInDomainOfProperty(SadlResource currentModel, SadlResource candidate) {
		// TODO: akitta the setup for the current model should be moved outside of this block.
		val ontologyModel = OntModelProvider.find(currentModel.eResource)
		if (ontologyModel !== null && candidate !== null) {
			val currentModelConceptUri = declarationExtensions.getConceptUri(candidate);
			val proposedConceptUri = declarationExtensions.getConceptUri(currentModel);
			val property = ontologyModel.getProperty(proposedConceptUri);
			if (property !== null) {
				val srtype = declarationExtensions.getOntConceptType(candidate);
				var OntResource ontrsrc;
				if (srtype.equals(OntConceptType.CLASS)) {
					ontrsrc = ontologyModel.getOntClass(currentModelConceptUri);
				} else if (srtype.equals(OntConceptType.INSTANCE)) {
					ontrsrc = ontologyModel.getIndividual(currentModelConceptUri);
				} else if (srtype.equals(OntConceptType.VARIABLE)) {
					// TBD
				} else if (srtype.equals(OntConceptType.DATATYPE_PROPERTY)) {
					ontrsrc = ontologyModel.getDatatypeProperty(currentModelConceptUri);
				}
				val pair = createValidator(currentModel.eResource);
				val validator = pair.key;
				val issues = pair.value;
				validator.checkPropertyDomain(ontologyModel, candidate, currentModel, true);
				return null !== ontrsrc && issues.filter[severity !== Severity.ERROR].empty;
			}
		}
		return false
	}

	private def createSelfClassOrPropertyFilter(EObject currentModel) {
		if (currentModel instanceof SadlClassOrPropertyDeclaration) {
			val filteredUris = currentModel.classOrProperty.map[EcoreUtil.getURI(it)];
			val Predicate<IEObjectDescription> predicate = [
				if (EClass === SADL_RESOURCE) {
					return !filteredUris.contains(EObjectURI);
				}
				return false;
			]
			return predicate;

		}
		return Predicates.alwaysFalse;
	}

	private def createPrimaryTypeRefFilter(EObject currentModel) {
		val model = currentModel.getContainerOfType(SadlModel);
		if (model !== null) {
			val importedResourceUris = importHelper.getAllImportedResourceUris(model);
			val Predicate<IEObjectDescription> predicate = [
				val declaration = EObjectOrProxy?.eContainer;
				return if (declaration instanceof SadlClassOrPropertyDeclaration) {
					val candidateResourceUri = declaration.getContainerOfType(SadlModel)?.baseUri;
					return Iterables.contains(importedResourceUris, candidateResourceUri);
				} else {
					false;
				}
			];
			return predicate;
		}
		return Predicates.alwaysFalse;
	}

	private def createValidator(Resource resource) {
		val issues = <Issue>newArrayList();
		val acceptor = new ValidationAcceptor([issues.add(it)] as IAcceptor<Issue>);
		val model = OntModelProvider.find(resource);
		val ^extension = extensionProvider.get;
		val processor = resource.processor;
		return new JenaBasedSadlModelValidator(acceptor, model, ^extension, processor, null) -> issues;
	}

	private def getProcessor(Resource resource) {
		val processor = processorProvider.getProcessor(resource);
		if (processor instanceof JenaBasedSadlModelProcessor) {
			return processor as JenaBasedSadlModelProcessor;
		} else if (processor instanceof CompositeModelProcessor) {
			return processor.processors.filter(JenaBasedSadlModelProcessor).head;
		}
		throw new IllegalStateException('''Cannot get Jena based model processor for resource: «resource».''');
	}

}
