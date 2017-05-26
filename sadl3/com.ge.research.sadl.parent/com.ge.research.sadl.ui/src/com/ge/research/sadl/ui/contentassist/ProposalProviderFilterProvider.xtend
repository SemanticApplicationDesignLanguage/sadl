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

import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.ISadlOntologyHelper
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.PropOfSubject
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
import com.google.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext

import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

import static extension org.eclipse.xtext.EcoreUtil2.*

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
	IOntologyContextProvider ontologyContextProvider;

	@Inject
	ISadlOntologyHelper ontologyHelper;
	
	@Inject
	IModelProcessorProvider modelProcessorProvider;

	def Predicate<IEObjectDescription> getCrossReferenceFilter(ContentAssistContext context) {
		if (context === null || context.currentModel === null || context.currentModel.eResource === null) {
			return Predicates.alwaysFalse;
		}
		
		val resource = context.currentModel.eResource;
		val processor = modelProcessorProvider.getProcessor(resource);
		val acceptor = new ProposalProviderValidationAcceptor;
		val ontologyContext = ontologyContextProvider.getOntologyContext(context, processor, acceptor).orNull;
		if (ontologyContext === null) {
			return Predicates.alwaysFalse;
		}

		val grammarContextId = ontologyContext.grammarContextId.orNull;
		val contextClass = ontologyContext.contextClass.orNull;
		
		if (ONTOLOGY_DEPENDENT_CONTEXT_IDS.contains(grammarContextId)) {
			return [
				if (SADL_RESOURCE == EClass) {
					ontologyHelper.validate(ontologyContext, EObjectOrProxy as SadlResource);
					return acceptor.apply(it);
				}
				return false;
			];
		}

		val it = context.currentModel;
		return switch (grammarContextId) {
			case grammarContextId == SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE ||
				grammarContextId == SADLPRIMARYTYPEREFERENCE_TYPE:
				createPrimaryTypeRefFilter
			case SADLPROPERTYINITIALIZER_PROPERTY:
				createPropertyInitializerFilter
			case SADLSTATEMENT_SUPERELEMENT: 
				switch (contextClass) {
					case SADL_CLASS_OR_PROPERTY_DECLARATION:
						createPrimaryTypeRefFilter.and(createNotSelfClassOrPropertyFilter)
					default:
						createPrimaryTypeRefFilter
				}
			case PRIMARYEXPRESSION_VALUE:
				switch (contextClass) {
					case TEST_STATEMENT:
						createPropertyFilter
					case PROP_OF_SUBJECT:
						createSubjectOfPropertyFilter
					default:
						Predicates.alwaysFalse
				}
			default:
				Predicates.alwaysFalse
		};
	}
	
	private def <T> Predicate<T> and(Predicate<? super T> left, Predicate<? super T> right) {
		return Predicates.and(left, right);
	}

	private def Predicate<IEObjectDescription> createSubjectOfPropertyFilter(EObject currentModel) {
		if (currentModel instanceof PropOfSubject) {
			val left = currentModel.left;
			if (left instanceof Name) {
				val resource = left.name;
				if (resource !== null) {
					val Predicate<IEObjectDescription> predicate = [
						return EClass === SADL_RESOURCE;
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
						return candidate.eContainer instanceof SadlProperty;
					}
					return false;
				];
				return predicate;
			}
		}
		return Predicates.alwaysFalse;
	}

	private def createNotSelfClassOrPropertyFilter(EObject currentModel) {
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

}
