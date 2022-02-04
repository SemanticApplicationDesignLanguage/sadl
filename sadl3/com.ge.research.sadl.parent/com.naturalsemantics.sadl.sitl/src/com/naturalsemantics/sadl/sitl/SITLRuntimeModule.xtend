/************************************************************************
 * 
 * Project: SADL
 * Copyright 2007-2022 - General Electric Company, All Rights Reserved
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
/*
 * SADL Extension for SADL Import Template Language (SITL)
 * Copyright 2022 - Natural Semantics, LLC, All Rights Reserved
 */
package com.naturalsemantics.sadl.sitl;

import com.ge.research.sadl.scoping.SadlQualifiedNameProvider
import com.google.inject.Binder
import org.eclipse.xtext.generator.IOutputConfigurationProvider
import com.ge.research.sadl.generator.SADLOutputConfigurationProvider
import javax.inject.Singleton
import org.eclipse.xtext.naming.IQualifiedNameConverter
import com.ge.research.sadl.scoping.SadlQualifiedNameConverter
import org.eclipse.xtext.linking.impl.LinkingDiagnosticMessageProvider
import com.ge.research.sadl.validation.SoftLinkingMessageProvider
import com.ge.research.sadl.ValueConverterService
import org.eclipse.xtext.validation.ResourceValidatorImpl
import com.ge.research.sadl.validation.ResourceValidator
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import org.eclipse.xtext.linking.impl.DefaultLinkingService
import com.ge.research.sadl.scoping.ErrorAddingLinkingService
import org.eclipse.xtext.parsetree.reconstr.IParseTreeConstructor
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.parsetree.reconstr.ITokenStream
import java.io.IOException
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionStrategy
import com.ge.research.sadl.resource.SadlResourceDescriptionStrategy
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionManager
import com.ge.research.sadl.resource.SadlResourceDescriptionManager
import org.eclipse.xtext.linking.impl.ImportedNamesAdapter
import com.ge.research.sadl.scoping.SilencedImportedNamesAdapter
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContextPreferenceValuesProvider

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class SITLRuntimeModule extends AbstractSITLRuntimeModule {
	override bindIQualifiedNameProvider() {	//same
		SadlQualifiedNameProvider
	}
	
	override configure (Binder binder) {	// same
		super.configure(binder);
		binder.bind(IOutputConfigurationProvider).to(SADLOutputConfigurationProvider).in(Singleton);
	}
	
//	def Class<? extends DefaultResourceDescriptionManager> bindDefaultResourceDescriptionManager() {
//		return SadlResourceDescriptionManager;
//	}
	
//	def Class<? extends IEObjectDocumentationProvider> bindIEObjectDocumentationProvider() {
//		return SadlEObjectDocumentationProvider;
//	}
	
	def Class<? extends IQualifiedNameConverter> bindIQualifiedNameCoverter() {		// same
		return SadlQualifiedNameConverter;
	}
	
	def Class<? extends LinkingDiagnosticMessageProvider> bindILinkingDiagnosticMessageProvider() {	// same
		SoftLinkingMessageProvider
	}
	
	override bindIValueConverterService() {	// same
		ValueConverterService
	}
	
	def Class<? extends ResourceValidatorImpl> bindResourceValidatorImpl() {	// same
		return ResourceValidator
	}
	
	def Class<? extends IModelProcessorProvider> bindIModelProcessorProvider() {	// similar
		return SadlModelProcessorProvider
//		return JenaBasedDialogModelProcessor
	}
	
// this is what's in SADL and SRL	
	def Class<? extends DefaultLinkingService> bindDefaultLinkingService() {	// same
		return ErrorAddingLinkingService;
	}
	
	def Class<? extends IParseTreeConstructor> bindIParseTreeConstructor() {	// same
		NoImplParseTreeConstructor
	}
	
//	def Class<? extends SadlMarkerLocationProvider> bindSadlMarkerLocationProvider() {	// no customization needed?
//		RequirementsMarkerLocationProvider
//	}
	
	static class NoImplParseTreeConstructor implements IParseTreeConstructor {	// same
		
		override serializeSubtree(EObject object, ITokenStream out) throws IOException {
			throw new UnsupportedOperationException("TODO: auto-generated method stub")
		}
		
	}
	
//	def Class<? extends DefaultResourceDescriptionStrategy> bindResourceDescritpionStrategy() {
//		return SadlResourceDescriptionStrategy;
//	}
// This is what it is for SRL
//	def Class<? extends DefaultResourceDescriptionStrategy> bindResourceDescritpionStrategy() {
//		return ResourceDescriptionStrategy
//	}
	
//	def Class<? extends IDeclarationExtensionsContribution> bindIDeclarationExtensionsContribution() {
//    	return RequirementsDeclarationExtensionsContribution;
//  	}

	def Class<? extends DefaultResourceDescriptionStrategy> bindResourceDescritpionStrategy() {
		return SadlResourceDescriptionStrategy;
	}

	def Class<? extends DefaultResourceDescriptionManager> bindDefaultResourceDescriptionManager() {
		return SadlResourceDescriptionManager;
	}

	def Class<? extends ImportedNamesAdapter> bindImportedNamesAdapter() {
		return SilencedImportedNamesAdapter; 
	}
	
	def Class<? extends ProcessorContextPreferenceValuesProvider> bindProcessorContextPreferenceValuesProvider() {
		return ProcessorContextPreferenceValuesProvider;
	}
	
}
