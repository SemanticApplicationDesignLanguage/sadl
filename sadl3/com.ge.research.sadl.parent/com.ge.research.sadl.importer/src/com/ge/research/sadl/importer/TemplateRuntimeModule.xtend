/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.importer

import com.ge.research.sadl.importer.scoping.TemplateQualifiedNameProvider
import com.ge.research.sadl.scoping.ErrorAddingLinkingService
import java.io.IOException
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.linking.impl.DefaultLinkingService
import org.eclipse.xtext.parsetree.reconstr.IParseTreeConstructor
import org.eclipse.xtext.parsetree.reconstr.ITokenStream

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class TemplateRuntimeModule extends AbstractTemplateRuntimeModule {
	
//	override bindIValueConverterService() {
//		return ValueConverterService;
//	}
	
	override bindIQualifiedNameProvider() {
		return TemplateQualifiedNameProvider;
	}
	
//	def Class<? extends IOutputConfigurationProvider> bindIOutputConfigurationProvider() {
//		return SADLOutputConfigurationProvider;	
//	}
//	
//	def Class<? extends IContextualOutputConfigurationProvider> bindIContextualOutputConfigurationProvider() {
//		return SADLOutputConfigurationProvider;
//	}
//	
//	def Class<? extends IQualifiedNameConverter> bindIQualifiedNameCoverter() {
//		return SadlQualifiedNameConverter;
//	}
//	
//	def Class<? extends LinkingDiagnosticMessageProvider> bindILinkingDiagnosticMessageProvider() {
//		return SoftLinkingMessageProvider;
//	}
	
//	def Class<? extends ResourceValidatorImpl> bindResourceValidatorImpl() {
//		return ResourceValidator;
//	}
//	
//	def Class<? extends DefaultResourceDescriptionStrategy> bindResourceDescritpionStrategy() {
//		return SadlResourceDescriptionStrategy;
//	}
	
	def Class<? extends DefaultLinkingService> bindDefaultLinkingService() {
		return ErrorAddingLinkingService;
	}
	
	def Class<? extends IParseTreeConstructor> bindIParseTreeConstructor() {
		NoImplParseTreeConstructor
	}
	
//	def Class<? extends ImportedNamesAdapter> bindImportedNamesAdapter() {
//		return SilencedImportedNamesAdapter; 
//	}
	
	static class NoImplParseTreeConstructor implements IParseTreeConstructor {
		
		override serializeSubtree(EObject object, ITokenStream out) throws IOException {
			throw new UnsupportedOperationException("TODO: auto-generated method stub")
		}
		
	}
}
