/************************************************************************
 * Copyright 2007-2018 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide

import com.ge.research.sadl.external.ExternalEmfResourcePredicate
import com.ge.research.sadl.ide.contentassist.antlr.lexer.jflex.JFlexBasedInternalSADLLexer
import com.ge.research.sadl.ide.editor.coloring.SadlColoringService
import com.ge.research.sadl.ide.editor.contentassist.IOntologyContextProvider
import com.ge.research.sadl.ide.editor.contentassist.SadlContentAssistContextFactory
import com.ge.research.sadl.ide.editor.contentassist.SadlIdeContentProposalProvider
import com.ge.research.sadl.ide.editor.contentassist.SadlIdeCrossrefProposalProvider
import com.ge.research.sadl.ide.editor.contentassist.SadlOntologyContextProvider
import com.ge.research.sadl.ide.external.SadlIdeExternalEmfResourcePredicate
import com.ge.research.sadl.ide.lsp.^extension.ISadlLanguageServerExtension
import com.ge.research.sadl.ide.lsp.^extension.SadlLanguageServerExtension
import com.ge.research.sadl.ide.preferences.SadlIdePreferenceValuesProvider
import com.ge.research.sadl.ide.scoping.SadlIdeGlobalScopeProviderFilterProvider
import com.ge.research.sadl.ide.validator.SadlIdeResourceValidator
import com.ge.research.sadl.model.SadlEObjectDocumentationProvider.DocumentationUtils
import com.ge.research.sadl.model.SadlEObjectDocumentationProvider.Markdown
import com.ge.research.sadl.scoping.GlobalScopeProviderFilterProvider
import com.ge.research.sadl.utils.SadlConsole
import com.google.inject.Binder
import com.google.inject.name.Names
import org.eclipse.xtext.ide.LexerIdeBindings
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.eclipse.xtext.ide.editor.contentassist.IdeCrossrefProposalProvider
import org.eclipse.xtext.ide.editor.contentassist.antlr.ContentAssistContextFactory
import org.eclipse.xtext.ide.editor.contentassist.antlr.internal.Lexer
import org.eclipse.xtext.ide.server.ILanguageServerExtension
import org.eclipse.xtext.ide.server.coloring.IColoringService
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.validation.ResourceValidatorImpl

/**
 * Use this class to register generic IDE components.
 */
class SADLIdeModule extends AbstractSADLIdeModule {

	override configureContentAssistLexer(Binder binder) {
		binder.bind(Lexer).annotatedWith(Names.named(LexerIdeBindings.CONTENT_ASSIST)).to(JFlexBasedInternalSADLLexer)
	}

	def Class<? extends IdeContentProposalProvider> bindIdeContentProposalProvider() {
		return SadlIdeContentProposalProvider;
	}

	def Class<? extends IdeCrossrefProposalProvider> bindIdeCrossrefProposalProvider() {
		return SadlIdeCrossrefProposalProvider;
	}

	def Class<? extends IColoringService> bindIColoringService() {
		return SadlColoringService;
	}

	def Class<? extends ILanguageServerExtension> bindILanguageServerExtension() {
		return ISadlLanguageServerExtension
	}

	def Class<? extends SadlConsole> bindSadlConsole() {
		return SadlLanguageServerExtension;
	}

	def Class<? extends IPreferenceValuesProvider> bindIPreferenceValuesProvider() {
		return SadlIdePreferenceValuesProvider;
	}

	def Class<? extends ResourceValidatorImpl> bindResourceValidatorImpl() {
		return SadlIdeResourceValidator;
	}

	def Class<? extends ExternalEmfResourcePredicate> bindExternalEmfResourcePredicate() {
		return SadlIdeExternalEmfResourcePredicate;
	}

	def Class<? extends IOntologyContextProvider> bindIOntologyContextProvider() {
		return SadlOntologyContextProvider;
	}

	def Class<? extends ContentAssistContextFactory> bindContentAssistContextFactory() {
		return SadlContentAssistContextFactory;
	}
	
	def Class<? extends DocumentationUtils> bindDocumentationUtils() {
		return Markdown;
	}
	
	def Class<? extends GlobalScopeProviderFilterProvider> bindGlobalScopeProviderFilterProvider() {
		return SadlIdeGlobalScopeProviderFilterProvider;
	}

}
