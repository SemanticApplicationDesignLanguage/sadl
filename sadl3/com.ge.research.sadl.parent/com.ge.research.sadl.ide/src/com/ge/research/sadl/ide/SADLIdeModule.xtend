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
package com.ge.research.sadl.ide

import com.ge.research.sadl.ide.contentassist.antlr.lexer.jflex.JFlexBasedInternalSADLLexer
import com.ge.research.sadl.ide.editor.coloring.SadlColoringService
import com.ge.research.sadl.ide.editor.contentassist.SadlIdeContentProposalProvider
import com.ge.research.sadl.ide.lsp.^extension.ISadlLanguageServerExtension
import com.google.inject.Binder
import com.google.inject.name.Names
import org.eclipse.xtext.ide.LexerIdeBindings
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.eclipse.xtext.ide.editor.contentassist.antlr.internal.Lexer
import org.eclipse.xtext.ide.server.ILanguageServerExtension
import org.eclipse.xtext.ide.server.coloring.IColoringService

/**
 * Use this class to register generic IDE components.
 */
class SADLIdeModule extends AbstractSADLIdeModule {

	@Override
	override configureContentAssistLexer(Binder binder) {
		binder.bind(Lexer).annotatedWith(Names.named(LexerIdeBindings.CONTENT_ASSIST)).to(JFlexBasedInternalSADLLexer)
	}

	def Class<? extends IdeContentProposalProvider> bindIdeContentProposalProvider() {
		return SadlIdeContentProposalProvider;
	}

	def Class<? extends IColoringService> bindISemanticHighlightService() {
		return SadlColoringService;
	}

	def Class<? extends ILanguageServerExtension> bindILanguageServerExtension() {
		return ISadlLanguageServerExtension
	}

//	def Class<? extends ICodeLensService> bindICodeLensService() {
//		return SadlCodeLensService;
//	}

}
