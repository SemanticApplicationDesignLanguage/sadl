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

import com.ge.research.sadl.ide.editor.contentassist.SadlIdeContentProposalProvider
import com.ge.research.sadl.ide.editor.syntaxcoloring.SadlSemanticHighlightService
import com.ge.research.sadl.ide.lsp.^extension.SadlLanguageServerExtension
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.eclipse.xtext.ide.server.ILanguageServerExtension
import org.eclipse.xtext.ide.server.syntaxColoring.ISemanticHighlightService

/**
 * Use this class to register generic IDE components.
 */
class SADLIdeModule extends AbstractSADLIdeModule {

	def Class<? extends IdeContentProposalProvider> bindIdeContentProposalProvider() {
		return SadlIdeContentProposalProvider;
	}

	def Class<? extends ILanguageServerExtension> bindILanguageServerExtension() {
		return SadlLanguageServerExtension;
	}

	def Class<? extends ISemanticHighlightService> bindISemanticHighlightService() {
		return SadlSemanticHighlightService;
	}

}
