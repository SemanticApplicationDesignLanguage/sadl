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
package com.ge.research.sadl.ide.lsp.^extension

import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.lsp4j.InitializedParams
import org.eclipse.xtext.ide.server.LanguageServerImpl

/**
 * Customized language server that runs a full build once after the LS has been initialized.
 */
class SadlLanguageServer extends LanguageServerImpl {
	
	@Inject
	Provider<SadlWorkspaceManager> workspaceManager;
	
	override initialized(InitializedParams params) {
		super.initialized(params)
		workspaceManager.get.fullRebuild;
	}
	
}