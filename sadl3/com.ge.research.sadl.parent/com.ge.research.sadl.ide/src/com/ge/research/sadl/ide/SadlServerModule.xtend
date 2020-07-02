/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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

import com.ge.research.sadl.ide.builder.SadlIdeIncrementalBuilder.SadlInternalStatefulIncrementalBuilder
import com.ge.research.sadl.ide.lsp.^extension.SadlProjectManager
import com.ge.research.sadl.ide.lsp.^extension.SadlWorkspaceManager
import com.google.inject.Scopes
import org.eclipse.xtext.build.IncrementalBuilder.InternalStatefulIncrementalBuilder
import org.eclipse.xtext.ide.server.ProjectManager
import org.eclipse.xtext.ide.server.ServerModule
import org.eclipse.xtext.ide.server.WorkspaceManager

/**
 * LS module for SADL. 
 */
class SadlServerModule extends ServerModule {

	override protected configure() {
		super.configure()
		bind(InternalStatefulIncrementalBuilder).to(SadlInternalStatefulIncrementalBuilder).in(Scopes.SINGLETON);
		bind(WorkspaceManager).to(SadlWorkspaceManager).in(Scopes.SINGLETON);
		bind(ProjectManager).to(SadlProjectManager).in(Scopes.SINGLETON);
	}

}
