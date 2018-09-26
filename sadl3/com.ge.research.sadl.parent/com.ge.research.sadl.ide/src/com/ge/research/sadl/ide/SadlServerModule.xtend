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
import com.ge.research.sadl.ide.utils.SadlIdeProjectHelper
import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.inject.Binder
import com.google.inject.Scopes
import com.google.inject.binder.AnnotatedBindingBuilder
import org.eclipse.xtext.build.IncrementalBuilder
import org.eclipse.xtext.ide.server.ProjectManager
import org.eclipse.xtext.ide.server.ServerModule
import org.eclipse.xtext.ide.server.WorkspaceManager

/**
 * LS module for SADL. 
 */
class SadlServerModule extends ServerModule {

	override protected configure() {
		super.configure()
		binder.bindInternalStatefulIncrementalBuilder;
		binder.bind(SadlProjectHelper).to(SadlIdeProjectHelper);
		bind(WorkspaceManager).to(SadlWorkspaceManager).in(Scopes.SINGLETON);
		bind(ProjectManager).to(SadlProjectManager).in(Scopes.SINGLETON);
	}

	/**
	 * Re-binds the builder to have a customized FS access that supports "dynamic" base URIs per Xtext resource. 
	 */
	protected def bindInternalStatefulIncrementalBuilder(Binder binder) {
		val fromClass = IncrementalBuilder.classLoader.loadClass(
			'org.eclipse.xtext.build.IncrementalBuilder$InternalStatefulIncrementalBuilder') as Class<Object>;
		(binder.bind(fromClass) as AnnotatedBindingBuilder<Object>).to(SadlInternalStatefulIncrementalBuilder);
	}

}
