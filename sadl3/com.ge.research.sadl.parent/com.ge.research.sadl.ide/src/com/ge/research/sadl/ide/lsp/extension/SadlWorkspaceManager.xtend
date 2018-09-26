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
import com.google.inject.Singleton
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.ide.server.BuildManager
import org.eclipse.xtext.ide.server.Document
import org.eclipse.xtext.ide.server.WorkspaceManager
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator

/**
 * Workspace manager that does not throw a CCE for external EMF resources but does nothing at all.
 * 
 * @author akos.kitta 
 */
@Singleton
class SadlWorkspaceManager extends WorkspaceManager {
	
	BuildManager _thisBuildManager;

	override <T> T doRead(URI uri, (Document, XtextResource)=>T work) {
		val resourceURI = uri.trimFragment
		val projectMnr = getProjectManager(resourceURI)
		val resource = projectMnr?.getResource(resourceURI);
		if (resource === null) {
			return work.apply(null, null)
		}
		if (resource instanceof XtextResource) {
			val doc = getDocument(resource)
			return work.apply(doc, resource)
		} else {
			return null;
		}
	}
	
	@Inject
	override void setBuildManager(BuildManager buildManager) {
		super.buildManager = buildManager;
		_thisBuildManager = buildManager;
	}
	
	/**
	 * Triggers a full build on all known projects.
	 */
	def void fullRebuild() {
		afterBuild(_thisBuildManager.doInitialBuild(projectManagers.map[projectDescription], CancelIndicator.NullImpl));
	}
	
}