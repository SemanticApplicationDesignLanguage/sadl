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

import org.eclipse.xtext.build.IncrementalBuilder.Result
import org.eclipse.xtext.ide.server.ProjectManager
import org.eclipse.xtext.util.CancelIndicator
import java.util.List
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.resource.IResourceDescription.Delta
import org.eclipse.xtext.resource.XtextResource
import com.google.inject.Singleton

/**
 * Project manager that discards any resource-based caching after the initial build.
 */
@Singleton
class SadlProjectManager extends ProjectManager {

	boolean initialBuild = false;

	override Result doInitialBuild(CancelIndicator cancelIndicator) {
		initialBuild = true;
		try {
			return super.doInitialBuild(cancelIndicator);
		} finally {
			initialBuild = false;
		}
	}

	override doBuild(List<URI> dirtyFiles, List<URI> deletedFiles, List<Delta> externalDeltas,
		CancelIndicator cancelIndicator) {
		val result = super.doBuild(dirtyFiles, deletedFiles, externalDeltas, cancelIndicator);
		// Make sure to discard the resource-based scope caching after the initial build.
		if (initialBuild) {
			resourceSetProvider.get.resources.filter(XtextResource).forEach[cache.clear(it)];
		}
		return result;
	}

}
