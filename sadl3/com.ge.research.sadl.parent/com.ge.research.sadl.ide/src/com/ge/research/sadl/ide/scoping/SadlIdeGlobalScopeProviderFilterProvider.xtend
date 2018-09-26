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
package com.ge.research.sadl.ide.scoping

import com.ge.research.sadl.scoping.GlobalScopeProviderFilterProvider
import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.inject.Inject
import com.google.inject.Singleton
import java.net.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.resource.XtextResource

import static com.google.common.base.Predicates.alwaysFalse
import com.ge.research.sadl.resource.UserDataHelper

/**
 * Scope provider for the IDE agnostic scoping. Ignores everything that does not belong to the same
 * SADL project.
 */
@Singleton
class SadlIdeGlobalScopeProviderFilterProvider extends GlobalScopeProviderFilterProvider {

	static val CONTAINER_PROJECT_URI = 'container-project-uri';

	@Inject
	SadlProjectHelper projectHelper;
	
	@Inject
	UserDataHelper userDataHelper;

	override getPredicate(Resource it) {
		if (!URI.file) {
			return alwaysFalse;
		}
		val projectUri = getAndCacheProjectUri(it);
		if (projectUri === null) {
			return alwaysFalse;
		}
		return [ toCheck |
			val expected = projectUri.toString;
			val actual = userDataHelper.getContainerProjectUri(toCheck).orNull; 
			return expected == actual;
		];
	}

	/**
	 * Retrieves the container project URI. Caches it, if the argument is an {@link XtextResource}.
	 * Does not assumes anything about the URI scheme and accepts everything. 
	 */
	protected def URI getAndCacheProjectUri(Resource it) {
		val uri = URI.toJavaNetUri;
		if (it instanceof XtextResource) {
			return cache.get(CONTAINER_PROJECT_URI, it, [
				return projectHelper.getRoot(uri);
			]);
		}
		return projectHelper.getRoot(uri);
	}

	protected def URI toJavaNetUri(org.eclipse.emf.common.util.URI it) {
		return new URI(it.toString);
	}

}
