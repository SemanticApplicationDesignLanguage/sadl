/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.syntaxcoloring

import com.ge.research.sadl.ide.editor.coloring.SadlIdeSemanticHighlightingCalculator
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.scoping.TestScopeProvider
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.impl.ResourceImpl
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.ide.editor.syntaxcoloring.ISemanticHighlightingCalculator
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess
import org.eclipse.xtext.ui.resource.ProjectByResourceProvider
import org.eclipse.xtext.util.CancelIndicator

class SadlSemanticHighlightingCalculator implements ISemanticHighlightingCalculator {

	@Inject IPreferenceStoreAccess preferenceStoreAccess
	@Inject ProjectByResourceProvider projectProvider
	@Inject SadlIdeSemanticHighlightingCalculator delegate

	override void provideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {

		if (resource === null || resource.contents.isEmpty())
			return;

		TestScopeProvider.registerResource(resource, false);
		delegate.provideHighlightingFor(resource, acceptor, cancelIndicator)

		// get the SADL preferences from a pseudo SADL resource and apply the preference setting to the current
		// resource (so it applies to both SADL and derived types)
		val r = new ResourceImpl();
		r.setURI(URI.createFileURI("/"));
		val ambiguousnames = getPrefStore(resource).getBoolean(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id);
		TestScopeProvider.registerResource(resource, ambiguousnames);
	}

	def protected getPrefStore(XtextResource resource) {
		val projectContext = projectProvider.getProjectContext(resource)
		if (projectContext !== null) {
			return preferenceStoreAccess.getContextPreferenceStore(projectContext)
		}
		return preferenceStoreAccess.preferenceStore
	}

}
