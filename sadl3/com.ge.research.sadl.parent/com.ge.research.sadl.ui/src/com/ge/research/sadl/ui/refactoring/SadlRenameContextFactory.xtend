package com.ge.research.sadl.ui.refactoring

import com.ge.research.sadl.external.ExternalEmfResourceExtension
import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.text.ITextSelection
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.refactoring.ui.IRenameContextFactory
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext

class SadlRenameContextFactory extends IRenameContextFactory.Default {

	@Inject
	extension ExternalEmfResourceExtension

	protected override IRenameElementContext createExternalRenameElementContext(EObject targetElement,
		XtextEditor editor, ITextSelection selection, XtextResource resource) {

		if (targetElement.isExternal) {
			return null
		}
		return super.createExternalRenameElementContext(targetElement, editor, selection, resource)
	}

}
