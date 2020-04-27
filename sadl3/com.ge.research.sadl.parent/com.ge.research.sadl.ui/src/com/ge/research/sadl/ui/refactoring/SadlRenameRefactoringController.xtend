package com.ge.research.sadl.ui.refactoring

import com.google.inject.Inject
import com.google.inject.Singleton
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext
import org.eclipse.xtext.ui.refactoring.ui.RenameRefactoringController

@Singleton
class SadlRenameRefactoringController extends RenameRefactoringController {

	@Inject
	EclipseRefactoringHelper refactoringHelper

	override startRefactoring(IRenameElementContext renameElementContext) {
		try {
			refactoringHelper.set(true)
			super.startRefactoring(renameElementContext)
		} catch (Exception e) {
			refactoringHelper.set(false)
		}
	}

	override cancelLinkedMode() {
		try {
			super.cancelLinkedMode()
		} finally {
			refactoringHelper.set(false)
		}
	}

	override protected startDirectRefactoring() throws InterruptedException {
		refactoringHelper.set(true)
		super.startDirectRefactoring()
	}

	override protected startRefactoringWithDialog(boolean previewOnly) throws InterruptedException {
		refactoringHelper.set(true)
		super.startRefactoringWithDialog(previewOnly)
	}

	override protected restoreOriginalSelection() {
		try {
			super.restoreOriginalSelection()
		} finally {
			refactoringHelper.set(false)
		}
	}

}
