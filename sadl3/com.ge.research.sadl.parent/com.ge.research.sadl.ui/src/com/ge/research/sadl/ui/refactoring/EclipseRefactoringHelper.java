package com.ge.research.sadl.ui.refactoring;

import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.xtext.ui.editor.validation.ValidationJob;

import com.ge.research.sadl.refactoring.RefactoringHelper;

public enum EclipseRefactoringHelper implements RefactoringHelper {

	INSTANCE;

	private AtomicBoolean inProgress = new AtomicBoolean(false);

	@Override
	public synchronized boolean isInProgress() {
		return inProgress.get();
	}

	public synchronized void set(boolean newValue) {
		if (inProgress.get() == newValue) {
			return;
		}
		if (inProgress.get() && !newValue) {
			waitForValidation();
		}
		inProgress.set(newValue);
	}

	private void waitForValidation() {
		boolean wasInterrupted = false;
		do {
			try {
				Job.getJobManager().join(ValidationJob.XTEXT_VALIDATION_FAMILY, null);
				wasInterrupted = false;
			} catch (OperationCanceledException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				wasInterrupted = true;
			}
		} while (wasInterrupted);
	}

}
