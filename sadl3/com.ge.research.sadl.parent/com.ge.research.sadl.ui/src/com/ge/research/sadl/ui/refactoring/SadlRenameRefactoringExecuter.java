package com.ge.research.sadl.ui.refactoring;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ltk.core.refactoring.PerformChangeOperation;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.ProcessorBasedRefactoring;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.xtext.ui.refactoring.impl.Messages;
import org.eclipse.xtext.ui.refactoring.ui.RenameRefactoringExecuter;
import org.eclipse.xtext.ui.refactoring.ui.SyncUtil;
import org.eclipse.xtext.ui.refactoring.ui.WorkbenchRunnableAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Inject;

public class SadlRenameRefactoringExecuter extends RenameRefactoringExecuter {

	@Inject
	private IWorkspace workspace;

	@Inject
	private SyncUtil syncUtil;

	@Inject
	private EclipseRefactoringHelper refactoringHelper;

	private static final Logger LOG = LoggerFactory.getLogger(SadlRenameRefactoringExecuter.class);

	public void execute(IEditorPart editor, ProcessorBasedRefactoring refactoring) throws InterruptedException {
		Assert.isTrue(Display.getCurrent() != null);
		refactoringHelper.set(true);
		IWorkbenchWindow window = editor.getSite().getWorkbenchWindow();
		IWorkbench workbench = window.getWorkbench();
		Shell shell = editor.getSite().getShell();
		if (!isApplicable(shell, refactoring)) {
			refactoringHelper.set(false);
			return;
		}
		final IJobManager manager = Job.getJobManager();
		final ISchedulingRule rule = workspace.getRoot();
		try {
			try {
				Runnable r = new Runnable() {
					@Override
					public void run() {
						manager.beginRule(rule, null);
					}
				};
				BusyIndicator.showWhile(shell.getDisplay(), r);
			} catch (OperationCanceledException e) {
				// User cancelled operation
				// Do nothing
				refactoringHelper.set(false);
				return;
			}
			CheckConditionsAndCreateChangeRunnable checkConditionsRunnable = new CheckConditionsAndCreateChangeRunnable(
					shell, refactoring);
			refactoring.setValidationContext(shell);
			window.run(false, true, new WorkbenchRunnableAdapter(checkConditionsRunnable, rule, true));
			PerformChangeOperation performChangeOperation = checkConditionsRunnable.getPerformChangeOperation();
			if (performChangeOperation != null) {
				window.run(false, false, new WorkbenchRunnableAdapter(performChangeOperation, rule, true));
				RefactoringStatus validationStatus = performChangeOperation.getValidationStatus();
				if (validationStatus != null && validationStatus.hasFatalError()) {
					MessageDialog.openError(shell, refactoring.getName(), Messages.format("Cannot execute refactoring",
							validationStatus.getMessageMatchingSeverity(RefactoringStatus.FATAL)));
					refactoringHelper.set(false);
					return;
				}
			}
		} catch (OperationCanceledException e) {
			refactoringHelper.set(false);
			throw new InterruptedException();
		} catch (InvocationTargetException e) {
			refactoringHelper.set(false);
			LOG.error(e.getMessage(), e);
		} finally {
			manager.endRule(rule);
			refactoring.setValidationContext(null);
		}
		Display.getDefault().asyncExec(() -> {
			syncUtil.reconcileAllEditors(workbench, false, new NullProgressMonitor());
			refactoringHelper.set(false);
		});
	}

}
