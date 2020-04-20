package com.ge.research.sadl.ui.tests

import java.lang.reflect.InvocationTargetException
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.jobs.ISchedulingRule
import org.eclipse.ui.actions.WorkspaceModifyOperation
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin

class DelagatingWorkspaceModifyOperation extends WorkspaceModifyOperation {

	val Run delegate

	new(Run fn) {
		this(IDEWorkbenchPlugin.pluginWorkspace.root, fn)
	}

	new(ISchedulingRule rule, Run fn) {
		super(rule)
		this.delegate = fn;
	}

	override protected execute(
		IProgressMonitor monitor) throws CoreException, InvocationTargetException, InterruptedException {

		delegate.execute(monitor)
	}

	@FunctionalInterface
	static interface Run {
		def void execute(IProgressMonitor monitor) throws CoreException, InvocationTargetException, InterruptedException
	}

}
