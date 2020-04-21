package com.ge.research.sadl.ui.outline

import org.eclipse.xtext.ui.editor.outline.impl.OutlineRefreshJob
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status

// https://github.com/GEGlobalResearch/DARPA-ASKE-TA1/issues/109
class NoopOutlineRefreshJob extends OutlineRefreshJob {

	override protected run(IProgressMonitor monitor) {
		return Status.OK_STATUS
	}

}
