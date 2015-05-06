/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.editor;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.xtext.builder.nature.Messages;
import org.eclipse.xtext.builder.nature.ToggleXtextNatureAction;
import org.eclipse.xtext.ui.editor.IXtextEditorCallback;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.validation.ValidatingEditorCallback;

import com.ge.research.sadl.builder.ResourceManager;
import com.google.inject.Inject;

//TODO: Revisit after Xtext Upgrade
//see https://www.eclipse.org/forums/index.php/t/827695/
//delegation to other callbacks might become obsolete
public class SadlEditorCallback extends ValidatingEditorCallback
		implements IPartListener2, IResourceChangeListener, IXtextEditorCallback {

	@Inject
	private ToggleXtextNatureAction toggleNature;

	@Override
	public void partActivated(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		// visitor.editorClosed(partRef.getPartName());
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		// visitor.removeResourceModel(visitor.getModelResource());
	}

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		// System.out.println("resourceChanged event " + event.getType());
		if (event.getType() == IResourceChangeEvent.PRE_CLOSE && event.getResource() instanceof IProject) {
			// visitor.removeAllProjectResourceModels(event.getResource()
			// .getName());
		}
	}

	@Override
	public void afterSave(XtextEditor editor) {
		super.afterSave(editor);
	}
	
	@Override
	public void afterSetInput(XtextEditor editor) {
		super.afterSetInput(editor);
		try {
			createEditorOpenIndicatorFile(editor);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void afterCreatePartControl(XtextEditor editor) {
		super.afterCreatePartControl(editor);
		handleNatureAdding(editor);
		try {
			createEditorOpenIndicatorFile(editor);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * Copied from NatureAddingEditorCallback
	 */
	private void handleNatureAdding(XtextEditor editor) {
		IResource resource = editor.getResource();
		if (resource!=null && !toggleNature.hasNature(resource.getProject()) && resource.getProject().isAccessible() && !resource.getProject().isHidden()) {
			String title = Messages.NatureAddingEditorCallback_MessageDialog_Title;
			String message = Messages.NatureAddingEditorCallback_MessageDialog_Msg0 + resource.getProject().getName() + Messages.NatureAddingEditorCallback_MessageDialog_Msg1;
			MessageDialog dialog = new MessageDialog(editor.getEditorSite().getShell(), title, null, message, MessageDialog.QUESTION, 
					new String[] { IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL, IDialogConstants.CANCEL_LABEL }, 0);
			int open = dialog.open();
			if (open==0) {
				toggleNature.toggleNature(resource.getProject());
			}
		}
	}

	@Override
	public void beforeDispose(XtextEditor editor) {
		super.beforeDispose(editor);
		try {
			deleteEditorOpenIndicatorFile(editor);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public synchronized boolean createEditorOpenIndicatorFile(XtextEditor editor) throws Exception {
		boolean status = false;
		File indicatorFile = getIndicatorFile(editor);
		if (indicatorFile != null && !indicatorFile.exists()) {
			status = indicatorFile.createNewFile();
		}
		// else {
		// System.out.println("Not creating file '" +
		// indicatorFile.getCanonicalPath() + "' as it already exists.");
		// }
		return status;
	}

	public synchronized boolean deleteEditorOpenIndicatorFile(XtextEditor editor) throws Exception {
		boolean status = false;
		File indicatorFile = getIndicatorFile(editor);
		if (indicatorFile != null && indicatorFile.exists()) {
			status = indicatorFile.delete();
		}
		// else {
		// System.out.println("Not deleting file '" +
		// indicatorFile.getCanonicalPath() + "' as it does not exist.");
		// }
		return status;
	}

	private File getIndicatorFile(XtextEditor editor) throws Exception {
		IPath prjLoc = null;
		if (editor.getResource() != null) {
			prjLoc = editor.getResource().getProject().getLocation();
		} else {
			System.out.println("getIndicatorFile called with an editor with a null resource. Please report.");
		}
		if (prjLoc != null) {
			IPath tempDir = prjLoc.append(ResourceManager.TEMPDIR);
			File tmpDir = ResourceManager.createFolderIfNotExists(tempDir.toOSString());
			File isopenDir = ResourceManager.createFolderIfNotExists(tmpDir.getCanonicalPath() + "/isOpen");
			File indFile = new File(isopenDir + "/" + editor.getEditorInput().getName() + ".isOpen");
			return indFile;
		}
		return null;
	}
	
}
