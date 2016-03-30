package com.ge.research.sadl.perspective.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtext.preferences.*;

import com.ge.research.sadl.perspective.util.Util;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Injector;

public class RunInference extends AbstractHandler {

	public String projectLocation;
	protected Process Process;

	public RunInference() {

	}

	public Object execute(ExecutionEvent event) throws ExecutionException {

		try {

			/**
			 * Project location code taken from ACL2sAnalysis
			 */
			// return the active workbench window
			IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);

			// access the workspace from the resources plug-in class
			// (org.eclipse.core.resources)
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			// get root location of workspace
			String workspace_loc = workspace.getRoot().getLocation().toString();
			// return null if no path can be determined
			String project_loc = null;

			Injector reqInjector = SadlActivator.getInstance()
					.getInjector(SadlActivator.COM_GE_RESEARCH_SADL_SADL);
			IPreferenceValuesProvider pvp = reqInjector.getInstance(IPreferenceValuesProvider.class);
			org.eclipse.emf.ecore.resource.Resource resource = new ResourceImpl();
			resource.setURI(org.eclipse.emf.common.util.URI.createFileURI("/"));

			// if user has an active window
			if (window != null) {
				// returns the selection service which tracks selection within the active window
				if ((window.getSelectionService().getSelection()) instanceof IStructuredSelection) {
					// returns the current selection
					IStructuredSelection selection = (IStructuredSelection) window.getSelectionService().getSelection();
					// returns the first element in this selection, or null if the selection is empty
					Object firstElement = selection.getFirstElement();

					// if user has selected a project in package explorer
					if (firstElement instanceof IProject) {
						// returns an object that is castable to the given class
						IProject project = (IProject) ((IAdaptable) firstElement).getAdapter(IProject.class);
						// returns the path as a string
						IPath path = project.getFullPath();
						project_loc = path.toString();
						// project context is obtained from platform resource
						// URI pointing to the project in the workspace
						@SuppressWarnings("deprecation")
						org.eclipse.emf.common.util.URI uri = org.eclipse.emf.common.util.URI
								.createPlatformResourceURI("/" + project.getName() + "/");
						resource.setURI(uri);
					}
				}
			}
			
			IPreferenceValues preferenceValues = pvp.getPreferenceValues(resource);
			//TODO: FIX
			//preferenceValues.getPreference(RequirementsPreference.P_PROLOG_FOLDER_PATH);
			
			if (project_loc == null) {
				// ask the user which SADL project they want to select for
				// analysis
				IProject project = Util.selectSADLProject();
				if (project == null) {
					// method failed to give us a project, give up.
					return null;
				}
				
				// get the path from the project
				project_loc = project.getFullPath().toString();
			}

			// get current file path
			String filePath = workspace_loc + project_loc;
			this.projectLocation = filePath;

			MessageConsole sadlInference = Util.findConsole("SADL Inference");
			MessageConsoleStream output = sadlInference.newMessageStream();

			sadlInference.activate();
		}
		finally {
			
		}

		return event;
	}

}