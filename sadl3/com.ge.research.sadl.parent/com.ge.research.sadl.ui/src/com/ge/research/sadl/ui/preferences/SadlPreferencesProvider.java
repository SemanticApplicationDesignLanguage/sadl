package com.ge.research.sadl.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;

import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Injector;

public class SadlPreferencesProvider {
	
	static public IPreferenceValues getPreferences(IWorkbenchWindow window) {
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
		return preferenceValues;
	}
}
