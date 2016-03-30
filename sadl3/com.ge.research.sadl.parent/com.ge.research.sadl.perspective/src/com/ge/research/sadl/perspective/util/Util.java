package com.ge.research.sadl.perspective.util;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
// This method identifies the specified console or creates it if it doesn't exist. 
// From https://wiki.eclipse.org/FAQ_How_do_I_write_to_the_console_from_a_plug-in%3F

public class Util {
	public static MessageConsole findConsole(String name) {
		ConsolePlugin plugin = ConsolePlugin.getDefault();
		IConsoleManager conMan = plugin.getConsoleManager();
		IConsole[] existing = conMan.getConsoles();
		for (int i = 0; i < existing.length; i++)
			if (name.equals(existing[i].getName()))
				return (MessageConsole) existing[i];
		// no console found, so create a new one
		MessageConsole myConsole = new MessageConsole(name, null);
		conMan.addConsoles(new IConsole[] { myConsole });
		myConsole.activate();
		return myConsole;
	}

	public static IProject selectSADLProject() {
		// this method returns a project that the user selects from a list of
		// SADL projects in the workspace

		// Get a list of all the projects available
		IProject[] allprojects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		// Using array list here because it can be dynamically sized and we
		// don't know how many
		// SADL projects there will be
		ArrayList<String> projectNamesAL = new ArrayList<String>();
		// First scan through the projects to get all the SADL ones
		for (IProject project : allprojects) {
			// Can't check nature unless project is open, so check it's open
			// first
			if (project.isOpen()) {
				// SADL doesn't have it's own nature but it has xtext, so filter
				// by this
				try {
					if (project
							.isNatureEnabled("org.eclipse.xtext.ui.shared.xtextNature")) {
						projectNamesAL.add(project.getName());
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}

		// The dialog needs a string array so copy from arraylist
		String[] projectNames = new String[projectNamesAL.size()];
		projectNames = projectNamesAL.toArray(projectNames);
		// Make a dialog box to show the projects
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				PlatformUI.getWorkbench().getDisplay().getActiveShell(),
				new LabelProvider());
		dialog.setTitle("Project selection");
		dialog.setMessage("Select an open SADL project");
		dialog.setElements(projectNames);
		// Call to open dialog and ensure user pressed OK (not cancel)
		if (dialog.open() == IDialogConstants.OK_ID) {
			String selected = dialog.getFirstResult().toString();
			// Find the project from the name and get the full path to it
			for (IProject project : allprojects) {
				String projectName = project.getName();
				if (projectName.equals(selected)) {
					return project;
				}
			}
		} else {
			// Give up, user pressed cancel
			return null;
		}
		// Give up, didn't match the project
		return null;
	}
}
