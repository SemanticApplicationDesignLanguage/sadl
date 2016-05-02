package com.ge.research.sadl.perspective.handlers;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtext.preferences.*;

import com.ge.research.sadl.actions.InferenceAction;
import com.ge.research.sadl.perspective.util.Util;
import com.ge.research.sadl.processing.SadlModelProcessorProvider;
import com.google.inject.Inject;

public class RunInference extends SadlActionHandler {

	@Inject SadlModelProcessorProvider processorProvider;
	@Inject IPreferenceValuesProvider preferenceProvider;

	public String projectLocation;
	protected Process Process;

	public RunInference() {

	}

	public Object execute(ExecutionEvent event) throws ExecutionException {
		MessageConsoleStream output = null;
		try {
			MessageConsole sadlInference = Util.findConsole("SADL Inference");
			output = sadlInference.newMessageStream();

			sadlInference.activate();
			
			String[] validTargetTypes = {"sadl","test"};
			File[] prjAndTarget = getTargetProjectAndFile(validTargetTypes);
			if (prjAndTarget == null || prjAndTarget.length < 2) {
				output.println("Unable to obtain target project and file");
			}
			File prjFolder = prjAndTarget[0];
			File trgtFile = prjAndTarget[1];

			Map<String,String> prefMap = getPreferences();
			if (trgtFile.getName().endsWith("sadl")) {
				// run inference on this model
				output.println("Inference of '" + trgtFile.getName() + "' requested.");
				InferenceAction action;
				action = new InferenceAction(prjFolder, trgtFile, prefMap);
				runAction(output, action, event);
			}
			else if (trgtFile.getName().endsWith("test")) {
				// run test suite
				output.println("Testing of suite '" +  trgtFile.getName() + "' requested.");
			}
		}
		catch (Exception e) {
			if (output != null) {
				output.println(e.getMessage());
			}
			else {
				System.err.println(e.getMessage());
			}
		}
		finally {
			if (output != null) {
				try {
					output.flush();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		return event;
	}

}