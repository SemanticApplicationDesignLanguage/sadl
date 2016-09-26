package com.ge.research.sadl.perspective.handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.preferences.*;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.processing.ISadlImportProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;

public class RunInference extends SadlActionHandler {

	public RunInference() {
		super();
	}
	
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {	
			String[] validTargetTypes = getValidTargetFileTypes();
			Object[] target = getCommandTarget(validTargetTypes);
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}

			Map<String,String> prefMap = getPreferences();
			if (trgtFile != null) {
				if (trgtFile.getFileExtension().equals("sadl")) {
					// run inference on this model
					Resource res = prepareActionHandler(project, trgtFile);
					SadlConsole.writeToConsole(MessageType.INFO, "Inference of '" + trgtFile.getFullPath().toPortableString() + "' requested.\n");
					final List<Issue> issues = new ArrayList<Issue>();
					final List<SadlMessage> results = null;
					String modelFolderPath = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toOSString());
					String owlModelPath = modelFolderPath + "/" + trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
		        	Object[] retvals = processor.runInference(res, owlModelPath, modelFolderPath, prefMap);
		        	if (retvals != null && retvals.length > 0) {
		        		Object infresults = retvals[0];
		        		if (infresults != null) {
		        			SadlConsole.writeToConsole(MessageType.INFO, infresults.toString());
		        		}
		        		if (retvals.length > 1) {
		        			Object errors = retvals[1];
		        			if (errors instanceof List<?>) {
		        				Iterator<?> erritr = ((List<?>)errors).iterator();
		        				while (erritr.hasNext()) {
		        					SadlConsole.writeToConsole(MessageType.ERROR, erritr.next().toString());
		        				}
		        			}
		        		}
		        	}
				}
				else if (trgtFile.getFileExtension().equals("test")) {
					// run test suite
					SadlConsole.writeToConsole(MessageType.INFO, "Testing of suite '" +  trgtFile.getFullPath().toPortableString() + "' requested.\n");
				}
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			
		}

		return event;
	}

	@Override
	protected String[] getValidTargetFileTypes() {
		String[] types = {"sadl","test"};
		return types;
	}

}