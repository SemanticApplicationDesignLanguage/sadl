package com.ge.research.sadl.perspective.handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.preferences.*;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;

public class RunInference extends SadlActionHandler {

	public RunInference() {
		super();
	}
	
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			
			String[] validTargetTypes = {"sadl","test"};
			IFile trgtFile = getTargetFile(validTargetTypes);

			Map<String,String> prefMap = getPreferences();
			if (trgtFile.getName().endsWith("sadl")) {
				// run inference on this model
				Resource res = prepareActionHandler(trgtFile);
				output.println("Inference of '" + trgtFile.getName() + "' requested.");
				final List<Issue> issues = new ArrayList<Issue>();
				processor.processCommands(res, new ValidationAcceptor(new IAcceptor<Issue>(){

					@Override
					public void accept(Issue t) {
						issues.add(t);
					}
					
				}),  new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(res)));
				if (issues.size() > 0) {
					for (Issue issue: issues) {
						output.println(issue.getMessage());
					}
				}
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