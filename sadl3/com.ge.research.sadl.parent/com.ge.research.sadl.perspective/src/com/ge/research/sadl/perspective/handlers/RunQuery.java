package com.ge.research.sadl.perspective.handlers;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtext.preferences.*;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.ge.research.sadl.perspective.util.Util;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Injector;
import com.google.inject.Key;

public class RunQuery extends SadlActionHandler {

	public String projectLocation;
	protected Process Process;

	public RunQuery() {
		super();
	}

	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = {"sadl","test"};
			IFile trgtFile = getTargetFile(validTargetTypes);

			Map<String,String> prefMap = getPreferences();
			if (trgtFile.getName().endsWith("sadl")) {
				// run query on this model
				Resource res = prepareActionHandler(trgtFile);
				output.println("Adhoc Query of '" + trgtFile.getName() + "' requested.");
				
				String query = getQuery();
				
				final List<Issue> issues = new ArrayList<Issue>();
				processor.processAdhocQuery(res, new ValidationAcceptor(new IAcceptor<Issue>(){

					@Override
					public void accept(Issue t) {
						issues.add(t);
					}
					
				}),  new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(res)), query);
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

	private String getQuery() {
		Shell shell = new Shell();
		InputDialog dlg = new InputDialog(
				shell,
				"Run Query",
				"SADL or SPARQL Query?",
				"",
				null);
		dlg.open();
		if (dlg.getReturnCode() != Window.OK) {
			return null;
		}
		return dlg.getValue();
	}

}