package com.ge.research.sadl.ui.handlers;

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
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.utils.ResourceManager;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.reasoner.ResultSet;

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
		        	if (retvals != null && retvals.length > 1) {
		        		List<SadlCommand> cmds = (List<SadlCommand>) retvals[0];
		        		Object infresults = retvals[1];
		        		if (infresults != null) {
		        			if (infresults instanceof List<?>) {
		        				for (int i = 0; i < ((List<?>)infresults).size(); i++) {
		        					if (((List<?>)infresults).get(i) instanceof ResultSet && cmds != null && cmds instanceof List<?> && ((List<?>)cmds).size() >= i && 
		        							((List<?>)cmds).get(i) instanceof Query) {
		        						Query query = (Query) ((List<?>)cmds).get(i);
		        						ResultSet rs = (ResultSet) ((List<?>)infresults).get(i);
		        						if (query.isGraph() ||
		        								(query.getSparqlQueryString() != null && 
		        										(query.getSparqlQueryString().toLowerCase().startsWith("construct")))) {
			        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (i + 1) + ":\n");
			    							String msg = "Graph: " + query.toString() + "\n";
			    							msg += rs.toStringWithIndent(5);
			        						SadlConsole.writeToConsole(MessageType.INFO, msg);
		        							String desc = query.getName();
		        							if (desc == null) desc = "Cmd " + (i + 1) + "  (Graph)";
		        							String baseFileName = trgtFile.getProjectRelativePath().removeFileExtension().lastSegment() + i; 							
			        						resultSetToGraph(project, trgtFile, rs, desc, baseFileName);
		        						}
		        						else {
			        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (i + 1) + ":\n");
			    							String msg = "Query: " + cmds.get(i).toString() + "\n";
			    							msg += rs.toStringWithIndent(5);
			        						SadlConsole.writeToConsole(MessageType.INFO, msg);
		        						}
		        					}
		        					else if (((List<?>)infresults).get(i) == null) {
		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (i + 1) + " is empty\n");
		        					}
		        					else if (((List<?>)infresults).get(i) instanceof TestResult) {
		        						TestResult tr = (TestResult)((List<?>)infresults).get(i);
		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (i + 1) + ":\n");
		        						String msg;
		        						if (tr.isPassed()) {
		        							msg = "Test passed: " + cmds.get(i).toString() + "\n";
		        						}
		        						else {
		        							msg = "Test failed: " + cmds.get(i).toString() + "(" + tr.toString() + ")\n";
		        						}
		        						SadlConsole.writeToConsole(MessageType.INFO, msg);
		        					}
		        					else {
		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (i + 1) + ":\n");
		        						SadlConsole.writeToConsole(MessageType.INFO, ((List<?>)infresults).get(i).toString());
		        					}
		        				}
		        			}
		        			else {
		        				SadlConsole.writeToConsole(MessageType.ERROR, "Results returned not of expected type. Please report.\n");
		        			}
		        		}
		        		if (retvals.length > 2) {
		        			Object errors = retvals[2];
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