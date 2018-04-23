package com.ge.research.sadl.ui.handlers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.gp.EndWrite;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlCommandResult;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.utils.ResourceManager;

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

			if (trgtFile != null) {
				Map<String,String> prefMap = getPreferences(trgtFile);
				if (trgtFile.getFileExtension().equals("sadl")) {
					// run inference on this model
					Resource res = prepareActionHandler(project, trgtFile);
					SadlConsole.writeToConsole(MessageType.INFO, "Inference of '" + trgtFile.getFullPath().toPortableString() + "' requested.\n");
					final List<Issue> issues = new ArrayList<Issue>();
					final List<SadlMessage> results = null;
					String modelFolderPath = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toOSString());
					String owlModelPath = modelFolderPath + "/" + trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
		        	Object[] retvals = processor.runInference(res, owlModelPath, modelFolderPath, prefMap);
		        	if (retvals == null || retvals.length < 1) {
		        		
		        	}
		        	for (int idx = 0; idx < retvals.length; idx++) {
		        		if (!(retvals[idx] instanceof SadlCommandResult)) {
		        			SadlConsole.writeToConsole(MessageType.ERROR, "Unexpected inference result is not a SadlCommandResult (" + retvals[idx].toString());
		        		}
		        		SadlCommandResult result = (SadlCommandResult) retvals[idx];
		        		SadlCommand cmd = result.getCmd();
		        		Object infresults = result.getResults();
		        		List<ModelError> errors = result.getErrors();
		        		
		        		if (infresults != null) {
		        			if (infresults instanceof ResultSet) {
	        					if (cmd instanceof Query) {
	        						Query query = (Query) cmd;
	        						ResultSet rs = (ResultSet) infresults;
	        						if (query.isGraph() ||
	        								(query.getSparqlQueryString() != null && 
	        										(query.getSparqlQueryString().toLowerCase().startsWith("construct")))) {
//		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
		    							String msg = "Graph: " + query.toString() + "\n";
		    							msg += rs.toStringWithIndent(5);
		        						SadlConsole.writeToConsole(MessageType.INFO, msg);
	        							String desc = query.getName();
	        							if (desc == null) desc = "Cmd " + (idx + 1) + "  (Graph)";
	        							String baseFileName = trgtFile.getProjectRelativePath().lastSegment() + idx; 
	        							baseFileName = baseFileName.replace(".", "_");
		        						resultSetToGraph(project, trgtFile, rs, desc, baseFileName, null);
	        						}
	        						else {
//		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
		    							String msg = "Query: " + query.toString() + "\n";
		    							msg += rs.toStringWithIndent(5);
		        						SadlConsole.writeToConsole(MessageType.INFO, msg);
	        						}
		        				}
		        			}
        					else if (infresults instanceof TestResult) {
        						TestResult tr = (TestResult)infresults;
//        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
        						String msg;
        						if (tr.isPassed()) {
        							msg = "Test passed: " + cmd.toString() + "\n";
        						}
        						else {
        							msg = "Test failed: " + cmd.toString() + "(" + tr.toString() + ")\n";
        						}
        						SadlConsole.writeToConsole(MessageType.INFO, msg);
        					}
        					else if (infresults instanceof List<?> && cmd instanceof Explain) {
//        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
								SadlConsole.writeToConsole(MessageType.INFO, "  " + cmd.toString() + ":\n");
								consoleOutput((List<?>) infresults);
        					}
        					else if (infresults instanceof String) {
        						if (cmd instanceof Print || cmd instanceof EndWrite) {
        							if (((String)infresults).startsWith("file:/")) {
        								SadlConsole.writeToConsole(MessageType.INFO, ((Print)cmd).getModel() + " written to '" + ((String)infresults).substring(6) + "\n");
        							}
        							else {
        								SadlConsole.writeToConsole(MessageType.INFO, (String)infresults + "\n");
        							}
        						}
        					}
		        			else {
		        				SadlConsole.writeToConsole(MessageType.ERROR, "Results returned not of expected type. Please report.\n");
        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
        						SadlConsole.writeToConsole(MessageType.INFO, infresults.toString() + "\n");
		        			}
		        		}
    					else {
    						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + " is empty\n");
    						if (cmd instanceof Query) {
    							String msg = "Query: " + ((Query)cmd).toString() + "\n";
    							SadlConsole.writeToConsole(MessageType.INFO, msg);
    						}
    					}
		        		if (errors != null) {
		        			for (int j = 0; j < errors.size(); j++) {
		        				ModelError error = errors.get(j);
	        					if (error != null && error.getErrorMsg() != null) {
	        						SadlConsole.writeToConsole(errorTypeToMessageType(error.getErrorType()), error.getErrorMsg() + "\n");
	        					}
		        			}
		        		}
		        	}
				}
				else if (trgtFile.getFileExtension().equals("test")) {
					// run test suite
					SadlConsole.writeToConsole(MessageType.INFO, "Testing of suite '" +  trgtFile.getFullPath().toPortableString() + "' requested. Not Yet Implemented.\n");
				}
			}
			else {
				SadlConsole.writeToConsole(MessageType.ERROR, "No currently selected file; can't test model.\n");
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			
		}

		return event;
	}

	private void consoleOutput(List<?> explanations) {
		if (explanations == null || explanations.size() == 0) {
			SadlConsole.writeToConsole(MessageType.INFO, "    No applicable rules found.\n");
		}
		for (int i = 0; explanations != null && i < explanations.size(); i++) {
			Object expl = explanations.get(i);
			GraphPatternElement tp = ((Explanation)expl).getGrpahPatternElement();
			String prefix = ((Explanation)expl).getPatternPrefix();
			if (prefix != null) {
				SadlConsole.writeToConsole(MessageType.INFO, "    " + prefix);
			}
			// else {
			// getMessageManager().equals("     ");
			// }
			if (tp != null) {
				SadlConsole.writeToConsole(MessageType.INFO, "    " + tp.toString() + ":\n");
			}
			// else {
			// SadlConsole.writeToConsole(MessageType.INFO, "     undefined triple:\n");
			// }
			List<String> explains = ((Explanation)expl).getExplanations();
			for (int j = 0; explains != null && j < explains.size(); j++) {
				SadlConsole.writeToConsole(MessageType.INFO, explains.get(j) + "\n");
			}
		}
	}

	private MessageType errorTypeToMessageType(ErrorType errorType) {
		if (errorType.equals(ErrorType.ERROR)) {
			return MessageType.ERROR;
		}
		else if (errorType.equals(ErrorType.WARNING)) {
			return MessageType.WARN;
		}
		else if (errorType.equals(ErrorType.INFO)) {
			return MessageType.INFO;
		}
		return MessageType.ERROR;
	}

	@Override
	protected String[] getValidTargetFileTypes() {
		String[] types = {"sadl","test"};
		return types;
	}

}