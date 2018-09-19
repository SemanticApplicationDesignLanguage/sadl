package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import javax.activation.DataSource;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.jena.importer.CsvImporter;
import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.gp.EndWrite;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.Read;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.SadlInferenceException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlCommandResult;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.utils.ResourceManager;

public class RunInference extends SadlActionHandler {

	public RunInference() {
		super();
	}
	
	protected void generateInferenceJob(Resource res, IProject project, IPath trgtFolder, IFile trgtFile, String owlModelPath, String modelFolderPath, Map<String, String> prefMap) {
		Job inferenceJob = new Job("Inferencing") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				Thread inferenceThread;
				try {
					inferenceThread = new Thread() {
						public void run() {
							Object[] retvals = null;
							try {
								retvals = processor.runInference(res, owlModelPath, modelFolderPath, prefMap);
								try {
									displayInferenceResults(retvals, project, trgtFolder, trgtFile, owlModelPath, modelFolderPath, prefMap);
								} catch (ConfigurationException | IOException | CoreException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							} catch (SadlInferenceException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
					};
					
					inferenceThread.start();
					
					TimeUnit.SECONDS.sleep(1);
					
					while (inferenceThread.isAlive()) {
						if (monitor.isCanceled()) {
							while (inferenceThread.isAlive()) {
								inferenceThread.stop();
								inferenceThread.join();
							}
							SadlConsole.writeToConsole(MessageType.INFO, "Inference canceled.");
							return Status.CANCEL_STATUS;
						}
						
						monitor.worked(1);
						TimeUnit.SECONDS.sleep(1);
					}
				}
				catch (Exception e) {
//					return Status.CANCEL_STATUS;
				}
		    	return Status.OK_STATUS;
			}

		};
		inferenceJob.schedule();		
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
//		        	Object[] retvals = processor.runInference(res, owlModelPath, modelFolderPath, prefMap);
					generateInferenceJob(res, project, trgtFolder, trgtFile, owlModelPath, modelFolderPath, prefMap);
				}
				else if (trgtFile.getFileExtension().equals("test")) {
					// run test suite
					SadlConsole.writeToConsole(MessageType.INFO, "Testing of suite '" +  trgtFile.getFullPath().toPortableString() + "' requested.\n");
			    	File f = new File(trgtFile.getRawLocationURI().getPath());
			    	List<String> templateImports = new ArrayList<String>();
					Scanner s = new Scanner(f).useDelimiter("\\n");
					int totalTestCount = 0;
					int passedTestCount = 0;
					while (s.hasNext()) {
						String templateLine = s.next();
						templateLine = CsvImporter.dropEOS(templateLine);
						if (templateLine.trim().startsWith("Test:")) {
							int testLoc = templateLine.indexOf("Test:");
							String testfile = templateLine.substring(testLoc + 5).trim();
							String modelFolderPath = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toOSString());
							String owlModelPath = modelFolderPath + "/" + trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
							IConfigurationManager configMgr = ConfigurationManagerFactory.getConfigurationManager(modelFolderPath, IConfigurationManager.RDF_XML_ABBREV_FORMAT);
							String actualUrl = new SadlUtils().fileUrlToFileName(configMgr.getAltUrlFromPublicUri(SadlUtils.stripQuotes(testfile)));
							File actualFile = new File(actualUrl);
							String fileName = actualFile.getName();
							fileName = fileName.substring(0, fileName.lastIndexOf(".")) + ".sadl";
							IFile file = findFileRecursively(project, fileName);
							if (file != null) {
								Resource res = prepareActionHandler(project, file);
								if (res != null) {
									Object[] retvals = processor.runInference(res, actualUrl, modelFolderPath, prefMap);
									for (int i = 0; retvals != null && i < retvals.length; i++) {
										SadlCommandResult result = (SadlCommandResult) retvals[i];
						        		SadlCommand cmd = result.getCmd();
						        		Object infresults = result.getResults();										Object retval = retvals[i];
										if (infresults instanceof TestResult) {
			        						TestResult tr = (TestResult)infresults;
			        						totalTestCount++;
//			        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
			        						String msg;
			        						if (tr.isPassed()) {
			        							msg = "Test passed: " + cmd.toString() + "\n";
			        							passedTestCount++;
			        						}
			        						else {
			        							msg = "Test failed: " + cmd.toString() + "(" + tr.toString() + ")\n";
			        						}
			        						SadlConsole.writeToConsole(MessageType.INFO, msg);
										}
									}
								}
							}
						}
					}
					SadlConsole.writeToConsole(MessageType.INFO, "Completed test suite'" +  trgtFile.getFullPath().toPortableString() + "': passed " + passedTestCount + " of " + totalTestCount + " tests.\n");
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
	
	protected void displayInferenceResults(Object[] retvals, IProject project, IPath trgtFolder, IFile trgtFile, String owlModelPath, String modelFolderPath, Map<String, String> prefMap) throws ConfigurationException, IOException, CoreException {
    	
		if (retvals == null || retvals.length < 1) {
    		
    	}
    	for (int idx = 0; idx < retvals.length; idx++) {
    		if (!(retvals[idx] instanceof SadlCommandResult)) {
    			SadlConsole.writeToConsole(MessageType.ERROR, "Unexpected inference result is not a SadlCommandResult (" + (retvals[idx] != null ? retvals[idx].toString() : "null"));
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
//    						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
							String msg = "Graph: " + query.toString() + "\n";
							msg += rs.toStringWithIndent(5);
    						SadlConsole.writeToConsole(MessageType.INFO, msg);
							String desc = query.getName();
							if (desc == null) desc = "Cmd " + (idx + 1) + "  (Graph)";
							String baseFileName = trgtFile.getProjectRelativePath().lastSegment() + idx; 
							baseFileName = baseFileName.replace(".", "_");
    						resultSetToGraph(project, trgtFile, rs, desc, baseFileName, null, prefMap);
						}
						else {
//    						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
							String msg = "Query: " + query.toString() + "\n";
							String nsiqr = prefMap.get(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId());
							rs.setShowNamespaces(Boolean.parseBoolean(nsiqr));
							msg += rs.toStringWithIndent(5);
    						SadlConsole.writeToConsole(MessageType.INFO, msg);
						}
    				}
    			}
				else if (infresults instanceof TestResult) {
					TestResult tr = (TestResult)infresults;
//					SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
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
//					SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
					SadlConsole.writeToConsole(MessageType.INFO, "  " + cmd.toString() + ":\n");
					consoleOutput((List<?>) infresults);
				}
				else if (infresults instanceof String) {
					if (cmd instanceof Print || cmd instanceof EndWrite) {
						if (((String)infresults).startsWith("file:/")) {
							if (cmd instanceof Print) {
								SadlConsole.writeToConsole(MessageType.INFO, ((Print)cmd).getModel() + " written to '" + ((String)infresults).substring(6) + "'\n");
							}
							else {
								SadlConsole.writeToConsole(MessageType.INFO, "Write statement output written to '" + ((EndWrite)cmd).getOutputFilename() + "'\n");
							}
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
				if (cmd instanceof Query) {
					String msg;
					if (((Query)cmd).isUpdate()) {
						msg = "Update: " + ((Query)cmd).toString() + "\n";
					}
					else {
						msg = "Query: " + ((Query)cmd).toString() + "\n";
					}
					SadlConsole.writeToConsole(MessageType.INFO, msg);
					if (!((Query)cmd).isUpdate()) {
						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + " is empty\n");
					}
				}
				else if (!(cmd instanceof Read)) {
					SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + " is empty\n");
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
    		if (result.getTimingInfo() != null) {
    			SadlConsole.writeToConsole(MessageType.INFO, "Timing info:\n");
    			for (ReasonerTiming tinfo : result.getTimingInfo()) {
    				SadlConsole.writeToConsole(MessageType.INFO, "   " + tinfo.toString() + "\n");
    			}
    			SadlConsole.writeToConsole(MessageType.INFO, "\n");
    		}
    		if (result.getDerivations() != null) {
    			DataSource ds = result.getDerivations();
    			SadlConsole.writeToConsole(MessageType.INFO, writeDerivationsToFile(project, trgtFile, ds));
    		}
    	}

	}
	
	public IFile findFileRecursively(IContainer container, String name) throws CoreException {
	    for (IResource r : container.members()) {
	        if (r instanceof IContainer) {
	            IFile file = findFileRecursively((IContainer) r, name);
	            if(file != null) {
	                return file;
	            }
	        } else if (r instanceof IFile && r.getName().equals(name)) {
	            return (IFile) r;
	        }
	    }

	    return null;
	}
	
	private String writeDerivationsToFile(IProject project, IFile trgtFile, DataSource ds) throws CoreException, IOException {
		String tempFolderPath = convertProjectRelativePathToAbsolutePath(project.getFullPath().append("Temp").toOSString());
		File tfFile = new File(tempFolderPath);
		tfFile.mkdirs();
		String baseName = trgtFile.getName();
		if (baseName.endsWith(ResourceManager.SADLEXT)) {
			baseName = baseName.substring(0,
					baseName.length() - 5);
		}
		String outputfilename = tempFolderPath + File.separator + baseName + ".Derivations.log";
		String msg = "Derivations written to '" + outputfilename + "'\n";
		File f = new File(outputfilename);
		f.createNewFile();
		OutputStream os = new FileOutputStream(f);
		InputStream is = ds.getInputStream();
		byte[] buf = new byte[1024];
		int i = 0;
		while ((i = is.read(buf)) != -1) {
			os.write(buf, 0, i);
		}
		is.close();
		os.close();
		project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		return msg;
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