/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.ide.handlers;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.resource.XtextResource;
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
import com.ge.research.sadl.model.persistence.SadlPersistenceFormat;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlCommandResult;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.common.base.Supplier;
import com.google.common.base.Throwables;

import jakarta.activation.DataSource;

/**
 * IDE agnostic handler for running the inferencer.
 *
 * <b>Note:</b> I have copied the logic from the {@code RunInference} class and tried to keep it as it was.
 *
 * @author akos.kitta
 */
public class SadlRunInferenceHandler extends SadlIdeActionHandler {

	public void run(Path path, Supplier<XtextResource> resourceSupplier) {
		this.run(path, resourceSupplier, getPreferences(resourceSupplier.get()));
	}
	
	public void run(Path path, Supplier<XtextResource> resourceSupplier, Map<String, String> properties) {
		try {
			// *** NOTE: Copied from com.ge.research.sadl.ui.handlers.RunInference.execute(ExecutionEvent) *** 
			String frmt = properties.get(SadlPreferences.OWL_MODEL_FORMAT.getId());
			if (frmt != null && frmt.length() > 0) {	// awc 2020/11/10: in some cases this is an empty string, treat same as null
				String ext = SadlPersistenceFormat.getFileExtension(SadlPersistenceFormat.getRDFFormat(frmt));
				if (ext != null) {
					frmt = "." + ext;
				}
				else {
					console.error("File extension for format '" + frmt + "' not found.");
				}
			}
			else {
				console.error("No OWL serialization format found.");
				frmt = ".owl";
			}
			
			if (path.getFileName().toString().endsWith(".sadl")) {
				// run inference on this model
				console.info("Inference of '" + path.toAbsolutePath().toString() + "' requested.\n");
				final List<Issue> issues = new ArrayList<Issue>();
				final List<SadlMessage> results = null;
				String modelFolderPath = getOwlModelsFolderPath(path).toString();
				
				String owlModelPath = modelFolderPath + "/" + path.getFileName().toString().replaceFirst("[.][^.]+$", frmt);
				Resource res = prepareResource(resourceSupplier.get());
				Object[] retvals = inferenceProcessor.runInference(res, owlModelPath, modelFolderPath, properties);
				displayInferenceResults(retvals, path, owlModelPath, modelFolderPath, properties);
			}
			else if (path.getFileName().toString().endsWith("test")) {
				// run test suite
				console.info("Testing of suite '" + path.toAbsolutePath().toString() + "' requested.\n");
		    	File f = path.toFile();
		    	List<String> templateImports = new ArrayList<String>();
				Scanner s = new Scanner(f);
				if (s != null) {
					s.useDelimiter("\\n");
					int totalTestCount = 0;
					int passedTestCount = 0;
					IConfigurationManager configMgr = null;
					ResourceSet resourceSet = resourceSupplier.get().getResourceSet();
					while (s.hasNext()) {
						String templateLine = s.next();
						templateLine = CsvImporter.dropEOS(templateLine);
						if (templateLine.trim().startsWith("Test:")) {
							int testLoc = templateLine.indexOf("Test:");
							String testfile = templateLine.substring(testLoc + 5).trim();
							String modelFolderPath = getOwlModelsFolderPath(path).toString();
							String owlModelPath = modelFolderPath + "/" + path.getFileName().toString().replaceFirst("[.][^.]+$", frmt);
							if (configMgr == null) {
								configMgr = ConfigurationManagerFactory.getConfigurationManager(modelFolderPath, SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT);
							}
							String actualUrl = new SadlUtils().fileUrlToFileName(configMgr.getAltUrlFromPublicUri(SadlUtils.stripQuotes(testfile)));
							File actualFile = new File(actualUrl);
							String fileName = actualFile.getName();
							fileName = fileName.substring(0, fileName.lastIndexOf(".")) + ".sadl";
	//						Path projectPath = Paths.get(projectHelper.getRoot(path.toUri()));
							Path projectPath = new File(modelFolderPath).getParentFile().toPath();
							Path file = findFileRecursively(projectPath, fileName);
							if (file != null) {
								Resource res = findAndPrepareResource(resourceSet, file);
								if (res != null) {
									Object[] retvals = inferenceProcessor.runInference(res, actualUrl, modelFolderPath, properties);
									for (int i = 0; retvals != null && i < retvals.length; i++) {
										SadlCommandResult result = (SadlCommandResult) retvals[i];
						        		SadlCommand cmd = result.getCmd();
						        		Object infresults = result.getResults();										Object retval = retvals[i];
										if (infresults instanceof TestResult) {
			        						TestResult tr = (TestResult)infresults;
			        						totalTestCount++;
	//		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
			        						String msg;
			        						if (tr.isPassed()) {
			        							msg = "Test passed: " + cmd.toString() + "\n";
			        							passedTestCount++;
			        						}
			        						else {
			        							msg = "Test failed: " + cmd.toString() + "(" + tr.toString() + ")\n";
			        						}
			        						console.info(msg);
										}
									}
								}
								else {
									console.error("Unable to find Resource for '" + testfile + "'\n");
								}
							}
							else {
								console.error("Unable to find actual file for '" + testfile + "'\n");
							}
						}
					}
					console.info("Completed test suite'" + path.toAbsolutePath().toString() + "': passed " + passedTestCount + " of " + totalTestCount + " tests.\n");
					s.close();
				}
			}
		} catch (Exception e) {
			console.error(Throwables.getStackTraceAsString(e));
		}
	}

	private DataSource lastDerivation = null;
	private DataSource getLastDerivation() {
		return lastDerivation;
	}

	private void setLastDerivation(DataSource lastDerivation) {
		this.lastDerivation = lastDerivation;
	}

	protected void displayInferenceResults(Object[] retvals, Path trgtFile, String owlModelPath, String modelFolderPath, Map<String, String> prefMap) throws ConfigurationException, IOException {
		if (inferenceProcessor != null) {
			String reasonerInfo = inferenceProcessor.getReasonerIdentity();
			if (reasonerInfo != null) {
				console.info(reasonerInfo + ".\n");
			}
		}
		if (retvals == null || retvals.length < 1) {
			console.error("There are no inference results.");
			return;
    	}
		int numTests = 0;
		int numTestsPassed = 0;
    	for (int idx = 0; idx < retvals.length; idx++) {
    		if (retvals[idx] != null && !(retvals[idx] instanceof SadlCommandResult)) {
    			if (retvals[idx] instanceof List<?>) {
    				// this should be errors but not in the context of a SadlCommandResult
    				for (int i = 0; i < ((List<?>)retvals[idx]).size(); i++) {
    					console.error(((List<?>)retvals[idx]).get(i).toString() + "\n");
    				}
    			}
    			else {
    				console.error("Unexpected inference result is not a SadlCommandResult (" + retvals[idx].toString());
    			}
    			break;
    		}
    		Object resultObj = retvals[idx];
    		SadlCommandResult result = (SadlCommandResult) resultObj;
			if (result != null && result.getDerivations() != null) {
				setLastDerivation(result.getDerivations());
			}
    		SadlCommand cmd = result != null ? result.getCmd() : null;
    		Object infresults = result != null ? result.getResults() : null;
    		List<ModelError> errors = result != null ? result.getErrors() : null;
    		
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
							String nsiqr = prefMap.get(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId());
							rs.setShowNamespaces(Boolean.parseBoolean(nsiqr));
							msg += rs.toStringWithIndent(5);
    						console.info(msg);
							String desc = query.getName();
							if (desc == null) desc = "Cmd " + (idx + 1) + "  (Graph)";
							String baseFileName = trgtFile.getFileName().toString() + idx; 
							baseFileName = baseFileName.replace(".", "_");
    						resultSetToGraph(trgtFile, rs, desc, baseFileName, null, prefMap);
						}
						else {
//    						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
							String msg = "Query: " + query.toString() + "\n";
							String nsiqr = prefMap.get(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId());
							rs.setShowNamespaces(Boolean.parseBoolean(nsiqr));
							msg += rs.toStringWithIndent(5);
							console.info(msg);
						}
    				}
    			}
				else if (infresults instanceof TestResult) {
					numTests++;
					TestResult tr = (TestResult)infresults;
//					SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
					String msg;
					if (tr.isPassed()) {
						msg = "Test passed: " + cmd.toString() + "\n";
						numTestsPassed++;
					}
					else {
						msg = "Test failed: " + cmd.toString() + "(" + tr.toString() + ")\n";
					}
					console.info(msg);
				}
				else if (infresults instanceof List<?> && cmd instanceof Explain) {
//					SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (idx + 1) + ":\n");
					console.info("  " + cmd.toString() + ":\n");
					consoleOutput((List<?>) infresults);
				}
				else if (infresults instanceof String) {
					if (cmd instanceof Print || cmd instanceof EndWrite) {
						if (((String)infresults).startsWith("file:/")) {
							if (cmd instanceof Print) {
								console.info(((Print)cmd).getModel() + " written to '" + ((String)infresults).substring(6) + "'\n");
							}
							else {
								console.info("Write statement output written to '" + ((EndWrite)cmd).getOutputFilename() + "'\n");
							}
						}
						else {
							console.info((String)infresults + "\n");
						}
					}
				}
    			else {
    				console.error("Results returned not of expected type. Please report.\n");
    				console.info("Inference result " + (idx + 1) + ":\n");
    				console.info(infresults.toString() + "\n");
    			}
    		}
			else if (cmd != null) {
				if (cmd instanceof Query) {
					String msg;
					if (((Query)cmd).isUpdate()) {
						msg = "Update: " + ((Query)cmd).toString() + "\n";
					}
					else {
						msg = "Query: " + ((Query)cmd).toString() + "\n";
					}
					console.info(msg);
					if (!((Query)cmd).isUpdate()) {
						console.info("Inference result " + (idx + 1) + " is empty\n");
					}
				}
				else if (!(cmd instanceof Read)) {
					console.info("Inference result " + (idx + 1) + " is empty\n");
				}
			}
    		if (errors != null) {
    			for (int j = 0; j < errors.size(); j++) {
    				ModelError error = errors.get(j);
					if (error != null && error.getErrorMsg() != null) {
						console.print(errorTypeToMessageType(error.getErrorType()), error.getErrorMsg() + "\n");
					}
    			}
    		}
    		if (result != null) {
	    		if (result.getTimingInfo() != null) {
	    			console.info("Timing info:\n");
	    			for (ReasonerTiming tinfo : result.getTimingInfo()) {
	    				console.info("   " + tinfo.toString() + "\n");
	    			}
	    			console.info("\n");
	    		}
    		}
    	}
		if (getLastDerivation() != null) {
			DataSource ds = getLastDerivation();
			console.info(writeDerivationsToFile(trgtFile, ds));
		}
		else if (inferenceProcessor != null) {
			String dl;
			try {
				dl = inferenceProcessor.getReasonerConfigurationItem(IReasoner.DerivationsRequestedKey);
				if (dl != null && dl.length() > 0 && !dl.equals("None")) {
					console.info("Derivation information requested but none available.");
				}
			} catch (Exception e) {
				console.error("Error getting reasoner configuration for derivations: " + e.getMessage());
			}
		}
    	if (numTests > 0) {
    		String msg = "Test summary: ";
    		msg += numTestsPassed;
    		msg += " of ";
    		msg += numTests;
    		msg += " tests passed.";
    		console.info(msg);
    	}
	}
	
	public Path findFileRecursively(Path path, String name) throws IOException {
		Optional<Path> result = Files.walk(path).filter(p -> p.toFile().isFile() && p.getFileName().toString().endsWith(name)).findFirst();
		return result.isPresent() ? result.get() : null;
	}
	
	private String writeDerivationsToFile(Path path, DataSource ds) throws IOException {
		URI pathUri = projectHelper.toUri(path);
		URI phRoot = projectHelper.getRoot(pathUri);
		String phStr = projectHelper.toString();
 		String tempFolderPath = phRoot.getPath() + "/Temp"; //phRoot.resolve("Temp").toString();
 		String baseName = path.getFileName().toString();
 		if (baseName.endsWith(ResourceManager.SADLEXT)) {
 			baseName = baseName.substring(0,
 					baseName.length() - 5);
 		}
 		String outputfilename = (new SadlUtils()).fileUrlToFileName(tempFolderPath + File.separator + baseName + ".Derivations.log");
 		String msg = "Derivations written to '" + outputfilename + "'\n";
 		File f = new File(outputfilename);
 		f.mkdirs();
 		if (f.exists()) {
 			f.delete();
 		}
 		f.createNewFile();
 		if (f.canWrite()) {
	 		OutputStream os = new FileOutputStream(f);
	 		InputStream is = ds.getInputStream();
	 		byte[] buf = new byte[1024];
	 		int i = 0;
	 		while ((i = is.read(buf)) != -1) {
	 			os.write(buf, 0, i);
	 		}
	 		is.close();
	 		os.close();
 		}
 		else {
 			console.error("Unable to write to derivation file '" + f.getCanonicalPath() + "'; writing derivations to console.");;
 			StringBuilder sb = new StringBuilder();
	 		InputStream is = ds.getInputStream();
	 		byte[] buf = new byte[1024];
	 		int i = 0;
	 		while ((i = is.read(buf)) != -1) {
	 			sb.append(buf);
	 		}
	 		is.close();
	 		console.info(sb.toString());
 		}
 		return msg;
 	}

	private void consoleOutput(List<?> explanations) {
		if (explanations == null || explanations.size() == 0) {
			console.info("    No applicable rules found.\n");
		}
		for (int i = 0; explanations != null && i < explanations.size(); i++) {
			Object expl = explanations.get(i);
			GraphPatternElement tp = ((Explanation)expl).getGrpahPatternElement();
			String prefix = ((Explanation)expl).getPatternPrefix();
			if (prefix != null) {
				console.info("    " + prefix);
			}
			// else {
			// getMessageManager().equals("     ");
			// }
			if (tp != null) {
				console.info("    " + tp.toString() + ":\n");
			}
			// else {
			// SadlConsole.writeToConsole(MessageType.INFO, "     undefined triple:\n");
			// }
			List<String> explains = ((Explanation)expl).getExplanations();
			for (int j = 0; explains != null && j < explains.size(); j++) {
				console.info(explains.get(j) + "\n");
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
	
}
