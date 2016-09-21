package com.ge.research.sadl.perspective.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
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
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.URIUtil;
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

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.external.ExternalEmfResource;
import com.ge.research.sadl.perspective.util.Util;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.hp.hpl.jena.assembler.exceptions.TransactionAbortedException;

public class RunQuery extends SadlActionHandler {

	public String projectLocation;
	protected Process Process;

	public RunQuery() {
		super();
	}

	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = {"sadl","owl"};
			Object[] target = getCommandTarget(validTargetTypes);
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}
			String owlFileName = null;

			Map<String,String> prefMap = getPreferences();
			if (trgtFile != null) {
				if (trgtFile.getFileExtension().equals("sadl")) {
					// run query on this model
	//				Resource res = prepareActionHandler(target[2]);
					SadlConsole.writeToConsole(MessageType.INFO, "Adhoc Query of '" + trgtFile.getFullPath().toPortableString() + "' requested.\n");
					
	//				final List<Issue> issues = new ArrayList<Issue>();
	//				processor.processAdhocQuery(res, new ValidationAcceptor(new IAcceptor<Issue>(){
	
	//					@Override
	//					public void accept(Issue t) {
	//						issues.add(t);
	//					}
						
	//				}),  new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(res)), query);
	//				if (issues.size() > 0) {
	//					for (Issue issue: issues) {
	//						output.writeToConsole(MessageType.ERROR, issue.getMessage() + "\n");
	//					}
	//				}
					owlFileName = trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
				}
				else if (trgtFile.getFileExtension().equals("owl")) {
					// run query on this model
	//				Resource res = prepareActionHandler(trgtFile);
					SadlConsole.writeToConsole(MessageType.INFO, "Adhoc Query of '" + trgtFile.getFullPath().toPortableString() + "' requested.\n");
					owlFileName = trgtFile.getFullPath().lastSegment();
				}
				String query = getQuery();
				File qf = new File(query);
				List<String> qlist = null;
				if (qf.exists()) {
					List<String[]> queries = new SadlUtils().parseQueries(qf);
					if (queries != null && queries.size() > 0) {
						qlist = new ArrayList<String>();
						for (int i = 0; i < queries.size(); i++) {
							qlist.add(queries.get(i)[1]);
						}
					}
				}
				else {
					qlist = new ArrayList<String>();
					qlist.add(query);
				}
				{
					String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
//					File owlFile = null; //trgtFile.getLocation().toFile();
//					String modelFolderUri = mfFolder.getCanonicalPath();
					final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
					IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
					IReasoner reasoner = configMgr.getReasoner();
					if (!reasoner.isInitialized()) {
						reasoner.setConfigurationManager(configMgr);
						String modelName = configMgr.getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName));
						reasoner.initializeReasoner(modelFolderUri, modelName, format);
					}
					if (reasoner != null) {	
						int qidx = 0;
						while (query != null) {
							if (qlist != null) {
								if (qidx < qlist.size()) {
									query = qlist.get(qidx++);
								}
								else {
									break;
								}
							}
							try {
								String currentQuery = reasoner.prepareQuery(query);
								ResultSet rs = reasoner.ask(currentQuery);
								if (rs != null) {
									SadlConsole.getInstance().writeToConsole(MessageType.INFO, rs.toStringWithIndent(5));
								}
								else {
									SadlConsole.getInstance().writeToConsole(MessageType.WARN, "Query '" + currentQuery + "' returned no results\n");
								}
							}
							catch (Throwable t) {
								System.err.println("Error processing query '" + query + "'");
								System.err.println("   " + t.getMessage());
							}
						}
					}
				}
//				else if (trgtFile.getName().endsWith("test")) {
//					// run test suite
//					output.writeToConsole(MessageType.INFO, "Testing of suite '" +  trgtFile.getName() + "' requested.\n");
//				}
			}
			else {
				throw new TranslationException("Please select a target for querying");
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {

		}

		return event;
	}

	private String getQuery() {
		Shell shell = new Shell();
		InputDialog dlg = new InputDialog(
				shell,
				"Run Query",
				"SADL or SPARQL Query?",
				"D:\\Users\\200005201\\Desktop\\SrlMetricsQueries3.txt",
				null);
		dlg.open();
		if (dlg.getReturnCode() != Window.OK) {
			return null;
		}
		return dlg.getValue();
	}

}