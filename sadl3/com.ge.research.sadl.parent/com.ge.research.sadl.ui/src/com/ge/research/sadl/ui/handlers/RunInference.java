package com.ge.research.sadl.ui.handlers;

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
import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.ConfigurationManager;
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
		        		Object cmds = retvals[0];
		        		Object infresults = retvals[1];
		        		if (infresults != null) {
		        			if (infresults instanceof List<?>) {
		        				for (int i = 0; i < ((List<?>)infresults).size(); i++) {
		        					if (((List<?>)infresults).get(i) instanceof ResultSet && cmds != null && cmds instanceof List<?> && ((List<?>)cmds).size() >= i && 
		        							((List<?>)cmds).get(i) instanceof Query && ((Query)((List<?>)cmds).get(i)).isGraph()) {
		        						ResultSet rs = (ResultSet) ((List<?>)infresults).get(i);
		        						if (rs.getColumnCount() >= 3) {
			        						String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			        						final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			        						IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
	
			        						IGraphVisualizer visualizer = getVisualizer(configMgr);
			        						if (visualizer != null) {
			        							String baseFileName = trgtFile.getProjectRelativePath().removeFileExtension().lastSegment() + i;
			        							String desc = ((Query)((List<?>)cmds).get(i)).getName();
			        							if (desc == null) desc = "Cmd " + (i + 1) + "  (Graph)";
			        							graphResultSet(visualizer, project, trgtFile, baseFileName, baseFileName, null, desc, rs);
			        						}
			        						else {
			        							SadlConsole.writeToConsole(MessageType.ERROR, "Unable to find an instance of IGraphVisualizer to render graph for query " + i + ".\n");
			        						}
		        						}
		        						else {
		        							SadlConsole.writeToConsole(MessageType.ERROR, "Unable to render graph for query " + i + ", ResultSet has less than 3 columns.\n");
		        						}
		        					}
		        					else if (((List<?>)infresults).get(i) == null) {
		        						SadlConsole.writeToConsole(MessageType.INFO, "Inference result " + (i + 1) + " is empty");
		        					}
		        					else {
		        						SadlConsole.writeToConsole(MessageType.INFO, ((List<?>)infresults).get(i).toString());
		        					}
		        				}
		        			}
		        			else {
		        				SadlConsole.writeToConsole(MessageType.ERROR, "Results returned not of expected type. Please report.");
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