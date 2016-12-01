package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.Workbench;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.visualize.OntologyGraphGenerator;
import com.ge.research.sadl.utils.ResourceManager;

public class OntologyGraphGeneratorHandler extends GraphGeneratorHandler {


	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			SadlConsole.writeToConsole(MessageType.INFO, "-----------------Ontology Graphing Started-------------------\n\n");
			IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(getCurrentProject());;
			IPath projectPath = project.getLocation();
			if(projectPath != null){
				IFolder ontFolder = project.getFolder("/Data_Model/");
				IResource[] ontFiles = ontFolder.members();

				if(ontFiles != null){
					for (int i = 0; i < ontFiles.length; i++) {
						boolean worked = GenerateOntologyFileGraph(ontFiles[i], project);
						if(!worked){
							SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontFiles[i].getName() + "' does not exist.\n");	
						}else{
							SadlConsole.writeToConsole(MessageType.INFO, "Graph of '" + ontFiles[i].getName() + "' Generated Successfully.\n\n");
						}
					}
				}
				//graph implicit model
				IFolder ontImplFolder = project.getFolder("/ImplicitModel/");
				IResource[] ontImplFiles = ontImplFolder.members();

				if(ontImplFiles != null){
					for (int i = 0; i < ontImplFiles.length; i++) {
						boolean worked = GenerateOntologyFileGraph(ontImplFiles[i], project);
						if(!worked){
							SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontImplFiles[i].getName() + "' does not exist.\n");	
						}else{
							SadlConsole.writeToConsole(MessageType.INFO, "Graph of '" + ontImplFiles[i].getName() + "' Generated Successfully.\n\n");
						}
					}
				}   
			}
		} catch( ClassCastException e){
			SadlConsole.writeToConsole(MessageType.ERROR, "Make sure a folder or file is selected in the desired project\n");
			SadlConsole.writeToConsole(MessageType.ERROR, "Error Message: " + e.getMessage() + "\n");
		}catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			SadlConsole.writeToConsole(MessageType.INFO, "-----------------Ontology Graphing Ended-------------------\n\n");
		}
		return event;
	}
	
	public boolean GenerateOntologyFileGraph(IResource ontFile, IProject project) throws IOException, ConfigurationException, URISyntaxException {
		if(ontFile instanceof IFile){
			String owlFileName = null;
			
			SadlConsole.writeToConsole(MessageType.INFO, "Generate graph of '" + ontFile.getName() + "' requested.\n");
		
			//Get the associated .owl file for this sadl file
			if (ontFile.getName().endsWith("owl")) {
				owlFileName = ontFile.getFullPath().lastSegment();
			}
			else {
				owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(ontFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment()).toPortableString());
				if (!(new File(owlFileName).exists())) {
					owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(ontFile.getFullPath().addFileExtension("owl").lastSegment()).toPortableString());
					if (!(new File(owlFileName).exists())) {
						return false;
					}
					else {
						owlFileName = ontFile.getFullPath().addFileExtension("owl").lastSegment();
					}
				}
				else {
					owlFileName = ontFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
				}
			}
			
			//get the configuration manager
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
			
			//get the visualizer interface
			IGraphVisualizer visualizer = getVisualizer(configMgr);
			if (visualizer != null) {
				String publicUri;
				String prefix = null;
				try {
					publicUri = configMgr.getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName));
					prefix = configMgr.getGlobalPrefix(publicUri);
				}
				catch (Exception e) {
					publicUri = new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName);
				}
				
				OntologyGraphGenerator ogg = new OntologyGraphGenerator(configMgr, publicUri, project);
				ResultSet oggResults = ogg.generateOntologyResultSet(null, publicUri);
				
				if (oggResults != null) {
					graphOntologyResultSet(visualizer, project, (IFile)ontFile, publicUri, prefix, oggResults);
					return true;
				}
				else {
					SadlConsole.writeToConsole(MessageType.ERROR, "file not found. Close and Open project if error persists.\n");
					return false;
				}
			}
		}
		return false;
	}


	protected String[] getValidTargetFileTypes(){
		String[] types = {"owl", "sadl"};
		return types;
	}
	
	private	String getCurrentProject(){
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
	    if (window != null)
	    {
	        IStructuredSelection selection = (IStructuredSelection) window.getSelectionService().getSelection();
	        Object firstElement = selection.getFirstElement();
	        if (firstElement instanceof IAdaptable)
	        {
	            
	        	String[] projName = firstElement.toString().split("/");
	        	
	            return projName[1];
	        }
	    }
	    return null;
	}

}
