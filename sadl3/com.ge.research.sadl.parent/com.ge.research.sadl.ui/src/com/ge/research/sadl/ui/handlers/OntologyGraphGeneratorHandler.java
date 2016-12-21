package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
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


	private IConfigurationManagerForIDE configMgr;
	private IProject project;
	private String modelFolderUri;
	private List<String[]> imports;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			SadlConsole.writeToConsole(MessageType.INFO, "-----------------Ontology Graphing Started-------------------\n\n");
			String curProject = getCurrentProject();
			if (curProject == null) {
				SadlConsole.writeToConsole(MessageType.ERROR, "An open project must be selected before graphing can occur.\n");
				return event;
			}
			project = ResourcesPlugin.getWorkspace().getRoot().getProject(curProject);
			modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			if (getConfigMgr() != null && getConfigMgr().getModelFolder() != null) {
				String configModelFolder = getConfigMgr().getModelFolderPath().getCanonicalPath().replace("\\", "/");
				if (!configModelFolder.equals(modelFolderUri)) {
					configMgr = null;		// this is an obsolete ConfigurationManager
				}
			}
			
			graphFolder(project);
			// now graph the base model and the list model
			IResource sbmr = project.findMember("OwlModels/SadlBaseModel.owl");
			if (sbmr != null) {
				generateOntologyFileGraph(sbmr, false);
			}
			IResource slmr = project.findMember("OwlModels/SadlListModel.owl");
			if (slmr != null) {
				generateOntologyFileGraph(slmr, false);
			}
			if (imports.size() > 0) {
				String[] headers = new String[3];
				headers[0] = "head";
				headers[1] = "imports";
				headers[2] = "tail";
				IFile trgtFile = null;
				boolean derivedFN = false;
				ResultSet importsRS = listToResultSet(headers, imports, trgtFile, derivedFN);
				String baseFileName = project.getName();
				String graphName = baseFileName;
				String description = "Project " + baseFileName + " imports";
				IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, null);
				
				IGraphVisualizer visualizer = getVisualizer(configMgr);
				if (visualizer != null) {
					graphResultSet(visualizer, project, trgtFile, baseFileName, graphName, null, description, importsRS);					
				}
				else {
					SadlConsole.writeToConsole(MessageType.ERROR, "Unable to find an instance of IGraphVisualizer to render graph for query.\n");
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
		try {
			project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return event;
	}

	private void graphFolder(IResource thisFolder)
			throws CoreException, IOException, ConfigurationException, URISyntaxException {
		if(thisFolder != null && thisFolder instanceof IContainer){
			IResource[] ontFiles = ((IContainer) thisFolder).members();

			if(ontFiles != null){
				imports = new ArrayList<String[]>();
				for (int i = 0; i < ontFiles.length; i++) {
					if (ontFiles[i].getType() == IResource.FILE && 
							(ontFiles[i].getFullPath().getFileExtension().equals("sadl") || 
									ontFiles[i].getFullPath().getFileExtension().equals("owl") ||
									ontFiles[i].getFullPath().getFileExtension().equals("nt") ||
									ontFiles[i].getFullPath().getFileExtension().equals("n3"))) {
						boolean worked = generateOntologyFileGraph(ontFiles[i], true);
						if(!worked){
							SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontFiles[i].getName() + "' does not exist, or does not contain valid data for graphing.\n");	
						}else{
							SadlConsole.writeToConsole(MessageType.INFO, "Graph of '" + ontFiles[i].getName() + "' Generated Successfully.\n\n");
						}
					}
					else if (ontFiles[i].getType() == IResource.FOLDER) {
						if (!isReservedFolder(ontFiles[i])) {
							graphFolder(ontFiles[i]);
						}
					}
				}
			}
		}
	}
	
	private boolean isReservedFolder(IResource iResource) {
		String name = iResource.getName();
		if (
				name.equals("OwlModels") || 
				name.equals("Graphs") || name.equals("Temp")) {
			return true;
		}
		return false;
	}

	public boolean generateOntologyFileGraph(IResource ontFile, boolean checkForDerivedFile) throws IOException, ConfigurationException, URISyntaxException {
		if(ontFile instanceof IFile){
			String owlFileName = null;
			
			SadlConsole.writeToConsole(MessageType.INFO, "Generate graph of '" + ontFile.getName() + "' requested.\n");
		
			//Get the associated .owl file for this sadl file
			if (ontFile.getName().endsWith("owl")) {
				if (checkForDerivedFile && isOwlFileOfSadlModel(ontFile)) {
					return false;
				}
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
			
			//get the visualizer interface
			IGraphVisualizer visualizer = getVisualizer(getConfigMgr());
			if (visualizer != null) {
				String fullFileName = modelFolderUri + "/" + owlFileName;
				String publicUri;
				String prefix = null;
				try {
					publicUri = getConfigMgr().getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(fullFileName));
					prefix = getConfigMgr().getGlobalPrefix(publicUri);
				}
				catch (Exception e) {
					publicUri = new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName);
				}
				
				OntologyGraphGenerator ogg = new OntologyGraphGenerator(getConfigMgr(), publicUri, project);
				ResultSet oggResults = ogg.generateOntologyResultSet(null, publicUri);
				List<String[]> newImports = ogg.getImports(getConfigMgr(), publicUri);
				if (newImports != null) {
					imports.addAll(newImports);
				}
				
				if (oggResults != null) {
					graphOntologyResultSet(visualizer, project, (IFile)ontFile, publicUri, prefix, oggResults);
					return true;
				}
				else {
					if (!(new File(fullFileName).exists())) {
						SadlConsole.writeToConsole(MessageType.ERROR, "file not found. Close and Open project if error persists.\n");
					}
					else {
						SadlConsole.writeToConsole(MessageType.WARN, "file '" + fullFileName + "' appears to have no content to graph.\n");
					}
					return false;
				}
			}
		}
		return false;
	}


	private boolean isOwlFileOfSadlModel(IResource ontFile) {
		String path = ontFile.getLocation().toOSString();
		try {
			String publicUri = getConfigMgr().getPublicUriFromActualUrl(path);
			if (publicUri != null) {
				return true;
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
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
	        ISelection selection = (IStructuredSelection) window.getSelectionService().getSelection();
	        if (selection instanceof IStructuredSelection) {
		        Object firstElement = ((IStructuredSelection) selection).getFirstElement();
		        if (firstElement instanceof IAdaptable)
		        {
		            
		        	String[] projName = firstElement.toString().split("/");
		        	
		            return projName[1];
		        }
	        }
	        else {
	        	return null;
	        }
	    }
	    return null;
	}

	private IConfigurationManagerForIDE getConfigMgr() throws ConfigurationException {
		if (configMgr == null) {
			//create the configuration manager
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			setConfigMgr(ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format));
		}
		return configMgr;
	}

	private void setConfigMgr(IConfigurationManagerForIDE configMgr) {
		this.configMgr = configMgr;
	}

}
