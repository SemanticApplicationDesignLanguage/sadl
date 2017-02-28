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
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.internal.Workbench;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.visualize.GraphGenerator.UriStrategy;
import com.ge.research.sadl.ui.visualize.GraphSegment;
import com.ge.research.sadl.ui.visualize.OntologyGraphGenerator;
import com.ge.research.sadl.utils.ResourceManager;

public class OntologyGraphGeneratorHandler extends GraphGeneratorHandler {


	private List<GraphSegment> imports;
	private IGraphVisualizer visualizer;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			if (imports != null) imports.clear();
			
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
				IGraphVisualizer visualizer = getVisualizer(getConfigMgr());
				OntologyGraphGenerator ogg = new OntologyGraphGenerator(getConfigMgr(), visualizer, project);
//				if (sbmr != null) {
//					GraphSegment sbmgs = new GraphSegment(SadlConstants.SADL_BASE_MODEL_PREFIX, null, null, getConfigMgr());
//					sbmgs.addHeadAttribute("headtooltip", "\"" + SadlConstants.SADL_BASE_MODEL_URI + "\"");
//					sbmgs.addHeadAttribute("URL", ogg.getCurrentFileLink(SadlConstants.SADL_BASE_MODEL_URI));
//					imports.add(sbmgs);
//				}
//				if (slmr != null) {
//					GraphSegment slmgs = new GraphSegment(SadlConstants.SADL_LIST_MODEL_PREFIX, null, null, getConfigMgr());
//					slmgs.addHeadAttribute("headtooltip", "\"" + SadlConstants.SADL_LIST_MODEL_URI + "\"");
//					slmgs.addHeadAttribute("URL", ogg.getCurrentFileLink(SadlConstants.SADL_LIST_MODEL_URI));
//					imports.add(slmgs);
//				}
//				GraphSegment simgs = new GraphSegment(SadlConstants.SADL_IMPLICIT_MODEL_PREFIX, null, null, getConfigMgr());
//				simgs.addHeadAttribute("headtooltip", "\"" + SadlConstants.SADL_IMPLICIT_MODEL_URI + "\"");
//				String simfn = SadlConstants.SADL_IMPLICIT_MODEL_FILENAME;
//				simfn = simfn.substring(0, simfn.lastIndexOf("."));
//				simgs.addHeadAttribute("URL", ogg.getCurrentFileLink(simfn));
//				imports.add(simgs);
//				GraphSegment sbfgs = new GraphSegment(SadlConstants.SADL_BUILTIN_FUNCTIONS_ALIAS, null, null, getConfigMgr());
//				sbfgs.addHeadAttribute("headtooltip", "\"" + SadlConstants.SADL_BUILTIN_FUNCTIONS_URI + "\"");
//				String sbffn = SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME;
//				sbffn = sbffn.substring(0, sbffn.lastIndexOf("."));
//				sbfgs.addHeadAttribute("URL", ogg.getCurrentFileLink(sbffn));
//				imports.add(sbfgs);

				IFile trgtFile = null;
				String baseFileName = "Project";
				String graphName = baseFileName;
				String description = "Project " + project.getName() + " import map";
//				IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, null);
				ResultSet importsRS = ogg.convertDataToResultSet(imports, UriStrategy.LOCALNAME_WITH_URI_TOOLTIP);
				if (visualizer != null) {
					graphResultSet(visualizer, project, baseFileName, graphName, null, description, importsRS, null, true);					
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
			throws Exception {
		if(thisFolder != null && thisFolder instanceof IContainer){
			IResource[] ontFiles = ((IContainer) thisFolder).members();

			if(ontFiles != null){
				if (imports == null) {
					imports = new ArrayList<GraphSegment>();
				}
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

	public boolean generateOntologyFileGraph(IResource ontFile, boolean checkForDerivedFile) throws Exception {
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
			
			visualizer = getVisualizer(getConfigMgr());
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
				
				OntologyGraphGenerator ogg = new OntologyGraphGenerator(getConfigMgr(), visualizer, project, publicUri);
				ResultSet oggResults = null;
				try {
					oggResults = ogg.generateOntologyResultSet(null, publicUri, UriStrategy.QNAME_ONLY);
				} catch (InvalidNameException e) {
					SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage());
				}
				List<GraphSegment> newImports = ogg.getImports(getConfigMgr(), publicUri);
				if (newImports != null && newImports.size() > 0) {
					imports.addAll(newImports);
				}
				else if (oggResults != null){
					GraphSegment simgs = new GraphSegment((prefix != null ? prefix : publicUri), null, null, getConfigMgr());
					simgs.addHeadAttribute("headtooltip", "\"" + publicUri + "\"");
					String simfn = getConfigMgr().getAltUrlFromPublicUri(publicUri);
					simfn = simfn.substring(0, simfn.lastIndexOf("."));
					try {
						simgs.addHeadAttribute("URL", ogg.getCurrentFileLink(simfn));
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					imports.add(simgs);
				}
				
				if (oggResults != null) {
					String baseFileName = ogg.getBaseFilenameFromPublicUri(publicUri);
					String graphName = prefix;
					String anchorNode = null;
					String descripition = "Graph of Ontology File";
					Orientation orientation = null;
//										graphOntologyResultSet(visualizer, project, (IFile)ontFile, publicUri, prefix, oggResults);
					graphResultSet(visualizer, project, baseFileName, graphName, anchorNode, descripition, oggResults, orientation, false);
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

//	protected void graphOntologyResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String publicUri, String prefix, ResultSet rs) throws IOException {
//		String baseFileName = trgtFile.getFullPath().lastSegment().toString();// + "_ONT";
//		String graphName = prefix;
//		String anchorNode = nodeText(publicUri, prefix);
//		if (prefix == null) {
//			anchorNode = publicUri.substring(publicUri.lastIndexOf('/') + 1);
//			if (anchorNode.endsWith(".owl")) {
//				anchorNode = anchorNode.substring(0, anchorNode.length() - 4);
//			}
//			if (anchorNode.indexOf('.') > 0) {
//				graphName = anchorNode.substring(0, anchorNode.indexOf('.'));
//			}
//			else {
//				graphName = anchorNode;
//			}
//		}
//		String description = "Graph of Ontology File";
//		createGraphFromResultSet(iGraphVisualizer, project, trgtFile, baseFileName, graphName, anchorNode, description, rs);
//	}

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
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	protected String[] getValidTargetFileTypes(){
		String[] types = {"owl", "sadl"};
		return types;
	}

}
