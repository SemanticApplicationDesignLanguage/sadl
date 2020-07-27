package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.common.util.URI;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.visualize.GraphGenerator;
import com.ge.research.sadl.ui.visualize.GraphGenerator.UriStrategy;
import com.ge.research.sadl.ui.visualize.GraphSegment;
import com.ge.research.sadl.ui.visualize.OntologyGraphGenerator;
import com.ge.research.sadl.utils.ResourceManager;

@SuppressWarnings("restriction")
public class OntologyGraphGeneratorHandler extends GraphGeneratorHandler {


	private List<GraphSegment> imports;
	private IGraphVisualizer visualizer;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		Job job = new Job("Ontology Graph Generation"){
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					if (imports != null) imports.clear();
					
					safeWriteToConsole(MessageType.INFO, "-----------------Ontology Graphing Started-------------------\n\n");
					List<String> selection = safeGetCurrentProject();
					if (selection == null || selection.size() < 2) {
						safeWriteToConsole(MessageType.ERROR, "An open project or an ontology file in a project must be selected before graphing can occur.\n");
						return Status.CANCEL_STATUS;
						
					}
					String curProject = selection.get(1);
					project = ResourcesPlugin.getWorkspace().getRoot().getProject(curProject);
					modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
					if (getConfigMgr() != null && getConfigMgr().getModelFolder() != null) {
						String configModelFolder = getConfigMgr().getModelFolderPath().getCanonicalPath().replace("\\", "/");
						if (!configModelFolder.equals(modelFolderUri)) {
							configMgr = null;		// this is an obsolete ConfigurationManager
						}
					}
					String targetFile = null;
					String owlFile = null;
					if (selection.size() > 2) {
						// something else was selected--is it a .sadl file?
						String sf = selection.get(selection.size() - 1);
						if (sf != null && sf.endsWith(".sadl")) {
							owlFile = sf.substring(0, sf.length() - 4) + "owl";
							StringBuilder sb = new StringBuilder(project.getName());
							sb.append("/");
							for (int i = 2; i < selection.size(); i++) {
								if (i > 2) sb.append("/");
								sb.append(selection.get(i));
							}
							targetFile = sb.toString();
						}
						else if (sf != null && (sf.endsWith(".owl") || sf.endsWith(".nt") || sf.endsWith(".n3"))) {
							targetFile = sf;
							owlFile = sf;
						}
					}
					if (targetFile != null) {
						//graph just this file
						generateOntologyFileGraph(targetFile, owlFile, monitor, false, true);
					}
					else {
						graphFolder(project);
						// now graph the base model and the list model
						IResource sbmr = project.findMember("OwlModels/SadlBaseModel.owl");
						if (sbmr != null) {
							graphSpecificModels(sbmr);
						}
						IResource slmr = project.findMember("OwlModels/SadlListModel.owl");
						if (slmr != null) {
							graphSpecificModels(slmr);
						}
						
						//Join all graphing jobs together before visualizing
						Job.getJobManager().join(GRAPH_JOB,monitor);
						
						if (imports.size() > 0) {
							Map<String,String> prefMap = sbmr != null ? getPreferences(URI.createFileURI(sbmr.getFullPath().toPortableString())) : null;
							IGraphVisualizer visualizer = getVisualizer(getConfigMgr(), prefMap);
							GraphGenerator ogg = new OntologyGraphGenerator(getConfigMgr(), visualizer, project);
							IFile trgtFile = null;
							String baseFileName = "Project";
							String graphName = baseFileName;
							String description = "Project " + project.getName() + " import map";
			//				IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, null);
							ResultSet importsRS = ogg.convertDataToResultSet(imports, UriStrategy.LOCALNAME_WITH_URI_TOOLTIP, null);
							if (visualizer != null) {
								graphResultSet(visualizer, project, baseFileName, graphName, null, description, importsRS, null, true);					
							}
							else {
								safeWriteToConsole(MessageType.ERROR, "Unable to find an instance of IGraphVisualizer to render graph for query.\n");
							}
						}
					}
				} catch( ClassCastException e){
					safeWriteToConsole(MessageType.ERROR, "Make sure a folder or file is selected in the desired project\n");
					safeWriteToConsole(MessageType.ERROR, "Error Message: " + e.getMessage() + "\n");
					return Status.CANCEL_STATUS;
				}catch (Exception e) {
					e.printStackTrace();
					safeWriteToConsole(MessageType.ERROR, e.getMessage() + "\n");
					return Status.CANCEL_STATUS;
				}
				finally {
					safeWriteToConsole(MessageType.INFO, "-----------------Ontology Graphing Ended-------------------\n\n");
				}
				try {
					project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
				} catch (CoreException e) {
					e.printStackTrace();
				}
				return Status.OK_STATUS;
			}
		};
		job.schedule();
		
		return event;
	}

	protected void graphSpecificModels(IResource resource) {
		executeGraphingMethod(this,"generateOntologyFileGraph", new Object[]{resource, false});
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
							ontFiles[i].getFullPath().getFileExtension() != null &&
							(ontFiles[i].getFullPath().getFileExtension().equals("sadl") || 
									ontFiles[i].getFullPath().getFileExtension().equals("owl") ||
									ontFiles[i].getFullPath().getFileExtension().equals("nt") ||
									ontFiles[i].getFullPath().getFileExtension().equals("n3"))) {
						executeGraphingMethod(this,"generateOntologyFileGraph", new Object[]{ontFiles[i], true});
//						boolean worked = generateOntologyFileGraph(ontFiles[i], true);
//						if(!worked){
//							SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontFiles[i].getName() + "' does not exist, or does not contain valid data for graphing.\n");	
//						}else{
////							SadlConsole.writeToConsole(MessageType.INFO, "Graph of '" + ontFiles[i].getName() + "' Generated Successfully.\n\n");
//						}
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

	public void generateOntologyFileGraph(IResource ontFile, boolean checkForDerivedFile, IProgressMonitor monitor) throws Exception{
		if(ontFile instanceof IFile){
			String ontfilename = ontFile.getName();
			String owlFileName = null;
			
			safeWriteToConsole(MessageType.INFO, "Generate graph of '" + ontFile.getName() + "' requested.\n");
		
			//Get the associated .owl file for this sadl file
			if (ontFile.getName().endsWith("owl")) {
				if (checkForDerivedFile && isOwlFileOfSadlModel(ontFile)) {
					safeWriteToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontFile.getName() + "' does not exist, or does not contain valid data for graphing.\n");
					return;
				}
				owlFileName = ontFile.getFullPath().lastSegment();
			}
			else {
				owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(ontFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment()).toPortableString());
				if (!(new File(owlFileName).exists())) {
					owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(ontFile.getFullPath().addFileExtension("owl").lastSegment()).toPortableString());
					if (!(new File(owlFileName).exists())) {
						safeWriteToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontFile.getName() + "' does not exist, or does not contain valid data for graphing.\n");
						return;
					}
					else {
						owlFileName = ontFile.getFullPath().addFileExtension("owl").lastSegment();
					}
				}
				else {
					owlFileName = ontFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
				}
			}
			generateOntologyFileGraph(ontfilename, owlFileName, monitor, true, false);
			return;
		}
		safeWriteToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontFile.getName() + "' does not exist, or does not contain valid data for graphing.\n");
	}

	private void generateOntologyFileGraph(String ontfilename, String owlFileName, IProgressMonitor monitor, boolean graphImports, boolean openGraph)
			throws ConfigurationException, URISyntaxException, IOException, Exception {
		Map<String,String> prefMap = getPreferences(URI.createFileURI(ontfilename));
		visualizer = getVisualizer(getConfigMgr(), prefMap);
		if (visualizer != null) {
			String fullFileName = modelFolderUri + "/" + owlFileName;
			String publicUri;
			String prefix = null;
			try {
				publicUri = getConfigMgr().getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(fullFileName));
				prefix = getConfigMgr().getGlobalPrefix(publicUri);
			}
			catch (Exception e) {
				if (owlFileName.equals(SadlConstants.SADL_LIST_MODEL_FILENAME + ".owl")) {
					// not imported--OK
					return;
				}
				else if (owlFileName.equals(SadlConstants.SADL_BASE_MODEL_FILENAME + ".owl")) {
					// don't show
					return;
				}
				else {
					publicUri = new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName);
				}
			}
			
			OntologyGraphGenerator ogg = new OntologyGraphGenerator(getConfigMgr(), visualizer, project, publicUri, monitor, getOntologyGraphPreferences());
			ResultSet oggResults = null;
			try {
				oggResults = ogg.generateOntologyResultSet(null, publicUri, UriStrategy.QNAME_IF_IMPORT);
			} catch (InvalidNameException e) {
				safeWriteToConsole(MessageType.ERROR, e.getMessage());
			}
			if (graphImports) {
				List<GraphSegment> newImports = ogg.getImports(getConfigMgr(), publicUri);
				if (newImports != null && newImports.size() > 0) {
					imports.addAll(newImports);
				}
				else if (oggResults != null){
					GraphSegment simgs = new GraphSegment(null, (prefix != null ? prefix : publicUri), null, null, getConfigMgr());
					simgs.addHeadAttribute("tooltip", "\"" + publicUri + "\"");
					try {
						simgs.addHeadAttribute("URL", ogg.getCurrentFileLink(publicUri));
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					imports.add(simgs);
				}
			}
			
			if (oggResults != null) {
				String baseFileName = ogg.getBaseFilenameFromPublicUri(publicUri);
				String graphName = prefix;
				String anchorNode = null;
				String descripition = "Graph of Ontology File: " + graphName;
				Orientation orientation = null;
				graphResultSet(visualizer, project, baseFileName, graphName, anchorNode, descripition, oggResults, orientation, openGraph);
			}
			else {
				if (!(new File(fullFileName).exists())) {
					safeWriteToConsole(MessageType.ERROR, "file not found. Close and Open project if error persists.\n");
				}
				else {
					safeWriteToConsole(MessageType.WARN, "file '" + fullFileName + "' appears to have no content to graph.\n");
				}
				safeWriteToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + ontfilename + "' does not exist, or does not contain valid data for graphing.\n");
			}
		}
		else {
			safeWriteToConsole(MessageType.ERROR, "Failed to get a visualizer for graphing task.\n");
		}
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
