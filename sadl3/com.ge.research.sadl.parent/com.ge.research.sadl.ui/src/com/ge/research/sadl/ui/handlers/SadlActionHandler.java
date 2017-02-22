package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;


import com.ge.research.sadl.errorgenerator.generator.SadlErrorMessages;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.ISadlInferenceProcessor;
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.google.inject.Injector;

@SuppressWarnings("restriction")
public abstract class SadlActionHandler extends AbstractHandler {

	private boolean isCanceled = false;
	@Inject
	protected IResourceSetProvider resourceSetProvider;
	@Inject
	protected SadlInferenceProcessorProvider processorProvider;
	@Inject
	protected IPreferenceValuesProvider preferenceProvider;
	
	protected ISadlInferenceProcessor processor;
	
	protected abstract String[] getValidTargetFileTypes();

	protected Object[] getCommandTarget(String[] validTargetTypes) throws TranslationException {
		IProject project = null;
		IPath trgtFolder = null;
		IFile trgtFile = null;
		EObject selectedConcept = null;
		IPath prjFolder;

		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
	    if (window != null)
	    {
	        ISelection selection = (ISelection) window.getSelectionService().getSelection();
	        if (!(selection instanceof IStructuredSelection)) {
	    		IWorkbenchPage page =  window.getActivePage();
	    		IEditorPart editorPart = page.getActiveEditor();
	    		if (editorPart.isDirty()) {
	    		    SadlConsole.writeToConsole(MessageType.ERROR, "Model has unsaved changes. Please save before running tests.\n");
	    		    return null;
	    		}
	    		IEditorInput iei = editorPart.getEditorInput();
	    		if (iei instanceof FileEditorInput) {
	    			trgtFile = ((FileEditorInput)iei).getFile();
	    			trgtFolder = trgtFile.getFullPath().removeLastSegments(1);
	    			IContainer prnt = null;
	    			do {
	    				prnt = prnt == null ? trgtFile.getParent() : prnt.getParent();
	    			} while (prnt != null && !(prnt instanceof IProject));
	    			if (prnt != null) {
	    				project = ((IProject)prnt).getProject();
	    			}
	    			trgtFolder = trgtFile.getFullPath().removeLastSegments(1);
	    		}
	    		else {
	    			System.err.println("No project is selected for action");
		        	return null;
	    		}
	            if (editorPart instanceof XtextEditor) {
	                ISelection sel = editorPart.getEditorSite().getSelectionProvider().getSelection();
	                try {
	    				if (sel instanceof TextSelection) {
	    					int offset = ((TextSelection)sel).getOffset();
	    					int length = ((TextSelection)sel).getLength();
	    		        	String seltxt = ((XtextEditor)editorPart).getDocument().get(offset, length);
	    		        	if (seltxt != null && seltxt.length() > 0) {
	    		        		final TextSelection xtsel = (TextSelection) sel;
	    		                IXtextDocument document = ((XtextEditor)editorPart).getDocument();
	    		                EObject selectedObject = document.readOnly(new IUnitOfWork<EObject, XtextResource>() {            
	    		                    public EObject exec(XtextResource resource) throws Exception {
	    		                      IParseResult parseResult = resource.getParseResult();
	    		                            if (parseResult == null) {
	    		                              return null;
	    		                            }
	    		                            ICompositeNode rootNode = parseResult.getRootNode();
	    		                            int offset = xtsel.getOffset();
	    		                            ILeafNode node = NodeModelUtils.findLeafNodeAtOffset(rootNode, offset);
	    		                            return NodeModelUtils.findActualSemanticObjectFor(node);
	    		                    }
	    		                });	
	    		        		if (selectedObject != null) {
    		    					selectedConcept = selectedObject;
    		    				}
    		    				else {
	    		        			SadlConsole.writeToConsole(MessageType.ERROR, "Unable to find a concept with name '" + seltxt + "'");
    		    				}
	    		        	}
	    				}
	    			} catch (Throwable e) {
	    				// TODO Auto-generated catch block
	    				e.printStackTrace();
	    			}
	            }
	        }
	        else {
		        Object firstElement = ((IStructuredSelection)selection).getFirstElement();
		        if (firstElement instanceof IAdaptable)
		        {
		        	if (firstElement instanceof org.eclipse.core.resources.IFile) {
	        			trgtFile = (IFile) firstElement;
	        			trgtFolder =  ((org.eclipse.core.resources.IFile)firstElement).getParent().getFullPath();	
	        			project = ((org.eclipse.core.resources.IFile)firstElement).getProject();
	        			prjFolder = project.getFullPath();
		        		boolean validType = false;
		        		for (int i = 0; validTargetTypes != null && i < validTargetTypes.length; i++) {
		        			if (((org.eclipse.core.resources.IFile)firstElement).getFileExtension().equals(validTargetTypes[i])) {
		        				validType = true;
		        				break;
		        			}
//		        			else if (validTargetTypes[i].equals("owl")) {
//		        				// a type of owl is also valid for any file that has a derived (same base name) .owl file in the OwlModels folder--return the file
//		        				String owlFileName = convertProjectRelativePathToAbsolutePath(
//		        						project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment()).toPortableString());
//		        				if (new File(owlFileName).exists()) {
//		        					validType = true;
//		        					break;
//		        				}
//		        				else {
//		        					owlFileName = convertProjectRelativePathToAbsolutePath(
//			        						project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().addFileExtension("owl").lastSegment()).toPortableString());
//			        				if (new File(owlFileName).exists()) {
//			        					validType = true;
//			        					break;
//			        				}
//		        				}
//		        			}
		        		}
		        		if (!validType) {
		        			StringBuilder sb = new StringBuilder();
		        			if (validTargetTypes == null) {
		        				sb.append("No valid file types for this command. Select a folder or project");
		        			}
		        			else {
		        				for (int i = 0; i < validTargetTypes.length; i++) {
		        					if (i > 0) sb.append(", ");
			        				sb.append(validTargetTypes[i]);
		        				}
		        			}
		        			throw new TranslationException(SadlErrorMessages.FILE_TYPE_ERROR.get(sb.toString()));
		        		}
		        	}
		        	else if (firstElement instanceof IFolder) {
		        		trgtFolder = ((IFolder)firstElement).getFullPath();
	        			prjFolder = ((IFolder)firstElement).getProject().getFullPath();
		        	}
		        	else if (firstElement instanceof IProject) {
		        		prjFolder = ((IProject)firstElement).getFullPath();
		        		trgtFolder = prjFolder;
		        	}
		        	else {
		        		// project?
		        		project = (IProject)((IAdaptable)firstElement).getAdapter(IProject.class);
			            if (project == null) {
			            	if (firstElement instanceof org.eclipse.core.resources.IFile) {
			            		project = ((org.eclipse.core.resources.IFile)firstElement).getProject();
			            	}
			            	else if (firstElement instanceof IFolder) {
			            		project = ((IFolder)firstElement).getProject();
			            	}
			            	else {
			            		throw new TranslationException("Unable to identify file, folder, or project to translate");
			            	}
			            }
			            IPath path = project.getFullPath();
			            prjFolder = path;
			            trgtFolder = prjFolder;
		        	}
		        }
	        }
	        if (project != null || trgtFolder != null || trgtFile != null) {
	     		Object[] results = new Object[4];
	    		results[0] = project;
	   			results[1] = trgtFolder;
	     		results[2] = trgtFile;
	     		results[3] = selectedConcept;
	    		return results;
	        }
	        else {
	        	return null;
	        }
	    }
	    throw new TranslationException("Nothing selected, unable to process command");
	}
	
	protected IFile getTargetFile(String[] validTargetTypes) throws ExecutionException {
			IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			if (window != null) {
			    ISelection selection = (ISelection) window.getSelectionService().getSelection();
			    if (selection instanceof TextSelection) {
			    	XtextEditor editor = EditorUtils.getActiveXtextEditor();
			    	if (editor != null) {
				    	IEditorInput editorInput = editor.getEditorInput();
				    	if (editorInput instanceof IFileEditorInput) {
				    		return ((IFileEditorInput) editorInput).getFile();
				    	}
			    	}
			    }
			    if (!(selection instanceof IStructuredSelection) || selection.isEmpty()) {
			    	throw new ExecutionException("Nothing is selected for action");
			    }
			    Object firstElement = ((IStructuredSelection)selection).getFirstElement();
			    if (firstElement instanceof IAdaptable)
			    {
			    	if (firstElement instanceof org.eclipse.core.resources.IFile) {
			    		String ext = ((org.eclipse.core.resources.IFile)firstElement).getFileExtension();
			    		boolean validTarget = false;
			    		for (String s : validTargetTypes) {
			    			if (s.equals(ext)) {
			    				validTarget = true;
			    				break;
			    			}
			    		}
			    		if (validTarget) {
			    			return  (IFile) firstElement;	
			    		}
			    		else {
			    			throw new ExecutionException("Only files of type .sadl or .test can be targets for this action");
			    		}
			    	}
			    	else {
			    		throw new ExecutionException("A valid target file must be selected");
			    	}
			    }
			}
			throw new ExecutionException("No project window selected");
		}

	protected Map<String,String> getPreferences() {
		Injector reqInjector = SadlActivator.getInstance()
				.getInjector(SadlActivator.COM_GE_RESEARCH_SADL_SADL);
		IPreferenceValuesProvider pvp = reqInjector.getInstance(IPreferenceValuesProvider.class);
		org.eclipse.emf.ecore.resource.Resource resource = new ResourceImpl();
		resource.setURI(org.eclipse.emf.common.util.URI.createFileURI("/"));
	
		IPreferenceValues preferenceValues = pvp.getPreferenceValues(resource);
		if (preferenceValues != null) {
			Map<String, String> map = new HashMap<String, String>();
			boolean bval = Boolean.parseBoolean(preferenceValues.getPreference(SadlPreferences.SHOW_TIMING_INFORMATION));
			if (bval) {
				map.put(SadlPreferences.SHOW_TIMING_INFORMATION.getId(), "true");
			}
			else {
				map.put(SadlPreferences.SHOW_TIMING_INFORMATION.getId(), "false");
			}
			bval = Boolean.parseBoolean(preferenceValues.getPreference(SadlPreferences.VALIDATE_BEFORE_TEST));
			if (bval) {
				map.put(SadlPreferences.VALIDATE_BEFORE_TEST.getId(), "true");
			}
			else {
				map.put(SadlPreferences.VALIDATE_BEFORE_TEST.getId(), "false");
			}			return map;
		}
		return null;
	}

	public static String convertProjectRelativePathToAbsolutePath(String relPath) {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		IPath path = root.getFile(new Path(relPath)).getLocation();
		String absolutePath = path.toString();
		return absolutePath;
	}

	protected boolean isCanceled() {
		return isCanceled;
	}

	protected void setCanceled(boolean isCanceled) {
		this.isCanceled = isCanceled;
	}

	protected Resource prepareActionHandler(IProject project, IFile trgtFile) throws ExecutionException {	
		XtextResource res = getXtextResource(project, trgtFile);
    	if (res != null) {
    		if (res instanceof XtextResource) {
	    		IResourceValidator validator = res.getResourceServiceProvider().getResourceValidator();
	    		validator.validate(res, CheckMode.FAST_ONLY, CancelIndicator.NullImpl);
				processor = processorProvider.getProcessor(res);
    		}
			return res;
    	}
    	else {
			throw new ExecutionException("Unable to obtain a resource for the target file '" + trgtFile.getName() + "'");
		}
	}
	
	protected XtextResource getXtextResource(IProject project, IFile trgtFile) throws ExecutionException {
		ResourceSet resourceSet = resourceSetProvider.get(project);
		if (resourceSet != null) {
	    	Resource res = resourceSet.getResource(URI.createPlatformResourceURI(trgtFile.getFullPath().toString(), true), true);
	    	if (res != null) {
	    		if (res instanceof XtextResource) {
	    			return (XtextResource) res;
	    		}
	    	}
		}
		else {
			throw new ExecutionException("Unable to obtain a resource set for the target file '" + trgtFile.getName() + "'");
		}
		return null;
	}

	protected IGraphVisualizer getVisualizer(IConfigurationManagerForEditing configMgr) {
		Map<String,String> prefMap = getPreferences();
		String renderClass = prefMap.get(SadlPreferences.GRAPH_RENDERER_CLASS.getId());
		
		List<IGraphVisualizer> visualizers = configMgr.getAvailableGraphRenderers();
	
		if (visualizers != null && visualizers.size() > 0) {
			IGraphVisualizer visualizer = visualizers.get(0);		// replace this by selection and setting preference
			return visualizer;
		}
		return null;
	}

	protected String graphResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String baseFileName, String graphName, String anchorNode,
			String description, ResultSet rs, IGraphVisualizer.Orientation orientation, IWorkbenchPage page) throws IOException {
		if (orientation == null) {
			orientation = IGraphVisualizer.Orientation.TD;
		}
		String tempDir = convertProjectRelativePathToAbsolutePath(getGraphDir(project)); 
		File tmpDirFile = new File(tempDir);
		tmpDirFile.mkdirs();
		iGraphVisualizer.initialize(tempDir, baseFileName, graphName, anchorNode, orientation, description);
		iGraphVisualizer.graphResultSetData(rs);
		String fileToOpen = iGraphVisualizer.getGraphFileToOpen();
		if (fileToOpen != null) {
			File fto = new File(fileToOpen);
			if (fto.isFile()) {
				IFileStore fileStore = EFS.getLocalFileSystem().getStore(fto.toURI());
				try {
					IDE.openEditorOnFileStore(page, fileStore);
				}
				catch (Throwable t) {
					SadlConsole.writeToConsole(MessageType.ERROR, "Error trying to display graph file '" + fileToOpen + "': " + t.getMessage());
				}
			}
			else if (fileToOpen != null) {
				SadlConsole.writeToConsole(MessageType.ERROR, "Failed to open graph file '" + fileToOpen + "'. Try opening it manually.");
			}
		}
		return fileToOpen;
	}

	public static String getGraphDir(IProject project) {
		return project.getFullPath().append("Graphs").toPortableString();
	}
	
	protected void createGraphFromResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String baseFileName, String graphName, String anchorNode,
			String description, ResultSet rs) throws IOException {
		String tempDir = convertProjectRelativePathToAbsolutePath(getGraphDir(project)); 
		File tmpDirFile = new File(tempDir);
		tmpDirFile.mkdirs();
		iGraphVisualizer.initialize(tempDir, baseFileName, graphName, anchorNode, IGraphVisualizer.Orientation.TD, description);
		iGraphVisualizer.graphResultSetData(rs);
	}
	
	protected void resultSetToGraph(IProject project, IFile trgtFile, ResultSet rs, String desc, String baseFileName, Orientation orientation, IWorkbenchPage page)
			throws ConfigurationException, IOException {
				if (rs.getColumnCount() >= 3) {
					String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
					final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
					IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
			
					IGraphVisualizer visualizer = getVisualizer(configMgr);
					if (visualizer != null) {
						graphResultSet(visualizer, project, trgtFile, baseFileName, baseFileName, null, desc, rs, orientation, page);
					}
					else {
						SadlConsole.writeToConsole(MessageType.ERROR, "Unable to find an instance of IGraphVisualizer to render graph for query.\n");
					}
				}
				else {
					SadlConsole.writeToConsole(MessageType.ERROR, "Unable to render graph for query; ResultSet has less than 3 columns.\n");
				}
			}

	public static String getGraphFileNameExtension() {
		return ".svg";
	}
}
