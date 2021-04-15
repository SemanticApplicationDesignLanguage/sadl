package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

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
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
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
import org.eclipse.xtext.ui.util.ResourceUtil;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.errorgenerator.generator.SadlErrorMessages;
import com.ge.research.sadl.external.ExternalEmfResource;
import com.ge.research.sadl.ide.handlers.SadlGraphVisualizerHandler;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.ISadlInferenceProcessor;
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.ge.research.sadl.utils.SadlConsole;
import com.google.inject.Inject;
import com.google.inject.Injector;

@SuppressWarnings("restriction")
public abstract class SadlActionHandler extends AbstractHandler {

	private static final Logger LOGGER = LoggerFactory.getLogger(SadlActionHandler.class);
	private boolean isCanceled = false;
	@Inject
	protected IResourceSetProvider resourceSetProvider;
	@Inject
	protected SadlInferenceProcessorProvider processorProvider;
	@Inject
	protected IPreferenceValuesProvider preferenceProvider;
	@Inject
	protected SadlConsole console;
	
	protected ISadlInferenceProcessor processor;
	
	private IGraphVisualizer visualizer = null;
	
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
	    		if (editorPart == null) {
	    		    console.print(MessageType.ERROR, "Invalid selection.\n");
	    		    return null;
	    		}
	    		if (editorPart.isDirty()) {
	    		    console.print(MessageType.ERROR, "Model has unsaved changes. Please save before running tests.\n");
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
    		    					console.error("Unable to find a concept with name '" + seltxt + "'");
    		    				}
	    		        	}
	    				}
	    			} catch (Throwable e) {
	    				LOGGER.error("Ignoring " + e, e);
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
		        		project = (IProject) firstElement;
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
	
	protected Map<String, String> getPreferences(IFile file) {
		final URI uri = URI.createPlatformResourceURI(file.getFullPath().toString(), true);
		return getPreferences(uri);
	}
	
	protected Map<String, String> getPreferences(URI uri) {
		Injector reqInjector = safeGetInjector(SadlActivator.COM_GE_RESEARCH_SADL_SADL);
		IPreferenceValuesProvider pvp = reqInjector.getInstance(IPreferenceValuesProvider.class);
		IPreferenceValues preferenceValues = pvp.getPreferenceValues(new XtextResource(uri));
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
			}
			bval = Boolean.parseBoolean(preferenceValues.getPreference(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS));
			if (bval) {
				map.put(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId(), "true");
			}
			else {
				map.put(SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId(), "false");
			}
			String sval = preferenceValues.getPreference(SadlPreferences.OWL_MODEL_FORMAT);
			if (sval != null) {
				map.put(SadlPreferences.OWL_MODEL_FORMAT.getId(), sval);
			}
			else {
				map.put(SadlPreferences.OWL_MODEL_FORMAT.getId(), "owl");
			}
			return map;
		}
		return null;
	}

	protected final Injector safeGetInjector(String name){
		final AtomicReference<Injector> i = new AtomicReference<Injector>();
		Display.getDefault().syncExec(new Runnable(){
			@Override
			public void run() {
				i.set(SadlActivator.getInstance().getInjector(name));
			}
		});
		
		return i.get();
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
		if (resourceSetProvider == null) {
			throw new ExecutionException("Unable to obtain a resource set for the target file '" + trgtFile.getName() + "'");
		}
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
	
	protected ExternalEmfResource getExternalEmfResource(IProject project, IFile trgtFile) throws ExecutionException {
		ResourceSet resourceSet = resourceSetProvider.get(project);
		if (resourceSet != null) {
	    	Resource res = resourceSet.getResource(URI.createPlatformResourceURI(trgtFile.getFullPath().toString(), true), true);
	    	if (res != null) {
	    		if (res instanceof ExternalEmfResource) {
	    			return (ExternalEmfResource) res;
	    		}
	    	}
		}
		else {
			throw new ExecutionException("Unable to obtain a resource set for the target file '" + trgtFile.getName() + "'");
		}
		return null;
	}

	protected IGraphVisualizer getVisualizer(IConfigurationManagerForEditing configMgr, Map<String,String> prefMap) {
		if (visualizer == null) {
			String renderClass = prefMap.get(SadlPreferences.GRAPH_RENDERER_CLASS.getId());
			List<IGraphVisualizer> visualizers = configMgr.getAvailableGraphRenderers();
					
			if (visualizers != null && visualizers.size() > 0) {
				visualizer = visualizers.get(0);		// replace this by selection and setting preference
				for (IGraphVisualizer igv : visualizers) {
					if (igv.getClass().getCanonicalName().equals(renderClass)) {
						visualizer = igv;
						break;
					}
				}
			}
		}
		return visualizer;	
	}

	protected void graphResultSet(IGraphVisualizer iGraphVisualizer, IProject project, String baseFileName, String graphName, String anchorNode,
			String description, ResultSet rs, IGraphVisualizer.Orientation orientation, boolean openGraph) throws IOException {
		if (orientation == null) {
			orientation = IGraphVisualizer.Orientation.TD;
		}
		final IGraphVisualizer.Orientation innerOrientation = orientation;
		String tempDir = convertProjectRelativePathToAbsolutePath(getGraphDir(project)); 
		File tmpDirFile = new File(tempDir);
		tmpDirFile.mkdirs();
		
		//Perform UI operations from background thread
		Display.getDefault().asyncExec(new Runnable(){
			@Override
			public void run(){
				try {
					iGraphVisualizer.initialize(tempDir, baseFileName, graphName, anchorNode, innerOrientation, description);
					iGraphVisualizer.graphResultSetData(rs);
					if (openGraph) {
						String fileToOpen = iGraphVisualizer.getGraphFileToOpen();
						if (fileToOpen != null) {
							File fto = new File(fileToOpen);
							if (fto.isFile()) {
								IFileStore fileStore = EFS.getLocalFileSystem().getStore(fto.toURI());
								IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
								try {
									IDE.openEditorOnFileStore(page, fileStore);
								}
								catch (Throwable t) {
									if (console != null) {
										console.error("Error trying to display graph file '" + fileToOpen + "': " + t.getMessage());
									}
									else {
										System.err.println(t.getMessage());
									}
								}
							}
							else if (fileToOpen != null) {
								String msg;
								if (fto.exists()) {
									msg = "Failed to open graph file '" + fileToOpen + "'. Try opening it manually.";
								}
								else {
									msg = "Failed to create graph file '" + fileToOpen + "'";
								}
								if (console != null) {
									console.error(msg);
								}
								else {
									System.err.println(msg);
								}
							}
						}
					}
				} catch (IOException e) {
					LOGGER.error("Ignoring " + e, e);
				}
			}
		});
		
	}

	public static String getGraphDir(IProject project) {
		return project.getFullPath().append("Graphs").toPortableString();
	}
	
	public static String getModelFolderFromResource(Resource rsrc) {
		URI uri = rsrc.getURI();
		java.nio.file.Path trgtpath;
		if (uri.isFile()) {
			trgtpath = new File(rsrc.getURI().toFileString()).toPath();
		}
		else {
			IFile trgtfile = ResourceUtil.getFile(rsrc);
			trgtpath = trgtfile.getLocation().toFile().toPath();
		}
		String modelFolderUri = ResourceManager.findModelFolderPath(trgtpath.toFile().getAbsolutePath());
		return modelFolderUri;
	}

	protected void createGraphFromResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String baseFileName, String graphName, String anchorNode,
			String description, ResultSet rs) throws IOException {
		String tempDir = convertProjectRelativePathToAbsolutePath(getGraphDir(project)); 
		File tmpDirFile = new File(tempDir);
		tmpDirFile.mkdirs();
		iGraphVisualizer.initialize(tempDir, baseFileName, graphName, anchorNode, IGraphVisualizer.Orientation.TD, description);
		iGraphVisualizer.graphResultSetData(rs);
	}
	
	protected void resultSetToGraph(IProject project, IFile trgtFile, ResultSet rs, String desc, String baseFileName, 
			Orientation orientation, Map<String,String> prefMap)
			throws ConfigurationException, IOException {
		if (rs.getColumnCount() >= 3) {
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
	
			IGraphVisualizer visualizer = getVisualizer(configMgr, prefMap);
			if (visualizer != null) {
				graphResultSet(visualizer, project, baseFileName, baseFileName, null, desc, rs, orientation, true);
			}
			else {
				console.error("Unable to find an instance of IGraphVisualizer to render graph for query.\n");
			}
		}
		else {
			console.error("Unable to render graph for query; ResultSet has less than 3 columns.\n");
		}
	}
	
	public class SadlEclipseGraphVisualizerHandler implements SadlGraphVisualizerHandler {
		
		private SadlActionHandler uiHandler;
		private IProject project;
		private IFile targetFile;

		public SadlEclipseGraphVisualizerHandler(SadlActionHandler uiHandler, IProject project, IFile targetFile) {
			this.uiHandler = uiHandler;
			this.project = project;
			this.targetFile = targetFile;
		}

		@Override
		public void resultSetToGraph(java.nio.file.Path path, ResultSet resultSet, String description,
				String baseFileName, Orientation orientation, Map<String, String> properties)
				throws ConfigurationException, IOException {

			uiHandler.resultSetToGraph(project, targetFile, resultSet, description, baseFileName, orientation, properties);
			
		}
		
	}

}
