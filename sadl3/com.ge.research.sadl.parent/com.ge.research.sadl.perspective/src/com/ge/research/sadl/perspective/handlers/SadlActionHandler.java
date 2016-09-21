package com.ge.research.sadl.perspective.handlers;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.internal.resources.Project;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.common.util.URI;
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
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.perspective.util.Util;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.ISadlInferenceProcessor;
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.google.inject.Injector;

public abstract class SadlActionHandler extends AbstractHandler {

	private boolean isCanceled = false;
	@Inject
	protected IResourceSetProvider resourceSetProvider;
	@Inject
	protected SadlInferenceProcessorProvider processorProvider;
	@Inject
	protected IPreferenceValuesProvider preferenceProvider;

	protected ISadlInferenceProcessor processor;
	
	public SadlActionHandler() {
		SadlActivator.getInstance().getInjector(SadlActivator.COM_GE_RESEARCH_SADL_SADL).injectMembers(this);
	}

	protected Object[] getCommandTarget(String[] validTargetTypes) throws TranslationException {
		IProject project = null;
		IPath trgtFolder = null;
		IFile trgtFile = null;
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
	    			IContainer prnt;
	    			do {
	    				prnt = trgtFile.getParent();
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
	        }
	        else {
		        Object firstElement = ((IStructuredSelection)selection).getFirstElement();
		        if (firstElement instanceof IAdaptable)
		        {
		        	if (firstElement instanceof org.eclipse.core.resources.IFile) {
		        		boolean validType = false;
		        		for (int i = 0; validTargetTypes != null && i < validTargetTypes.length; i++) {
		        			if (((org.eclipse.core.resources.IFile)firstElement).getFileExtension().equals(validTargetTypes[i])) {
		        				validType = true;
		        				break;
		        			}
		        		}
		        		if (validType) {
		        			trgtFile = (IFile) firstElement;
		        			trgtFolder =  ((org.eclipse.core.resources.IFile)firstElement).getParent().getFullPath();	
		        			project = ((org.eclipse.core.resources.IFile)firstElement).getProject();
		        			prjFolder = project.getFullPath();
		        		}
		        		else {
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
		        			throw new TranslationException("Only files of type " + sb.toString() + " are valid for this command");
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
//	            System.out.println(path);
//	            IPath owlModelsPath =  prjFolder.append("OwlModels");
//	            String absOwlModelsPath = convertProjectRelativePathToAbsolutePath(owlModelsPath.toString());
//	            File ifPathFile =  new File(absOwlModelsPath);
//	            if (ifPathFile.exists() && !ifPathFile.isDirectory()) {
//	            	throw new TranslationException("Folder '" + absOwlModelsPath + "' exists but is not a directory");
//	            }
//	            if (!ifPathFile.exists()) {
//	            	ifPathFile.mkdirs();
//	            }
	        }
     		Object[] results = new Object[3];
    		results[0] = project;
   			results[1] = trgtFolder;
//        		IPath trgtFilePath = trgtFile.getFullPath();
     		results[2] = trgtFile;
    		return results;
	    }
	    throw new TranslationException("Nothing selected, unable to process command");
	}
	
	protected IFile getTargetFile(String[] validTargetTypes) throws ExecutionException {
			IPath prjFolder = null;
			IPath trgtFile = null;
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
	//	        System.out.println(path);
//			    IPath owlModelsPath =  prjFolder.append("OwlModels");
//			    String absOwlModelsPath = convertProjectRelativePathToAbsolutePath(owlModelsPath.toString());
//			    File ifPathFile =  new File(absOwlModelsPath);
//			    if (ifPathFile.exists() && !ifPathFile.isDirectory()) {
//			    	throw new ExecutionException("Folder '" + absOwlModelsPath + "' exists but is not a directory");
//			    }
//			    if (!ifPathFile.exists()) {
//			    	ifPathFile.mkdirs();
//			    }
//			    File prjFile = ifPathFile.getParentFile();
//				File[] results = new File[2];
//				results[0] = prjFile;
//				results[1] = new File(convertProjectRelativePathToAbsolutePath(trgtFile.toFile().getAbsolutePath()));
//				return results;
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

	protected static String convertProjectRelativePathToAbsolutePath(String relPath) {
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
		
		ResourceSet resourceSet = resourceSetProvider.get(project);
		if (resourceSet != null) {
	    	Resource res = resourceSet.getResource(URI.createPlatformResourceURI(trgtFile.getFullPath().toString(), true), true);
	    	if (res != null) {
	    		if (res instanceof XtextResource) {
		    		IResourceValidator validator = ((XtextResource)res).getResourceServiceProvider().getResourceValidator();
		    		validator.validate(res, CheckMode.FAST_ONLY, CancelIndicator.NullImpl);
					processor = processorProvider.getProcessor(res);
	    		}
				return res;
	    	}
			throw new ExecutionException("Unable to obtain a resource for the target file '" + trgtFile.getName() + "'");
		}
		throw new ExecutionException("Unable to obtain a resource set for the target file '" + trgtFile.getName() + "'");
	}
	

}
