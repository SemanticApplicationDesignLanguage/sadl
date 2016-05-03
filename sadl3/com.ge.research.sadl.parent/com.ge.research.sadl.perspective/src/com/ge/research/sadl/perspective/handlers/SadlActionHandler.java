package com.ge.research.sadl.perspective.handlers;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
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
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;

import com.ge.research.sadl.perspective.util.Util;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.SadlModelProcessorProvider;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Inject;
import com.google.inject.Injector;

public abstract class SadlActionHandler extends AbstractHandler {

	private boolean isCanceled = false;
	@Inject
	protected IResourceSetProvider resourceSetProvider;
	@Inject
	protected SadlModelProcessorProvider processorProvider;
	@Inject
	protected IPreferenceValuesProvider preferenceProvider;
	protected MessageConsoleStream output;
	protected IModelProcessor processor;
	
	public SadlActionHandler() {
		SadlActivator.getInstance().getInjector(SadlActivator.COM_GE_RESEARCH_SADL_SADL).injectMembers(this);
	}

	protected IFile getTargetFile(String[] validTargetTypes) throws ExecutionException {
			IPath prjFolder = null;
			IPath trgtFile = null;
			IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			if (window != null) {
			    ISelection selection = (ISelection) window.getSelectionService().getSelection();
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

	private static String convertProjectRelativePathToAbsolutePath(String relPath) {
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

	protected Resource prepareActionHandler(IFile trgtFile) throws ExecutionException {
		MessageConsole sadlInference = Util.findConsole("SADL Inference");
		output = sadlInference.newMessageStream();
		sadlInference.activate();
		
		ResourceSet resourceSet = resourceSetProvider.get(trgtFile.getProject());
		if (resourceSet != null) {
	    	Resource res = resourceSet.getResource(URI.createPlatformResourceURI(trgtFile.getFullPath().toString(), true), true);
	    	if (res != null) {
	    		IResourceValidator validator = ((XtextResource)res).getResourceServiceProvider().getResourceValidator();
	    		validator.validate(res, CheckMode.FAST_ONLY, CancelIndicator.NullImpl);
				processor = processorProvider.getProcessor(res);
				return res;
	    	}
			throw new ExecutionException("Unable to obtain a resource for the target file '" + trgtFile.getName() + "'");
		}
		throw new ExecutionException("Unable to obtain a resource set for the target file '" + trgtFile.getName() + "'");
	}
	

}
