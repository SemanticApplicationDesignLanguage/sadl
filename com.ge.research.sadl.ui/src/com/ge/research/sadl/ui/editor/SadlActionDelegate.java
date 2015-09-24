package com.ge.research.sadl.ui.editor;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.internal.resources.File;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.internal.ObjectPluginAction;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.resource.containers.IAllContainersState;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.builder.ResourceManager;
import com.google.common.collect.Lists;
import com.google.inject.Injector;

@SuppressWarnings("restriction")
public abstract class SadlActionDelegate extends AbstractHandler {
	private static final Logger logger = LoggerFactory.getLogger(SadlActionDelegate.class);

    protected static class RsContainerState extends AdapterImpl implements IAllContainersState {

		private final static String HANDLE = "default";
		private ResourceSet rs;

		public RsContainerState(ResourceSet rs) {
			super();
			this.rs = rs;
		}

		public Collection<URI> getContainedURIs(String containerHandle) {
			List<URI> uris = Lists.newArrayList();
			for (Resource r : rs.getResources())
				uris.add(r.getURI());
			return uris;
		}

		public String getContainerHandle(URI uri) {
			return HANDLE;
		}

		public List<String> getVisibleContainerHandles(String handle) {
			return Collections.singletonList(HANDLE);
		}

		@Override
		public boolean isEmpty(String containerHandle) {
			return getContainedURIs(containerHandle).isEmpty();
		}

	}

	public SadlActionDelegate() {
		super();
		Injector injector = SadlActivator.getInstance().getInjector("com.ge.research.sadl.Sadl");
		injector.injectMembers(this);
	}

	public void run(IAction action) {
		if (action instanceof ObjectPluginAction) {
			ISelection selection = ((ObjectPluginAction)action).getSelection();
			if (selection instanceof TreeSelection) {
				Object element = ((TreeSelection)selection).getFirstElement();
				if (element instanceof File) {
					//TODO there should be a check here to see if the file has unsaved changes and to ask the user if they would
					//	like to save or tell them to save if they want to run tests on modified model
					IPath testFilePath = ((File)element).getFullPath(); //getRawLocation();
					run(testFilePath);
				}
			}
		}
	}

	protected abstract void run(IPath testFilePath);

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow iww = HandlerUtil.getActiveWorkbenchWindow(event);
		IWorkbenchPage iwp = iww.getActivePage();
		IEditorPart iep = iwp.getActiveEditor();
		IEditorInput iei = iep.getEditorInput();
		if (iei instanceof FileEditorInput) {
			IPath editorFile = ((FileEditorInput)iei).getPath();
			URI editorFileUri = URI.createFileURI(editorFile.toPortableString());
			try {
				URI kbaseUri = ResourceManager.getProjectUri(editorFileUri).appendSegment(ResourceManager.OWLDIR);
				String kbaseFolder;
				kbaseFolder = new SadlUtils().fileUrlToFileName(kbaseUri.toString());
//				logger.debug(kbaseFolder + ", " + editorFile);
				int segcnt = editorFile.segmentCount();
				if (segcnt > 2) {
					URI trimmed = kbaseUri.trimSegments(2);
					editorFile = editorFile.makeRelativeTo(new org.eclipse.core.runtime.Path(trimmed.toFileString()));
					run(editorFile);
					return null;
				}
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
		// TODO note failure
		return null;
	}

	protected void prepareModel(SadlModelManager manager, IPath testFilePath, XtextResourceSet rs) {
		URI uri = URI.createPlatformResourceURI(testFilePath.toPortableString(), true);
		rs.eAdapters().add(new RsContainerState(rs));
		Resource resource = rs.getResource(uri, true);
		try {
			resource.unload();
			resource.load(null);
			EObject o = resource.getContents().get(0);
			if (o instanceof Model) {
	        	boolean editorOpen = false;
	        	String resourceName = resource.getURI().lastSegment();
	        	if (PlatformUI.getWorkbench() != null &&
	        			PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null &&
	        			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() != null) {
					IEditorReference[] editors = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences();
					for (int i = 0; editors != null && i < editors.length; i++) {
						IEditorReference editor = editors[i];
						if (editor.getPartName().equals(resourceName)) {
							editorOpen = true;
						}
					}
	        	}
				manager.processModel(((Model)o).eResource(), false, editorOpen, null);
			}
		} catch (IOException e) {
			logger.debug("Error in SADL Action Delegate: " + e.getLocalizedMessage());
			throw new RuntimeException(e);
		}
		catch (Throwable t) {
			t.printStackTrace();
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {
	//		logger.info("selectionChanged called in TestModel");
		}

	public void setActivePart(IAction action, IWorkbenchPart workbenchPart) {
	//		logger.info("setActivePart called in TestModel");
		}

}