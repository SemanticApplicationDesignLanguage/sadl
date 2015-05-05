package com.ge.research.sadl.ui.syntaxcoloring;

import java.io.File;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.IXtextEditorCallback;
import org.eclipse.xtext.ui.editor.XtextEditor;
//import org.eclipse.xtext.ui.editor.IXtextEditorCallback;
//import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightedPositionAcceptor;
import org.eclipse.xtext.ui.editor.syntaxcoloring.ISemanticHighlightingCalculator;
import org.eclipse.xtext.ui.editor.validation.ValidatingEditorCallback;

import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.sadl.ClassDeclaration;
import com.ge.research.sadl.sadl.EnumeratedInstances;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;

// TODO: Revisit after Xtext Upgrade
// see https://www.eclipse.org/forums/index.php/t/827695/
// do not derive from ValidatingEditorCallback after upgrade
public class SadlSemanticHighlightingCalculator extends ValidatingEditorCallback implements
		ISemanticHighlightingCalculator, IPartListener2,
		IResourceChangeListener, IXtextEditorCallback {

	@Override
	public void provideHighlightingFor(XtextResource resource,
			IHighlightedPositionAcceptor acceptor) {
		if (resource==null || resource.getContents().isEmpty()) return;
		
		Model model = (Model) resource.getContents().get(0);
		
		// Highlighting for URI strings
		for (Import imp: model.getImports()) {
			List<INode> nodesForUri = NodeModelUtils.findNodesForFeature(imp, SadlPackage.Literals.IMPORT__IMPORT_URI);
			for (INode node: nodesForUri) {
				acceptor.addPosition(node.getOffset(), node.getLength(), SadlHighlightingConfiguration.URI_ID);
			}
		}
		if (model.getModelName()!=null && model.getModelName().getBaseUri()!=null) {
			for (INode node: NodeModelUtils.findNodesForFeature(model.getModelName(), SadlPackage.Literals.MODEL_NAME__BASE_URI)) {
				acceptor.addPosition(node.getOffset(), node.getLength(), SadlHighlightingConfiguration.URI_ID);
			}
		}
		
		// Highlighting for ResourceNames:
		for (ResourceName rn: EcoreUtil2.getAllContentsOfType(model, ResourceName.class)) {
			ICompositeNode node = NodeModelUtils.findActualNodeFor(rn);
			String highlightingId = getHighlightingId(rn);
			acceptor.addPosition(node.getOffset(), node.getLength(), highlightingId);
		}

		for (ResourceByName rn: EcoreUtil2.getAllContentsOfType(model, ResourceByName.class)) {
			ICompositeNode node = NodeModelUtils.findActualNodeFor(rn);
			String highlightingId = getHighlightingId(rn.getName());
			acceptor.addPosition(node.getOffset(), node.getLength(), highlightingId);
		}
	}

	private String getHighlightingId(ResourceName rn) {
		if (rn.eContainer()==null) {
			// Variables are virtual ResourceNames created during linking and are stored as direct child in the Resource
			return SadlHighlightingConfiguration.VARIABLE_ID;
		}
		switch (rn.eContainer().eClass().getClassifierID()) {
		// TODO: How to identify ANNOTATION PROPERTY?
		case SadlPackage.ADDL_CLASS_INFO: return SadlHighlightingConfiguration.OBJECT_PROPERTY_ID;
		case SadlPackage.PROPERTY_DECLARATION: return SadlHighlightingConfiguration.DATA_PROPERTY_ID; // TODO: Distinguish DATA PROPERTY and OBJECT PROPERTY
		case SadlPackage.INSTANCE_DECLARATION: return SadlHighlightingConfiguration.INSTANCE_ID;
		case SadlPackage.TYPE_DECLARATION: return SadlHighlightingConfiguration.INSTANCE_ID;
		case SadlPackage.CLASS_DECLARATION: return SadlHighlightingConfiguration.CLASS_ID;
		case SadlPackage.NECESSARY_AND_SUFFICIENT: return SadlHighlightingConfiguration.CLASS_ID;
		case SadlPackage.CONSTRUCT_EXPRESSION: return SadlHighlightingConfiguration.VARIABLE_ID;
		case SadlPackage.ORDER_ELEMENT: return SadlHighlightingConfiguration.VARIABLE_ID;
		case SadlPackage.VARIABLE_LIST: return SadlHighlightingConfiguration.VARIABLE_ID;

		default: break;
		}
		
		// not found by direct parent, now try other strategies
		if (EcoreUtil2.getContainerOfType(rn.eContainer(), EnumeratedInstances.class)!=null) {
			return SadlHighlightingConfiguration.INSTANCE_ID;
		}
		if (EcoreUtil2.getContainerOfType(rn.eContainer(), ClassDeclaration.class)!=null) {
			return SadlHighlightingConfiguration.CLASS_ID;
		}
		
		return SadlHighlightingConfiguration.DEFAULT_ID;
	}


	@Override
	public void partActivated(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
//		visitor.editorClosed(partRef.getPartName());
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		int i = 0;
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
//		visitor.removeResourceModel(visitor.getModelResource());
	}

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		// System.out.println("resourceChanged event " + event.getType());
		if (event.getType() == IResourceChangeEvent.PRE_CLOSE
				&& event.getResource() instanceof IProject) {
//			visitor.removeAllProjectResourceModels(event.getResource()
//					.getName());
		}
	}

	@Override
	public void afterSetInput(XtextEditor xtextEditor) {
		super.afterSetInput(xtextEditor);
		try {
			createEditorOpenIndicatorFile(xtextEditor);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void afterCreatePartControl(XtextEditor editor) {
		super.afterCreatePartControl(editor);
		try {
			createEditorOpenIndicatorFile(editor);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void beforeDispose(XtextEditor editor) {
		super.beforeDispose(editor);
		try {
			deleteEditorOpenIndicatorFile(editor);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public synchronized boolean createEditorOpenIndicatorFile(XtextEditor editor)
			throws Exception {
		boolean status = false;
		File indicatorFile = getIndicatorFile(editor);
		if (indicatorFile != null && !indicatorFile.exists()) {
			status = indicatorFile.createNewFile();
		}
		// else {
		// System.out.println("Not creating file '" +
		// indicatorFile.getCanonicalPath() + "' as it already exists.");
		// }
		return status;
	}

	public synchronized boolean deleteEditorOpenIndicatorFile(XtextEditor editor)
			throws Exception {
		boolean status = false;
		File indicatorFile = getIndicatorFile(editor);
		if (indicatorFile != null && indicatorFile.exists()) {
			status = indicatorFile.delete();
		}
		// else {
		// System.out.println("Not deleting file '" +
		// indicatorFile.getCanonicalPath() + "' as it does not exist.");
		// }
		return status;
	}

	private File getIndicatorFile(XtextEditor editor) throws Exception {
		IPath prjLoc = null;
		if (editor.getResource() != null) {
			prjLoc = editor.getResource().getProject().getLocation();
		} else {
			System.out
					.println("getIndicatorFile called with an editor with a null resource. Please report.");
		}
		if (prjLoc != null) {
			IPath tempDir = prjLoc.append(ResourceManager.TEMPDIR);
			File tmpDir = ResourceManager.createFolderIfNotExists(tempDir
					.toOSString());
			File isopenDir = ResourceManager.createFolderIfNotExists(tmpDir
					.getCanonicalPath() + "/isOpen");
			File indFile = new File(isopenDir + "/"
					+ editor.getEditorInput().getName() + ".isOpen");
			return indFile;
		}
		return null;
	}
}
