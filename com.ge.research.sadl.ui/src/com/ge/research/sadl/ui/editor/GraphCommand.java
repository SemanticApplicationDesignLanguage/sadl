package com.ge.research.sadl.ui.editor;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtext.builder.EclipseResourceFileSystemAccess2;
//import org.eclipse.xtext.generator.IGenerator;
//import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
//import org.eclipse.xtext.ui.editor.model.IXtextDocument;
//import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
//import org.eclipse.jface.text.BadLocationException;
//import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;

import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.ui.SadlConsole;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class GraphCommand extends AbstractHandler implements IHandler {
	     
//    @Inject
//    private IGenerator generator;
 
    @Inject
    private Provider<EclipseResourceFileSystemAccess2> fileAccessProvider;
     
    @Inject
    private SadlModelManager visitor;

    //    @Inject
//    IResourceDescriptions resourceDescriptions;
//     
//    @Inject
//    IResourceSetProvider resourceSetProvider;
//     
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
         
        IEditorPart activeEditor = HandlerUtil.getActiveEditor(event);
        if (activeEditor instanceof XtextEditor) {
            ISelection sel = activeEditor.getEditorSite().getSelectionProvider().getSelection();
			try {
				if (sel instanceof TextSelection) {
					int offset = ((TextSelection)sel).getOffset();
					int length = ((TextSelection)sel).getLength();
		        	String seltxt = ((XtextEditor)activeEditor).getDocument().get(offset, length);
		        	if (seltxt != null && seltxt.length() > 0) {
		        		ConceptName cname = visitor.validateConceptName(visitor.getCurrentResource(), new ConceptName(seltxt));
		        		visitor.graphNeighborhood(cname);
		        	}
				}
			} catch (Throwable e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			SadlConsole.displayMessages(visitor);
        }
        IFile file = (IFile) activeEditor.getEditorInput().getAdapter(IFile.class);
        if (file != null) {
            IProject project = file.getProject();
            IFolder tempFolder = project.getFolder(ResourceManager.TEMPDIR);
            if (!tempFolder.exists()) {
                try {
                    tempFolder.create(true, true,
                            new NullProgressMonitor());
                } catch (CoreException e) {
                    return null;
                }
            }
     
            final EclipseResourceFileSystemAccess2 fsa = fileAccessProvider.get();
            fsa.setOutputPath(tempFolder.getFullPath().toString());
             
             
            if (activeEditor instanceof XtextEditor) {
                ((XtextEditor)activeEditor).getDocument().readOnly(new IUnitOfWork<Boolean, XtextResource>() {
                 
                    @Override
                    public Boolean exec(XtextResource state)
                            throws Exception {
//                        generator.doGenerate(state, fsa);
                        return Boolean.TRUE;
                    }
                });
                 
            }
        }
        return null;
    }
 
    @Override
    public boolean isEnabled() {
        return true;
    }
 
}

