package com.ge.research.sadl.externalmodels.editors;


import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.util.ArrayList;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.swing.JOptionPane;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.MultiPageEditorPart;

/**
 * An example showing how to create a multi-page editor.
 * This example has 3 pages:
 * <ul>
 * <li>page 0 contains a nested text editor.
 * <li>page 1 allows you to change the font used in page 2
 * <li>page 2 shows the words in page 0 in sorted order
 * </ul>
 */
public class UrlListEditor extends MultiPageEditorPart implements IResourceChangeListener{

	/** The text editor used in page 0. */
	private UrlListTextEditor editor;

	/**
	 * Creates a multi-page editor.
	 */
	public UrlListEditor() {
		super();
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
	}
	/**
	 * Creates page 0 of the multi-page editor,
	 * which contains a text editor.
	 */
	void createPage0() {
		try {
			editor = new UrlListTextEditor();
			int index = addPage(editor, getEditorInput());
			setPageText(index, editor.getTitle());
		} catch (PartInitException e) {
			ErrorDialog.openError(
				getSite().getShell(),
				"Error creating nested text editor",
				null,
				e.getStatus());
		}
	}
	/**
	 * Creates page 1 of the multi-page editor,
	 * which allows you to change the font used in page 2.
	 */
	void createPage1() {

		Composite composite = new Composite(getContainer(), SWT.NONE);
		GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		layout.numColumns = 2;

		Button downloadButton = new Button(composite, SWT.NONE);
		GridData gd = new GridData(GridData.BEGINNING);
		gd.horizontalSpan = 2;
		downloadButton.setLayoutData(gd);
		downloadButton.setText("Download External Models");
		
		downloadButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				downloadModels();
			}
		});

		int index = addPage(composite);
		setPageText(index, "Download");
	}
	/**
	 * Creates the pages of the multi-page editor.
	 */
	protected void createPages() {
		createPage0();
		createPage1();
	}
	/**
	 * The <code>MultiPageEditorPart</code> implementation of this 
	 * <code>IWorkbenchPart</code> method disposes all nested editors.
	 * Subclasses may extend.
	 */
	public void dispose() {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
		super.dispose();
	}
	/**
	 * Saves the multi-page editor's document.
	 */
	public void doSave(IProgressMonitor monitor) {
		getEditor(0).doSave(monitor);
	}
	/**
	 * Saves the multi-page editor's document as another file.
	 * Also updates the text for page 0's tab, and updates this multi-page editor's input
	 * to correspond to the nested editor's.
	 */
	public void doSaveAs() {
		IEditorPart editor = getEditor(0);
		editor.doSaveAs();
		setPageText(0, editor.getTitle());
		setInput(editor.getEditorInput());
	}
	/* (non-Javadoc)
	 * Method declared on IEditorPart
	 */
	public void gotoMarker(IMarker marker) {
		setActivePage(0);
		IDE.gotoMarker(getEditor(0), marker);
	} 
	/**
	 * The <code>MultiPageEditorExample</code> implementation of this method
	 * checks that the input is an instance of <code>IFileEditorInput</code>.
	 */
	public void init(IEditorSite site, IEditorInput editorInput)
		throws PartInitException {
		if (!(editorInput instanceof IFileEditorInput))
			throw new PartInitException("Invalid Input: Must be IFileEditorInput");
		super.init(site, editorInput);
	}
	/* (non-Javadoc)
	 * Method declared on IEditorPart.
	 */
	public boolean isSaveAsAllowed() {
		return true;
	}
	/**
	 * Closes all project files on project close.
	 */
	public void resourceChanged(final IResourceChangeEvent event){
		if(event.getType() == IResourceChangeEvent.PRE_CLOSE){
			Display.getDefault().asyncExec(new Runnable(){
				public void run(){
					IWorkbenchPage[] pages = getSite().getWorkbenchWindow().getPages();
					for (int i = 0; i<pages.length; i++){
						if(((FileEditorInput)editor.getEditorInput()).getFile().getProject().equals(event.getResource())){
							IEditorPart editorPart = pages[i].findEditor(editor.getEditorInput());
							pages[i].closeEditor(editorPart,true);
						}
					}
				}            
			});
		}
	}
	/**
	 * Downloads the files to the local folder.
	 */
	void downloadModels() {
		if (editor.isDirty())
		{
			JOptionPane.showMessageDialog( null, "Please save the .url file.","Please save",JOptionPane.OK_OPTION);
		}
		else
		{
			String editorText =
				editor.getDocumentProvider().getDocument(editor.getEditorInput()).get();
	
			StringTokenizer tokenizer =
				new StringTokenizer(editorText, "\n\r");
			ArrayList<String> urls = new ArrayList<String>();
			while (tokenizer.hasMoreTokens()) {
				urls.add(tokenizer.nextToken());
			}
			IFile editorFile = ((FileEditorInput) editor.getEditorInput()).getFile();
			String sFolder = editorFile.getName().substring(0, editorFile.getName().lastIndexOf("."));
			IPath outputPath = (editorFile.getParent().getLocation())
					.append(sFolder);
			
			DeleteRecursive(outputPath.toFile());
			
			for (int i = 0; i < urls.size(); i++) {
				downloadURL((String) urls.get(i), outputPath);
			}
			try {
				((FileEditorInput) editor.getEditorInput()).getFile().getProject().refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	void DeleteRecursive(File fileOrDirectory) {
	    if (fileOrDirectory.isDirectory())
	        for (File child : fileOrDirectory.listFiles())
	            DeleteRecursive(child);

	    fileOrDirectory.delete();
	}
	
	void downloadURL(String urlString, IPath iPath) {
		URL url;
	    InputStream is = null;

	    if (urlString != null && !urlString.isEmpty() && !urlString.startsWith("--")) {
			try {
				Properties p = System.getProperties();
				p.put("http.proxyHost", "http-proxy.ae.ge.com");
				p.put("http.proxyPort", "80");
				p.put("https.proxyHost", "http-proxy.ae.ge.com");
				p.put("https.proxyPort", "80");
				System.setProperties(p);
				url = new URL(urlString);
				is = url.openStream(); // throws an IOException
				ReadableByteChannel rbc = Channels.newChannel(is);
				String urlPath = url.getHost() + url.getPath();

				if (url.getPath() == null || url.getPath().isEmpty())
					urlPath = urlPath + "/" + url.getHost() + ".owl";
				else if (!url.getPath().contains("."))
					urlPath = urlPath + "/" + urlPath.substring(urlPath.lastIndexOf("/") + 1) + ".owl";

				String outputPath = iPath.append(urlPath).toString();
				File file1 = new File(outputPath.substring(0, outputPath.lastIndexOf("/")));
				file1.mkdirs();
				FileOutputStream fos = new FileOutputStream(outputPath);
				fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
				fos.close();

			} catch (MalformedURLException mue) {
				mue.printStackTrace();
			} catch (IOException ioe) {
				ioe.printStackTrace();
			} finally {
				try {
					if (is != null)
						is.close();
				} catch (IOException ioe) {
					// nothing to see here
				}
			} 
		}
	}
}
