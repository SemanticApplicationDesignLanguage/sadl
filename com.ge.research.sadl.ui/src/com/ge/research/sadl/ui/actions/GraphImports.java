package com.ge.research.sadl.ui.actions;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IObjectActionDelegate;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.editor.SadlActionDelegate;

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
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
//import org.eclipse.jface.text.BadLocationException;
//import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ModelManager;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.editor.SadlActionDelegate;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.visualize.GraphGenerator.Orientation;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class GraphImports extends SadlActionDelegate implements IObjectActionDelegate {

	private boolean conversionComplete = false;
	
	private String msg = null;
	
	/**
	 * Constructor for Action1.
	 */
	public GraphImports() {
		super();
	}

	@Override
	protected void run(IPath filePath) {
		msg = null;
		conversionComplete = false;
		try {
//			Activator.getInstance().setCursorBusy(true);
			String rsrcFileName = ResourceManager.convertProjectRelativePathToAbsolutePath(filePath.toPortableString());
			URI fileUri = URI.createFileURI(rsrcFileName);
			URI prjUri = ResourceManager.getProjectUri(fileUri);
			String modelFolder = prjUri.appendSegment(ResourceManager.OWLDIR).toString();
			SadlUtils su = new SadlUtils();
			IConfigurationManagerForIDE configMgr = new ConfigurationManagerForIDE(su.fileUrlToFileName(modelFolder), null);
			URI owlUri = ResourceManager.validateAndReturnOwlUrlOfSadlUri(fileUri);
			List<String[]> importList = new ArrayList<String[]>();
			String publicUri = configMgr.getPublicUriFromActualUrl(owlUri.toString());
			String prefix = configMgr.getGlobalPrefix(publicUri);
			
			importList = findImports(importList, configMgr, publicUri, prefix);
			String[][] data = new String[importList.size()][];
			for (int i = 0; i < importList.size(); i++) {
				data[i] = importList.get(i);
			}
			ResultSet rs = new ResultSet(data);
			
//			rs.setShowNamespaces(getShowNamespaces());
			File tmpdir = tempFolderFromModelFolder(su.fileUrlToFileName(modelFolder));
			if (tmpdir != null) {
				String bfn = owlUri.lastSegment();
				File dotfile = ModelManager.constructResultSetToDotFile(rs, tmpdir, bfn, prefix, 
						nodeText(publicUri, prefix), "kbase import graph", Orientation.TD);
				ModelManager.createGraphVizGraph(dotfile.getAbsolutePath());
			}		
		} catch (FileNotFoundException e) {
			addErrorMessage(e.getMessage());
			e.printStackTrace();
		} catch (IOException e) {
			addErrorMessage(e.getMessage());
			e.printStackTrace();
		} catch (Throwable t) {
			addErrorMessage(t.getMessage());
			if (msg == null) {
				addErrorMessage(t.getClass().toString());
			}
			t.printStackTrace();
		}
	}

	private String nodeText(String publicUri, String prefix) {
		return prefix + " (" + publicUri + ")";
	}

	private List<String[]> findImports(List<String[]> importList,
			IConfigurationManagerForIDE configMgr, String parentPublicUri, String parentPrefix) throws ConfigurationException, IOException {
		Map<String,String> map = configMgr.getImports(parentPublicUri);
		if (map != null) {
			Iterator<String> itr = map.keySet().iterator();
			while (itr.hasNext()) {
				String key = itr.next();
				String val = map.get(key);
				String[] row = new String[3];
				row[2] = nodeText(parentPublicUri, parentPrefix);
				row[1] = "importedBy";
				row[0] = nodeText(key, val);
				if (!rowAlreadyInList(importList,row)) {
					importList.add(row);
					importList = findImports(importList, configMgr, key, val);
				}
			}
		}
		return importList;
	}

	private boolean rowAlreadyInList(List<String[]> importList, String[] row) {
		if (row != null) {
			for (int i = 0; importList != null && i < importList.size(); i++) {
				String[] thisrow = importList.get(i);
				if (thisrow.length == row.length) {
					boolean allEqual = true;
					for (int j = 0; j < thisrow.length; j++) {
						if (!thisrow[j].equals(row[j])) {
							allEqual = false;
							break;
						}
					}
					if (allEqual) {
						return true;
					}
				}
			}
		}
		return false;
	}

	private File tempFolderFromModelFolder(String owlModelDir) throws IOException {
		File omd = new File(owlModelDir);
		if (omd != null) {
			File prjdir = omd.getParentFile();
			if (prjdir.exists()) {
				File tmpdir = ResourceManager.createFolderIfNotExists(
						prjdir.getAbsoluteFile()
								+ File.separator
								+ ResourceManager.TEMPDIR);
				return tmpdir;
			}
		}
		return null;
	}
	
	public void addErrorMessage(String _msg) {
		if (msg == null) {
			msg = _msg;
		}
		else {
			msg += "\n" + _msg;
		}
	}
	
	public void setConversionComplete() {
		conversionComplete = true;
	}

}

