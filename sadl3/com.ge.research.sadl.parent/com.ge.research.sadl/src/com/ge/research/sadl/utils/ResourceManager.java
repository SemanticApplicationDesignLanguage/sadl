/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
package com.ge.research.sadl.utils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.List;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.EMFPlugin;
import org.eclipse.emf.common.util.URI;
import org.osgi.framework.Bundle;

import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.google.common.base.Predicate;

public class ResourceManager {

    private static final String PLUGIN_ID = "com.ge.research.sadl.ui";

    public static final String OWLDIR = "OwlModels";
	public static final String ACUITY_DEFAULTS_URI = "http://research.ge.com/Acuity/defaults.owl";
	public static final String ACUITY_DEFAULTS_NS = ACUITY_DEFAULTS_URI + "#";
//	public static final String ACUITY_DEFAULTS_PREFIX = "defs";
	public static final String ACUITY_DEFAULTS_OWL_FN = "defaults.owl";
	public static final String ServicesConfigurationConceptsURI = "http://com.ge.research.sadl/sadlserver/Services";
	public static final String ServicesConfigurationConcepts_FN = "SadlServicesConfigurationConcepts.owl";
	public static final String ServicesConf_FN = "ServicesConfig.owl";
	public static final String ServicesConf_SFN = "ServicesConfig.sadl";
	public static final String ServicesConfigurationURI = "http://com.ge.research.sadl/sadlserver/ServicesConfig";
    public static final String OWLFILEEXT = "owl";
    public static final String OWLFILEEXTWITHPREFIX = ".owl";
    public static final String SADLEXT = "sadl";
    public static final String SADLEXTWITHPREFIX = ".sadl";

    private static final Predicate<File> MODEL_FOLDER = (File f) -> f.getAbsolutePath().endsWith(OWLDIR) && f.isDirectory();
    private static final Predicate<URI> IS_SYNTHETIC = (URI uri) -> uri.toString().startsWith("synthetic") || uri.toString().startsWith("__synthetic");

	/**
     * Method to return the absolute path to a bundle resource given its local path
     *
     * @param relPath
     *
     * @return File in the Bundle
     * @throws IOException
     * @throws URISyntaxException
     */
    public static File getAbsoluteBundlePath(String relPath, String file) throws IOException, URISyntaxException {
        String symbolicName = PLUGIN_ID;
        Bundle bndl = Platform.getBundle(symbolicName);
        if (bndl != null) {
            Enumeration<URL> en = bndl.findEntries(relPath, file, true);
            while (en != null && en.hasMoreElements()) {
                URL bndlurl = en.nextElement();
                URL fileUrl = FileLocator.toFileURL(bndlurl);
                if (fileUrl != null) {
                	File bundleFile = new File(fileUrl.getFile());
                    if (bundleFile.exists()) {
                        return bundleFile;
                    }
                }
            }
        }
        else {
            try {
                throw new IOException("SADL bundle not found! Unable to get absolute path from relative path '" + relPath + "'");
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        return null;
    }
    
	/**
	 * Locates and returns with the absolute files system path of the model folder
	 * for the given URI. Checks the current URI, direct descendants and then
	 * traverses up.
	 */
	public static String findModelFolderPath(URI uri) {
		File file = new File(uri.path());
		// Check self.
		if (MODEL_FOLDER.apply(file)) {
			return file.getAbsolutePath();
		}
		// Then direct descendants.
		File[] children = file.listFiles();
		if (children != null) {
			for (File child : children) {
				if (MODEL_FOLDER.apply(child)) {
					return child.getAbsolutePath();
				}
			}
		}
		// Traverse up.
		if (file.getParentFile() != null) {
			return findModelFolderPath(uri.trimSegments(1));
		}
		return null;
	}
	
	/**
	 * Locates and returns with the absolute files system path of the model folder
	 * for the given URI. Checks the current URI, direct descendants and then
	 * traverses up.
	 */
	public static String findModelFolderPath(String path) {
		File file = new File(path);
		// Check self.
		if (MODEL_FOLDER.apply(file)) {
			return file.getAbsolutePath();
		}
		// Then direct descendants.
		File[] children = file.listFiles();
		if (children != null) {
			for (File child : children) {
				if (MODEL_FOLDER.apply(child)) {
					return child.getAbsolutePath();
				}
			}
		}
		// Traverse up.
		if (file.getParentFile() != null) {
			return findModelFolderPath(file.getParent());
		}
		return null;
	}
	
	/**
	 * Returns with the absolute file system path from the EMF URI argument.
	 * Currently, it handles file and platform resource schemes. 
	 */
	public static String toAbsoluteFilePath(URI uri) {
		if (uri.isFile()) {
			return new File(uri.toFileString()).getAbsolutePath();
		} else if (uri.isPlatformResource()) {
			IWorkspaceRoot root = getWorkspaceRoot();
			return root.getFile(new Path(uri.toPlatformString(true))).getLocation().toFile().getAbsolutePath();
		} else if ("synthetic".equals(uri.scheme())) {
			// This is just a sugar for better feedback on tests.
			throw new IllegalArgumentException("URI scheme was 'synthetic'. Are you running tests? Try to use the SadlExternalResourceInjectorProvider.");
		} else {
			throw new IllegalArgumentException("Unexpected URI scheme: " + uri.scheme() + ".");
		}
	}
 
	/**
	 * {@code true} if the URI argument is synthetic (for testing) and the model folder path name argument is {@code null};
	 */
	public static boolean isSyntheticUri(String modelFolderPath, URI uri) {
		return modelFolderPath == null && IS_SYNTHETIC.apply(uri);
	}

	/**
	 * Returns with the EMF URI of the resource given as the absolute FS path string.
	 * <p>
	 * If the Eclipse platform is up and available, then creates a platform resource URI,
	 * otherwise creates an EMF URI with the {@code file} scheme. 
	 */
	public static URI toEmfUri(String absoluteFilePath) {
		if (EMFPlugin.IS_ECLIPSE_RUNNING) {
			File file = new File(absoluteFilePath);
			java.nio.file.Path path = Paths.get(file.toURI());
			java.nio.file.Path relativePath = Paths.get(getWorkspaceRoot().getLocationURI()).relativize(path);
			return URI.createPlatformResourceURI(relativePath.toString(), true);
		} else {
			return URI.createFileURI(absoluteFilePath);
		}
	}

	/**
	 * Returns with the Eclipse workspace root. Assumes a running platform.
	 * If the platform is not available, returns with {@code null}.
	 */
	private static IWorkspaceRoot getWorkspaceRoot() {
		return ResourcesPlugin.getWorkspace().getRoot();			
	}
    
	public static URI getProjectUri(org.eclipse.emf.ecore.resource.Resource someProjectResource ) {
		URI rsrcuri = someProjectResource.getURI();
		URI prjuri = null;
		if (rsrcuri.isPlatform()) {
			prjuri = rsrcuri.trimSegments(rsrcuri.segmentCount() - 2);	// project is second segment
		}
		else{
			//Added to handle automation, not platform
			prjuri = findProjectUriByTrimming(rsrcuri);
		}
		return prjuri;
    }
    
    public static URI findProjectUriByTrimming(URI uri){
    	String fstr = uri.toFileString();
    	if (fstr != null) {
	    	File file = new File(uri.toFileString());
	    	if(file != null){
	    		if(file.isDirectory()){
	    			for(String child : file.list()){
	    				if(child.endsWith(".project")){
	    					return uri;
	    				}
	    			}
	    			//Didn't find a project file in this directory, check parent
	    			if(file.getParentFile() != null){
	    				return findProjectUriByTrimming(uri.trimSegments(1));
	    			}
	    		}
	    		if(file.isFile() && file.getParentFile() != null){
	    			return findProjectUriByTrimming(uri.trimSegments(1));
	    		}
	    	}

    	}
    	
    	return null;
    }
    
	public static URI validateAndReturnOwlUrlOfSadlUri(URI createFileURI) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String getModelNameFromSadlFile(File sadlfile) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String sadlFileNameOfOwlAltUrl(String owlAltUrl, boolean errorIfNotExists) throws MalformedURLException, FileNotFoundException {
    	SadlUtils su = new SadlUtils();
    	File owlfile = new File(su.fileUrlToFileName(owlAltUrl));
    	if (errorIfNotExists && !owlfile.exists()) {
    		throw new FileNotFoundException(owlfile.getPath());
    	}
    	// TODO: Check that the URI is actually SADL derived
		String fn = owlfile.getName();
		int extLength = (fn.length() - fn.indexOf('.')) - 1;
    	return owlfile.getName().substring(0, fn.length() - extLength) + "sadl";
	}

	/**
	 * This method can be used to find a SADL file in a specified directory. The
	 * desire is that the SADL files can be moved around to various folders
	 * as a project is organized without breaking import references.
	 *
	 * @param fileString
	 * @return
	 * @throws MalformedURLException 
	 */
	public static String findSadlFilesInDir(String prjDir, String fileString) throws MalformedURLException {
	   	SadlUtils su = new SadlUtils();
	   	fileString = su.fileUrlToFileName(fileString);
		String foundFile = findFile(prjDir, fileString);
		return foundFile;
	}

	private static String findFile(String folder, String file) {
		String ffn = folder + File.separator + file;
		File fl = new File(ffn);
		if (fl.exists()) {
			return ffn;
		}
		File fldfile = new File(folder);
		if (fldfile.exists() && fldfile.isDirectory()) {
			File[] files = fldfile.listFiles();
			for (int i = 0; i < files.length; i++) {
				File felement = files[i];
				if (felement.isDirectory()) {
					String nextlvlfile = findFile(felement.getAbsolutePath(), file);
					if (nextlvlfile != null) {
						return nextlvlfile;
					}
				}
			}
		}
		return null;
	}

	public static List<File> findSadlFilesInDir(File projectDir) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String getOwlFileExtension(String format) throws TranslationException {
		return SadlSerializationFormat.getFileExtension(SadlSerializationFormat.getRDFFormat(format));
	}

//    /**
//     * Method to determine, from preferences, the OWL file format to use
//     * @return -- the file extension, preceded by a period, specified by the format selected in preferences
//     */
//    public static String getOwlFileExtensionWithPrefix() {
//    	return "." + getOwlFileExtension();
//    }

	public static boolean copyDefaultsFileToOwlModelsDirectory(String defaultsActual) {
		// TODO Auto-generated method stub
		return false;
	}

	public static boolean copyServicesConfigurationFileToOwlModelsDirectory(String servicesConfigurationActual) {
		// TODO Auto-generated method stub
		return false;
	}
    
	/**
	 * This method finds a file in a folder structure
	 * @param folder
	 * @param file
	 * @param excludeFolders
	 * @return
	 */
	public static String findFile(String folder, String file, List<File> excludeFolders) {
		String ffn = folder + File.separator + file;
		File fl = new File(ffn);
		if (fl.exists()) {
			return ffn;
		}
		File fldfile = new File(folder);
		if (fldfile.exists() && fldfile.isDirectory()) {
			File[] files = fldfile.listFiles();
			for (int i = 0; i < files.length; i++) {
				File felement = files[i];
				// don't look for if directory is excluded
				if (felement.isDirectory() && (excludeFolders == null || !excludeFolders.contains(felement))) {
					String nextlvlfile = findFile(felement.getAbsolutePath(), file, excludeFolders);
					if (nextlvlfile != null) {
						return nextlvlfile;
					}
				}
			}
		}
		return null;
	}

}
