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
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Enumeration;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.util.URI;
import org.osgi.framework.Bundle;

public class ResourceManager {

    private static final String pluginId = "com.ge.research.sadl.ui";

    public static final String OWLDIR = "OwlModels";
	public static final String ACUITY_DEFAULTS_URI = "http://research.ge.com/Acuity/defaults.owl";
	public static final String ACUITY_DEFAULTS_NS = ACUITY_DEFAULTS_URI + "#";
//	public static final String ACUITY_DEFAULTS_PREFIX = "defs";
	public static final String ACUITY_DEFAULTS_OWL_FN = "defaults.owl";
	public static final String ServicesConfigurationURI = "http://com.ge.research.sadl/sadlserver/Services";
	public static final String ServicesConfigurationConcepts_FN = "SadlServicesConfigurationConcepts.owl";
	public static final String ServicesConf_FN = "ServicesConfig.owl";
	public static final String ServicesConf_SFN = "ServicesConfig.sadl";
    public static final String OWLFILEEXT = "owl";
  public static final String OWLFILEEXTWITHPREFIX = ".owl";
    public static final String SADLEXT = "sadl";
    public static final String SADLEXTWITHPREFIX = ".sadl";

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
        String symbolicName = pluginId;
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
    
    public static URI getProjectUri(org.eclipse.emf.ecore.resource.Resource someProjectResource ) {
		URI rsrcuri = someProjectResource.getURI();
		URI prjuri = null;
		if (rsrcuri.isPlatform()) {
			prjuri = rsrcuri.trimSegments(rsrcuri.segmentCount() - 2);
			if (prjuri != null) {
				return prjuri;
			}
		}
//		someProjectResource.getResourceSet().getURIConverter() ???
		return null;
    }

    /**
     * This method takes any actual file URI in the project as input and
     * returns the URI of the project's directory to use as a base part
     * for other file URIs or just to get the name of the project itself.
     */
    //TODO change to pass in Resource
    public static URI getProjectUri(URI someFileInProject) {
   		// Get absolute location of workspace.
    	try {
	   		IWorkspace workspace = ResourcesPlugin.getWorkspace();
	   		// IPath workspaceDirectory = workspace.getRoot().getLocation().makeAbsolute();
	   		// int wsSegCnt = workspaceDirectory.segmentCount();
	   		// Get location of project from file URI; it's one more segment than workspace.
	   		// someFileInProject = convertPlatformUriToAbsoluteUri(someFileInProject);
	   		// int prjUrlSegCnt = someFileInProject.segmentCount();
	   		// URI prjDir = someFileInProject.trimSegments(prjUrlSegCnt - (wsSegCnt + 1));
	   		// new code
	   		if (someFileInProject.isPlatform()) {
		   		String prjName = someFileInProject.segment(1);
		   		IProject prj = workspace.getRoot().getProject(prjName);
		   		URI prjDir = URI.createFileURI(prj.getLocation().toString());
		   		return prjDir;
	   		} else if (someFileInProject.isFile()) {
	   			String s = someFileInProject.toFileString();
	   			IFile file = workspace.getRoot().getFileForLocation(new Path(s));
		   		String prjName = file.getFullPath().segment(0);
		   		IProject prj = workspace.getRoot().getProject(prjName);
		   		URI prjDir = URI.createFileURI(prj.getLocation().toString());
		   		return prjDir;
	   		} else {
	   			String s = someFileInProject.toString();
	   			IFile file = workspace.getRoot().getFileForLocation(new Path(s));
		   		String prjName = file.getFullPath().segment(0);
		   		IProject prj = workspace.getRoot().getProject(prjName);
		   		URI prjDir = URI.createFileURI(prj.getLocation().toString());
		   		return prjDir;
	   		}
    	}
    	catch (IllegalStateException e) {
    		// this is presumably a standalone (testing) situation--take a different approach
    		// get current directory--this should be the project dir if direct testing,
    		//	the ...\target\test-classes\ if Maven install tests
    		File curdir = new File(".");
    		String curdirStr = curdir.getAbsolutePath();
    		if (curdirStr.endsWith("target\\test-classes\\.")) {		// Maven test
    			curdirStr = curdirStr.substring(0, curdirStr.length() - 22);
    		}
    		else if (curdirStr.endsWith(".")) {							// plain JUnit test
    			curdirStr = curdirStr.substring(0, curdirStr.length() - 2);
    		}
    		curdir = new File(curdirStr);
    		if (curdir.exists() && curdir.isDirectory()) {
    			URI prjUri = URI.createFileURI(curdir.getAbsolutePath());
//    			File wkspcdir = curdir.getParentFile();
//    			String prjdirStr = wkspcdir.getAbsolutePath() + File.separator + someFileInProject.segment(1);
//        		URI prjUri = URI.createFileURI(prjdirStr);
        		return prjUri;
    		}
    		throw e;
    	}
    }

	public static URI validateAndReturnOwlUrlOfSadlUri(URI createFileURI) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String getModelNameFromSadlFile(File sadlfile) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String sadlFileNameOfOwlAltUrl(String string, boolean b) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String findSadlFileInProject(String parent, String sadlFileName) {
		// TODO Auto-generated method stub
		return null;
	}

	public static List<File> findSadlFilesInDir(File projectDir) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String getOwlFileExtension() {
		// TODO Auto-generated method stub
		return "owl";	// reasonable default?
	}

    /**
     * Method to determine, from preferences, the OWL file format to use
     * @return -- the file extension, preceded by a period, specified by the format selected in preferences
     */
    public static String getOwlFileExtensionWithPrefix() {
    	return "." + getOwlFileExtension();
    }

	public static boolean copyDefaultsFileToOwlModelsDirectory(String defaultsActual) {
		// TODO Auto-generated method stub
		return false;
	}

	public static boolean copyServicesConfigurationFileToOwlModelsDirectory(String servicesConfigurationActual) {
		// TODO Auto-generated method stub
		return false;
	}
    
}
