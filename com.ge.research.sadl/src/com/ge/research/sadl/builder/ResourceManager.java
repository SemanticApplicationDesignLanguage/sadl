/************************************************************************
 * Copyright ��� 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $
 * $Revision: 1.4 $ Last modified on   $Date: 2014/11/03 19:20:22 $
 ***********************************************************************/

package com.ge.research.sadl.builder;

//import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.channels.IllegalSelectorException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.osgi.framework.Bundle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.utils.SadlUtils;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.rdf.model.impl.Util;
import com.hp.hpl.jena.util.LocationMapper;

public class ResourceManager {
	private static final Logger logger = LoggerFactory.getLogger(ResourceManager.class);

    private static final String pluginId = "com.ge.research.sadl.ui";
    public static final String FILE_SHORT_PREFIX = "file:/";
    public static final String FILE_URL_PREFIX = "file://";
    public static final String FILE_ABS_URL_PREFIX = "file:///";
    public static final String HTTP_URL_PREFIX = "http://";
    public static final String OWLDIR = "OwlModels";
    public static final String TEMPDIR = "Temp";
    public static final String OWLFILEEXT = "owl";
//    public static final String OWLFILEEXTWITHPREFIX = ".owl";
    public static final String SADLEXT = "sadl";
    public static final String SADLEXTWITHPREFIX = ".sadl";

	public static final String ACUITY_DEFAULTS_URI = "http://research.ge.com/Acuity/defaults.owl";
	public static final String ACUITY_DEFAULTS_NS = ACUITY_DEFAULTS_URI + "#";
//	public static final String ACUITY_DEFAULTS_PREFIX = "defs";
	public static final String ACUITY_DEFAULTS_OWL_FN = "defaults.owl";

	public static final String ServicesConfigurationURI = "http://com.ge.research.sadl/sadlserver/Services";
	public static final String ServicesConfigurationConcepts_FN = "SadlServicesConfigurationConcepts.owl";
	public static final String ServicesConf_FN = "ServicesConfig.owl";
	public static final String ServicesConf_SFN = "ServicesConfig.sadl";

	public static final String PLATFORM_RESOURCE_MODELS_RDF_SCHEMA_RDF = "platform:/resource/TestSadlIde/OwlModels/rdf-schema.rdf";
	public static final String PLATFORM_RESOURCE_MODELS_RDF_SYNTAX_NS_RDF = "platform:/resource/TestSadlIde/OwlModels/rdf-syntax-ns.rdf";

    /**
     * Returns the OWL model file for a given SADL file.
     *
     * @param resource A Resource containing the SADL file.
     * @return The OWL model file's path.
     * @throws CoreException If the OWL models directory doesn't exist.
     */
    public static File getOwlFileForSadlResource(Resource resource) throws CoreException {

        // Get the SADL file's corresponding OWL file.
        URI owlFile = validateAndReturnOwlUrlOfResource(resource);
        String s1 = owlFile.toFileString();
        File retFile = new File(s1);
        return retFile;
    }

    /**
     * Given a project Resource [corresponding to a SADL file in this project], find and
     * validate the actual file URL of the corresponding OWL file built from the SADL file.
     *
     * @param resource
     * @return
     * @throws CoreException
     */
    public static URI validateAndReturnOwlUrlOfResource(Resource resource) throws CoreException {

        URI sadlFile = resource.getURI();
        return validateAndReturnOwlUrlOfSadlUri(sadlFile);
    }

    /**
     * Given the file URL of a SADL file in this project, find and
     * validate the actual file URL of the corresponding OWL file built from the SADL file.
     *
     * @param sadlFile
     * @return
     * @throws CoreException
     */
    public static URI validateAndReturnOwlUrlOfSadlUri(URI sadlFile) throws CoreException {
    	logger.info("validateAndReturnOwlUrlOfSadlUri: sadlFile = "+sadlFile.toString());
        if (!SADLEXT.equalsIgnoreCase(sadlFile.fileExtension())) {
            String message = "SADL URI '" + sadlFile + "' must be a SADL file (ending in ." + SADLEXT + ")";
            IStatus status = new Status(IStatus.ERROR, pluginId, message);
            throw new CoreException(status);
        }

        // Get the path of the OWL models directory and check that it exists.
        URI prjDir = null;
        URI owlDir = null;
        try {
	   		IWorkspace workspace = ResourcesPlugin.getWorkspace();
	   		// IPath workspaceDirectory = workspace.getRoot().getLocation().makeAbsolute();
	   		// int wsSegCnt = workspaceDirectory.segmentCount();
	   		// sadlFile = convertPlatformUriToAbsoluteUri(sadlFile);
	   		// int prjUrlSegCnt = sadlFile.segmentCount();
	   		// prjDir = sadlFile.trimSegments(prjUrlSegCnt - (wsSegCnt + 1));
	   		// new code
	   		if (sadlFile.isPlatform()) {
	   			String prjName = sadlFile.segment(1);
	   			IProject prj = workspace.getRoot().getProject(prjName);
	   			IFolder owlFolder = prj.getFolder(OWLDIR);
	   			IPath owlPath = owlFolder.getLocation();
	   			owlDir = URI.createFileURI(owlPath.toString());
	   		} else if (sadlFile.isFile()) {
	   			String s = sadlFile.toFileString();
	   			IFile file = workspace.getRoot().getFileForLocation(new Path(s));
		   		String prjName = file.getFullPath().segment(0);
	   			IProject prj = workspace.getRoot().getProject(prjName);
	   			IFolder owlFolder = prj.getFolder(OWLDIR);
	   			IPath owlPath = owlFolder.getLocation();
	   			owlDir = URI.createFileURI(owlPath.toString());
	   		} else {
	   			String s = sadlFile.toString();
	   			IFile file = workspace.getRoot().getFileForLocation(new Path(s));
		   		String prjName = file.getFullPath().segment(0);
	   			IProject prj = workspace.getRoot().getProject(prjName);
	   			IFolder owlFolder = prj.getFolder(OWLDIR);
	   			IPath owlPath = owlFolder.getLocation();
	   			owlDir = URI.createFileURI(owlPath.toString());
	   		}
        }
        catch (IllegalStateException e) {
        	// for standalone testing
        	prjDir = getProjectUri(sadlFile);
        	File sf = new File(sadlFile.toFileString());
        	File parentFile = sf.getParentFile();
        	while (parentFile.getAbsolutePath().length() >= prjDir.toFileString().length()) {
	       		owlDir = findOwlModelsFolder(parentFile);
	       		if (owlDir != null) {
	       			break;
	       		}
	       		parentFile = parentFile.getParentFile();
        	}
        }
        if (owlDir == null) {
        	throw new CoreException(new Status(IStatus.ERROR, pluginId, "Failed to find an OwlModels folder for resource '" + sadlFile.toFileString() + "'"));
        }
     	// URI owlDir = prjDir.appendSegment(getOwlDirName());
     	String owlDirPath = owlDir.toFileString();
     	if (owlDirPath == null) {
            String message = "The OWL models directory could not be determined from the SADL file URI '" + sadlFile + "'.";
            IStatus status = new Status(IStatus.ERROR, pluginId, message);
            throw new CoreException(status);
     	}
        File dir = new File(owlDirPath);
        if (!dir.exists()) {
            String message = "The OWL models directory '" + dir + "' should exist but does not.";
            IStatus status = new Status(IStatus.ERROR, pluginId, message);
            throw new CoreException(status);
        }
        else if (!dir.isDirectory()) {
            String message = "The OWL models '" + dir + "' exists but is not a directory.";
            IStatus status = new Status(IStatus.ERROR, pluginId, message);
            throw new CoreException(status);
        }

        // Get the SADL file's name and return the corresponding OWL file.
        String name = sadlFile.lastSegment();
        URI owlFile = owlDir.appendSegment(name).trimFileExtension().appendFileExtension(getOwlFileExtension());
        return owlFile;
    }

    private static URI findOwlModelsFolder(File folder) {
    	File[] contents = folder.listFiles();
    	for (int i = 0; contents != null && i < contents.length; i++) {
    		if (contents[i].getName().equals(OWLDIR)) {
    			return URI.createFileURI(contents[i].getAbsolutePath());
    		}
    	}
    	for (int i =0; contents != null && i < contents.length; i++) {
    		URI found = findOwlModelsFolder(contents[i]);
    		if (found != null) {
    			return found;
    		}
    	}
    	return null;
    }

    /**
     * Given the actual file URL of an OWL file, return the corresponding SADL filename,
     * otherwise return the original URL.
     *
     * @param owlFile
     * @return
     */
    public static URI validateAndReturnSadlFileEquivalentOfOwlUrl(URI owlFile) {
    	if (uriInOwlModelsDirectory(owlFile)) {
    		if (getOwlFileExtension().equalsIgnoreCase(owlFile.fileExtension())) {
    			// this is a file ending in .owl
    			URI prjDir = getProjectUri(owlFile);
    			String name = owlFile.trimFileExtension().appendFileExtension(SADLEXT).lastSegment();
    			// now search for file
    			URI sadlFile = findSadlFileInFolder(prjDir, name);
    			if (sadlFile != null) {
    				return sadlFile;
    			}
    		}
    	}
    	return owlFile;
    }

    /**
     * Method to find a SADL file in a given folder by looking in this folder and sub-folders until found or folders exhausted
     *
     * @param folder -- the search folder
     * @param name -- the SADL filename
     * @return -- the SADL file's actual URI in the project
     */
    private static URI findSadlFileInFolder(URI folder, String name) {
		URI sadlFile = folder.appendSegment(name).trimFileExtension().appendFileExtension(SADLEXT);
		File file = new File(sadlFile.toFileString());
		if (file.exists()) {
			return sadlFile;
		}
		if (folder.isFile()) {
			File ff = new File(folder.toFileString());
			File[] files = ff.listFiles();
			for (int i = 0; i < files.length; i++) {
				File subf = files[i];
				if (subf.isDirectory()) {
					URI subfolderUri = folder.appendSegment(subf.getName());
					URI searchUri = findSadlFileInFolder(subfolderUri, name);
					if (searchUri != null) {
						return searchUri;
					}
				}
			}
		}
		return null;
	}

    /**
     * Method to convert an altUrl to an OWL file into the corresponding name of a SADL file (without path)
     *
     * @param owlAltUrl
     * @return
     * @throws MalformedURLException
     */
    public static String sadlFileNameOfOwlAltUrl(String owlAltUrl) throws IOException {
    	SadlUtils su = new SadlUtils();
    	File owlfile = new File(su.fileUrlToFileName(owlAltUrl));
    	if (!owlfile.exists()) {
    		throw new FileNotFoundException(owlfile.getPath());
    	}
    	// TODO: Check that the URI is actually SADL derived
		String fn = owlfile.getName();
		int extLength = (fn.length() - fn.indexOf('.')) - 1;
    	return owlfile.getName().substring(0, fn.length() - extLength) + "sadl";
    }

	/**
     * Check the actual file URL to make sure that it exists as a file
     *
     * @param owlfn
     * @return
     */
	public static boolean validateOwlUri(URI owlfn) {
		File f = new File(owlfn.toFileString());
		if (f.exists()) {
			return true;
		}
		return false;
	}


    /**
     * Method to return the absolute path within the plugin [jar] given the relative path with respect to the current directory.
     *
     * @param relPath Relative path
     *
     * @return Absolute path for resource identified by relPath
     */
    public static String getAbsolutePluginPathFromRelativePath(String relPath) {
        File curdir = new File(".");
        return curdir.getAbsolutePath() + File.separator + relPath;
    }

//    /**
//     * This method converts an OS filename (e.g., "C:\\folder\file.ext")
//     * to a file URL
//     *
//     * @param fileName
//     * @return
//     */
//    public static String fileNameToFileUrl(String fileName) {
//    	if (fileName == null) {
//    		return null;
//    	}
//    	URI fileUri = null;
//        if (fileName.startsWith("http:") || fileName.startsWith("file:")) {
//            fileUri = URI.createURI(fileName);
//        }
//        else {
//            fileUri = URI.createFileURI(fileName);
//        }
//        return fileUri.toString();
//    }
//
//    /**
//     * This method converts the string form of a file URL to an
//     * OS filename
//     *
//     * @param url
//     * @return
//     */
//    public static String fileUrlToFileName(String url) {
//        URI fileUri = URI.createURI(url);
//        if (fileUri.isFile()) {
//            return fileUri.toFileString();
//        }
//        else {
//            return fileUri.toString();
//        }
//    }

    /**
     * Method to copy contents of one file to another
     *
     * @param in File to be copied
     * @param out File to copy to
     *
     * @throws IOException
     */
    public static void copyFile(File in, File out) throws IOException {
        FileInputStream fis  = new FileInputStream(in);
        FileOutputStream fos = new FileOutputStream(out);
        byte[] buf = new byte[1024];
        int i = 0;
        while((i=fis.read(buf))!=-1) {
            fos.write(buf, 0, i);
        }
        fis.close();
        fos.close();
    }

    public static boolean copyDefaultsFileToOwlModelsDirectory(String defaultsFilename) throws IOException, URISyntaxException {
        // copy default definitions file to OWL model directory, then try again
        boolean stat = copyModelFileToTarget(defaultsFilename, false);
        if (stat) {
        	refreshResource(defaultsFilename);
        }
        logger.debug("Attempted to copy defaults file to '" + defaultsFilename + "', status " + stat);

        return stat;
    }

	public static boolean copyServicesConfigurationFileToOwlModelsDirectory(String targetFilename) throws IOException, URISyntaxException {
		boolean stat = false;
		try {
			File scfile = new File(targetFilename);
			if (scfile.exists()) {
				return false;
			}
			stat = copyModelFileToTarget(targetFilename, false);
			if (stat) {
				refreshResource(targetFilename);
			}
		}
		catch (IOException e) {
			// ok to fail if already exists
		}
		logger.debug("Attempted to copy Services file to '" + targetFilename + "', status " + stat);

		return stat;
	}

	 /**
	  * Change the contents of text file in its entirety, overwriting any
	  * existing text.
	  *
	  * This implementation throws all exceptions to the caller.
	  *
	  * @param aFile is an existing file which can be written to.
	  * @param contents is the string to be written
	  * @param writeProtect if true write protect the file
	  *
	  * @throws IOException if problem encountered during write.
	  */
    public static void stringToFile(File aFile, String contents, boolean writeProtect) throws IOException {
    	if (aFile == null) {
    		throw new IllegalArgumentException("File should not be null.");
    	}
    	if (aFile.exists() && !aFile.isFile()) {
    		throw new IllegalArgumentException("Should not be a directory: " + aFile);
    	}
    	if (aFile.exists()) {
    		aFile.delete();
    	}
    	if (!aFile.exists()) {
    		aFile.createNewFile();
    	}
    	if (!aFile.canWrite()) {
    		throw new IllegalArgumentException("File cannot be written: " + aFile);
    	}

    	//declared here only to make visible to finally clause; generic reference
    	Writer output = null;
    	try {
    		//use buffering
    		//FileWriter always assumes default encoding is OK!
    		output = new BufferedWriter( new FileWriter(aFile) );
    		output.write( contents );
    	}
    	finally {
    		//flush and close both "output" and its underlying FileWriter
    		if (output != null) output.close();
    	}
    	if (writeProtect) {
    		try {
    			aFile.setReadOnly();
    		}
    		catch (SecurityException e) {
    			e.printStackTrace();
    		}
    	}
    }

    /**
     * Call this method to copy a minimal ont-policy.rdf mapping file to an OwlModels folder that does not contain one. If one
     * exists the copy will fail.
     *
     * @param owlModelsFolder
     * @return
     * @throws IOException
     * @throws URISyntaxException
     */
    public static boolean copyMinimalPolicyFileToOwlModelsDirectory(String owlModelsFolder) throws IOException, URISyntaxException {
    	File omff = new File(owlModelsFolder);
    	if (omff.exists() && omff.isDirectory()) {
    		String mappingFilename = omff.getAbsolutePath() + File.separator + IConfigurationManager.ONT_POLICY_RDF;
        	boolean otherStat = copyModelFileToTarget(mappingFilename, false);
        	if (otherStat) {
        		refreshResource(mappingFilename);
        		logger.debug("Copied minimal mapping file to '" + mappingFilename + "'");
        		return true;
        	}
    	}
		logger.error("Failed to copy minimal mapping file to folder '" + owlModelsFolder + "'");
		return false;
    }

    /**
     * Call this method to copy a minimal configuration.rdf file to an OwlModels folder that does not contain one. If one exists
     * the copy will fail.
     *
     * @param owlModelsFolder
     * @return
     * @throws IOException
     * @throws URISyntaxException
     */
    public static boolean copyMinimalConfigurationFileToOwlModelsDirectory(String owlModelsFolder) throws IOException, URISyntaxException {
    	File omff = new File(owlModelsFolder);
    	if (omff.exists() && omff.isDirectory()) {
    		String configFilename = omff.getAbsolutePath() + File.separator + IConfigurationManager.CONFIG_FILENAME;
        	boolean otherStat = copyModelFileToTarget(configFilename, false);
        	if (otherStat) {
        		refreshResource(configFilename);
        		logger.debug("Copied minimal configuration file to '" + configFilename + "'");
        		return true;
        	}
    	}
		logger.error("Failed to copy minimal configuration file to folder '" + owlModelsFolder + "'");
		return false;
    }

    /**
     * Call this method to make sure the OwlModels folder is correctly set up with everything it needs
     * @return
     * @throws URISyntaxException
     * @throws IOException
     * @throws ConfigurationException
     */
    public static boolean validateOwlModelsFolder(String owlModelsFolder) throws IOException, URISyntaxException, ConfigurationException {
		File mfpnf = new File(owlModelsFolder);
		if (!mfpnf.exists()) {
			boolean stat = mfpnf.mkdir();
			if (!stat) {
				throw new ConfigurationException("Unable to create model folder '" + owlModelsFolder + "'");
			}
		}
		if (!mfpnf.isDirectory()) {
			throw new ConfigurationException("'" + owlModelsFolder + "' is not a directory.");
		}
		String mappingFilename = owlModelsFolder + File.separator + IConfigurationManager.ONT_POLICY_RDF;
		File mappingFile = new File(mappingFilename);
		if (!mappingFile.exists()) {
			// new mapping model--initialize content
			ResourceManager.copyMinimalPolicyFileToOwlModelsDirectory(owlModelsFolder);
			return true;
		}
		return false;
    }

    public static void refreshResource(String uri) {
    	try {
			IPath location = Path.fromOSString(uri);
			IWorkspaceRoot myWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			java.io.File jfile = new File(uri);
			if (jfile.isDirectory()) {
				IFolder fldr = myWorkspaceRoot.getFolder(location);
				try {
					fldr.refreshLocal(IResource.DEPTH_INFINITE, null);
				} catch (CoreException e) {
					logger.error(e.getMessage());
					e.printStackTrace();
				}
			}
			else {
				IResource rsrc = myWorkspaceRoot.getFileForLocation(location);
				try {
					if (rsrc != null) {
						rsrc.refreshLocal(IResource.DEPTH_INFINITE, null);
					}
				} catch (CoreException e) {
					logger.error(e.getMessage());
					e.printStackTrace();
				}
			}
    	}
    	catch (Throwable t) {
    		// this will happen if in test environment
    	}
	}

//    private static void convertV1FilesToV2(IPath iPath) {
//    	File prjfldr = iPath.toFile();
//    	convertV1FilesToV2(prjfldr);
//
//	}

    /**
     * Call this method to convert a SADL V1 project to a SADL V2 project
     * @param prjfldr
     */
	public static void convertV1FilesToV2(File prjfldr) {
//		List<File> sadlFiles = findSadlFilesInDir(prjfldr);
//        for (File sf : sadlFiles) {
//        	try {
//            	if (sf != null && sf.exists() && sf.isFile()) {
//            		verifyWhitespaceAtEnd(sf);
//            	}
//            }
//        	catch (Exception e) {
//        		e.printStackTrace();
//        	}
//
//        }
	}

//	private static void verifyWhitespaceAtEnd(File file) {
//		FileInputStream fis = null;
//		BufferedInputStream bis = null;
//		DataInputStream dis = null;
//		boolean changed = false;
//
//		try {
//			fis = new FileInputStream(file);
//
//			// Here BufferedInputStream is added for fast reading.
//			bis = new BufferedInputStream(fis);
//			dis = new DataInputStream(bis);
//			if (fis.available() > 8) {
//				fis.skip(fis.available() - 8);
//				byte[] endBytes = new byte[10];
//				int cnt = fis.read(endBytes);
//
//				if (cnt > 0) {
//					String endingtext = new String(endBytes);
//					int period = endingtext.length() - 1;
//					while (period >= 0 && endingtext.charAt(period) != '.') {
//						period--;
//					}
//					if (period > 0) {
//						endingtext = endingtext.substring(period);
//					}
//					boolean endsWithWhitespace = false;
//					for (int i = 0; i < endingtext.length(); i++) {
//						if (Character.isWhitespace(endingtext.charAt(i))) {
//							endsWithWhitespace = true;
//							break;
//						}
//					}
//					if (!endsWithWhitespace) {
//						BufferedWriter bw = null;
//						try {
//							System.out.println("No whitespace found at end of '" + file.getName() + "':" + endingtext);
//							FileWriter foutstream = new FileWriter(file, true);
//							bw = new BufferedWriter(foutstream);
//							bw.newLine();
//							bw.flush();
//							changed = true;
//						}
//						finally {
//							if (bw != null) {
//								bw.close();
//							}
//						}
//					}
//				}
//			}
//
//			// dispose all the resources after using them.
//			fis.close();
//			bis.close();
//			dis.close();
//
//			if (changed) {
//				refreshResource(file.getAbsolutePath());
//			}
//
//		} catch (FileNotFoundException e) {
//			e.printStackTrace();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//	}
//
	/**
     * Method to find all of the .sadl files in the specified directory and its subdirectories recursively
     *
     * @param folder
     * @return
     */
	public static List<File> findSadlFilesInDir(File folder) {
    	return findFilesInDirWithExtension(new ArrayList<File>(), folder, SADLEXTWITHPREFIX);
    }

	public static List<File> findFilesInDirWithExtension(File folder, String ext) {
		return findFilesInDirWithExtension(new ArrayList<File>(), folder, ext);
	}

	/**
	 * Helper method to be used recursively to find all of the .sadl files in a directory
	 *
	 * @param list -- the current list of .sadl files
	 * @param folder -- the directly currently being searched
	 *
	 * @return -- the list of .sadl files found so far
	 */
    public static List<File> findFilesInDirWithExtension(List<File> list, File folder, String ext) {
		File[] contents = folder.listFiles();
		for (int i = 0; contents != null && i < contents.length; i++) {
			if (contents[i].isDirectory()) {
				list = findFilesInDirWithExtension(list, contents[i], ext);
			}
			else if (contents[i].getName().endsWith(ext)) {
				list.add(contents[i]);
			}
		}
		return list;
	}
    
    public static IProject getProject(URI someFileInProject) {
   		// Get absolute location of workspace.
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
	   		return prj;
   		} else if (someFileInProject.isFile()) {
   			String s = someFileInProject.toFileString();
   			IFile file = workspace.getRoot().getFileForLocation(new Path(s));
	   		String prjName = file.getFullPath().segment(0);
	   		IProject prj = workspace.getRoot().getProject(prjName);
	   		return prj;
   		} else {
   			String s = someFileInProject.toString();
   			IFile file = workspace.getRoot().getFileForLocation(new Path(s));
	   		String prjName = file.getFullPath().segment(0);
	   		IProject prj = workspace.getRoot().getProject(prjName);
	   		return prj;
   		}
    }

    /**
     * This method takes any actual file URI in the project as input and
     * returns the URI of the project's directory to use as a base part
     * for other file URIs or just to get the name of the project itself.
     */
    public static URI getProjectUri(URI someFileInProject) {
    	if (someFileInProject == null) {
    		throw new NullPointerException("Method called with null value for argument 'someFileInProject'");
    	}
   		// Get absolute location of workspace.
    	try {
    		IProject prj = getProject(someFileInProject);
    		if (prj != null) {
    			URI prjDir = URI.createFileURI(prj.getLocation().toString());
		   		return prjDir;
    		}
    		throw new IllegalSelectorException();
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

    /**
     * This method takes any actual file URL in the project as input and returns the String representation of
     * the projects policy file (ont-policy.rdf) URL as a string.
     *
     * @param someProjectUrl
     * @return
     * @throws URISyntaxException
     * @throws IOException
     */
    public static String getValidatedPolicyFileUrlForProject(URI someProjectUrl) throws IOException, URISyntaxException {
   		URI policyFileUrl = getPolicyFileUrlForProject(someProjectUrl);
        File file = new File(policyFileUrl.toFileString());
		if (file.exists()) {
			return policyFileUrl.toString();
		}
		copyMinimalPolicyFileToOwlModelsDirectory(policyFileUrl.toFileString());
		if (file.exists()) {
			return policyFileUrl.toFileString();
		}
    	return null;
    }

	public static URI getPolicyFileUrlForProject(URI someProjectUrl) {
		URI prjDir = getProjectUri(someProjectUrl);
     	URI owlDir = prjDir.appendSegment(getOwlDirName());
    	URI policyFileUrl = owlDir.appendSegment(IConfigurationManager.ONT_POLICY_RDF);
		return policyFileUrl;
	}

    public static String getPolicyFileUrlForModel(String someModelFile) throws IOException {
    	SadlUtils su = new SadlUtils();
    	File mfile = new File(su.fileUrlToFileName(someModelFile));
    	File modeldir = mfile.getParentFile();
    	return modeldir.getCanonicalPath() + File.separator + IConfigurationManager.ONT_POLICY_RDF;
    }

    public static String getOwlModelsFolder(URI someModelUri) throws MalformedURLException {
    	URI prjUri = getProjectUri(someModelUri);
    	String existingOwlModelsFolder = findFolderInFolder(prjUri.toString(), getOwlDirName());
    	SadlUtils su = new SadlUtils();
    	if (existingOwlModelsFolder == null) {
    		// this is ok; it might not exist yet in a new project
    		return su.fileUrlToFileName(prjUri.appendSegment(getOwlDirName()).toString());
    	}
		return su.fileUrlToFileName(existingOwlModelsFolder);
    }

    private static String findFolderInFolder(String containingFolder, String findFolder) throws MalformedURLException {
    	SadlUtils su = new SadlUtils();
    	File someModelFolder = new File(su.fileUrlToFileName(containingFolder));
    	File[] contents = someModelFolder.listFiles();
    	for (int i = 0; contents != null && i < contents.length; i++) {
    		File thisfile = contents[i];
    		if (thisfile.getName().equals(findFolder)) {
    			return thisfile.getAbsolutePath();
    		}
    		else if (thisfile.isDirectory()) {
    			String subsearchresult = findFolderInFolder(thisfile.getAbsolutePath(), findFolder);
    			if (subsearchresult != null) {
    				return subsearchresult;
    			}
    		}
    	}
    	return null;
    }

    /**
     * Method to determine if an input URL is in this project's OwlModels directory.
     * Note that this method does NOT check existence but works entirely from the
     * URL path versus the path to the OwlModels directory.
     *
     * @param owlFile
     * @return
     */
	public static boolean uriInOwlModelsDirectory(URI owlFile) {
   		//get object which represents the workspace
   		IWorkspace workspace = ResourcesPlugin.getWorkspace();
   		//get location of workspace (java.io.File)
   		IPath workspaceDirectory = workspace.getRoot().getLocation().makeAbsolute();
   		int wsSegCnt = workspaceDirectory.segmentCount();
   		owlFile = convertPlatformUriToAbsoluteUri(owlFile);
   		if (owlFile.isFile()) {
	   		int prjUrlSegCnt = owlFile.segmentCount();
	   		URI prjDir = owlFile.trimSegments(prjUrlSegCnt - (wsSegCnt + 1));
	     	URI owlDir = prjDir.appendSegment(getOwlDirName());
	   		if (owlFile.toFileString().startsWith(owlDir.toFileString())) {
	   			return true;
	   		}
   		}
		return false;
	}

    /**
     * Return the absolute path for a named file sibling to the absolute path of an input file
     * @param namedFilePath
     * @param siblingFilename
     * @return
     */
    public static String getAbsolutePathFromSiblingFile(String namedFilePath, String siblingFilename) {
        File namedFile = new File(namedFilePath);
        return namedFile.getParent() + File.separator + siblingFilename;
    }

    private static boolean copyModelFileToTarget(String targetFN, boolean bOverwriteOK) throws IOException, URISyntaxException {
        File target = new File(targetFN);
        if (target.exists() && !bOverwriteOK) {
            throw new IOException("File '" + targetFN + "' already exists and overwrite not authorized.");
        }
        File source = getBundleModelFile(target.getName());
        if (source != null) {
            copyFile(source, target);
            return true;
        }
        throw new IOException("Unable to find model file '" + target.getName() + "' in bundle.");
    }
    
    public static File getBundleModelFile(String bundleFileName) throws IOException, URISyntaxException {
        File source = getAbsoluteBundlePath("Models", bundleFileName);
        return source;
    }

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

    /**
     * Method to get the name of the directory in the project that contains the
     * OWL and rule files created from the SADL models.
     *
     * @return -- the OWL files directory name
     */
    private static String getOwlDirName() {
        return OWLDIR;
    }

	public static URI convertPlatformUriToAbsoluteUri(URI uri) {
   		//get object which represents the workspace
		if (uri != null && uri.isPlatform()) {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
	   		//get location of workspace
	   		// IPath workspaceDirectory = workspace.getRoot().getLocation();
	   		// String s = uri.toPlatformString(true);
	   		// IPath absPath = workspaceDirectory.append(s);
	    	// uri = URI.createFileURI(absPath.toPortableString());
	    	// new code
	    	IWorkspaceRoot root = workspace.getRoot();
	   		String s = uri.toPlatformString(true);
	    	IPath path = root.getFile(new Path(s)).getLocation();
	    	URI absoluteUri = URI.createFileURI(path.toString());
			return absoluteUri;
		}
		return uri;
	}

	/**
	 * Convert a path relative to the current workspace project directory to an absolute path
	 *
	 * @param relPath
	 * @return
	 */
	public static String convertProjectRelativePathToAbsolutePath(String relPath) {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
   		//get location of workspace
   		// IPath workspaceDirectory = workspace.getRoot().getLocation();
   		// IPath absPath = workspaceDirectory.append(relPath);
   		// return absPath.toPortableString();
    	// new code
    	IWorkspaceRoot root = workspace.getRoot();
    	IPath path = root.getFile(new Path(relPath)).getLocation();
    	String absolutePath = path.toString();
		return absolutePath;
	}

	/**
	 * Convert a path relative to the current workspace project directory to an absolute path
	 *
	 * @param relPath
	 * @return
	 */
	public static String convertAbsolutePathToProjectRelativePath(String absPath) {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
   		//get location of workspace
   		IPath workspaceDirectory = workspace.getRoot().getLocation();
   		File absFile = new File(absPath);
   		File parent = absFile.getParentFile();
   		File lastParent = null;
   		String relPath = null;
   		while (parent != null) {
   			if (parent.equals(workspaceDirectory.toFile())) {
   				if (lastParent != null) {
   					relPath = absPath.substring(lastParent.getAbsolutePath().length() + 1);
   				}
   				else {
   					relPath = absPath.substring(parent.getAbsolutePath().length() + 1);
   				}
   				break;
   			}
   			lastParent = parent;
   			parent = parent.getParentFile();
   		}
//   		workspaceDirectory.lastSegment();
//   		URI uri = URI.createURI(absPath);
//   		String[] segs = uri.segments();
//   		int wssegnum = 0;
//   		String relPath = null;
//   		for (int i = 0; i < segs.length; i++) {
//   			if (segs[i].equals(workspaceDirectory.lastSegment())) {
//   				wssegnum = i;
//   			}
//   			else if (i > wssegnum) {
//   				if (relPath == null) {
//   					relPath = segs[i];
//   				}
//   				else {
//   					relPath = relPath + "/" + segs[i];
//   				}
//   			}
//   		}
   		return relPath;
	}

	/**
	 * Reverses the effect of convertPlatformUriToAbsoluteUri.
	 *
	 * @param uri Absolute uri
	 * @return Platform uri or same absolute uri if absolute uri
	 * can't be found within the workspace.
	 */
	public static URI convertAbsoluteUriToPlatformUri(URI uri) {
   		//get object which represents the workspace
		if (uri != null && !uri.isPlatform() && uri.hasAbsolutePath()) {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
	   		//get location of workspace
	   		String workspaceDirectory = workspace.getRoot().getLocation().toOSString();
	   		String filename = uri.toFileString();
	   		if (filename.startsWith(workspaceDirectory)) {
	   			String pathName = filename.substring(workspaceDirectory.length());
	   			String fragment = uri.fragment();
	   			uri = URI.createPlatformResourceURI(pathName, true);
	   			uri = uri.appendFragment(fragment);
	   		}
		}
		return uri;
	}

//	/**
//	 * Given an actual URL in this project, check to see if there is
//	 * a policy file and add it to the OntDocumentManager
//	 *
//	 * @param actualUri
//	 * @throws URISyntaxException
//	 * @throws IOException
//	 */
//	public static void checkForPolicyFile(URI actualUri) throws IOException, URISyntaxException {
//		String policyFileUrl = getValidatedPolicyFileUrlForProject(actualUri);
//		if (policyFileUrl != null) {
//			String mdsp = OntDocumentManager.getInstance().getMetadataSearchPath();
//			if (mdsp == null || !mdsp.contains(policyFileUrl)) {
//				OntDocumentManager.getInstance().setMetadataSearchPath(policyFileUrl, true);
//			}
//		}
//	}

	/**
	 * Given a URI, return the local name part of the URI
	 * (Not that this is modeled after the Jena Node_URI method of the same name.)
	 *
	 * @param uri
	 * @return
	 */
	public static String getLocalName(String uri) {
        return uri.substring( Util.splitNamespace( uri ) );
	}

	/**
	 * Given a URI, return the namespace part of the URI
	 * (Not that this is modeled after the Jena Node_URI method of the same name.)
	 *
	 * @param uri
	 * @return
	 */
	public static String getNameSpace(String uri) {
        return uri.substring( 0, Util.splitNamespace( uri ) );
	}

	/**
	 * This method can be used to find a SADL file in a SADL project. The
	 * desire is that the SADL files can be moved around to various folders
	 * as a project is organized without breaking import references.
	 *
	 * @param fileString
	 * @return
	 */
	public static String findSadlFileInProject(String prjDir, String fileString) {
		if (fileString.startsWith(ResourceManager.FILE_SHORT_PREFIX)) {
			fileString = fileString.substring(ResourceManager.FILE_SHORT_PREFIX.length());
		}
		else if (fileString.startsWith(ResourceManager.FILE_URL_PREFIX)) {
			fileString = fileString.substring(ResourceManager.FILE_URL_PREFIX.length());
		}
		else if (fileString.startsWith(ResourceManager.FILE_ABS_URL_PREFIX)) {
			fileString = fileString.substring(ResourceManager.FILE_ABS_URL_PREFIX.length());
		}
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

	/**
	 * Method to find all known imports for a project. These include
	 * 	1) SADL files
	 * 	2) OWL files which are not associated with a SADL file
	 * 	3) Listings in the ont-policy file which are not one of the above
	 * (Can't all of these just be gotten from the policy file?)
	 * @param someFileInProject
	 * @return
	 * @throws URISyntaxException
	 * @throws IOException
	 */
	public static List<String> findAllKnownImports(URI someFileInProject) throws IOException, URISyntaxException {
		String policyFileUrl = getValidatedPolicyFileUrlForProject(someFileInProject);
    	if (policyFileUrl != null) {
	        OntDocumentManager mgr = OntDocumentManager.getInstance(); // new OntDocumentManager(policyFileUrl);
	        if (!mgr.getMetadataSearchPath().contains(policyFileUrl)) {
	        	mgr.setMetadataSearchPath(policyFileUrl, true);
	        }
	        LocationMapper mapper = mgr.getFileManager().getLocationMapper();
	        Iterator<String> itr = mapper.listAltEntries();
	        if (itr.hasNext()) {
	        	List<String> publicUris = new ArrayList<String>();
		        while (itr.hasNext()) {
		        	String altEntry = itr.next();
		        	String mapping = mapper.altMapping(altEntry);
		        	logger.debug("Location Mapper AltEntry: " + altEntry + ", " + mapping);
		        	publicUris.add(altEntry);
		        }
		        return publicUris;
	        }
		}
		return null;
	}

	public static String getModelNameFromSadlFile(File sadlfile) throws IOException {
		if (sadlfile.exists()) {
			String modelname = null;
			BufferedReader in = new BufferedReader(new FileReader(sadlfile));
			String line;
			do {
				line = in.readLine();
				if (line != null && line.startsWith("uri")) {
					int srt = line.indexOf('\"');
					int end = line.indexOf('\"', srt + 1);
					if (srt > 0 && end > srt) {
						modelname = line.substring(srt + 1, end);
					}
				}
			} while (modelname == null && line != null);
			in.close();
			return modelname;
		}
		return null;
	}

	public static String validateHTTP_URI(String uri) throws java.net.MalformedURLException {
	    final URL url;
	    try {
	        url = new URL(uri);
	    }
	    catch (Exception e1) {
	    	throw new java.net.MalformedURLException("'" + uri + "' is not a valid URL: " + e1.getLocalizedMessage());
	    }
	    if (!"http".equals(url.getProtocol())) {
	    	throw new java.net.MalformedURLException("'" + uri + "' is not a valid URL: Model name must use http protocol.");
	    }
	    return uri;
	}

    /**
     * Method to determine, from preferences, the OWL file format to use
     * @return -- the file extension specified by the format selected in preferences
     */
    public static String getOwlFileExtension() {
    	IPreferencesService service = Platform.getPreferencesService();
    	String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", IConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
    	if (format.equals(IConfigurationManager.RDF_XML_ABBREV_FORMAT) || format.equals(IConfigurationManager.RDF_XML_FORMAT)) {
    		return "owl";
    	}
    	else if (format.equals(IConfigurationManager.N3_FORMAT))	{
    		return "n3";
    	}
    	else if (format.equals(IConfigurationManager.N_TRIPLE_FORMAT)) {
    		return "nt";
    	}
    	else {
    		return "owl";	// reasonable default?
    	}
    }

    /**
     * Method to determine, from preferences, the OWL file format to use
     * @return -- the file extension, preceded by a period, specified by the format selected in preferences
     */
    public static String getOwlFileExtensionWithPrefix() {
    	return "." + getOwlFileExtension();
    }
    
    /**
     * Method to create a folder if it doesn't already exists
     * @param fullPath -- complete path to folder to be created
     * @return - true if created else false if already exists
     * @throws IOException 
     */
    public static File createFolderIfNotExists(String fullPath) throws IOException {
		File tmpdir = new File(fullPath);
		if (!tmpdir.exists()) {
			boolean bdirok = tmpdir.mkdir();
			if (!bdirok) {
				throw new IOException("Failed to create folder '" + fullPath + "'.");
			}
		}
		return tmpdir;
    }
    
    /**
     * Method to find a ConfigurationManager with a mapping for a URI in another project (upon which this project depends)
     * @param sadlModelManagerProvider
     * @param thisProjectURI
     * @param otherProjectURI
     * @return
     */
    public static IConfigurationManagerForIDE findConfigurationManagerInOtherProject(SadlModelManagerProvider sadlModelManagerProvider, 
    		URI thisProjectURI, String otherProjectURI) {
		IProject prj = ResourceManager.getProject(thisProjectURI);
		try {
			IProject[] referencedProjects = prj.getReferencedProjects();
			for (int i = 0; referencedProjects != null && i < referencedProjects.length; i++) {
				IProject refedPrg = referencedProjects[i];
				IFile modelFolder = refedPrg.getFile(ResourceManager.OWLDIR);
				SadlModelManager sadlModelMgr = sadlModelManagerProvider.get(URI.createURI(modelFolder.getLocationURI().toString()));
				IConfigurationManagerForIDE otherProjectConfigMgr = sadlModelMgr.getConfigurationMgr(modelFolder.getLocation().toOSString());
				if (otherProjectConfigMgr.containsMappingForURI(otherProjectURI)) {
					return otherProjectConfigMgr;
				}
			}
		} catch (CoreException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (URISyntaxException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
    }
}
