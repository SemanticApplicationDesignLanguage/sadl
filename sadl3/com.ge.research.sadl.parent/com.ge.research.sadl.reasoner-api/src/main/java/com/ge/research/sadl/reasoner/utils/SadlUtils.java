package com.ge.research.sadl.reasoner.utils;

import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.ge.research.sadl.reasoner.ConfigurationException;

public class SadlUtils {
	
    public static final String FILE_SHORT_PREFIX = "file:/";
    public static final String FILE_URL_PREFIX = "file://";
    public static final String FILE_ABS_URL_PREFIX = "file:///";
    public static final String HTTP_URL_PREFIX = "http://";

    public static String validateUri(String uri) throws java.net.MalformedURLException {
	    final URL url;
	    try {
	        url = new URL(uri);
	    }
	    catch (Exception e1) {
	    	return ("'" + uri + "' is not a valid URL: " + e1.getLocalizedMessage());
	    }
	    if (!"http".equals(url.getProtocol())) {
	    	return ("'" + uri + "' is not a valid URL: Model name must use http protocol.");
	    }
	    return null;
	}
	
	public String validateHTTP_URI(String uri) throws java.net.MalformedURLException { 
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
	 * This method converts an OS filename (e.g., "C:\\folder\file.ext")
	 * to a file URL
	 *
	 * @param fileName
	 * @return
	 * @throws URISyntaxException 
	 */
	public String fileNameToFileUrl(String fileName) throws URISyntaxException {
		if (fileName == null) {
			return null;
		}
		URI fileUri = null;
		if (fileName.startsWith("http:") || fileName.startsWith("file:")) {
			fileUri = URI.create(fileName);
		}
		else {
			//            fileUri = URI.createFileURI(fileName);
			if (fileName.contains("\\")) {
				fileName = fileName.replace('\\', '/');
			}
			File f = new File(fileName);
			fileUri = f.toURI();
			//        	if (fileName.startsWith("/")) {
			//        		fileUri = new URI("file", "//" + fileName, null); 
			//        	}
			//        	else {
			//        		fileUri = new URI("file", "///" + fileName, null);         		
			//        	}
		}
		return fileUri.toString();
	}

	/**
	 * This method converts the string form of a file URL to an
	 * OS filename
	 *
	 * @param url
	 * @return
	 * @throws MalformedURLException 
	 */
	public String fileUrlToFileName(String url) throws MalformedURLException {
		if (!url.startsWith(FILE_SHORT_PREFIX)) {
			return url;
		}
		if (url.indexOf('\\') > 0) {
			// this appears to be needed on Windows
			url = url.replace('\\', '/');
		}
		URI fileUri = URI.create(url);
		return fileUri.toURL().getPath();
	}
	
	/**
	 * Method to recursively delete a directory structure
	 * @param imcf folder to be deleted recursively
	 * @return true if successful
	 */
	public boolean recursiveDelete(File imcf) {
		boolean stat = true;
		File[] children = imcf.listFiles();
		for (int i = 0; children != null && i < children.length; i++) {
			if (children[i].isDirectory()) {
				if (!recursiveDelete(children[i])) {
					stat = false;
				}
			}
			else {
				if (!children[i].delete()) {
					stat = false;
				}
			}
		}
		if (!imcf.delete()) {
			stat = false;
		}
		return stat;
	}


	public List<String>[] getUrlsAndPrefixesFromExternalUrlContent(String editorText) {
		StringTokenizer tokenizer =
				new StringTokenizer(editorText, "\n\r");
		ArrayList<String> urls = new ArrayList<String>();
		while (tokenizer.hasMoreTokens()) {
			String urlString =tokenizer.nextToken(); 
		    if (urlString != null && !urlString.isEmpty() && !urlString.startsWith("--")) {
		    	urls.add(urlString);
		    }
		}
		List<String> prefixes = new ArrayList<String>();
		for (int i = 0; i < urls.size(); i++) {
			String urlString = urls.get(i);
			String prefix = null;
			int sep = urlString.indexOf(" as ");
			if (sep > 0) {
				prefix = urlString.substring(sep + 4).trim();
				prefixes.add(prefix);
				urlString = urlString.substring(0, sep).trim();
				urls.set(i, urlString);
			}
			else {
				prefixes.add(null);
			}
		}
		List<String>[] retvals = new List[2];
		retvals[0] = urls;
		retvals[1] = prefixes;
		return retvals;
	}


	public String externalUrlToRelativePath(String urlString) throws MalformedURLException {
		URL url = new URL(urlString);
		String urlPath = url.getHost() + url.getPath();

		if (url.getPath() == null || url.getPath().isEmpty())
			urlPath = urlPath + "/" + url.getHost() + ".owl";
		else if (!url.getPath().contains("."))
			urlPath = urlPath + "/" + urlPath.substring(urlPath.lastIndexOf("/") + 1) + ".owl";
		return urlPath;
	}
	
	
	/**
	 * Method to convert the file identified by a fully qualified name to a string representing its contents.
	 *
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public String fileToString(File file) throws IOException {
		String result = null;
		DataInputStream in = null;

		try {
			byte[] buffer = new byte[(int) file.length()];
			in = new DataInputStream(new FileInputStream(file));
			in.readFully(buffer);
			result = new String(buffer);
		} finally {
			try {
				in.close();
			} catch (IOException e) { /* ignore it */
			}
		}
		return result;
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
	public void stringToFile(File aFile, String contents, boolean writeProtect) throws IOException {
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


	public String getExternalModelRootFromUrlFilename(File editorFile) {
		if (editorFile != null) {
			return editorFile.getName().substring(0, editorFile.getName().lastIndexOf("."));
		}
		return null;
	}


	public String getPublicUriFromUrl(String url, String altUrl) throws MalformedURLException {
		File owlFile = new File(fileUrlToFileName(altUrl));
		// TODO  open file and get base URI, append '#'
		return url;
	}

    /**
     * Call this method to remove double quotes from the beginning and end of a string so quoted.
     * @param quotedString -- the string from which quotes are to be removed
     */
    public static String stripQuotes(String quotedString) {
        if (quotedString != null && !quotedString.isEmpty()) {
            while (quotedString.charAt(0) == '\"') {
                quotedString = quotedString.substring(1);
            }
            while (quotedString.length() > 0 && quotedString.charAt(quotedString.length() - 1) == '\"') {
                quotedString = quotedString.substring(0, quotedString.length() - 1);
            }
        }
        return quotedString;
    }

	public List<String[]> parseQueries(File qf) throws IOException, ConfigurationException {
		List<String[]> results = new ArrayList<String[]>();
		String string = fileToString(qf);
		String[] lines = string.split(System.getProperty("line.separator"));
		String[] current = new String[2];
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < lines.length; i++) {
			String line = lines[i];
			if (line.trim().startsWith("Ask")) {
				if (i > 0) {
					current[1] = trimQuery(sb);
					results.add(current);
					current = new String[2];
					sb.setLength(0);
				}
				int colonloc = line.indexOf(":");
				if (colonloc < 0) {
					throw new ConfigurationException("Ask line of query missing ':'");
				}
				current[0] = line.substring(3,colonloc).trim();
				int quoteloc = line.indexOf(colonloc,'\"');
				if (quoteloc > 0 && line.length() > quoteloc) {
					sb.append(line.substring(quoteloc + 1));
				}
			}
			else {
				sb.append(line);
			}
		}
		current[1] = trimQuery(sb);
		results.add(current);
		return results;
	}

	private String trimQuery(StringBuilder sb) {
		String qstr = sb.toString().trim();
		if (qstr.endsWith(".")) {
			qstr = qstr.substring(0, qstr.length() - 1);
		}
		qstr = SadlUtils.stripQuotes(qstr);
		return qstr;
	}

}
