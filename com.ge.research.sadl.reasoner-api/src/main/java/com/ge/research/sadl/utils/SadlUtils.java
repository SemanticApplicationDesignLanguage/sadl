package com.ge.research.sadl.utils;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import javax.activation.DataSource;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

public class SadlUtils {

	/**
	 * This enum defines the concept types in a non-Jena way that can be used
	 * throughout the SADL code
	 */
	public enum ConceptType {
		ANNOTATIONPROPERTY, DATATYPEPROPERTY, OBJECTPROPERTY, ONTCLASS, INDIVIDUAL, MODELNAME, 
		RDFDATATYPE, CONCEPT_NOT_FOUND_IN_MODEL
	}

	/**
	 * Call this method to remove double quotes from the beginning and end of a
	 * string so quoted.
	 * 
	 * @param quotedString
	 *            -- the string from which quotes are to be removed
	 */
	public String stripQuotes(String quotedString) {
		if (quotedString != null && !quotedString.isEmpty()) {
			if (quotedString.charAt(0) == '\"') {
				while (quotedString.charAt(0) == '\"') {
					quotedString = quotedString.substring(1);
				}
				while (quotedString.length() > 0
						&& quotedString.charAt(quotedString.length() - 1) == '\"') {
					quotedString = quotedString.substring(0,
							quotedString.length() - 1);
				}
			}
			else if (quotedString.charAt(0) == '\'') {
				while (quotedString.charAt(0) == '\'') {
					quotedString = quotedString.substring(1);
				}
				while (quotedString.length() > 0
						&& quotedString.charAt(quotedString.length() - 1) == '\'') {
					quotedString = quotedString.substring(0,
							quotedString.length() - 1);
				}
			}
		}
		return quotedString;
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
	 * Method to find a Resource in this model by URI and return it as a
	 * ConceptName
	 * 
	 * @param uri
	 *            -- the concept URI string
	 * @return
	 */
	public ConceptName getConceptByUri(OntModel m, String uri) {
		if (m == null) {
			return null;	// this may happen on startup
		}
		ConceptType ctype = null;
		Resource r;
		if (uri.equals(RDF.type.getURI())) {
			r = RDF.type;
			ctype = ConceptType.OBJECTPROPERTY;
		}
		else {
			r = m.getOntResource(uri);
			if (r != null) {
				if (r instanceof Individual) {
					ctype = ConceptType.INDIVIDUAL;
				}
				else if (r.canAs(DatatypeProperty.class)){
					ctype = ConceptType.DATATYPEPROPERTY;
				}
				else if (r.canAs(ObjectProperty.class)) {
					ctype = ConceptType.OBJECTPROPERTY;
				}
				else if (r.canAs(OntClass.class)) {
					ctype = ConceptType.ONTCLASS;
				}
				else if (r.canAs(AnnotationProperty.class)) {
					ctype = ConceptType.ANNOTATIONPROPERTY;
				}
				else if (r.canAs(Individual.class)) {
					ctype = ConceptType.INDIVIDUAL;
				}
			}
		}
		if (r == null) {
			if (uri.equals(RDFS.label.getURI())) {
				r = RDFS.label;
				ctype = ConceptType.ANNOTATIONPROPERTY;
			}
			else if (uri.equals(RDFS.domain.getURI())) {
				r = RDFS.domain;
				ctype = ConceptType.OBJECTPROPERTY;
			}
			else if (uri.equals(RDFS.range.getURI())) {
				r = RDFS.range;
				ctype = ConceptType.OBJECTPROPERTY;
			}
			else if (uri.equals(OWL.Class.getURI())) {
				r = OWL.Class;
				ctype = ConceptType.ONTCLASS;
			}
			else if (uri.equals(OWL.DatatypeProperty.getURI())) {
				r = OWL.DatatypeProperty;
				ctype = ConceptType.ONTCLASS;
			}
			else if (uri.equals(OWL.ObjectProperty.getURI())) {
				r = OWL.ObjectProperty;
				ctype = ConceptType.ONTCLASS;
			}
		}
		if (r != null) {
			ConceptName cn = new ConceptName(r.getLocalName());
			cn.setNamespace(r.getNameSpace());
			cn.setType(ctype);
			return cn;
		}
		return null;
	}

	/**
	 * return true if the first argument class is a subclass of the second
	 * argument class
	 * 
	 * @param subcls
	 * @param cls
	 * @return
	 */
	public boolean classIsSubclassOf(OntClass subcls, OntResource cls, boolean rootCall) {
		if (subcls == null || cls == null) {
			return false;
		}
		if (cls.isURIResource() && subcls.isURIResource()
				&& cls.getURI().equals(subcls.getURI())) {
			return true;
		}
		if (cls.isAnon()) {
			if (cls.canAs(OntClass.class)) {
				OntClass ocls = cls.as(OntClass.class);
				if (ocls.isUnionClass()) {
					UnionClass ucls = cls.as(UnionClass.class);
					ExtendedIterator<? extends OntClass> eitr = ucls
							.listOperands();
					while (eitr.hasNext()) {
						OntClass uclsmember = eitr.next();
						if (classIsSubclassOf(subcls, uclsmember, false)) {
							eitr.close();
							return true;
						}
					}
				}
			}
		}
		try {
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eitr = cls.as(OntClass.class).listSubClasses();
				while (eitr.hasNext()) {
					OntClass subCls = eitr.next();
					if (subCls.equals(subcls)) {
						eitr.close();
						return true;
					}
					else {
						if (classIsSubclassOf(subcls, subCls, false)) {
							eitr.close();
							return true;
						}
					}
				}
				eitr.close();
				if (rootCall && classIsSuperClassOf(cls.as(OntClass.class), subcls)) {
					return true;
				}
			}
			//			if (subcls.hasSuperClass(cls, false)) {  // this doesn't work, don't know why awc 6/8/2012
			//				return true;
			//			}
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return false;
	}

	private boolean classIsSuperClassOf(OntClass cls, OntClass subcls) {
		ExtendedIterator<OntClass> eitr = subcls.listSuperClasses();
		while (eitr.hasNext()) {
			OntClass sprcls = eitr.next();
			if (sprcls.equals(cls)) {
				return true;
			}
			if (classIsSuperClassOf(cls, sprcls)) {
				return true;
			}
		}
		eitr.close();
		return false;
	}

	/**
	 * Method to determine, from preferences, the OWL file format to use
	 * @return -- the file extension, preceded by a period, specified by the format selected in preferences
	 */
	public static String getOwlFileExtensionWithPrefix() {
		return IConfigurationManager.OWLFILEEXTWITHPREFIX;
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
		if (!url.startsWith(IConfigurationManager.FILE_SHORT_PREFIX)) {
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
	 * Method to convert the file identified by a fully qualified name to a string representing its contents.
	 *
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public String fileToString(String file) throws IOException {
		String result = null;
		DataInputStream in = null;

		try {
			File f = new File(file);
			byte[] buffer = new byte[(int) f.length()];
			in = new DataInputStream(new FileInputStream(f));
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
	 * Method to convert a DataSource to a String
	 * @param ds
	 * @return
	 * @throws IOException
	 */
	public String convertDataSourceToString(DataSource ds) throws IOException {
		InputStream is = ds.getInputStream();
		if (is != null) {
			Writer writer = new StringWriter();
			char[] buffer = new char[1024];
			Reader reader = null;
			try {
				reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
				int n;
				while ((n = reader.read(buffer)) != -1) {
					writer.write(buffer, 0, n);
				}
			} finally {
				if (reader != null) {
					reader.close();
				}
				is.close();
			}
			return writer.toString();
		} else {       
			return "";
		}
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

	/**
	 * Method to convert an altUrl to an OWL file into the corresponding name of a SADL file (without path)
	 * 
	 * @param owlAltUrl
	 * @return
	 * @throws MalformedURLException 
	 */
	public String sadlFileNameOfOwlAltUrl(String owlAltUrl) throws MalformedURLException {
		File owlfile = new File(fileUrlToFileName(owlAltUrl));
		String fn = owlfile.getName();
		int extLength = (fn.length() - fn.indexOf('.')) - 1;
		return owlfile.getName().substring(0, fn.length() - extLength) + "sadl";
	}

	/**
	 * Method to recursively delete a directory structure
	 * @param imcf folder to be deleted recursively
	 * @return true if successful
	 */
	public boolean recursiveDelete(File imcf) {
		boolean stat = true;
		File[] children = imcf.listFiles();
		for (int i = 0; i < children.length; i++) {
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

	/**
	 * Method to determine if a query contains any QNames that need expansion.
	 * @param q
	 * @return
	 */
	public static synchronized boolean queryContainsQName(String q) {
		int start = 0;
		int openBracket = q.indexOf('<');
		while (openBracket > start) {
			int closeBracket = q.indexOf('>', openBracket);
			if (closeBracket <= openBracket) {
				// this could be a comparison in a FILTER...
				start = openBracket + 1;
				continue;
			}
			String url = q.substring(openBracket, closeBracket);
			start = closeBracket + 1;
			openBracket = q.indexOf('<', start);
			if (url.indexOf('#') > 0) {
				// a full URI
				continue;
			}
			else if (url.indexOf(':') > 0) {
				// a QName
				return true;
			}
			else {
				// a local fragment only
				return true;
			}
		}
		return false;
	}


}
