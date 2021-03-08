package com.ge.research.sadl.reasoner.utils;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import javax.activation.DataSource;

import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.ontology.AnnotationProperty;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.IntersectionClass;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.UnionClass;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;
import org.pojava.datetime.DateTime;
import org.pojava.datetime.Duration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.TranslationException;

public class SadlUtils {
	private static final Logger logger = LoggerFactory.getLogger(SadlUtils.class);
	
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
	    if (!"http".equals(url.getProtocol()) && !"https".equals(url.getProtocol())) {
	    	return ("'" + uri + "' is not a valid URL: Model name must use http or https protocol.");
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
		if (!"http".equals(url.getProtocol()) && !"https".equals(url.getProtocol())) {
			throw new java.net.MalformedURLException("'" + uri + "' is not a valid URL: Model name must use http or https protocol.");
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
			return url.replace('\\', '/');
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
	public static boolean recursiveDelete(File imcf) {
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
				if (in != null) {
					in.close();
				}
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
		try {
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
		catch (Exception e) {
			System.err.println("Exception writing file '" + aFile.getAbsolutePath() + "'");
			e.printStackTrace();
			throw new IOException(e.getMessage(), e);
		}
	}

	public void stringToFile(File aFile, String contents, boolean writeProtect, Charset charset) throws IOException {
		if (aFile == null) {
			throw new IllegalArgumentException("File should not be null.");
		}
		if (aFile.exists() && !aFile.isFile()) {
			throw new IllegalArgumentException("Should not be a directory: " + aFile);
		}
		try {
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
				//output = new BufferedWriter( new FileWriter(aFile) );
				//AG: the default charset sometimes results in bad dot/svg files on Windows
				// The UTF_8 charset seems to work fine on both MacOS and Windows.
				output = Files.newBufferedWriter(aFile.toPath(), charset);
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
		catch (Exception e) {
			System.err.println("Exception writing file '" + aFile.getAbsolutePath() + "'");
			e.printStackTrace();
			throw new IOException(e.getMessage(), e);
		}
	}
	
	/**
	 * Change the contents of text file by appending content to any
	 * existing text.
	 *
	 * This implementation throws all exceptions to the caller.
	 *
	 * @param aFile is an existing file which can be written to.
	 * @param contents is the string to be appended
	 * @param writeProtect if true write protect the file
	 * 
	 * @throws IOException if problem encountered during write.
	 */
	public void appendStringToFile(File aFile, String contents, boolean writeProtect) throws IOException {
		if (aFile == null) {
			throw new IllegalArgumentException("File should not be null.");
		}
		if (aFile.exists() && !aFile.isFile()) {
			throw new IllegalArgumentException("Should not be a directory: " + aFile);
		}
		try {
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
				output = new BufferedWriter( new FileWriter(aFile, true) );
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
		catch (Exception e) {
			System.err.println("Exception writing file '" + aFile.getAbsolutePath() + "'");
			e.printStackTrace();
			throw new IOException(e.getMessage(), e);
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
        	if (quotedString.startsWith("\"") && quotedString.endsWith("\"")) {
	            while (quotedString.length() >= 1 && quotedString.charAt(0) == '\"') {
	                quotedString = quotedString.substring(1);
	            }
	            while (quotedString.length() > 0 && quotedString.charAt(quotedString.length() - 1) == '\"') {
	                quotedString = quotedString.substring(0, quotedString.length() - 1);
	            }
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

	public static boolean queryContainsQName(String q) {
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

    /**
     * Method to check a URI to see if it is valid in the context of RDF.
     * 
     * @param uri -- URI (as String) to validate
     * @return -- null if valid else an error description if not valid
     */
	public static synchronized String validateRdfUri(String uri) {
		int lbsign = uri.indexOf('#');
		if (lbsign >= 0) {
			String ns = uri.substring(0, uri.indexOf('#'));
			String nsregex = "^(http)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@/%=~_|]";
			if (!ns.matches(nsregex)) {
				return "invalid namespace '" + ns + "'";
			}
		}
		String fragment = lbsign >= 0 ? uri.substring(uri.lastIndexOf('#') + 1) : uri;
		if (fragment == null) {
			return "missing '#' followed by fragment";
		}
		String fragmentregex = "[a-zA-Z]+[a-zA-Z\\._\\-0-9]*";
		if (! fragment.matches(fragmentregex)) {
			return "invalid fragment '" + fragment + "'";
		}
		return null;
	}

	/**
	 * Method to generate a unique URI string in a given ontology model (OntModel), also given a baseUri. 
	 * If the baseUri ends with a series of digits (a number without decimal or any character following),
	 * then the number will be extracted and used as a base "counter" and incremented by 1 until a unique
	 * URI is obtained. If the baseUri does not end with a number then a number will be found, with the
	 * search starting with 1, which when added to the baseUri gives a unique URI.
	 * 
	 * @param ontModel -- the model in which the URI is to be unique
	 * @param baseUri -- the base URI to which the counter is to be added and incremented until unique
	 * @return -- the unique URI string
	 */
	public static synchronized String getUniqueOntUri(OntModel ontModel, String baseUri) {
		Object[] split = splitUriIntoBaseAndCounter(baseUri);
		baseUri = (String) split[0];
		long cntr = (long) split[1];
		String uri = baseUri + cntr;
		while (ontModel.getOntResource(uri) != null) {
			uri = baseUri + ++cntr;
		}
		return uri;
	}
	
	/**
	 * Method to take a URI that may end in an integer and split it into the base (before the integer) and the counter (the integer)
	 * @param uri
	 * @return-- Object array containing 0) base URI, 1) counter
	 */
	public static Object[] splitUriIntoBaseAndCounter(String uri) {
		long cntr = 0;
		int numDigitsAtEnd = 0;
		for (int i = uri.length() - 1; i >= 0; i--) {
			int exp = uri.length() - (i + 1);
			char c = uri.charAt(i);
			if (Character.isDigit(c)) {
				int cint = c - 48;
				long mplier = (long) Math.pow(10, exp);
				cntr += cint * mplier;
				numDigitsAtEnd++;
			}
			else {
				break;
			}
		}
		if (numDigitsAtEnd > 0) {
			uri = uri.substring(0, uri.length() - numDigitsAtEnd);
		}
		else {
			cntr = 1;
		}
		Object[] retval = new Object[2];
		retval[0] = uri;
		retval[1] = cntr;
		return retval;
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
     * Call this method to convert a value (v) as a Java object to a typed 
     * Literal matching the range of the property.
     *
     * @param m
     * @param prop
     * @param v
     * @return
     * @throws CircularDependencyException 
     */
    public static synchronized Literal getLiteralMatchingDataPropertyRange(OntModel m, OntProperty prop, Object v) throws TranslationException {
        Literal val = null;
        String errMsg = null;
        if (prop == null || prop.isAnnotationProperty()) {
        	return m.createTypedLiteral(v);
        }
        // SADL only has DoubleLiterals--if this property has range float convert v to Float.
        OntResource rng = prop.getRange();
        String rnguri = rng != null ? rng.getURI() : null;
        if (rng == null) {
            errMsg = "Range not given.";
        }
        else if (rng.isAnon()) {
            // this is a complex range--needs work. Try to do something with it....
            // If value is a String
            if (v instanceof String) {
                v = stripQuotes((String)v);
                val = m.createTypedLiteral(v);                
            }
            else {
                val = m.createTypedLiteral(v);
                if (val == null) {
                    errMsg = "Range is an unsupported complex type, failed to create a Literal value for '" + v.toString() + "'.";
                }
            }
        }
        else {           
        	val = getLiteralMatchingDataPropertyRange(m, rnguri, v);
        }
        if (errMsg != null) {
        	errMsg += " (Property is '" + prop.getLocalName() + "'.)";
            throw new TranslationException(errMsg);
        }
        return val;
    }
    
    public static synchronized Literal getLiteralMatchingDataPropertyRange(OntModel m, String rnguri, Object v) throws TranslationException {
    	v = preProcessValueString(rnguri, v.toString());
        Literal val = null;
        String errMsg = null;
        boolean rdfTypeValid = false;
        boolean isNumeric = isNumericRange(rnguri);
        RDFDatatype rdftype = TypeMapper.getInstance().getTypeByName(rnguri);
        if (rdftype != null) {
         	val = m.createTypedLiteral(v, rdftype);
        	if (val != null) {
        		return val;
         	}
	        Resource rng = m.getResource(rnguri);
			OntClass eqcls = null;
			if (rng.canAs(OntClass.class)) {
				eqcls = rng.as(OntClass.class).getEquivalentClass();
			}
			if (eqcls != null) {
				rdftype = TypeMapper.getInstance().getTypeByName(rnguri);
				Statement pv = eqcls.getProperty(OWL2.onDatatype);
				if (pv != null) {
					RDFNode pvobj = pv.getObject();
					if (pvobj.isURIResource()) {
						rnguri = pvobj.asResource().getURI();
						rdftype = TypeMapper.getInstance().getTypeByName(rnguri);
						isNumeric = isNumericRange(rnguri);
					}
				}
				else {
					Statement uof = eqcls.getProperty(OWL2.unionOf);
					if (uof != null) {
						RDFNode uofnode = uof.getObject();
						if (uofnode.isResource()) {
							java.util.List<RDFNode> l = convertList(uofnode, m);
							if (l != null) {
								Iterator<RDFNode> opsitr = l.iterator();
								List<Throwable> exceptions = null;
								while (opsitr.hasNext()) {
									RDFNode op = opsitr.next();
									if (op.isURIResource()) {
										try {
											Literal oplit = getLiteralMatchingDataPropertyRange(m, op.asResource().getURI(), v);
											if (oplit != null) {
												return oplit;
											}
										}
										catch (Throwable t) {
											if (opsitr.hasNext()) {
												if (exceptions == null) {
													exceptions = new ArrayList<Throwable>();
												}
												exceptions.add(t);
											}
											else {
												throw t;
											}
										}
									}
								}
								if (exceptions != null) {
									StringBuilder sb = new StringBuilder("Union operand results: ");
									for (int i = 0; i < exceptions.size(); i++) {
										sb.append(exceptions.get(i).getMessage() + "\n");
									}
									throw new TranslationException(sb.toString());
								}
							}
						}
					}
					StmtIterator pvitr = eqcls.listProperties();
					while (pvitr.hasNext()) {
						System.out.println(pvitr.nextStatement().toString());
					}
				}
			}

        }
    	if (rnguri != null) {
    		Object vorig = v;
    		try {
    			if (isNumeric) {
    				if (v instanceof String) {
    					v = new BigDecimal((String)v);
    		        	rdfTypeValid = rdftype.isValidValue(v);
     					if (rdfTypeValid) {
    						val = m.createTypedLiteral(vorig, rdftype);
    						if (val != null) {
    							return val;
    						}
    					}
    				}
			        if (rnguri.contains("float")) {
			        	if (v instanceof BigDecimal) {
			        		v = new Float(((BigDecimal)v).floatValue());
			        		val = m.createTypedLiteral(v);
			        	}
			        	else if (v instanceof Double) {
			                v = new Float(((Double)v).floatValue());
			                val = m.createTypedLiteral(v);
			            }
			            else if (v instanceof Float){
			                val = m.createTypedLiteral(v);
			            }
			            else if (v instanceof Integer) {
			                v = new Float(((Integer)v).floatValue());
			                val = m.createTypedLiteral(v);
			            }
			            else if (v instanceof Long) {
			            	v = new Float(((Long)v).floatValue());
			                val = m.createTypedLiteral(v);
			            }
			            else {
			                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range float";
			            }
			        }
			        else if (rnguri.contains("double")) {
			        	if (v instanceof BigDecimal) {
			        		v = new Double(((BigDecimal)v).doubleValue());
			        		val = m.createTypedLiteral(v);
			        	}
			        	else if (v instanceof Double) {
			                val = m.createTypedLiteral(v);
			            }
			            else if (v instanceof Float){
			                v = new Double(((Float)v).doubleValue());
			                val = m.createTypedLiteral(v);
			            }
			            else if (v instanceof Integer) {
			                v = new Double(((Integer)v).doubleValue());
			                val = m.createTypedLiteral(v);
			            }
			            else if (v instanceof Long) {
			            	v = new Double(((Long)v).doubleValue());
			                val = m.createTypedLiteral(v);
			            }
			            else {
			                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range double";
			            }
			        }
			        else if (rnguri.contains("decimal")) {
			            if (v instanceof BigDecimal) {
			            	// nothing required
			            }
			            else if (v instanceof Double) {
			                v= new BigDecimal(((Double)v).doubleValue());
			            }
			            else if (v instanceof Float){
			                v= new BigDecimal(((Float)v).doubleValue());
			            }
			            else if (v instanceof Integer) {
			                v= new BigDecimal(((Integer)v).doubleValue());
			            }
			            else if (v instanceof Long) {
			            	v = new BigDecimal(((Long)v).doubleValue());
			            }
			            else {
			                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range decimal";
			            }
			            val = m.createTypedLiteral(v);
			        }
			        else if (rnguri.contains("int") || rnguri.endsWith("long")) {
			        	if (vorig instanceof String) {
			        		if (((String)vorig).trim().contains(".")) {
			        			errMsg = "Value '" + v.toString() + "' doesn't match range " + rnguri;
			        		}
			        		try {
				        		if (rnguri.contains("int")) {
				        			v = Integer.parseInt(stripQuotes((String)vorig));
				        		}
				        		else {
				        			// must be long
				        			v = Long.parseLong(stripQuotes((String)vorig));
				        		}
			        		}
			        		catch (NumberFormatException e) {
			        			errMsg = "Value '" + vorig.toString() + "' doesn't match range " + rnguri;
			        		}
			         	}
			        	if (v instanceof BigDecimal) {
			        		v = new Long(((BigDecimal)v).longValue());
			        	}
			        	if (v instanceof Long) {
			            	v = new Integer(((Long)v).intValue());
			        	}
			            if (v instanceof Integer) {
			                val = m.createTypedLiteral(v);
			            }
			            else if (errMsg == null) {
			                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range " + rnguri;
			            }
			        }
    			}
		        else if (rnguri.contains("string")) {
		            if (v instanceof String) {
		                v = stripQuotes((String)v);
		                val = m.createTypedLiteral(v);
		            }
		            else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range string";
		            }
		        }
		        else if (rnguri.endsWith("date")) {
		            if (v instanceof String) {
		                v = stripQuotes((String)v);
						DateTime dt = new DateTime((String)v);
						String xsdFormat = "yyyy-MM-dd";
						String modifiedV = dt.toString(xsdFormat);
		                val = m.createTypedLiteral(modifiedV, rnguri);
		            }
		            else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range date/dateTime/time";
		            }
		        }
		        else if (rnguri.endsWith("dateTime")) {
		            if (v instanceof String) {
		                v = stripQuotes((String)v);
		                if (v != null && ((String) v).length() > 0) {
							DateTime dt = new DateTime((String)v);
							String xsdFormat = "yyyy-MM-dd'T'HH:mm:ssZZ";
							String modifiedV = dt.toString(xsdFormat);
			                val = m.createTypedLiteral(modifiedV, rnguri);
		                }
		            }
		            else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range date/dateTime/time";
		            }
		        }
		        else if (rnguri.endsWith("time")) {
		            if (v instanceof String) {
		                v = stripQuotes((String)v);
		                val = m.createTypedLiteral(v, rnguri);
		            }
		            else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range date/dateTime/time";
		            }
		        }
		        else if (rnguri.endsWith("duration")) {
		        	if (v instanceof String) {
		        		v = stripQuotes((String)v);
		        		if (v != null && ((String)v).length() > 0) {
		        			String lc = ((String)v).toLowerCase();
		        			if (lc.contains("ye") ||lc.contains("yr") || lc.contains("mo")) {
		        				errMsg = "Durations containing years or months are not supported";
		        			}
		        			else {
		        				Duration dur = new Duration((String)v);
		        				String modDur = "PT" + dur.toString().toUpperCase();
		        				val = m.createTypedLiteral(modDur, rnguri);
		        			}
		        		}
		        		else {
			                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range duration";
		        		}
		        	}
		        	else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range duration";
		        	}
		        }
		        else if (rnguri.endsWith("boolean")) {
		        	if (v instanceof String) {
		       			v = Boolean.parseBoolean(stripQuotes((String)v));
		         	}
		            if (v instanceof Boolean) {
		                val = m.createTypedLiteral(v);
		            }
		            else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range boolean";
		            }
		        }
		        else if (rnguri.endsWith("anyURI")) {
		        	if (v instanceof String) {
		                v = stripQuotes((String)v);
		                try {
		                	URI.create((String) v);
		                	val = m.createTypedLiteral(v, rnguri);
		                }
		                catch (Exception e) {
		                	errMsg = "Error converting '" + v.toString() + "' to URI: " + e.getMessage();
		                }
		        	}
		        	else {
		                errMsg = "Unexpected value '" + v.toString() + "' (" + v.getClass().getSimpleName() + ") doesn't match range anyURI";
		        	}
		        }
		        else {
		        	Resource rsrc = m.getResource(rnguri);
		        	if (rsrc != null) {
		        		Statement typ = rsrc.getProperty(RDF.type);
		        		if (typ != null) {
		        			if (typ.getObject().equals(RDFS.Datatype)) {
		        				TypeMapper.getInstance().getSafeTypeByName(rnguri);
		        				return getLiteralMatchingDataPropertyRange(m, rnguri, v);
		        			}
		        			else {
				        		errMsg = "Unhandled range " + rnguri;
		        			}
		        		}
		        		else {
			        		errMsg = "Unhandled range " + rnguri;
		        		}
		        	}
		        	else {
		        		errMsg = "Unhandled range " + rnguri;
		        	}
		        }
    		}
    		catch (Throwable t) {
    			StringBuilder sb = new StringBuilder("Unable to convert value '");
    			sb.append(v);
    			if (!v.equals(vorig)) {
    				sb.append(" (");
    				sb.append(vorig);
    				sb.append(")");
    			}
    			sb.append("' to type '");
    			sb.append(rnguri);
    			sb.append("'");
    			throw new TranslationException(sb.toString() + "(" + t.getMessage() + ")");
    		}
    	}
    	else {
    		errMsg = "Range should not be null.";
    	}
        if (errMsg != null) {
            throw new TranslationException(errMsg);
        }
        return val;
    }
    
    /*
     * Method to convert a date, dateTime, or duration to the XSD format expected using POJava classes
     */
    public static String preProcessValueString(String rnguri, String v) throws TranslationException {
    	String retval = v;
        if (rnguri.endsWith("date")) {
            if (v instanceof String) {
                v = stripQuotes((String)v);
				DateTime dt = new DateTime((String)v);
				String xsdFormat = "yyyy-MM-dd";
				String modifiedV = dt.toString(xsdFormat);
				retval = modifiedV;
            }
        }
        else if (rnguri.endsWith("dateTime")) {
            if (v instanceof String) {
                v = stripQuotes((String)v);
                if (v != null && ((String) v).length() > 0) {
					DateTime dt = new DateTime((String)v);
					String xsdFormat = "yyyy-MM-dd'T'HH:mm:ssZZ";
					String modifiedV = dt.toString(xsdFormat);
	                retval = modifiedV;
                }
            }
        }
        else if (rnguri.endsWith("time")) {
            if (v instanceof String) {
                v = stripQuotes((String)v);
                retval = v;
            }
        }
        else if (rnguri.endsWith("duration")) {
        	if (v instanceof String) {
        		v = stripQuotes((String)v);
        		if (v != null && ((String)v).length() > 0) {
        			String lc = ((String)v).toLowerCase();
        			if (lc.contains("ye") ||lc.contains("yr") || lc.contains("mo")) {
        				throw new TranslationException("Durations containing years or months are not supported");
        			}
        			else {
        				Duration dur = new Duration((String)v);
        				String modDur = "PT" + dur.toString().toUpperCase();
        				retval = modDur;
        			}
        		}
        	}

        }
        return retval;
    }

    /**
     * Method to try to convert 
     * @param v -- the string to be converted, potentially, to a number
     * @return -- the number from conversion
     */
	public static Object stringToNumber(String v) {
		Object vRet = v;
		if (((String) v).contains(".")) {
			vRet = Double.parseDouble(stripQuotes(v));
		}
		else {
			vRet = Long.parseLong(stripQuotes(v));
		}
		return vRet;
	}

	private static boolean isNumericRange(String rnguri) {
        if (rnguri.endsWith("float")) {
        	return true;
        }
        else if (rnguri.endsWith("double")) {
        	return true;
        }
        else if (rnguri.endsWith("decimal")) {
        	return true;
        }
        else if (rnguri.endsWith("int")) {
        	return true;
        }
        else if (rnguri.endsWith("long")) {
        	return true;
        }
        else if (rnguri.endsWith("integer")) {
        	return true;
        }
        else if (rnguri.endsWith("negativeInteger")) {
        	return true;
        }
        else if (rnguri.endsWith("nonNegativeInteger")) {
        	return true;
        }
        else if (rnguri.endsWith("positiveInteger")) {
        	return true;
        }
        else if (rnguri.endsWith("nonPositiveInteger")) {
        	return true;
        }
		return false;
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
		ConceptName cn = null;
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
				else if (r.canAs(Property.class)) {
					ctype = ConceptType.RDFPROPERTY;
				}
				else if (r.canAs(Individual.class)) {
					ctype = ConceptType.INDIVIDUAL;
				}
			}
		}
		if (r != null) {
			cn = new ConceptName(r.getLocalName());
			cn.setNamespace(r.getNameSpace());
			cn.setType(ctype);
		}
		else {
			int lbloc = uri.indexOf('#');
			String ns = uri.substring(0, lbloc + 1);
			Object[] retvals = null;
			if (ns.equals(OWL.getURI())) {
				retvals = getImplicitResourceAndType(uri, ns, OWL.class);
				if (retvals == null) {
					retvals = getImplicitResourceAndType(uri, ns, OWL2.class);
				}
			}
			else if (ns.equals(RDFS.getURI())) {
				retvals = getImplicitResourceAndType(uri, ns, RDFS.class);
			}
			else if (ns.equals(RDF.getURI())) {
				retvals = getImplicitResourceAndType(uri, ns, RDF.class);
			}
			if (retvals != null) {
				cn = new ConceptName((String) retvals[0]);
				cn.setType((ConceptType) retvals[1]);
			}

//			if (uri.equals(RDFS.label.getURI())) {
//				r = RDFS.label;
//				ctype = ConceptType.ANNOTATIONPROPERTY;
//			}
//			else if (uri.equals(RDFS.domain.getURI())) {
//				r = RDFS.domain;
//				ctype = ConceptType.OBJECTPROPERTY;
//			}
//			else if (uri.equals(RDFS.range.getURI())) {
//				r = RDFS.range;
//				ctype = ConceptType.OBJECTPROPERTY;
//			}
//			else if (uri.equals(OWL.Class.getURI())) {
//				r = OWL.Class;
//				ctype = ConceptType.ONTCLASS;
//			}
//			else if (uri.equals(OWL.DatatypeProperty.getURI())) {
//				r = OWL.DatatypeProperty;
//				ctype = ConceptType.ONTCLASS;
//			}
//			else if (uri.equals(OWL.ObjectProperty.getURI())) {
//				r = OWL.ObjectProperty;
//				ctype = ConceptType.ONTCLASS;
//			}
//			else if (uri.equals(OWL2.onClass.getURI())) {
//				r = OWL2.onClass;
//				ctype = ConceptType.OBJECTPROPERTY;
//			}
		}
		return cn;
	}

	private Object[] getImplicitResourceAndType(String uri, String ns, Class cls) {
		Field[] fields = cls.getDeclaredFields();
		for (Field field : fields) {
			if ((ns + field.getName()).equals(uri)) {
				Object ft = field.getType();
				Object[] retvals = new Object[2];
				retvals[0] = uri;
				if (field.getType().equals(Resource.class)) {
					retvals[1] = ConceptType.OBJECTPROPERTY;
				}
				else if (field.getType().equals(Property.class)) {
					retvals[1] = ConceptType.OBJECTPROPERTY;
				}
				return retvals;
			}
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
	 * @throws CircularDependencyException 
	 */
	public static boolean classIsSubclassOf(OntClass subcls, OntResource cls, boolean rootCall, List<OntResource> previousClasses) throws CircularDependencyException {
		if (subcls == null || cls == null) {
			return false;
		}
		if (cls.isURIResource() && subcls.isURIResource()
				&& cls.getURI().equals(subcls.getURI())) {
			return true;
		}
		int previousClassesSizeOnCall;
		if (previousClasses == null) {
			previousClassesSizeOnCall = 0;
			previousClasses = new ArrayList<OntResource>();
			previousClasses.add(cls);
		}
		else {
			previousClassesSizeOnCall = previousClasses.size();
		}
//		if (cls.isAnon()) {
			if (cls.canAs(OntClass.class)) {
				OntClass ocls = cls.as(OntClass.class);
				if (ocls.isUnionClass()) {
					UnionClass ucls = cls.as(UnionClass.class);
					try {
						ExtendedIterator<? extends OntClass> eitr = ucls
								.listOperands();
						while (eitr.hasNext()) {
							OntClass uclsmember = eitr.next();
							previousClasses = checkPreviousClassList(previousClasses, uclsmember);
							if (classIsSubclassOf(subcls, uclsmember, false, previousClasses)) {
								eitr.close();
								return true;
							}
						}
					}
					catch (Exception e) {
						logger.error("Unexpected error during deep validation: apparent Union Class does not return operands.");
					}
				}
			}
//		}
		try {
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eitr = cls.as(OntClass.class).listSubClasses();
				while (eitr.hasNext()) {
					OntClass subClsOfCls = eitr.next();
					if (subClsOfCls.equals(subcls)) {
						eitr.close();
						return true;
					}
					else {
						previousClasses = checkPreviousClassList(previousClasses, subClsOfCls);
						if (classIsSubclassOf(subcls, subClsOfCls, false, previousClasses)) {
							eitr.close();
							return true;
						}
						else {
							previousClasses.remove(subClsOfCls);
						}
					}
				}
				eitr.close();
//				if (rootCall && classIsSuperClassOf(cls.as(OntClass.class), subcls)) {
//					return true;
//				}
				if (rootCall) {
					// One more condition: if subcls has a superclass which is an intersection containing cls
					//	or a superclass of cls,
					//	then subcls is a subclass of cls
					ExtendedIterator<OntClass> eitr2 = subcls.as(OntClass.class).listSuperClasses(true);
					while (eitr2.hasNext()) {
						OntClass superClsOfCls = eitr2.next();
						if (superClsOfCls.isIntersectionClass()) {
							IntersectionClass icls = superClsOfCls.asIntersectionClass();
							try {
								ExtendedIterator<? extends OntClass> eitr3 = icls.listOperands();
								while (eitr3.hasNext()) {
									OntClass iclsmember = eitr3.next();
	//								previousClasses = checkPreviousClassList(previousClasses, iclsmember);
									// this is a new start for cycles
									if (classIsSubclassOf(cls.as(OntClass.class), iclsmember, false, null)) {
										eitr2.close();
										eitr3.close();
										return true;
									}
								}
							}
							catch (CircularDependencyException e) {
								throw e;
							}
							catch (Exception e) {
								logger.error("Unexpected error during deep validation: apparent Intersection Class does not return operands.");
							}						
						}
					}
				}
			}
//			if (subcls.isAnon()) {
				if (subcls.isIntersectionClass()) {
					IntersectionClass icls = subcls.asIntersectionClass();
					try {
						ExtendedIterator<? extends OntClass> eitr = icls.listOperands();
						while (eitr.hasNext()) {
							OntClass iclsmember = eitr.next();
							previousClasses = checkPreviousClassList(previousClasses, iclsmember);
							if (classIsSubclassOf(cls.as(OntClass.class), iclsmember, false, previousClasses)) {
								eitr.close();
								return true;
							}
						}
					}
					catch (CircularDependencyException e) {
						throw e;
					}
					catch (Exception e) {
						logger.error("Unexpected error during deep validation: apparent Intersection Class does not return operands.");
					}
				}
//			}
				
// TODO We need to look for equivalent classes that provide a definition for a subclass, 
//			e.g. Component is equivalent to System is class, (System and connectedTo someValueFrom Network) => Component subclass of System.
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eqitr = cls.as(OntClass.class).listEquivalentClasses();
				while (eqitr.hasNext()) {
					OntClass eqcls = eqitr.next();
					previousClasses = checkPreviousClassList(previousClasses, eqcls);
					if (classIsSubclassOf(subcls, eqcls, false, previousClasses)) {
						return true;
					}
					// if the eqcls didn't pan out, remove as other wise it may be reached in a different path
					if (previousClasses.contains(eqcls)) {
						previousClasses.remove(eqcls);
					}
				}
			}
			// if there is an equivalent class for the subclass which is an intersection and the intersection includes the cls, then return true
			if (subcls.canAs(OntClass.class )) {
				ExtendedIterator<OntClass> eqitr = subcls.as(OntClass.class).listEquivalentClasses();
				while (eqitr.hasNext()) {
					OntClass eqcls = eqitr.next();
					if (eqcls.isIntersectionClass()) {
						ExtendedIterator<? extends OntClass> icitr = eqcls.asIntersectionClass().listOperands();
						while (icitr.hasNext()) {
							OntClass icnext = icitr.next();
//							previousClasses = checkPreviousClassList(previousClasses, eqcls);
							if (classIsSubclassOf(icnext, cls, false, previousClasses)) {
								return true;
							}
						}
					}
				}
			}
		} 
		catch (CircularDependencyException e) {
			throw e;
		}
		catch (Throwable t) {
			t.printStackTrace();
			logger.debug("Error in classIsSubclassOf: " + t.getMessage());
			throw new CircularDependencyException(t.getMessage(), t);
		}
		if (previousClasses.size() > previousClassesSizeOnCall) {
			// roll back to what it was on call
			for (int i = previousClasses.size() - 1; i >= previousClassesSizeOnCall; i--) {
				previousClasses.remove(i);
			}
		}
		return false;
	}

	private static List<OntResource> checkPreviousClassList(List<OntResource> previousClasses, OntClass cls) throws CircularDependencyException {
		if (previousClasses.contains(cls)) {
			StringBuilder msg = new StringBuilder("Cycle encountered while checking subclasses of ");
			msg.append(previousClasses.get(0).toString());
			int loopstart = previousClasses.indexOf(cls);
			msg.append(" (loop contains: ");
			for (int i = loopstart; i < previousClasses.size(); i++) {
				if (i > loopstart) msg.append(", ");
				msg.append(previousClasses.get(i).toString());
			}
			msg.append(")");
			throw new CircularDependencyException(msg.toString());
		}
		previousClasses.add(cls);
		return previousClasses;
	}
	
	public static boolean classIsSuperClassOf(OntClass cls, OntClass subcls) {
		ExtendedIterator<OntClass> eitr = subcls.listSuperClasses();
		try {
			while (eitr.hasNext()) {
				OntClass sprcls = eitr.next();
				if (sprcls.equals(cls)) {
					return true;
				}
				if (sprcls.isIntersectionClass()) {
					ExtendedIterator<? extends OntClass> exitr = sprcls.asIntersectionClass().listOperands();
					while (exitr.hasNext()) {
						OntClass intersectionClass = exitr.next();
						if (classIsSuperClassOf(cls, intersectionClass)) {
							exitr.close();
							return true;
						}
					}
				}
				if (classIsSuperClassOf(cls, sprcls)) {
					return true;
				}
			}
			eitr.close();
			
			eitr = cls.listSuperClasses();
			while (eitr.hasNext()) {
				OntClass equivCls = eitr.next();
				if (classIsSubclassOf(subcls, equivCls, false, null)) {
					eitr.close();
					return true;
				}
			}
		}
		catch (Throwable t) {
			logger.error("Error checking if class '" + cls.toString() + "' is a superclass of '" + subcls.toString() + 
					"' : " + t.getMessage());
		}
		finally {
			eitr.close();
		}
		return false;
	}

    /**
     * Helper - returns the (singleton) value for the given property on the given
     * root node in the data graph.
     */
    public static RDFNode getPropValue(RDFNode rdfNode, Property prop, OntModel m) {
        return doGetPropValue(m.listStatements(rdfNode.asResource(), prop, (RDFNode)null));
    }

    /**
     * Internal implementation of all the getPropValue variants.
     */
    private static RDFNode doGetPropValue(StmtIterator stmtIterator) {
        RDFNode result = null;
        if (stmtIterator.hasNext()) {
            result = stmtIterator.next().getObject();
        }
        stmtIterator.close();
        return result;
    }

    /**
     * Convert an (assumed well formed) RDF list to a java list of RDFNodes
     * @param root -- the root node of the list
     * @param context the graph containing the list assertions
     */
    public static List<RDFNode> convertList(RDFNode root, OntModel m) {
        return convertList(root, m, new LinkedList<RDFNode>());
    }

    /**
     * Convert an (assumed well formed) RDF list to a java list of RDFNodes
     */
    private static List<RDFNode> convertList( RDFNode rdfNode, OntModel m, List<RDFNode> sofar ) {
        if (rdfNode == null || rdfNode.equals(RDF.nil)) return sofar;
        RDFNode next = getPropValue(rdfNode, RDF.first, m);
        if (next != null) {
            sofar.add(next);
            return convertList(getPropValue(rdfNode, RDF.rest, m), m, sofar);
        } else {
            return sofar;
        }
    }
    
    /**
     * Method to replace an extension on a file name
     * @param fn -- the file name
     * @param oldExt -- the old extension (without the leading "."
     * @param newExt -- the new extension
     * @return -- the new file name
     * @throws InvalidNameException 
     */
    public static String replaceFileExtension(String fn, String oldExt, String newExt) throws InvalidNameException {
		if (fn.endsWith("." + oldExt)) {
			return fn.substring(0, fn.length() - oldExt.length()) + newExt;
		}
		else {
			throw new InvalidNameException("File name '" + fn + "' does not have extension '" + oldExt + "'");
		}
    }

    /**
     * Method to take in a list of number of the form [3.45678, 4.56789, 5.6789] and return a list of numbers with the number of
     * specified significant figures. If there is only one number in the list the square brackets are dropped.
     * @param numberList
     * @param numSigFig
     * @return
     */
    public static String formatNumberList(String numberList, int numSigFig) {
     	boolean needsSquareBrackets = false;
    	String nl = numberList.trim();
    	if (nl.startsWith("[") && nl.endsWith("]")) {
    		nl = nl.substring(1, nl.length() - 1);
    	}
    	String[] numbers = nl.split(",");
    	if (numbers.length > 1) {
    		needsSquareBrackets = true;
    	}
    	for (int i = 0; i < numbers.length; i++) {
    		String num = numbers[i];
    		numbers[i] = formatNumber(num, numSigFig);
    	}
    	StringBuilder sb = new StringBuilder();
    	if (needsSquareBrackets) {
    		sb.append("[");
    	}
		for (int i = 0; i < numbers.length; i++) {
			if (i > 0) {
				sb.append(", ");
			}
			sb.append(numbers[i]);
		}
		if (needsSquareBrackets) {
			sb.append("]");
		}
    	return sb.toString();
    }

    /**
     * Method to format a number represented as a String to the number of significant figures specified
     * @param num
     * @param numSigFig
     * @return
     */
	public static String formatNumber(String num, int numSigFig) {
		double val = Double.parseDouble(num);
//		return Double.toString(Math.round(val*Math.pow(10, numSigFig))/Math.pow(10, numSigFig));
//		BigDecimal d = BigDecimal.valueOf(val).setScale(numSigFig, RoundingMode.HALF_UP);
//		double scaled = d.doubleValue();
//		return Double.toString(scaled);
	    String s = String.format("%."+numSigFig+"G", BigDecimal.valueOf(val));
	    return s;
	}
}
