/************************************************************************
 * Copyright (c) 2007-2015 - General Electric Company, All Rights Reserved
 *
 * Project: SADL Knowledge Server
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language. 
 * 
 * The SADL Knowledge Server is a set of Java classes implementing 
 * a service interface for deploying ontology-based knowledge bases
 * for use in a client-server environment.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/09 14:06:51 $
 ***********************************************************************/

package com.ge.research.sadl.server.client;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.naming.NameNotFoundException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.InvalidDerivationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.server.ISadlServer;
import com.ge.research.sadl.server.NamedServiceNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;

/**
 * This class provides a command line interface to the SadlSever (Version 2). 
 * 
 * @author crapo
 *
 */
public class SadlServerMain {
	static Logger logger = LoggerFactory.getLogger(SadlServerMain.class);
	
	protected ISadlServer sadlSvc = null;
	protected String thisSessionKey = null;
	
	String serverClassName = null;
	String kbroot = null;
	String serviceName = null;
	int derivationLevel = 0;
	String verboseFlag = "NONE";
	
	private String outputFormat = null;
		
	private String thisQuery = null;
	

	/**
	 * This is the main method, inputs are command-line arguments. Note that the method "process" duplicates
	 * this for ease in calling from Java code.
	 * 
	 * Main arguments needed for SadlServer V2; first 2  and 4th are required, rest are optional but an earlier argument 
	 *   must be present (could be empty) for a later to be included.
	 * 
	 * arg0. service class name -- the fully qualified name of the class implementing the service defined in com.ge.research.sadl.server.ISadlServer
	 * 	e.g, com.ge.research.sadl.sadlserver.SadlServerImpl
	 * arg1. service identifier -- either 1) kbase root--the path to the directory on the local file system in or under under which model folders and/or service definition files are located
	 *                                 or 2) the URL of the Web service to invoke
	 * arg2. service name -- either 1) the name of the named service to be invoked. This maps to a kbase and an entry point model.
	 *                           or 2) the URI of the model to be loaded (only valid for local service)
	 * arg3. input data (n-triple) file name--fully qualified path and name of data input file on local file system (optional)
	 * arg4. either the SPARQL query to be used to retrieve results OR the keyword for meta-data retrieval followed by one or two names as required (see method).
	 * 	The keyword must be one of the following: InstancesOfClass, LeafClasses, PropertyDomain, PropertyRange, RequiredRangeOfClass, AllowedRangeOfClass, 
	 * 	AllowedDataValuesOfClass, AllowedObjectValuesOfClass, IsDatatypeProperty, IsObjectProperty
	 * 		
	 * arg5. output data file name -- the fully qualified path and name of the file in which output data is to be placed (optional)
	 * arg6. output format -- OWL results file output format, SPARQL CONSTRUCT query only (optional, only used for "construct" queries)
	 *  Possible values are "N-TRIPLE", "TURTLE", "N3", "RDF/XML", or "RDF/XML-ABBREV"
	 * arg7. debug output level -- "NONE", "TRACE", or "DEBUG" (optional)
	 * arg8. derivation level -- (optional, 0, 1, or 2)
	 * arg9. derivation output file name -- (optional)
	 *
	 * @param args -- an array of values as described below
	 * 
	 * @throws NameNotFoundException 
	 * @throws InvalidDerivation 
	 */
	public static void main(String[] args) throws IOException, NameNotFoundException {
		logger.info("SadlServerMain main, version $Revision: 1.4 $");
		SadlServerMain client = new SadlServerMain();
		try {
			client.process(args);
		} catch (QueryParseException e) {
			logger.error("QueryParseException: " + e.getMessage());
			logger.debug(e.toString());
		} catch (ReasonerNotFoundException e) {
			logger.error("ReasonerNotFoundException: " + e.getMessage());
			logger.debug(e.toString());
		}
		catch (InvalidNameException e) {
			
		}
		catch (Throwable t) {
			logger.error("SessionNotFoundException: " + t.getMessage());
			logger.debug(t.toString());
		}
	}
	
	/**
	 * This method has the same meaning of args as for the main method but is not a static invocation and returns the session key.
	 * @param args -- see main method documentation
	 * @return the session key of the session established by this processing call
	 * @throws ApplicationLaunchException
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws QueryParseException 
	 * @throws ReasonerNotFoundException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws NamedServiceNotFoundException 
	 * @throws InvalidDerivation 
	 */
	public String process(String[] args) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, NamedServiceNotFoundException {
		// arg[0]: ISadlServer implementing class fully qualified name
		serverClassName = null;
		if (args.length > 0 && args[0] != null && args[0].length() > 0) {
			serverClassName = args[0];
		}
		
		// args[1]: knowledge base[s] locator (kbroot)
		kbroot = null;
		if (args.length > 1 && args[1] != null && args[1].length() > 0) {
			kbroot = args[1];
		}
		else {
			return null;
		}
		
		// args[2]: service name of named service or model name
	    serviceName = null; 
		if (args.length > 2 && args[2] != null && args[2].length() > 0) {
			serviceName = args[2];
		}
		else {
			return null;
		}
		
		// args[3]: input file  (ok for this to not be supplied--no input file, but must be done with empty string)
		String inFile = null;
		if (args.length > 3 && args[3] != null && args[3].length() > 0) {
			inFile = args[3];
		}

		// args[4]: query or keyword (ok for this to not be supplied--no query)
		if (args.length > 4 && args[4] != null && args[4].length() > 0) {
			thisQuery = args[4];
		}

		// args[5]: output file (ok for this to not be supplied--if there is a query use the default output filename)
		String outFile = null;
		if (args.length > 5 && args[5] != null && args[5].length() > 0) {
			outFile = args[5];
		}
		
		// args[6]: output format for query
		//		construct: "N-TRIPLE", "TURTLE", "N3", "RDF/XML", or "RDF/XML-ABBREV"
		//		non-construct: "CSV", CSV-H" (comma-separated values with header)
		if (args.length > 6 && args[6] != null && args[6].length() > 0) {
			if (isOwlFormatValid(args[6])) {
				outputFormat = args[6];
			}
			else {
				logger.equals("OWL output file format '" + args[6] +
						"' is not a supported format. Use one of \"N-TRIPLE\", \"TURTLE\", \"N3\", \"RDF/XML\", \"RDF/XML-ABBREV\".");
			}
		}
						
		// args[7]: verboseFlag ("NONE", "TRACE", or "DEBUG") (optional)
		if (args.length > 7 && args[7] != null && args[7].length() > 0) {
			verboseFlag = args[7].trim().toUpperCase(Locale.US);
			if (verboseFlag.equals("DEBUG")) {
//				logger.setLevel(Level.DEBUG);
				logger.debug("Arguments:");
				for (int i = 0; i < args.length; i++) {
					logger.debug("   arg " + i + ": " + args[i]);
				}
				
				logger.debug("main calling process with " + args.length + " command-line arguments.");
				logger.debug("Current directory (\".\") is '" + new File(".").getAbsoluteFile() + "'");
			}
		}
		
		// args[8]: derivation level (0 for no derivations)
		if (args.length > 8 && args[8] != null && args[8].length() > 0) {
			try {
				derivationLevel = Integer.parseInt(args[8].trim());
			}
			catch (NumberFormatException e) {
				if (logger.isDebugEnabled()) logger.debug("Invalid derivation level: " + args[8]);
			}
		}
		
		// args[9]: derivation output file
		//	used below only if derivations are specified
				
		try {
			sadlSvc = getServerClassInstance(serverClassName, kbroot); 
			if (logger.isDebugEnabled()) {
				logger.debug("Service class: " + sadlSvc.getClassName());
			}
	//		if (!verboseFlag.equals("NONE")) {
	//			sadlSvc.setActiveHandler(true);
	//		}
			
			List<ConfigurationItem> preferences = null;
			if (derivationLevel > 0) {
				preferences = new ArrayList<ConfigurationItem>();
				String[] catHier = new String[1];
				catHier[0] = "Jena";		// or "JenaForGE"
				ConfigurationItem ci = new ConfigurationItem(catHier);
				ci.addNameValuePair(ci.new NameValuePair("pDerivationLogging", (derivationLevel == 1 ? "Shallow" : "Deep")));
			}
			if (!verboseFlag.equals("NONE")) {
				sadlSvc.collectTimingInformation(true);
			}
			if (serviceName.startsWith("http://")) {
				setSessionKey(sadlSvc.selectServiceModel(kbroot, serviceName, preferences));
			}
			else {
	//			sadlSvc.setKbaseRoot(kbroot);
				setSessionKey(sadlSvc.selectServiceModel(serviceName, preferences));
			}
		}
		catch (Throwable t) {
			logger.error("Exception setting up server: " + t.getMessage());
		}

		try {
			if (inFile != null) {
				File rmdFile = new File(inFile);
				if (rmdFile.exists()) {
					DataSource src = new FileDataSource(rmdFile);
					if (verboseFlag.compareToIgnoreCase("DEBUG") == 0) {
						if (logger.isDebugEnabled()) {
							logger.debug("Input data to stdout:");
							writeDataSource(src, System.out);
						}
					}
					if (inFile.toLowerCase().endsWith("n-triple")) {
						sadlSvc.sendData(src, "N-TRIPLE");
					}
					else if (inFile.toLowerCase().endsWith("n3")) {
						sadlSvc.sendData(src, "N3");
					}
					else {
						sadlSvc.sendData(src);
					}
				}
				else {
					logger.error("File '" + inFile + "' does not exist!");
				}
			}
		}
		catch (Throwable t) {
			logger.error("Exception sending data to server: " + t.getMessage());
		}

		try {
			if (thisQuery != null) {
				if (thisQuery.toLowerCase().startsWith("construct")) {
					if (outputFormat == null) {
						outputFormat = "N-TRIPLE";
					}
					sadlSvc.setOwlFileOutputFormat(outputFormat);
					if (outFile == null) {
						outFile = kbroot + File.separator + generateOutputFilename(inFile, outputFormat);
					}
					DataSource output = sadlSvc.construct(thisQuery);
					if (output != null) 
					{			
			//			if (logger.isDebugEnabled()) logger.debug("getData returned DataSource '" + output.getName() + "'");
						writeDataSource(output, outFile);
						if (verboseFlag.compareToIgnoreCase("DEBUG") == 0) {
							if (logger.isDebugEnabled()) {
								logger.debug("Output data to stdout:");
								writeDataSource(output, System.out);
							}
						}
					}
				}
				else {
					if (outFile == null) {
						outFile = kbroot + File.separator + generateOutputFilename(inFile, outputFormat);
					}
					if (thisQuery.toLowerCase().startsWith("select")) {
						thisQuery = sadlSvc.prepareQuery(thisQuery);
						String[][] rs = getDataAsStringArray(thisQuery);
						displayDataStringArray(rs);
						resultsToFile(rs, outFile, outputFormat);
					}
					else {
						StringTokenizer stknzr = new StringTokenizer(thisQuery);
						thisQuery = stknzr.nextToken();
						Object results = null;
						if (thisQuery.equals("InstancesOfClass")) {
							results = getInstancesOfClass(stknzr.nextToken());
						}
						else if (thisQuery.equals("LeafClasses")) {
							results = getLeafClassesOfTaxonomy(stknzr.nextToken());
						}
						else if (thisQuery.equals("PropertyDomain")) {
							results = getPropertyDomain(stknzr.nextToken());
						}
						else if (thisQuery.equals("PropertyRange")) {
							results = getPropertyRange(stknzr.nextToken());
						}
						else if (thisQuery.equals("RequiredRangeOfClass")) {
							results = getRequiredRangeClassesOfPropertyOfClass( 
									stknzr.nextToken(), stknzr.nextToken());
						}
						else if (thisQuery.equals("AllowedRangeOfClass")) {
							results = getAllowedRangeClassesOfPropertyOfClass( 
									stknzr.nextToken(), stknzr.nextToken());
						}
						else if (thisQuery.equals("AllowedDataValuesOfClass")) {
							results = getAllowedValuesOfDataPropertyOfClass(
									stknzr.nextToken(), stknzr.nextToken());
						}
						else if (thisQuery.equals("AllowedObjectValuesOfClass")) {
							results = getAllowedValuesOfObjectPropertyOfClass( 
									stknzr.nextToken(), stknzr.nextToken());
						}
						else if (thisQuery.equals("IsDatatypeProperty")) {
							boolean bval = isDatatypeProperty(stknzr.nextToken());
							results = new Boolean(bval);
						}
						else if (thisQuery.equals("IsObjectProperty")) {
							boolean bval = isObjectProperty(stknzr.nextToken());
							results = new Boolean(bval);
						}
						if (results != null) {
							resultsToFile(results, outFile, outputFormat);
						}
					}
				}
			}
		}
		catch (Throwable t) {
			logger.error("Exception querying server: " + t.getMessage());
		}

		
		if (verboseFlag.compareToIgnoreCase("TRACE") == 0 ||
				verboseFlag.compareToIgnoreCase("DEBUG") == 0) {
			if (logger.isDebugEnabled()) logger.debug("Service: " + sadlSvc.getServiceVersion());
			if (logger.isDebugEnabled()) logger.debug("Model Folder (KBase Identifier): " + sadlSvc.getKBaseIdentifier());
			if (logger.isDebugEnabled()) logger.debug("Model Name: " + sadlSvc.getServiceModelName());
			if (logger.isDebugEnabled())
				try {
					logger.debug("Reasoner Version: " + sadlSvc.getReasonerVersion());
				} catch (ConfigurationException e) {
					logger.error("Failed to get Reasoner Version: " + e.getLocalizedMessage());
				}
		}
		
		if (verboseFlag.compareToIgnoreCase("TRACE") == 0 ||
				verboseFlag.compareToIgnoreCase("DEBUG") == 0) {
			if (logger.isDebugEnabled()) logger.debug("query = '" + thisQuery + "'");
			if (logger.isDebugEnabled()) logger.debug("DataSource output = sadlSvc.getData(query);");
			if (logger.isDebugEnabled()) logger.debug("returned from DataSource output = sadlSvc.getData(query);");
		}
		
		try {
			if (!verboseFlag.equals("NONE")) {
				ReasonerTiming[] timingInfo = sadlSvc.getTimingInformation();
				if (timingInfo != null) {
					long totalTime = 0;
					logger.info("Timing information:");
					for (int i = 0; i < timingInfo.length; i++) {
						ReasonerTiming rt = timingInfo[i];
						logger.info("    " + rt.getMilliseconds() + " (ms): " + rt.getDescription());
						totalTime += rt.getMilliseconds();
					}
					logger.info("    Total time (ms):" + totalTime);
				}
	
			}
		}
		catch (Throwable t) {
			logger.error("Exception getting timing information from server: " + t.getMessage());
		}

		try {
			if (derivationLevel > 0) {
				DataSource derivations = null;
				try {
					derivations = sadlSvc.getDerivations();
				} catch (ConfigurationException e) {
					logger.error("ConfigurationException: " + e.getLocalizedMessage());
				} catch (InvalidDerivationException e) {
					logger.error("InvalidDerivationException: " + e.getLocalizedMessage());
				}
				if (derivations != null) {
					String dfn = "c:/derivations.txt";
					if (args.length > 9 && args[9] != null && args[9].length() > 0) {
						dfn = args[9];
					}
					if (logger.isDebugEnabled()) logger.debug("Derivations written to: " + dfn);
					writeDataSource(derivations, dfn);
				}
			}
		}
		catch (Throwable t) {
			logger.error("Exception getting derivations from server: " + t.getMessage());
		}

		return getSessionKey();
	}
	
	private void resultsToFile(Object results, String outFile, String outputFormat) throws IOException {
		File outputFile = new File(outFile);
    	if (outputFile.exists() && !outputFile.isFile()) {
    		throw new IllegalArgumentException("Output file cannot be a directory: " + outputFile);
    	}
    	if (outputFile.exists()) {
    		outputFile.delete();
    	}
    	if (!outputFile.exists()) {
    		outputFile.createNewFile();
    	}
    	if (!outputFile.canWrite()) {
    		throw new IllegalArgumentException("File cannot be written: " + outputFile);
    	}
    	
    	StringBuilder sb = new StringBuilder();
    	if (results instanceof Boolean) {
    		sb.append(((Boolean)results).toString());
    	}
    	else if (results instanceof String[][]) {
    		if (((String[][])results).length > 0) {
    			int startrow = 0;
    			if (outputFormat != null && outputFormat.equals("CSV")) {
    				startrow = 1;
    			}
    			for (int i = startrow; i < ((String[][])results).length; i++) {
    				for (int j = 0; j < ((String[][])results)[i].length; j++) {
    					if (j > 0) {
    						sb.append(", ");
    					}
    					sb.append(((String[][])results)[i][j]);
    				}
    				sb.append(System.getProperty("line.separator"));
    			}
    			logger.info(sb.toString());
    		}
   		
    	}
    	else if (results instanceof Object[]) {
    		for (int i = 0; i < ((Object[])results).length; i++) {
    			if (i > 0) {
    				sb.append("\n");
    			}
    			sb.append(((Object[])results)[i].toString());
    		}
    	}

    	//declared here only to make visible to finally clause; generic reference
    	Writer output = null;
    	try {
    		//use buffering
    		//FileWriter always assumes default encoding is OK!
    		output = new BufferedWriter( new FileWriter(outputFile) );
    		output.write( sb.toString() );
    	}
    	finally {
    		//flush and close both "output" and its underlying FileWriter
    		if (output != null) output.close();
    	}
	}

	private String generateOutputFilename(String inFile, String outputFormat2) {
		String ifn;
		if (inFile != null) {
			ifn = new File(inFile).getName();
			int lastdot = ifn.lastIndexOf('.');
			if (lastdot > 0) {
				ifn = ifn.substring(0, lastdot - 1);
			}
		}
		else {
			ifn = getSessionKey();
		}
		ifn += "_output";
		if (outputFormat != null) {
			if (outputFormat.equals("N-TRIPLE")) {
				ifn += ".ntriple";
			}
			else if (outputFormat.equals("TURTLE")) {
				ifn += ".turtle";
			}
			else if (outputFormat.equals("N3")) {
				ifn += ".n3";
			}
			else if (outputFormat.equals("RDF/XML")) {
				ifn += ".owl";
			}
			else if (outputFormat.equals("RDF/XML-ABBREV")) {
				ifn += ".owl";
			}
			else if (outputFormat.equals("CSV")) {
				ifn += ".csv";
			}
			else if (outputFormat.equals("CSV-H")) {
				ifn += ".csv";
			}
			else {
				ifn += ".txt";
			}
		}
		else {
			ifn += ".txt";
		}
		return ifn;
	}

	public static void writeDataSource(DataSource out, String fileOut){// throws IOException {
		String outFile = fileOut;
		OutputStream os = null;
		File file;
		try {
			file = new File(outFile);
			file.createNewFile();
			if (file.exists()) {
			    os = new FileOutputStream(file);
			    writeDataSource(out, os);
			}
		} catch (FileNotFoundException e) {
			logger.error("Exception in writeDataSource():"+e.getLocalizedMessage());
		} catch (IOException e) {
			logger.error("Exception in writeDataSource():"+e.getLocalizedMessage());
		} finally{
			try {
				if(os!=null)
					os.close();
			} catch (IOException e) {
				logger.error("Exception in writeDataSource():"+e.getLocalizedMessage());
			}
		}
	}
	
	public static void writeDataSource(DataSource out, OutputStream os) throws IOException {
		InputStream is = out.getInputStream();
	    byte[] buf = new byte[1024];
	    int ival = 0;        
	    while((ival=is.read(buf))!=-1) 
	    {
	        os.write(buf, 0, ival);	            
	    }
	    is.close();
	}
	
	public String[][] getDataAsStringArray(String query) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, SessionNotFoundException, QueryCancelledException, IOException, ConfigurationException, InvalidNameException, URISyntaxException {
		ResultSet results = sadlSvc.query(query);
		if (results != null) {
			int colCnt = results.getColumnCount();
			int rowCnt = results.getRowCount();
			String[][] modified = new String[rowCnt + 1][colCnt]; 
			for (int i = 0; i < colCnt; i++) {
				modified[0][i] = results.getColumnNames()[i];
			}
			for (int i = 0; i < rowCnt; i++) {
				for (int j = 0; j < colCnt; j++) {
					modified[i + 1][j] = results.getResultAt(i, j).toString();
				}
			}
			return modified;
		}
		return null;
	}
	
	private void displayDataStringArray(String[][] data) {
		if (data != null && data.length > 0) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < data.length; i++) {
				for (int j = 0; j < data[i].length; j++) {
					if (j > 0) {
						sb.append(", ");
					}
					sb.append(data[i][j]);
				}
				sb.append("\n");
			}
			logger.info(sb.toString());
		}
		else {
			logger.warn("data for display is empty");
		}
	}
//	
//	public String[][] getBaseSchemaModelDataAsStringArray(String sessionKey, String query) throws NameNotFoundException {
//		return sadlSvc.askBaseSchemaModelAsStrings(sessionKey, query);
//	}
	
//	/**
//	 * This method allows derivation information (if derivation is enabled--args[9] to process > 0) to be retrieved for one or
//	 * more matching statements.
//	 * @param sessionKey
//	 * @param subj
//	 * @param pred
//	 * @param obj
//	 * @return the derivation of the triple(s)
//	 * @throws InvalidDerivation 
//	 */
//	public String getStatementDerivation(String sessionKey, String subj, String pred, Object obj) throws InvalidDerivation {
//		if (sadlSvc == null) {
//			return "No service available to return Derivation.";
//		}
//		return sadlSvc.getStatementDerivation(sessionKey, subj, pred, obj);
//	}
	
	/**
	 * This method returns the query associated with this service model name
	 * @return -- query associated with the given modelName
	 */
	public static String getModelQuery(String modelName) {
//		String query = "construct { ?s ?p ?o} where { {<http://djstservice#ESN123456> <iws:module> ?m . ?m <iws:component> ?s . ?s ?p ?o . FILTER (sameTerm(?p, <iws:remaining_cycles>))}  UNION {<http://djstservice#ESN123456> <iws:module> ?m . ?m <iws:component> ?s . ?s ?p ?o . FILTER (sameTerm(?p, <llp_life_limit>))} UNION  { <http://djstservice#ESN123456> <iws:module> ?m . ?m <iws:component> ?s . OPTIONAL {?s ?p ?o . FILTER (sameTerm(?o, <iws:Replace>) ) }  . OPTIONAL {?s ?p ?o . FILTER (sameTerm(?o, <iws:Continue>) ) } . FILTER (sameTerm(?p, <iws:disposition>) ) } UNION { <http://djstservice#ESN123456> <iws:module> ?s . OPTIONAL { ?s ?p ?o . FILTER (sameTerm(?o, <iws:Heavy> ) ) } . OPTIONAL { ?s ?p ?o . FILTER (sameTerm(?o, <iws:Light> ) ) } . OPTIONAL { ?s ?p ?o . FILTER (sameTerm(?o, <iws:TC> ) ) } } UNION { OPTIONAL {?s ?p ?o . FILTER (sameTerm(?p, <http://www.w3.org/2000/01/rdf-schema#comment>) && regex(str(?s), \"djstservice\") )} } }";
		String resourceBundleName = "queries";
		ResourceBundle rb = ResourceBundle.getBundle (resourceBundleName) ;
		String query = (String) rb.getString(modelName);
		return query;
	}
	
	public void setOutputFormat(String outputFormat) {
		this.outputFormat = outputFormat;
	}

	public String getOutputFormat() {
		return outputFormat;
	}

	private void setSessionKey(String sessionKey) {
		this.thisSessionKey = sessionKey;
	}

	public String getSessionKey() {
		return thisSessionKey;
	}
	
		/**
	 * This method returns all the leaf classes of the classification hierarchy starting at root
	 * @param root -- the localname, prefix:localname, or complete URI of the root class
	 * @return - a String array containing the local names of the leaf classes
	 * @throws IOException
	 * @throws NameNotFoundException
		 * @throws ReasonerNotFoundException 
		 * @throws QueryParseException 
		 * @throws ConfigurationException 
		 * @throws InvalidNameException 
		 * @throws SessionNotFoundException 
		 * @throws QueryCancelledException 
		 * @throws URISyntaxException 
	 */
	public String[] getLeafClassesOfTaxonomy(String root) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, URISyntaxException {
		String query = "select ?et where {?et <rdfs:subClassOf> <" + root + "> . OPTIONAL {?et2 <rdfs:subClassOf> ?et . FILTER ((?et2 != <owl:Nothing> && ?et2 != ?et)) } FILTER (!bound(?et2)) }";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		return null;
	}

	/**
	 * This method returns all the instances of the given class
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array containing the localnames of the instances of the class
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public String[] getInstancesOfClass(String cls) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, URISyntaxException {
		String query = "select ?i where { ?i <rdf:type> <" + cls + "> }";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		return null;
	}
	
	/**
	 * This method returns true if and only if the property is an ObjectProperty
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - true if an ObjectProperty else false
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public boolean isObjectProperty(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException {
		String query = "select ?t where {<" + property + "> <rdf:type> ?t . FILTER(?t = <http://www.w3.org/2002/07/owl#ObjectProperty>)}";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			return true;
		}
		return false;
	}
	
	/**
	 * This method returns true if and only if the property is an DatatypeProperty
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - true if an DatatypeProperty else false
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public boolean isDatatypeProperty(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException {
		String query = "select ?t where {<" + property + "> <rdf:type> ?t . FILTER(?t = <http://www.w3.org/2002/07/owl#DatatypeProperty>)}";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			return true;
		}
		return false;
	}
	
	/**
	 * This method returns the class(es) which are the domain of the property, if any.
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - a String array containing the domain classes
	 * @throws NameNotFoundException 
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public String[] getPropertyDomain(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException {
		String query = "select ?d where { <" + property + "> <rdfs:domain> ?d }";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		return null;		
	}
	
	/**
	 * This method returns the class(es) or XSD types which are the range of the property, if any.
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - a String array containing the range classes or XSD types
	 * @throws NameNotFoundException 
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public String[] getPropertyRange(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException {
		String query = "select ?r where { <" + property + "> <rdfs:range> ?r }";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		return null;		
	}
	
	/**
	 * This method returns all the classes that must be represented in the range of the given property on the given class
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array containing the range classes that are required to be represented in the values of the property
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public String[] getRequiredRangeClassesOfPropertyOfClass(String cls, String property) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, URISyntaxException {
		String query = "select ?v where { <" + cls + "> <rdfs:subClassOf> ?r . ?r <rdf:type> <owl:Restriction> . ?r <owl:someValuesFrom> ?v . ?r <owl:onProperty> <" + property + ">}";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		return null;
	}
	
	/**
	 * This method returns all the names of classes that are allowed in the range of the given property on the given class
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array containing the range classes that are allowed to be represented in the values of the property
	 * @throws NameNotFoundException 
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryParseException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public String[] getAllowedRangeClassesOfPropertyOfClass(String cls, String property) throws NameNotFoundException, InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException {
		String query = "select ?v where { <" + cls + "> <rdfs:subClassOf> ?r . ?r <rdf:type> <owl:Restriction> . ?r <owl:allValuesFrom> ?v . ?r <owl:onProperty> <" + property + ">}";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		return null;
	}
	
	/**
	 * This method returns all the names of instances that are allowed values of the given property on the given class
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array of the allowed value instances
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public String[] getAllowedValuesOfObjectPropertyOfClass(String cls, String property) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, URISyntaxException {
		String query = "select ?v where { <" + cls + "> <rdfs:subClassOf> ?r . ?r <rdf:type> <owl:Restriction> . ?r <owl:onProperty> <" + property + "> . ?r <owl:allValuesFrom> ?o . ?o <owl:oneOf> ?l . ?l <http://jena.hpl.hp.com/ARQ/list#member> ?v}";
		query = sadlSvc.prepareQuery(query);
		String[][] results = getDataAsStringArray(query);
		if (results != null) { 
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		return null;
	}
	
	/**
	 * This method returns all the allowed values of the given property on the given class. Note that in the case of xsd:string values,
	 * the values will be wrapped in double quotes.
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - an Object array of the allowed values, which will be of type Integer, String, Float, etc.
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public Object[] getAllowedValuesOfDataPropertyOfClass(String cls, String property) throws QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException {
		String query = "select distinct ?v where { <" + cls + "> <rdfs:subClassOf> ?r . ?r <rdf:type> <owl:Restriction> . ?r <owl:onProperty> <" + property + "> . ?r <owl:hasValue> ?v}";
		query = sadlSvc.prepareQuery(query);
		ResultSet results = sadlSvc.query(query);
		if (results != null) {
			int size = results.getRowCount();
			Object[] retVal = new Object[size];
			int i = 0;
			while (results.hasNext()) {		
				retVal[i++] = results.next()[0];
			}
	 		return retVal;
		}
		else {		
			query = "select distinct ?v where { <" + cls + "> <rdfs:subClassOf> ?r . ?r <rdf:type> <owl:Restriction> . ?r <owl:onProperty> <" + property + "> . ?r <owl:allValuesFrom> ?dr . ?dr <owl:oneOf> ?l . ?l <http://jena.hpl.hp.com/ARQ/list#member> ?v}";
			query = sadlSvc.prepareQuery(query);
			results = sadlSvc.query(query);
			if (results != null) {
				if (results != null) {
					int size = results.getRowCount();
					Object[] retVal = new Object[size];
					int i = 0;
					while (results.hasNext()) {		
						retVal[i++] = results.next()[0];
					}
			 		return retVal;
				}
			}
		}
		return null;
	}

//	/**
//	 * This method obtains the available service models as specified in the servers sadlmodels.properties file.
//	 * @return array containing available service model names
//	 */
//	public String[][] getAvailableServiceModels() {
//		return sadlSvc.getAvailableServiceModels();
//	}
	
	/**
	 * Method to test an OWL output format to see if it is supported
	 */
	static public boolean isOwlFormatValid(String format) {
		if (format != null && 
				(format.equalsIgnoreCase("N-TRIPLE") ||
				format.equalsIgnoreCase("TURTLE") ||
				format.equalsIgnoreCase("N3") ||
				format.equalsIgnoreCase("RDF/XML") ||
				format.equalsIgnoreCase("RDF/XML-ABBREV"))) {
			return true;
		}
		return false;
	}

	private ISadlServer getServerClassInstance(String serviceClassname, String serviceIdentifier) {
		try {
			return (ISadlServer)this.getClass().getClassLoader().loadClass(serviceClassname).getConstructor(String.class).newInstance(serviceIdentifier);
		} catch (InstantiationException e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;	
	}
}