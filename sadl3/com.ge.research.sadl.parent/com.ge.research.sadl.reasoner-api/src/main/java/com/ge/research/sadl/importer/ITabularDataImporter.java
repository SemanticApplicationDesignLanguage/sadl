package com.ge.research.sadl.importer;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.List;

import javax.activation.DataSource;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;

public interface ITabularDataImporter {

	String HTTP_URI_SCHEME = "http://";

	/**
	 * Method to create a Validation and its key for storage. Validations can be one of:
	 * 	validate <elementID> not blank <action>
	 *  validate <elementID> exists <action>
	 *  validate <elementID> <xsdtype> <action>
	 *  validate <subj> <pred> <obj> <action>
	 *  
	 *  where <xsdtype> can be one of string, boolean, decimal, integer, dateTime, time, date, hexBinary, base64Binary, or anyURI
	 *  and <action> can be "skip" or "abort"
	 * 
	 * @param rawTemplate
	 * @return
	 * @throws TemplateException
	 */
	Object[] makeValidate(String rawTemplate, int modelArrayPosition) throws TemplateException;

	/**
	 * Set the folder containing the 
	 * @param _modelDir
	 * @throws IOException
	 */
	void setModelFolder(String _modelDir) throws IOException;

	/**
	 * Method to specify the import file to be used as input.
	 * @param _importFilename -- path and name to import data file
	 * @param _includesHeader -- true if the import data file first row is header information (which will be used as variable names)
	 * @throws IOException
	 */
	void setImportFilename(String _importFilename, boolean _includesHeader) throws IOException;

	/**
	 * Set the import input to a DataSource data stream.
	 * 
	 * @param importDs
	 * @param _includesHeader
	 * @throws IOException
	 */
	void setImportDataSource(DataSource importDs, boolean _includesHeader) throws IOException;

	/**
	 * Set the namespace to be used as the base URI of the created model.
	 * 
	 * @param ns
	 * @throws MalformedURLException
	 */
	void setImportModelNamespace(String ns) throws MalformedURLException;

	/**
	 * Method to get the final saveAs file name, which might have been changed (for TDB) by the importer
	 * @return
	 * @throws ConfigurationException
	 */
	String getSaveAsFileName() throws ConfigurationException;

	/**
	 * Set the project-relative filename for the output file; 
	 *   ".sadl" files are translated to SADL, 
	 *   ".owl" filenames are translated to OWL in RDF/XML format
	 *   ".n-triple" filenames are translated to OWL in N-TRIPLE format
	 *   ".n3" filenames are translated to N3 format
	 *   ".TDB" filenames are TDB repository folder names
	 *   
	 * @param saveAsFileName
	 */
	void setSaveAsFileName(String saveAsFileName);

	/**
	 * Set the model's imports. Prefixes (aliases) will be retrieved from the
	 * policy file specified.
	 * 
	 * @param _imports -- a list of import URIs
	 * @throws ConfigurationException
	 */
	void setImports(List<String> _imports) throws ConfigurationException;

	/**
	 * Set the model's imports. Prefixes (aliases) will be retrieved from the
	 * policy file specified.
	 * 
	 * @param _imports -- a comma-separated list of import URIs
	 * @throws ConfigurationException
	 */
	void setImports(String _imports) throws ConfigurationException;

	/**
	 * Set the RDF Triple patterns to be used in generating the OWL output. 
	 * The Triple patterns can be preceeded by "validate" and "transform" statements.
	 * Variables in the pattern that are to be replaced with content from the 
	 * import file are of the form "[Prefix]<ID>[Postfix]" where the square brackets
	 * indicate that the prefix to the required identifying section are manditory
	 * and the "ID" can be one of:
	 *   1) the name of a column header in the import file
	 *   2) the import file column identified by letter, e.g., "A", "Z", "AA"
	 *   3) a number identifying a blank node that will be constructed to link triples
	 *   4) the result of a transform
	 * For example, if the header of the the first two columns in the import data were "Name"
	 * and "SSN", the data in the second row, first two columns were "JohnDoe" and "123-45-6789",
	 * and the imported ontology (imp) included the class "Person" and the property "hasSSN"
	 * with range xsd:string, then the template
	 * 		<Name> rdf:type Person, <Name> hasSSN <SSN>
	 * would produce, from the first row, the triples
	 * 		:JohnDoe rdf:type imp:Person
	 * 		:JohnDoe imp:hasSSN "123-45-6789"^^xsd:string
	 * 
	 * @param template -- A set of comma-separated RDF Triple patterns to be used to generate the OWL output.
	 * @throws ConfigurationException
	 * @throws TemplateException 
	 */
	void setTemplates(String template) throws ConfigurationException, TemplateException;

	/**
	 * Save the OWL model created from the import data file in the file specified.
	 * 
	 * @param saveAsFileName -- the name of the file with path, relative to the specified project directory, in which to write the output OWL file
	 * @return -- true if successful else false
	 * @throws InvalidNameException 
	 * @throws IOException 
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 */
	boolean saveOwlModel(String saveAsFileName)
			throws ConfigurationException, IOException, InvalidNameException, ReasonerNotFoundException;

	/**
	 * Method to save the mapping from the imported model namespace to the actual OWL file. This should be called if the client
	 * calls getOwlModel and then writes the file to a SADL project. It does not need to be called if the client calls
	 * saveOwlFile(..) or if the OWL file is not written to a SADL-IDE project.
	 * 
	 * @param publicUri
	 * @param actualUrl
	 * @return
	 * @throws ConfigurationException
	 */
	boolean saveSadlProjectOwlFileMapping(String publicUri, String actualUrl, String prefix)
			throws ConfigurationException;

	long processImport() throws ConfigurationException, IOException, InvalidNameException, AbortDataRowException,
			QueryCancelledException, ReasonerNotFoundException;

	/**
	 * Return the OWL model created from the import data file serialized in the specified format as a DataSource data stream.
	 * 
	 * @param format -- "RDF/XML", "RDF/XML-ABBREV", "N-TRIPLE", or "N3"; if null output will be "RDF/XML-ABBREV"
	 * @return
	 * @throws InvalidNameException 
	 * @throws IOException 
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 */
	DataSource getOwlModel(String format)
			throws ConfigurationException, IOException, InvalidNameException, ReasonerNotFoundException;

	/**
	 * Return the OWL model as an in-memory model structure.
	 * @return
	 * @throws ConfigurationException
	 * @throws IOException
	 * @throws InvalidNameException
	 * @throws ReasonerNotFoundException 
	 */
	Object getOwlModel() throws ConfigurationException, IOException, InvalidNameException, ReasonerNotFoundException;

	boolean enableTriplesAddedInOrderLogging(String filename) throws IOException;

	String getOwlModelFormat();

	void setOwlModelFormat(String owlModelFormat) throws InvalidNameException;

}