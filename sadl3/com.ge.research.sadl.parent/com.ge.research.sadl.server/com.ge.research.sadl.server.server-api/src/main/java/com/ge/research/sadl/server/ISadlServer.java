/************************************************************************
 * Copyright 2007-2015 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.server;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import com.ge.research.sadl.importer.TemplateException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.InvalidDerivationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TripleNotFoundException;

/**
 * This class defines the interface contract for the SadlServer knowledge
 * service. It uses knowledge bases along with configuration information
 * specifying a particular semantic reasoner to use with the knowledge
 * base. The knowledge will have been translated to the target representation
 * (e.g., OWL + SWRL) previously, e.g., by the SADL IDE.
 *
 * $Author: hathawa $
 * $Revision: 1.5 $ Last modified on   $Date: 2014/10/07 22:36:04 $
 *
 */
/**
 * @author 200005201
 *
 */
@SuppressWarnings("restriction")
public interface ISadlServer {
	
	/**
	 * This method sets the service name map. It is not normally necessary to call this from the client 
	 * as the service name map is normally set by scanning the kbaseroot and its subfolders when the call
	 * is made to setKbaseRoot. However, in theory this could be set from the client without setting a
	 * kbaseroot.
	 * 
	 * @param serviceNameMap -- service name map
	 */
	public void setServiceNameMap(Map<String, String[]> serviceNameMap);
	
	/**
	 * This method returns the service name map whose keys are the names of the services and whose values are 
	 * String arrays of size 2 with the 1st element being the actual URL of the knowledge base folder (the folder
	 * containing the OWL models, rule files, etc.; the knowledgeBaseIdentifier) and the 2nd element being the 
	 * public URI of the model which is the entry point into the kbase for the named service.
	 * 
	 * @return -- service name map
	 */
	public Map<String, String[]> getServiceNameMap();
	
	/**
	 * This method returns the name of the class implementing the ISadlService interface.
	 * 
	 * @return -- class name
	 * @throws SessionNotFoundException 
	 * @throws com.ge.research.sadl.sadlserver.SessionNotFoundException 
	 */
	abstract public String getClassName() throws SessionNotFoundException;
	
	/**
	 * This method returns the Source Code Control System (SCC) Version of the class implementing the ISadlService interface
	 * @return the CVS version number of the ISadlServer implementation
	 * @throws SessionNotFoundException 
	 */
	abstract public String getServiceVersion() throws SessionNotFoundException;

    /**
     * This method retrieves the results of a kbase query as a List array, 1st element being a List of
     * the column headings and second being a List of rows, each of which is a List of values. The query
     * string will depend upon the knowledge representation and reasoner; it might be SPARQL, Prolog, etc.
     *
     * @param query a query string
     * @return List[] with first element being the column titles (names of query variables) and second element a list of lists of values
     * @throws QueryCancelledException 
     * @throws QueryParseException 
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     */
    abstract public ResultSet query(String query) throws QueryCancelledException, QueryParseException, ReasonerNotFoundException, SessionNotFoundException;

    /**
     * Method to provide instance data and then execute a set of queries in an atomic (stateless) manner, returning a ResultSet for each query. The query
     * strings will depend upon the knowledge representation and reasoner; it might be SPARQL, Prolog, etc.
     * 
     * @param serviceName -- the name of the service, which is used to lookup the knowledgeBaseIdentifier and modelName
     * @param dataSrc -- the DataSource to supply instance data to the model
     * @param inputFormat -- "N-TRIPLE", "N3", or "RDF/XML"
     * @param query -- a array of queries to be executed
     * @return -- an array of ResultSet instances containing the results of each query
     * @throws IOException
     * @throws ConfigurationException
     * @throws NamedServiceNotFoundException
     * @throws QueryCancelledException 
     * @throws QueryParseException
     * @throws ReasonerNotFoundException
     * @throws SessionNotFoundException
     * @throws InvalidNameException 
     */
    abstract public ResultSet[] atomicQuery(String serviceName, DataSource dataSrc, String inputFormat, String[] query) throws IOException, ConfigurationException, NamedServiceNotFoundException, QueryCancelledException, QueryParseException, ReasonerNotFoundException, SessionNotFoundException, InvalidNameException;
                              

    /**
     * Method to provide CSV instance data and then execute a set of queries in an atomic (stateless) manner, returning a ResultSet for each query. The query
     * strings will depend upon the knowledge representation and reasoner; it might be SPARQL, Prolog, etc.
     * 
     * @param serviceName -- the name of the service, which is used to lookup the knowledgeBaseIdentifier and modelName
     * @param csvDataSrc -- the CSV DataSource to supply instance data to the model
     * @param includesHeader -- true if the CSV file contains header information in the first row
     * @param csvTemplate -- template mapping data stream
     * @param query -- a array of queries to be executed
     * @return -- an array of ResultSet instances containing the results of each query
     * @throws IOException
     * @throws ConfigurationException
     * @throws NamedServiceNotFoundException
     * @throws QueryCancelledException 
     * @throws QueryParseException
     * @throws ReasonerNotFoundException
     * @throws SessionNotFoundException
     * @throws InvalidNameException 
     * @throws TemplateException 
     */
    abstract public ResultSet[] atomicQueryCsvData(String serviceName, DataSource csvDataSrc, boolean includesHeader, String csvTemplate, String[] query) throws IOException, ConfigurationException, NamedServiceNotFoundException, QueryCancelledException, QueryParseException, ReasonerNotFoundException, SessionNotFoundException, InvalidNameException, TemplateException;

    /**
     * This method retrieves the results of an RDF triple matching request as a list of matching statements. Zero or more of the
     * subjName, propName, and objValue may be null.
     *
     * @param subjName
     * @param propName
     * @param objValue
     * @return List of matching statements, each element is Object[3]: subject, predicate, object
     * @throws QueryCancelledException 
     * @throws TripleNotFoundException 
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     */
    abstract public ResultSet ask(String subjName, String propName, Object objValue) throws TripleNotFoundException, ReasonerNotFoundException, QueryCancelledException, SessionNotFoundException;

	/**
	 * Method to expand prefixes, find namespaces, and do whatever
	 * other processing may be necessary to prepare a query string
	 * for execution by the target query engine.
	 * Note: this is provided as a callable functionality so that queries which are known to be completely expanded
	 * need not have the overhead of checking.
	 * 
	 * @param query
	 * @return the expanded query prepared for execution
	 * @throws InvalidNameException 
	 * @throws ReasonerNotFoundException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 */
	public String prepareQuery(String query) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, InvalidNameException, SessionNotFoundException;

	/**
	 * Method to substitute parameter values for "?" terms in a query string.
	 * 
	 * @param queryStr -- SPARQL query or update string
	 * @param values -- parameter values to be substituted
	 * @return parameterized query
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws ReasonerNotFoundException 
	 * @throws SessionNotFoundException 
	 */
	public String parameterizeQuery(String queryStr, List<Object> values) throws InvalidNameException, ConfigurationException, ReasonerNotFoundException, SessionNotFoundException;

	/**
     * This method is called to cause abox data to be loaded from the server environment.
     *
     * @param serverDataLocator -- the location of the data to be loaded in the server environment. This will be reasoner-specifc. For some reasoners it may be a relative file path.
     * @return true if successful else false
     * @throws IOException 
	 * @throws ReasonerNotFoundException 
	 * @throws SessionNotFoundException 
	 * @throws ConfigurationException
     */
    abstract public boolean loadData(String serverDataLocator) throws IOException, ReasonerNotFoundException, SessionNotFoundException, ConfigurationException;

    /**
     * This method is called to cause abox data to be loaded by mapping from a CSV data file to semantic data using the mapping template specified
     * @param serverCsvDataLocator -- server-side location of CSV data 
     * @param includesHeader -- true if the CSV file contains header information in the first row
     * @param serverCsvTemplateLocator -- server-side location of template mapping file
     * @return -- true if successful else false
     * @throws ConfigurationException 
     * @throws IOException 
     * @throws InvalidNameException 
     * @throws SessionNotFoundException 
     * @throws TemplateException 
     */
    abstract public boolean loadCsvData(String serverCsvDataLocator, boolean includesHeader, String serverCsvTemplateLocator) 
    		throws TemplateException, ConfigurationException, IOException, InvalidNameException, SessionNotFoundException, TemplateException;

    /**
     * This method is called to identify the model (tbox) to use by service name.
     * 
     * @param serviceName -- the name of the service, which is used to lookup the knowledgeBaseIdentifier and modelName
     * 
     * @return -- the unique session key for this knowledge service session
     * @throws ReasonerNotFoundException 
     * @throws ConfigurationException 
     * @throws NamedServiceNotFoundException 
     * @throws SessionNotFoundException 
     */
    abstract String selectServiceModel(String serviceName) throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException;
    
    /**
     * This method is called to identify the model (tbox) to use in this knowledge service session and to simultaneously override certain default configuration settings.
     *
     * @param serviceName -- the name of the service, which is used to lookup the knowledgeBaseIdentifier and modelName
     * @param preferences -- preferences override the configuration from the development environment (configuration.rdf)
     * 
     * @return -- the unique session key for this knowledge service session
     * @throws ConfigurationException 
     * @throws ReasonerNotFoundException 
     * @throws NamedServiceNotFoundException 
     * @throws SessionNotFoundException 
     */
    abstract String selectServiceModel(String serviceName, List<ConfigurationItem> preferences) throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException;

    /**
     * This method is called to identify the knowledgeBaseIdentifier and model (tbox) to use in this knowledge service session.
     *
     * @param knowledgeBaseIdentifier -- the identity of the knowledge base, e.g., the folder containing the OWL/Rule files OR the name of the service of a service that does not have a model name specified, in which case the model name is specified by the next parameter
     * @param modelName -- the entry point (specific model) within the knowledge base, e.g., the URI corresponding with the base namespace of the OWL model
     *
     * @return -- the unique session key for this knowledge service session
     * @throws ReasonerNotFoundException 
     * @throws ConfigurationException 
     * @throws SessionNotFoundException 
     */
    abstract String selectServiceModel(String knowledgeBaseIdentifier, String modelName) throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException;

    /**
     * This method is called to identify the knowledgeBaseIdentifier and model (tbox) to use in this knowledge service session.
     *
     * @param knowledgeBaseIdentifier -- the identity of the knowledge base, e.g., the folder containing the OWL/Rule files OR the name of the service of a service that does not have a model name specified, in which case the model name is specified by the next parameter
     * @param modelName -- the entry point (specific model) within the knowledge base, e.g., the URI corresponding with the base namespace of the OWL model
     * @param preferences -- preferences override the configuration from the development environment (configuration.rdf)
     * 
     * @return -- the unique session key for this knowledge service session
     * @throws ConfigurationException 
     * @throws ReasonerNotFoundException 
     */
    abstract String selectServiceModel(String knowledgeBaseIdentifier, String modelName, List<ConfigurationItem> preferences) throws ConfigurationException, ReasonerNotFoundException;

    /**
     * This method passes input data as a semantic model as a DataSource
     * @param dataSrc
     * @return true if successful else false
     * @throws IOException 
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     * @throws ConfigurationException
     */
    abstract public boolean sendData(DataSource dataSrc) throws IOException, ReasonerNotFoundException, SessionNotFoundException, ConfigurationException;

    /**
     * This method passes input data as a semantic model as a DataSource with a specified format
     * @param dataSrc
     * @param inputFormat -- "N-TRIPLE", "N3", or "RDF/XML"
     * @return true if successful else false
     * @throws IOException 
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     * @throws ConfigurationException
     */
    abstract public boolean sendData(DataSource dataSrc, String inputFormat) throws IOException, ReasonerNotFoundException, SessionNotFoundException, ConfigurationException;

    /**
     * This method is called to send abox data in CSV format to be converted to semantic data using the identified mapping template. The template identifier can be a serialized string or it can be a URL to a template.
     * 
     * @param csvDataSrc -- CSV data stream
     * @param includesHeader -- true if the CSV file contains header information in the first row
     * @param csvTemplateIdentifier -- template mapping identifier
     * @return -- true if successful else false
     * @throws ConfigurationException
     * @throws IOException
     * @throws InvalidNameException
     * @throws SessionNotFoundException 
     */
    abstract public boolean sendCsvData(DataSource csvDataSrc, boolean includesHeader, String csvTemplateIdentifier) 
    		throws TemplateException, ConfigurationException, IOException, InvalidNameException, SessionNotFoundException;

    /**
     * This method sets the default instance data namespace. Instance data nodes without a specified namespace will use this namespace.
     * 
     * @param namespace -- the default instance data namespace
     * @return -- the old instance data namespace or null if there was none
     * @throws InvalidNameException 
     */
    abstract public String setInstanceDataNamespace(String namespace) throws InvalidNameException, SessionNotFoundException;
    
    /**
     * This method retrieves the default instance data namespace.
     * 
     * @return
     */
    abstract public String getInstanceDataNamespace();
    
    /**
     * This method retrieves the instance model associated with the default instance data namespace.
     * (The model namespace is the model name with a "#" added at the end.)
     * @return
     */
    abstract public String getInstanceModelName();
    
    /**
     * This method is called to add a triple to the instance data. The object value will be interpreted based on type of the Object
     * and on the range of the property if it is an OntProperty.
     * 
     * @param subjName the URI of the subject
     * @param predName the URI of the property
     * @param objValue the URI of the object value for an ObjectProperty or a value that will be interpreted for a DatatypeProperty
     * 
     * @return true if successful
     * @throws ConfigurationException
     * @throws TripleNotFoundException 
     * @throws ReasonerNotFoundException 
     * @throws InvalidNameException 
     * @throws SessionNotFoundException 
     */
    abstract public boolean addTriple(String subjName, String predName, Object objValue) 
    			throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException, InvalidNameException, SessionNotFoundException, ConfigurationException;
    
    /**
     * This method creates a new Individual of the class identified.
     * 
     * @param name -- the name of the new instance to be created. If only a local fragment is given (no namespace), the instance will be created in the default namespace.
     * @param className -- the name of the OntClass to which the new Individual will belong
     * @return -- the identity of the new Individual; the fully qualified name (URI) for a named resource
     * @throws ConfigurationException
     * @throws InvalidNameException
     * @throws IOException
     * @throws SessionNotFoundException
     */
    abstract public String createInstance(String name, String className) throws ConfigurationException, InvalidNameException, IOException, SessionNotFoundException;
    
    /**
     * This method is called to delete a triple from the instance data. The subject, predicate, and/or object can be null to delete everything 
     * matching the pattern.
     * 
     * @param subjName
     * @param predName
     * @param objValue
     * @return true if successful else false
     * @throws ConfigurationException
     * @throws TripleNotFoundException
     * @throws ReasonerNotFoundException 
     * @throws InvalidNameException 
     * @throws SessionNotFoundException 
     */
    abstract public boolean deleteTriple(String subjName, String predName, Object objValue) 
    		throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException, InvalidNameException, SessionNotFoundException, ConfigurationException;
    
    /**
     * This method is called to delete all instance data added by calls to addTriple or forms of loadInstanceData. The infModel is 
     * reset to reflect those removals.
     * 
     * @return true if successful else false
     * @throws TripleNotFoundException
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     */
    abstract public boolean reset() throws ReasonerNotFoundException, SessionNotFoundException;

    /**
     * Sets the Owl file output format to be used for output, e.g., when responding to a SPARQL construct query.
     * 
     * @param outputFormat -- "N-TRIPLE", "N3", or "RDF/XML"
     * @throws SessionNotFoundException
     */
	public abstract void setOwlFileOutputFormat(String outputFormat) throws SessionNotFoundException;

    /**
     * Gets the knowledge base identifier for the current session.
     * 
     * @return knowledge base identifier
     * @throws ConfigurationException 
     */
	public abstract String getKBaseIdentifier() throws ConfigurationException;

    /**
     * Gets the model name for the current session.
     * 
     * @return model name
     * @throws IOException 
     */
	public abstract String getModelName() throws IOException;

    /**
     * Gets the version of the reasoner.
     * 
     * @return reasoner version
     * @throws ConfigurationException
     * @throws SessionNotFoundException
     */
	public abstract String getReasonerVersion() throws ConfigurationException, SessionNotFoundException;

    /**
     * Gets derivations of all inferred statements (if the reasoner supports derivations)
     * 
     * @return Derivations datasource
     * @throws ConfigurationException
     * @throws InvalidDerivationException
     * @throws SessionNotFoundException
     */
	public abstract DataSource getDerivations() throws ConfigurationException, InvalidDerivationException, SessionNotFoundException;

    /**
     * Executes a construct query, which will return a subgraph of the knowledge base and instance data.
     * 
     * @return Construct datasource
     * @throws QueryCancelledException
     * @throws QueryParseException
     * @throws SessionNotFoundException
     */
	public abstract DataSource construct(String query) throws QueryCancelledException, QueryParseException, SessionNotFoundException;

    /**
     * Turn on/off collection of timing information
     * 
     * @param bCollect -- true to collect timing information; false otherwise
     * @throws SessionNotFoundException
     */
	public abstract void collectTimingInformation(boolean bCollect) throws SessionNotFoundException;
	
    /**
     * Gets timing information for a given session, if the reasoner supports this capability. Normally a call to collectTimingInformation(true) 
     * must be made before the inference session begins for timing to be collected.
     * 
     * @return timing information
     * @throws SessionNotFoundException
     */
	public abstract ReasonerTiming[] getTimingInformation() throws SessionNotFoundException;
	
	/**
	 * Method to set the kbase root folder--a folder on the server file
	 * system in which and under which server configuration files and
	 * folders of model files will be found.
	 * 
	 * @param kbaseRoot
	 * @throws ConfigurationException 
	 */
	public abstract void setKbaseRoot(String kbaseRoot) throws ConfigurationException;

	/**
	 * Method to get the kbase root folder path.
	 * 
	 * @return -- the kbase root else null if not set or request not supported (Web service)
	 */
	public abstract String getKbaseRoot();

	/**
	 * Method to set the timeout for queries.
	 * Default is no timeout, which is equivalent to calling this method with a value of -1.
	 * 
	 * @param timeout (in milliseconds)
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException
	 */
	public abstract void setQueryTimeout(long timeout) throws ReasonerNotFoundException, SessionNotFoundException;

	/**
	 * Method to clear the cache for the current kbase model. Inferred models can be cached, depending on the configuration settings, to provide faster performance when the inputs have not changed.
	 * @return true if successful else false
	 * @throws InvalidNameException 
	 */
	public boolean clearCache() throws InvalidNameException, SessionNotFoundException;
}
