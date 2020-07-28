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

package com.ge.research.sadl.server;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.List;

import com.ge.research.sadl.reasoner.AmbiguousNameException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.server.ISadlServer;
import com.ge.research.sadl.server.SessionNotFoundException;

/**
 * This Interface class defines additional methods beyond those in 
 * the superclass ISadlServer. These methods support editing of models
 * and the persistence of changes/additions to models.
 * In particular, any method that can add triples to the knowledge base,
 * or which can query for information, will have a new companion method
 * that allows the specification of which model to use as the added first
 * argument. When super class method is called (no model specified), the
 * default model is used. 
 * The default model is the starting model identified in the call to 
 * selectServiceModel, or set by the kbase configuration for a named service.
 * However, if a call is made to setInstanceDataNamespace with a namespace
 * different from the default model, then the model with this namespace
 * becomes the default model. 
 * 
 * The companion methods of this interface which take a model name as first
 * parameter are a way to override the default model, whatever it is, and
 * do operations on any specified model identifiable in the kbase.
 * 
 * P--persistence
 * E--editing
 * => SadlServerPE
 *  
 * @author Andy Crapo
 *
 */
public interface ISadlServerPE extends ISadlServer {

	/**
	 * Call this method to persist the instance data which has been added
	 * during this session as model on the server.
	 * 
	 * @param owlInstanceFileName File name containing instance in OWL format. For absolute paths use "file:///"
	 * @param globalPrefix Global prefix (must be unique in the knowledge base)
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.ConfigurationException
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws IOException 
	 */
	abstract public boolean persistInstanceModel(String owlInstanceFileName, String globalPrefix) throws ConfigurationException, SessionNotFoundException, IOException;
	
	/**
	 * Call this method to persist the instance data which has been added
	 * during this session as model on the server with the specified model URI. (This is like a "save as".)
	 * Issue: are the URI's of all instances in the default instance namespace changed to this namespace??
	 * 
	 * @param modelName Model name
	 * @param owlInstanceFileName File name containing instance in OWL format
	 * @param globalPrefix Global prefix
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.ConfigurationException
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws IOException 
	 */
	abstract public boolean persistInstanceModel(String modelName, String owlInstanceFileName, String globalPrefix) throws ConfigurationException, SessionNotFoundException, IOException;

	/**
	 * Call this method to persist any changes that have been made to the
	 * service model or to any of the models that it imports.
	 * 
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	abstract public boolean persistChangesToServiceModels()	throws SessionNotFoundException;
	
	/**
	 * Call this method to retrieve the errors that have been accumulated. Note that calling this method 
	 * clears the errors--a second immediate call will return no errors.
	 * 
	 * @return List of errors since last call or null if none
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	abstract public List<ModelError> getErrors() throws SessionNotFoundException;

	/**
	 * Call this method to add the specified triple to the specified existing model.
	 * 
	 * @param modelName Model name
	 * @param subject Subject
	 * @param predicate Predicate
	 * @param value Object value
	 * @return True if operation was successful, false otherwise
	 * @throws com.ge.research.sadl.server.ConfigurationException
	 * @throws com.ge.research.sadl.server.ReasonerNotFoundException 
	 * @throws com.ge.research.sadl.server.TripleNotFoundException 
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	abstract public boolean addTriple(String modelName, String subject, String predicate, Object value) 
			throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException, SessionNotFoundException;
	
    /**
     * This method creates a new Individual of the class identified in the specified named model.
     * 
	 * @param modelName Model name
     * @param name -- the name of the new instance to be created or null to create a blank node
     * @param className -- the name of the OntClass to which the new Individual will belong
     * @return -- the identity of the new Individual; the fully qualified name for a named resource, the AnodeId as a string for a blank node
     * @throws ConfigurationException
     * @throws InvalidNameException
     * @throws SessionNotFoundException
     * @throws IOException 
     * @throws AmbiguousNameException 
     */
    abstract public String createInstance(String modelName, String name, String className) throws ConfigurationException, InvalidNameException, SessionNotFoundException, IOException, AmbiguousNameException;

    /**
	 * Call this method to delete the specified triple from the specified existing model.
	 * 
	 * @param modelName Model name
	 * @param subject Subject
	 * @param predicate Predicate
	 * @param value Object value
	 * @return True if operation was successful, false otherwise
	 * @throws com.ge.research.sadl.server.ConfigurationException
	 * @throws com.ge.research.sadl.server.ReasonerNotFoundException 
	 * @throws com.ge.research.sadl.server.TripleNotFoundException 
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	abstract public boolean deleteTriple(String modelName, String subject, String predicate, Object value)
			throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException, SessionNotFoundException;

	/**
	 * Call this method to create a new class in the default model
	 * 
	 * @param className Class name or null if none
	 * @param superClassName Super class name or null if none
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addClass(String className, String superClassName) throws SessionNotFoundException, InvalidNameException;
	
	/**
	 * Call this method to create a new class in the specified named model
	 * 
	 * @param modelName Model name
	 * @param className Class name or null if none
	 * @param superClassName Super class name or null if none
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addClass(String modelName, String className, String superClassName) throws SessionNotFoundException, InvalidNameException;
	
	/**
	 * Call this method to obtain a unique unused URI based on the namespace and baseLocalName provided. If the baseLocalName
	 * ends in a series of digits (a number), then this number becomes the starting counter value. If not, the counter starts
	 * at 1 and is appended to the name provided. The counter is incremented by 1 until a URI is generated that is not used
	 * for a ontology resource in the named model. This URI is then returned as a string.
	 * 
	 * @param namespace -- namespace of the new URI
	 * @param baseLocalName -- local fragment base of the URI
	 * @return -- a unique URI with the namespace and base fragment augmented to make it unique
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws com.ge.research.sadl.server.InvalidNameException 
	 */
	String getUniqueInstanceUri(String namespace, String baseLocalName) throws InvalidNameException, SessionNotFoundException;

	/**
	 * Call this method to obtain a unique unused URI based on the namespace and baseLocalName provided. If the baseLocalName
	 * ends in a series of digits (a number), then this number becomes the starting counter value. If not, the counter starts
	 * at 1 and is appended to the name provided. The counter is incremented by 1 until a URI is generated that is not used
	 * for a ontology resource in the named model. This URI is then returned as a string.
	 * 
	 * @param modelName Model name
	 * @param namespace -- namespace of the new URI
	 * @param baseLocalName -- local fragment base of the URI
	 * @return -- a unique URI with the namespace and base fragment augmented to make it unique
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws com.ge.research.sadl.server.InvalidNameException 
	 */
	String getUniqueInstanceUri(String modelName, String namespace, String baseLocalName) throws InvalidNameException, SessionNotFoundException;

	/**
	 * Call this method to obtain a unique unused namespace URI based on the base namespace provided. If the base namespace
	 * ends in a series of digits (a number), then this number becomes the starting counter value. If not, the counter starts
	 * at 1 and is appended to the name provided. The counter is incremented by 1 until a URI is generated that is not used
	 * for an ontology in the knowledge base. This URI is then returned as a string.
	 * 
	 * @param baseNamespace -- base namespace of the new URI
	 * @return -- a unique namespace URI with base namespace augmented to make it unique
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws com.ge.research.sadl.server.InvalidNameException 
	 * @throws ConfigurationException 
	 * @throws MalformedURLException 
	 */
	String getUniqueNamespaceUri(String baseNamespace) throws InvalidNameException, SessionNotFoundException, MalformedURLException, ConfigurationException;

	/**
	 * Call this method to create a new ontology property, either ObjectProperty or DatatypeProperty,  in the named model.
	 * 
	 * @param modelName Model name
	 * @param propertyName Property name
	 * @param superPropertyName Super property name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addOntProperty(String modelName, String propertyName,
			String superPropertyName) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a max cardinality restriction to a property on a class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param cardValue Cardinality value
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addMaxCardinalityRestriction(String modelName, String className,
			String propertyName, int cardValue) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a min cardinality restriction to a property on a class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param cardValue Cardinality value
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addMinCardinalityRestriction(String modelName, String className,
			String propertyName, int cardValue) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a cardinality restriction to a property on a class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param cardValue Cardinality value
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addCardinalityRestriction(String modelName, String className,
			String propertyName, int cardValue) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a max cardinality restriction to a property on a class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param cardValue Cardinality value
	 * @param restrictedToType class values are restricted to
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addMaxQualifiedCardinalityRestriction(String modelName, String className,
			String propertyName, int cardValue, String restrictedToType) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a min cardinality restriction to a property on a class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param cardValue Cardinality value
	 * @param restrictedToType class values are restricted to
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addMinQualifiedCardinalityRestriction(String modelName, String className,
			String propertyName, int cardValue, String restrictedToType) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a cardinality restriction to a property on a class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param cardValue Cardinality value
	 * @param restrictedToType class values are restricted to
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addQualifiedCardinalityRestriction(String modelName, String className,
			String propertyName, int cardValue, String restrictedToType) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add to restrict an object property on a class to a particular value in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param valueInstanceName Value instance name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addHasValueRestriction(String modelName, String className,
			String propertyName, String valueInstanceName) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to require that the values of a object property on a class include instances from a particular class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param restrictionName Restriction name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addSomeValuesFromRestriction(String modelName, String className,
			String propertyName, String restrictionName) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to restrict the values of a object property on a class to instances from a particular class in the named model.
	 * 
	 * @param modelName Model name
	 * @param className Class name
	 * @param propertyName Property name
	 * @param restrictionName Restriction name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	abstract boolean addAllValuesFromRestriction(String modelName, String className,
			String propertyName, String restrictionName) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to create a new named service with the specified new model name 
	 * and global prefix to be saved to the specified OWL file in the specified kbase.
	 * 
	 * @param kbid Knowledge base identifier
	 * @param serviceName Service name
	 * @param modelName Model name
	 * @param owlFileName OWL file name
	 * @param prefix global prefix name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	boolean createServiceModel(String kbid, String serviceName, String modelName,
			String owlFileName, String prefix) throws SessionNotFoundException;

	/**
	 * Call this method to create a new mapping to the specified existing model.
	 * This will allow the model to be queried, imported, and used in the kbase
	 * but will not copy the model from its given location. 
	 * Note that all imported models not in the kbase must also be explicitly 
	 * added or they cannot be found.
	 * 
	 * @param modelPublicUri
	 * @param modelAltUrl
	 * @param modelPrefix
	 * @return -- true if successful else false 
	 * @throws InvalidNameException 
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	boolean addExistingModel(String modelPublicUri, String modelAltUrl, String modelPrefix) throws InvalidNameException, ConfigurationException, IOException, URISyntaxException;
	
	/**
	 * Call this model to add an import to the named model.
	 * @param modelName
	 * @param importedModelUri
	 * @return
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws InvalidNameException 
	 * @throws IOException 
	 */
	boolean addImport(String modelName, String importedModelUri) throws ConfigurationException, IOException, InvalidNameException, URISyntaxException;
	
	/**
	 * Call this method to add a class to the domain of a property in the named model.
	 * 
	 * @param modelName Model name
	 * @param propertyName Property name
	 * @param domainClassName Domain class name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addOntPropertyDomainClass(String modelName, String propertyName,
			String domainClassName) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to add a class to the range of an object property in the named model.
	 * 
	 * @param modelName Model name
	 * @param propertyName Property name
	 * @param rangeClassName Range class name
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean addObjectPropertyRangeClass(String modelName, String propertyName,
			String rangeClassName) throws SessionNotFoundException, InvalidNameException;

	/**
	 * Call this method to set the range of a datatype property in the named model.
	 * 
	 * @param modelName Model name
	 * @param propertyName Property name
	 * @param xsdRange XSD range fragment: string | boolean | decimal | int | long | float | double | duration | dateTime | time | date | gYearMonth | gYear | gMonthDay | gDay | gMonth | hexBinary | base64Binary | anyURI | data
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 * @throws InvalidNameException 
	 */
	boolean setDatatypePropertyRange(String modelName, String propertyName,
			String xsdRange) throws SessionNotFoundException, InvalidNameException;
	
	/**
	 * Call this method to add a rule to the specified model
	 * 
	 * @param modelName Model name
	 * @param ruleAsString Rule
	 * @return True if successful else false
	 * @throws com.ge.research.sadl.server.ConfigurationException 
	 * @throws IOException 
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	boolean addRule(String modelName, String ruleAsString) throws ConfigurationException, IOException, SessionNotFoundException;

	/**
	 * Call this method to delete a model from the knowledge base. This will
	 * clean up mappings and repository to completely and permanently remove
	 * the model from the server.
	 * 
	 * @param modelName -- the public URI identifying the model to be deleted
	 * @return
	 * @throws IOException 
	 * @throws com.ge.research.sadl.reasoner.ConfigurationException 
	 * @throws com.ge.research.sadl.server.SessionNotFoundException
	 */
	boolean deleteModel(String modelName) throws ConfigurationException, IOException, SessionNotFoundException;
	
	/**
	 * Method to add or update the rdfs:label for the ontology Resource identified by uri and of the specified language
	 * @param uri -- uri identifying Resource for which the label is to be added or updated
	 * @param label -- the new label
	 * @param language -- the language to be updated; if null update any label
	 * @return -- return true if a label is updated else false if there was no prior matching label
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws InvalidNameException 
	 * @throws IOException 
	 */
	boolean updateRdfsLabel(String uri, String label, String language) throws ConfigurationException, IOException, InvalidNameException, URISyntaxException;

	/**
	 * Method to add or update the rdfs:label for the ontology Resource identified by uri and of the specified language
	 * @param modelName -- the public URI identifying the model to be edited
	 * @param uri -- uri identifying Resource for which the label is to be added or updated
	 * @param label -- the new label
	 * @param language -- the language to be updated; if null update any label
	 * @return -- return true if a label is updated else false if there was no prior matching label
	 * @throws URISyntaxException 
	 * @throws InvalidNameException 
	 * @throws IOException 
	 * @throws ConfigurationException 
	 */
	boolean updateRdfsLabel(String modelName, String uri, String label, String language) throws ConfigurationException, IOException, InvalidNameException, URISyntaxException;

    /**
     * This method retrieves the results of an RDF triple matching request as a list of matching statements. Zero or more of the
     * subjName, propName, and objValue may be null.
     *
	 * @param modelName -- the public URI identifying the model to be edited
     * @param subjName
     * @param propName
     * @param objValue
     * @return List of matching statements, each element is Object[3]: subject, predicate, object
     * @throws QueryCancelledException 
     * @throws TripleNotFoundException 
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     * @throws URISyntaxException 
     * @throws InvalidNameException 
     * @throws ConfigurationException 
     * @throws IOException 
     */
    abstract public ResultSet ask(String modelName, String subjName, String propName, Object objValue) throws TripleNotFoundException, ReasonerNotFoundException, QueryCancelledException, SessionNotFoundException, IOException, ConfigurationException, InvalidNameException, URISyntaxException;

    /**
     * This method retrieves the results of a kbase query as a List array, 1st element being a List of
     * the column headings and second being a List of rows, each of which is a List of values. The query
     * string will depend upon the knowledge representation and reasoner; it might be SPARQL, Prolog, etc.
     *
	 * @param modelName -- the public URI identifying the model to be edited
     * @param query a query string
     * @return List[] with first element being the column titles (names of query variables) and second element a list of lists of values
     * @throws QueryCancelledException 
     * @throws QueryParseException 
     * @throws ReasonerNotFoundException 
     * @throws SessionNotFoundException 
     * @throws URISyntaxException 
     * @throws InvalidNameException 
     * @throws ConfigurationException 
     * @throws IOException 
     */
    abstract public ResultSet query(String modelName, String query) throws QueryCancelledException, QueryParseException, ReasonerNotFoundException, SessionNotFoundException, IOException, ConfigurationException, InvalidNameException, URISyntaxException;

	/**
	 * Method to expand prefixes, find namespaces, and do whatever
	 * other processing may be necessary to prepare a query string
	 * for execution by the target query engine.
	 * Note: this is provided as a callable functionality so that queries which are known to be completely expanded
	 * need not have the overhead of checking.
	 * 
	 * @param modelName
	 * @param query
	 * @return the expanded query prepared for execution
	 * @throws InvalidNameException 
	 * @throws ReasonerNotFoundException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws AmbiguousNameException 
	 */
	public String prepareQuery(String modelName, String query) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, InvalidNameException, SessionNotFoundException, AmbiguousNameException;

	/**
	 * Method to substitute parameter values for "?" terms in a query string.
	 * 
	 * @param modelName
	 * @param queryStr -- SPARQL query or update string
	 * @param values -- parameter values to be substituted
	 * @return parameterized query
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws ReasonerNotFoundException 
	 * @throws SessionNotFoundException 
	 * @throws AmbiguousNameException 
	 */
	public String parameterizeQuery(String modelName, String queryStr, List<Object> values) throws InvalidNameException, ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, AmbiguousNameException;

}
