/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.6 $ Last modified on   $Date: 2014/06/11 17:46:51 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.Model;

public interface IReasoner {
	
	public final static String TotalReasoningTime = "TotalReasoningTime";
	/**
	 * Method to configure the reasoner properties
	 * @param  preferences
	 * @return integer (0 if not successful, number of properties set otherwise)
	 * @throws ReasonerNotFoundException
	 * @throws ConfigurationException 
	 */	
	public int initializeReasoner(String KBIdentifier, String modelName, List<ConfigurationItem> preferences, String repoType) throws ReasonerNotFoundException, ConfigurationException;
	public int initializeReasoner(String KBIdentifier, String modelName, String repoType) throws ReasonerNotFoundException, ConfigurationException;
	public int initializeReasoner(URI KBIdentifier, String modelName, List<ConfigurationItem> preferences, String repoType) throws ReasonerNotFoundException, ConfigurationException;
	public int initializeReasoner(URI KBIdentifier, String modelName, String repoType) throws ReasonerNotFoundException, ConfigurationException;
		
	/**
	 * Method to set the ConfigurationManager. If not set, a new one will be created.
	 * 
	 * @param configMgr
	 * @throws ConfigurationException 
	 */
	public void setConfigurationManager(IConfigurationManager configMgr) throws ConfigurationException;
	
	/**
	 * Method to restore the state of the reasoner to what it was following initialization.
	 * 
	 * @return
	 */
	public boolean reset();
	/**
	 * Method to upload the set of rules written in a file to the Reasoner
	 * @param fileName
	 * @return boolean
	 * @throws IOException
	 */
	public boolean loadRules(String ruleFileName) throws IOException;
	public boolean loadRules(URI ruleFileName) throws IOException;	
	/**
	 * Methods to pass the rule or list of rules to the reasoner 
	 * @param rule
	 * @param modelName
	 * @return true for success and false for failure
	 * @throws NameNotFoundException
	 */
	public boolean addRule(String rule);
	/**
	 * Method to delete a rule.
	 * @param 
	 * @return
	 * @throws RuleNotFoundException
	 */

	public boolean deleteRule(String ruleName) throws RuleNotFoundException;
	
	/**
	 * Method to set the instance data namespace for this scenario's instance data
	 * 
	 * @param ns
	 */
	public void setInstanceDataNamespace(String ns);
	
	/**
	 * Method to get the instance data namespace for this scenario's instance data
	 * @return
	 */
	public String getInstanceDataNamespace();
	
	/**
	 * Method to pass the instance data or (Abox) as a stream 
	 * (data on client), as a file identifier (data on server),
	 * or as an in-memory model (e.g., Jena OntModel).
	 * 
	 * @return true for success and false for failure
	 * @throws IOException
	 */
	public boolean loadInstanceData(String instanceDatafile) throws IOException;
	public boolean loadInstanceData(URI instanceDatafile) throws IOException;
	public boolean loadInstanceData(InputStream is, String format) throws IOException;
	public boolean loadInstanceData(OntModel model);
	public boolean addRules(List<String> rules);

	/**
	 * Method to add instance data
	 * @param instance data(format?)
	 * @param modelName
	 * @return boolean value
	 * @throws NameNotFoundException
	 */
	public boolean addTriple(String sub, String pred, String obj) throws TripleNotFoundException;
	
	/**
	 * 
	 * @param modelName
	 * @return
	 * @throws NameNotFoundException
	 */
	public boolean deleteTriple(String sub, String pred, String obj) throws TripleNotFoundException;
	
	/**
	 * 
	 * @param oldTriple
	 * @param newTriple
	 * @param modelName
	 * @throws NameNotFoundException
	 */
	public void updateTriple(String oldSub, String oldPred, String oldObj, String newSub, String newPred, String newObj) throws TripleNotFoundException;
	
	/**
	 * Method to execute a SPARQL select query
	 * @param askQuery
	 * @return ResultSet containing results of select query
	 * throws QueryParseException, QueryCancelledException
	 */
	public ResultSet ask(String askQuery) throws QueryParseException, QueryCancelledException;
	
	/**
	 * Method to expand prefixes, find namespaces, and do whatever
	 * other processing may be necessary to prepare a query string
	 * for execution by the target query engine.
	 * 
	 * @param query
	 * @return
	 * @throws InvalidNameException 
	 * @throws ConfigurationException 
	 */
	public String prepareQuery(String query) throws InvalidNameException, ConfigurationException;
	
	/**
	 * Method to return a subject and/or object and/or predicate of a triple. This method takes an incomplete triple and 
	 * returns a complete triple. The result set will have as many columns as there are null arguments: if subject is null
	 * there will be a subject column, if predicate is null there will be a predicate column, if object is null there will
	 * be an object column.
	 * 
	 * @param sub - subject of triple to match or null
	 * @param pred - predicate of triple to match or null
	 * @param obj - object of triple to match or null
	 * @return - a ResultSet with the results of the match else null 
	 * @throws TipleNotFoundException
	 */
	public ResultSet ask(String sub, String pred, String obj) throws TripleNotFoundException;
	
	/**
	 * Method to set a configuration name/value pair.
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public boolean configure(ConfigurationItem configItem);
	
	/**
	 * Method to construct a subgraph from a SPARQL query
	 * @param constructQuery
	 * @return - subgraph as OWL file streamed as DataSource
	 * @throws QueryParseException
	 * @throws QueryCancelledException
	 */
	public DataSource construct(String constructQuery) throws QueryParseException, QueryCancelledException;
	/**
	 * Method to identify the family of reasoners to which this reasoner belongs, e.g., "Jena-Based".
	 * 
	 * @return - the name of the family of reasoners to which this reasoner belongs.
	 */
	public String getReasonerFamily();
	
	/**
	 * Method to obtain the source code control version of this reasoner.
	 *  
	 * @return - source code control version
	 */
	public String getReasonerVersion();
	
	/**
	 * Method to turn timing information collection on and off
	 * 
	 * @param bCollect -- true to collect timing, false to not collect
	 * @return - previous value
	 */
	public boolean collectTimingInformation(boolean bCollect);
	
	/**
	 * Method to return timing information from the reasoner. The reasoner should
	 * include in the list all relevant timing information by category. Since times
	 * are not necessarily additive, the reasoner should include the category defined
	 * in this class by the constant TotalReasoningTime.
	 * 
	 * @return -- a list of timings accompanied by category and descripition 
	 */
	public List<ReasonerTiming> getTimingInformation();
	
	/**
	 * Method to identify the Java class defining the built-in functions.
	 * 
	 * @return
	 */
	public Class<?> getBuiltinClass();
	/**
	 * Method to return the category of the reasoner, which is used to save and retrieve configuration information
	 * @return
	 */
	public String getConfigurationCategory();
	
	/**
	 * Method to obtain all of information about all configuration options for this
	 * reasoner. Each returned ConfigurationItem includes the category, name,
	 * description, default value, and legal options (if constrained).
	 * 
	 * @return
	 */
	public Map<String, ConfigurationOption> getReasonerConfigurationOptions();
	
	/**
	 * Enable explanations. Some reasoners require advance notice that explanations should be available.
	 * 
	 * @param bVal - true if explanations are to be available else false
	 */
	public void enableExplanation(boolean bVal);

	/**
	 * 	Call to find out if explanations are enabled.
	 * @return
	 */
	boolean isExplanationEnabled();

	/**
	 * Provide an explanation of the named rule in the current inference model: matches allowing it fire or premise(s) not matched.
	 * 
	 * @param rulename
	 */

	public List<Explanation> explain(String rulename);
	
	/**
	 * Obtain a list of derived triples with explanation of each.
	 * 
	 * @return
	 */
	public DataSource getDerivations() throws InvalidDerivationException;
		
	/**
	 * Check the validity of the inferred model (including data model) and report any conflicts
	 * 
	 * @return
	 */
	public List<ModelError> checkModelValidity();
	
	/**
	 * Provide an explanation of the derivation (or lack of derivation) of the specified list of one or more GraphPatternElements.
	 * 
	 * @param patterns
	 */
	public List<Explanation> explain(List<GraphPatternElement> patterns);
	
	/**
	 * Write the inferred model (or deductions only) to the specified filename
	 * @param filename
	 * @param deductionsOnly
	 * @return
	 * @throws
	 */
	public boolean saveInferredModel(String filename, String modelname, boolean deductionsOnly) throws FileNotFoundException;

	/**
	 * Model to get the inferred model, in its entirety or deductions only
	 * @param deductionsOnly
	 * @return
	 * @throws ConfigurationException
	 */
	public Model getInferredModel(boolean deductionsOnly) throws ConfigurationException;
	/**
	 * Convert a Java object value to a String. This assumes that the Object is a type that can be converted to a TypedLiteral 
	 * and then back to an XSD string. If predicate is not null a range will be used, if possible, to guide the typing of the literal.
	 * 
	 * @param objValue
	 * @param predicate 
	 * @return
	 */
	public String objectValueToStringValue(Object objValue, String predicate);
	
	/**
	 * Method to get a list of implicit (not registered as a service) builts.
	 * 
	 * @return
	 */
	public List<BuiltinInfo> getImplicitBuiltins();
	
	/**
	 * Method to extract the BuiltinInfo information from a reasoner-specific (or reasoner family-specific) builtin class.
	 * 
	 * @param trans
	 * @return
	 */
	public BuiltinInfo getBuiltinInfo(Class<?> trans);
	
	/**
	 * Method to set the format (syntax) of the OWL output file from a construct query.
	 * @param outputFmt valid formats include "N3", "N-TRIPLE", "RDF/XML", "RDF/XML-ABBREV"
	 */
	public void setOutputFormat(String outputFmt);

	/**
	 * Method to set the expected format of models to be retrieved from the repository
	 * @param owlModelFormat
	 */
	public void setModelInputFormat(String owlModelFormat);
	
	/**
	 * Method to clear the cache for a particular kbase model.
	 * @return true if successful else false
	 * @throws InvalidNameException 
	 */
	public boolean clearCache() throws InvalidNameException;
}
