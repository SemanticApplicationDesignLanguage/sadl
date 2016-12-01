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
 * $Revision: 1.4 $ Last modified on   $Date: 2014/10/28 14:42:28 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.*;
import com.hp.hpl.jena.ontology.OntModel;

import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.io.IOException;
import java.net.URISyntaxException;

public interface ITranslator {
	
	/**
	 * Method to return the configuration category (name) of the translator
	 * 
	 * @return
	 */
	public String getConfigurationCategory();
	
	/**
	 * Method to set the ConfigurationManager to be used by the translator
	 * 
	 * @param configMgr
	 * @throws ConfigurationException 
	 */
	public void setConfigurationManager(IConfigurationManager configMgr) throws ConfigurationException;
	
	/**
	 * Method to translate and save a model when there are no rules
	 * 
	 * @param model--Jena OntModel to be saved
	 * @param translationFolder--folder in which file-based saving is to place save files
	 * @param modelName--the URI of the model to be saved
	 * @param orderedImports--the URIs, if any, of the imported models in order of appearance
	 * @return saveFilename--the file name to which the model is to be saved if file-based 
	 * @throws TranslationException
	 * @throws IOException 
	 * @throws URISyntaxException 
	 */
	public List<ModelError> translateAndSaveModel(OntModel model, String translationFolder, String modelName, List<String> orderedImports, String saveFilename) throws TranslationException, IOException, URISyntaxException;
	
	/**
	 * Method to translate and save a model when there are no rules
	 * 
	 * @param model--Jena OntModel to be saved
	 * @param ruleList--the list of rules (SADL Intermediate Form) to be saved
	 * @param translationFolder--folder in which file-based saving is to place save files
	 * @param modelName--the URI of the model to be saved
	 * @param orderedImports--the URIs, if any, of the imported models in order of appearance
	 * @return saveFilename--the file name to which the model is to be saved if file-based 
	 * @throws TranslationException
	 * @throws IOException 
	 * @throws URISyntaxException 
	 */
	public List<ModelError> translateAndSaveModel(OntModel model, List<Rule> ruleList, String translationFolder, String modelName, List<String> orderedImports, String saveFilename) throws TranslationException, IOException, URISyntaxException;

	
	/**
	 * Method to translate and save a model when there are other knowledge structures besides rules
	 * @param model--Jena OntModel to be saved
	 * @param otherStructure -- the other knowledge structures
	 * @param translationFolder--folder in which file-based saving is to place save files
	 * @param modelName--the URI of the model to be saved
	 * @param orderedImports--the URIs, if any, of the imported models in order of appearance
	 * @return saveFilename--the file name to which the model is to be saved if file-based 
	 * @throws TranslationException
	 * @throws IOException 
	 * @throws URISyntaxException 
	 */
	public List<ModelError> translateAndSaveModelWithOtherStructure(OntModel model, Object otherStructure, String translationFolder, String modelName, List<String> orderedImports, String saveFilename) throws TranslationException, IOException, URISyntaxException;

	/**
	 * Method to translate a rule in intermediate form to the target representation
	 * 
	 * @param model
	 * @param rule
	 * @return String
	 * @throws TranslationException
	 */
	
	public String translateRule(OntModel model, Rule rule) throws TranslationException;	
	
	/**
	 * Method to translate an Equation (internal or external) to a String.
	 * @param model
	 * @param equation
	 * @return
	 * @throws TranslationException
	 */
	public String translateEquation(OntModel model, Equation equation) throws TranslationException;
	
	/**
	 * Method to translate a query in intermediate form to the target representation
	 * 
	 * @param model
	 * @param query
	 * @return String
	 * @throws TranslationException
	 * @throws InvalidNameException 
	 */
	public String translateQuery(OntModel model, Query query) throws TranslationException, InvalidNameException;	
	
	/**
	 * Method to identify the family of reasoners to which this translator can be applied, e.g., "Jena-Based".
	 * 
	 * @return - the name of the family of reasoners this translates for.
	 */
	public String getReasonerFamily();
	

	/**
	 * Method to obtain all of information about all configuration options for this
	 * translator. Each returned ConfigurationItem includes the category, name,
	 * description, default value, and legal options (if constrained).
	 * 
	 * @return
	 */
	public Map<String, ConfigurationOption> getTranslatorConfigurationOptions();
	
	/**
	 * 
	 * @param model
	 * @param test
	 * @return String
	 * @throws TranslationException
	 */
	
	/**
	 * Method to set a configuration name/value pair.
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public boolean configure(ConfigurationItem configItem);
	
	/**
	 * Method to set a configuration name/value pair.
	 * 
	 * @param name
	 * @param value
	 * @return
	 */
	public boolean configure(List<ConfigurationItem> configItems);
	
	/**
	 * Method to prepare a query by doing such actions as replacing concept
	 * names with completely qualified URIs, etc.
	 * 
	 * @param model
	 * @param queryStr
	 * @return
	 * @throws InvalidNameException
	 */
	public String prepareQuery(OntModel model, String queryStr) throws InvalidNameException;
	
	/**
	 * Method to explore translator configuration options to discover those
	 * that provide optimal performance for the entire project or for a subset
	 * of the project.
	 * 
	 * @param translationFolder -- folder in which OWL models are found
	 * @param modelName -- URI of entry point model or null to use entire project
	 * @param configurationManager -- ConfigurationManager, from which Reasoner can be obtained
	 * @param queries -- List of queries (Query, Intermediate Form) to be used for optimizaion
	 * @return
	 * @throws FunctionNotSupportedException 
	 * @throws ConfigurationException 
	 * @throws TranslationException 
	 */
	public List<ConfigurationItem> discoverOptimalConfiguration(String translationFolder, String modelName, 
			IConfigurationManager configMgr, List<Query> queries) throws FunctionNotSupportedException, ConfigurationException, TranslationException;
	
	/**
	 * Method to save a set of rules to a file
	 * 
	 * @param model
	 * @param ruleList
	 * @param modelName
	 * @return
	 * @throws TranslationException
	 */
	public String translateAndSaveRules(OntModel model, List<Rule> ruleList, String modelName) throws TranslationException;
	
	/**
	 * Method to validate a rule per the requirements of a specific translator
	 * @param rule -- the Rule to be validated
	 * @return -- any errors found
	 */
	public List<ModelError> validateRule(com.ge.research.sadl.model.gp.Rule rule);
	
	/**
	 * Checks if function name is a built-in function for a reasoner/translator pair
	 * @param name of built-in function
	 * @return true if valid built-in function name for translator/reasoner pair, false otherwise
	 */
	public boolean isBuiltinFunction(String name);
	
	/**
	 * Checks if reasoner/translator pair provide any type-checking information for built-in functions
	 * @return -- an enumeration of the amount of information available for type-checking built-in functions:
	 */
	public Enum isBuiltinFunctionTypeCheckingAvailable();
	
	/**
	 * Method to obtain the implicit model for built-in functions for this reasoner/translator pair
	 * @return -- Built-in Function Implicit model as a string
	 */
	public String getBuiltinFunctionModel();
}