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

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.OntModel;

import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.Literal.LiteralType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.Rule;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;

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
	
	public String translateRule(OntModel model, String modelName, Rule rule) throws TranslationException;	
	
	/**
	 * Method to translate an Equation (internal or external) to a String.
	 * @param model
	 * @param equation
	 * @return
	 * @throws TranslationException
	 */
	public String translateEquation(OntModel model, String modelName, Equation equation) throws TranslationException;
	
	/**
	 * Method to translate a query in intermediate form to the target representation
	 * 
	 * @param model
	 * @param query
	 * @return String
	 * @throws TranslationException
	 * @throws InvalidNameException 
	 */
	public String translateQuery(OntModel model, String modelName, Query query) throws TranslationException, InvalidNameException, AmbiguousNameException;	
	
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
	public String prepareQuery(OntModel model, String queryStr) throws InvalidNameException, AmbiguousNameException;
	
	/**
	 * Method to substitute parameter values for "?" terms in a query string.
	 * 
	 * @param model
	 * @param queryStr
	 * @param values
	 * @return parameterized query
	 * @throws InvalidNameException
	 */
	public String parameterizeQuery(OntModel model, String queryStr, List<Object> values) throws InvalidNameException, AmbiguousNameException;
	
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
	public IReasoner.SADL_BUILTIN_FUNCTIONS_TYPE_CHECKING_AVAILABILITY isBuiltinFunctionTypeCheckingAvailable();
	
	/**
	 * Method to obtain the implicit model for built-in functions for this reasoner/translator pair.
	 * Note that built-in functions model is obtained by calling method getBuildingFUnctionsModel in
	 * the translator.
	 * @param reservedWords -- a list of reserved words in the language that must be escaped with "^" prefix
	 * @return -- Built-in Function Implicit model as a string
	 */
	public String getBuiltinFunctionModel(List<String> reservedWords);
	
	/**
	 * Method to take the name (local fragment) of a concept or construct (e.g., class or rule) and return the
	 * namespace in which that concept or construct is defined.
	 * @param localFragmentName
	 * @return namespace URI
	 * @throws InvalidNameException
	 * @throws ConfigurationException
	 */
	public String getLocalFragmentNamespace(String localFragmentName) throws InvalidNameException, AmbiguousNameException, ConfigurationException;
	
	/**
	 * Returns supported data types for the project to show a warning for RCE that 
	 * a declared data type within .sreq files may not be compatible with downstream projects 
	 * @return supportedDataTypes string list of the projects supported data types
	 */
	public List<String> getSupportedDataTypes();
	
	/**
	 * Method to determine if a constant is "known"
	 * @param obj --  the ConstantNode (or not)
	 * @return -- true if node is a ConstantNode with name "known"
	 */
	public static boolean isKnownNode(Object obj) {
		if (obj instanceof ConstantNode && ((ConstantNode)obj).getName().equals("known")) {
			return true;
		}
		return false;
	}

	/**
	 * Method to convert a Constant to a Literal, valid only at this time for PI and e
	 * @param node
	 * @return
	 * @throws TranslationException
	 */
	public static Literal constantToLiteral(ConstantNode node) throws TranslationException {
		if (node.getName().equals("PI")) {
			Literal lit = new Literal(LiteralType.NumberLiteral);
			lit.setValue(Math.PI);
			lit.setOriginalText(node.getName());
			return lit;
		}
		else if (node.getName().equals("e")) {
			Literal lit = new Literal(LiteralType.NumberLiteral);
			lit.setValue(Math.E);
			lit.setOriginalText(node.getName());
			return lit;
		}
		throw new TranslationException("Unknown constant '" + node.getName() + "' cannot be translated");
	}

	/**
	 * Method to get the particular reasoner's name for a BuiltinElement.
	 * @param bin -- the BuiltinElement in question
	 * @return -- the name of the BuiltinElement as it should appear in this translator
	 * @throws TranslationException
	 */
	public String builtinTypeToString(BuiltinElement bin) throws TranslationException;
	
	/**
	 * Method to determine the BuiltinType, if there is one, for the reasoner-specific built-in
	 * name. This is needed because the SADL model may use the reasoner-specific name but we want
	 * to treat the expression the same as if the grammar operator had been used.
	 * 
	 * @param biName
	 * @return
	 */
	public BuiltinType reasonerBuiltinNameToBuiltinType(String biName);
	
	/**
	 * Method to set the function name in the input BuiltinElement using the BuiltinType
	 * and return the new name
	 * @param bin
	 * @return
	 * @throws TranslationException
	 */
	public String setBuiltinElementNameByBuiltinType(BuiltinElement bin) throws TranslationException;
	
	/**
	 * Return the full package and name of the class implementing the named builtin
	 * @param builtinName -- name of builtin for which class name is desired
	 * @return class name
	 */
	public String getBuiltinClassName(String builtinName);
	
	/**
	 * Method to get the default implementation of the ITypedBuiltinFunctionHelper. In the future the
	 * expectation is that this can be overridden via preferences (or via translator properties?), once there 
	 * are multiple implementations
	 * @param be
	 * @return
	 */
	public String getDefaultTypedBuiltinFunctionHelperClassname();
	
	/**
	 * Method to validate a the argument types of a built-in and return the return types.
	 * @param model
	 * @param args
	 * @param argTypes
	 * @return
	 * @throws UnittedQuantityHandlerException
	 * @throws ConfigurationException 
	 * @throws TranslationException 
	 */
	public Node[] validateArgumentTypes(BuiltinElement be, OntModel model, List<Node> args, List<Node> argTypes) throws TranslationException;

	/**
	 * Method to set the instance of ITypedBuiltinFunctionHelper to be used in translation validation
	 * @param tfbHelper
	 */
	public void setTypedBuiltinFunctionHelper(ITypedBuiltinFunctionHelper tfbHelper);
	
	/**
	 * Method to set the instance of ITypedBuiltinFunctionHelper to be used in translation validation
	 * @return the ITypedBuiltinFunctionHelper instance
	 */
	public ITypedBuiltinFunctionHelper getTypedBuiltinFunctionHelper();
	
	/**
	 * Method to get the UnittedQuantity handing type from the translator for non-common built-ins, that
	 * is built-ins that aren't part of the grammar but are supported by a particular reasoner/translator pair.
	 * @param builtinUri
	 * @return
	 */
	public UnittedQuantityBuiltinHandlingType getUnittedQuantityBuiltinHandlingTypeOfBuiltin(String builtinUri);

}
