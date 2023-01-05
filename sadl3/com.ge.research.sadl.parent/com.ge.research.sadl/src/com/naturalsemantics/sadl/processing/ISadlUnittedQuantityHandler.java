/************************************************************************
 * Copyright © 2022 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemantics.sadl.processing;

import java.util.List;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.processing.I_IntermediateFormTranslator;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;

public interface ISadlUnittedQuantityHandler {
	/**
	 * Method to set the Intermediate Form translator
	 */
	abstract void setIntermediateFormTranslator(I_IntermediateFormTranslator ift);
	
	/**
	 * Method to compute the return type of a built-in based on the built-in type and the types
	 * of the arguments. If there is an error (incompatibility between the built-in and the argument
	 * types) a TranslationException will be thrown. If an argument of type UnittedQuantity is encountered
	 * but UnittedQuantity arguments are not supported, a UnittedQuantityNotSupportedException (subclass of
	 * TranlationException) will be thrown. If UnittedQuantities are being expanded, the UnittedQuantityNotSupportedException
	 * can be ignored by the caller.
	 * 
	 * @param be -- the BuiltinElement for the built-in, identifying the built-in implementation
	 * @param argTcis -- URIs of argument types as Strings
	 * @return -- URI of return type as String
	 * @throws TranslationException 
	 */
	abstract Node computeBuiltinReturnType(BuiltinElement be, List<Node>argTcis) throws TranslationException;
	
	/*
	 * Methods used in translation of statements containing references to UnittedQuantity when 
	 * the SADL preference "Expand Unitted Quantities in translation" is checked.
	 *  
	 */
	
	/**
	 * Method to add any combineUnits and there exists constructs to the rule at the end of rule processing.
	 * @param rule
	 * @return
	 * @throws TranslationException 
	 */
	abstract Rule checkRuleForNewUnittedQuantityCreation(Rule rule) throws TranslationException;
	
	/**
	 * Method to expand a binary operation on UnittedQuantity arguments. The TripleElement instances needed to expand 
	 * the UnittedQuantity instances or variables to patterns referencing the UnittedQuantity unit and value property
	 * values are identifed and returned. The new arguments for the value objects to be passed to the binary operator
	 * are put into the BuiltinElement's args list.
	 * @param gpe
	 * @return
	 * @throws TranslationException
	 * */
	abstract List<GraphPatternElement> expandUnittedQuantities(List<GraphPatternElement> patterns, BuiltinElement be,
			boolean isRuleThen) throws TranslationException;
	
	/**
	 * Method to validate a the argument types of a built-in and return the URIs of the return types.
	 * @param model
	 * @param argTypes
	 * @return
	 * @throws UnittedQuantityHandlerException
	 * @throws ConfigurationException 
	 * @throws TranslationException 
	 */
	abstract Object validateArgumentTypes(BuiltinElement be, Object model, java.util.List<Node> argTypes) throws UnittedQuantityHandlerException, ConfigurationException, TranslationException;
	
	/**
	 * Method to reset the instance of ISadlUnittedQuantityHandler for a new rule, query, or test.
	 */
	abstract void reset();
	
	/*
	 * Methods used in inference (called by Jena built-in functions) when
	 * the SADL preference "Expand Unitted Quantities in translation" is not checked
	 * are invoked by Java reflection so the methods are in the specific implementation classes.
	 */
	
	/**
	 * Method to get the name of the implementation of IUnittedQuantityInferenceHelper Interface class
	 * to be used during inference to handle UnittedQuantity arguments to built-in functions.
	 * @return
	 */
	abstract String getUnittedQuantityInferenceHelperClassname();

}
