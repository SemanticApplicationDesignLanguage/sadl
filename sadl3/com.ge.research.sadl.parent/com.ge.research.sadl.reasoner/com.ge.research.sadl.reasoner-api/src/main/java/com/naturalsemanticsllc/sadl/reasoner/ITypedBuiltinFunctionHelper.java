/************************************************************************
 * Copyright © 2023 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemanticsllc.sadl.reasoner;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.TranslationException;

/*
 * This Interface class defines the methods that implementations must have to help process typed builtin functions. 
 */
public interface ITypedBuiltinFunctionHelper {

	/*
	 * This delineates the types of UnittedQuantity handling that can be supported
	 */
	public static enum UnittedQuantityBuiltinHandlingType {SingleArgument, SameUnitsRequired, 
		   												   DifferentUnitsAllowedOrLeftOnly, 
		   												   LeftUnitsOnly, UnitsNotSupported}

	/** Method to compute the unit of a computed UnittedQuantity based on the operation and the input units
	 * 
	 * @param operator
	 * @param arg1Units
	 * @param arg2Units
	 * @return
	 * @throws TypedBuiltinFunctionException
	 */
	abstract String combineUnits(String operator, String arg1Units, String arg2Units) throws TypedBuiltinFunctionException;
	
	/**
	 * Method to do unit conversion operations on arguments, e.g., 1 ft + 1 in might convert the 
	 * first argument to 12 in or might convert the second argument to 0.08333333 ft.
	 * @param operator
	 * @param arg1Value
	 * @param arg1Units
	 * @param arg2Value
	 * @param arg2Units
	 * @return -- an array of size 4: 0) newArg1Value, 1) newArg1Units, 2) new arg2Value, 3) new arg2Units
	 * @throws UnittedQuantityHandlerException
	 */
	abstract Object[] performUnitConversions(Object operator, Number arg1Value, Object arg1Units, Number arg2Value, 
			Object arg2Units) throws TypedBuiltinFunctionException;
	
	/**
	 * Method to validate a the argument types of a built-in and return the URIs of the return types.
	 * @param model
	 * @param args
	 * @param argTypes
	 * @return -- URIs of return types encapsulated in Nodes
	 * @throws UnittedQuantityHandlerException
	 * @throws ConfigurationException 
	 * @throws TranslationException 
	 */
	abstract Node[] validateArgumentTypes(BuiltinElement be, Object model, java.util.List<Node> args, java.util.List<Node> argTypes) throws TypedBuiltinFunctionException, ConfigurationException, TranslationException;
	
	/**
	 * Method to obtain an instance of ITranslator to use in processing
	 * 
	 * @return
	 */
	abstract ITranslator getTranslator();
	
	/**
	 * Method to set the instance of ITranslator to be used in processing
	 * @param reasoner
	 */
	abstract void setTranslator(ITranslator translator);
	
	/**
	 * Method to get the UnittedQuantity handing type from the translator for non-common built-ins, that
	 * is built-ins that aren't part of the grammar but are supported by a particular reasoner/translator pair.
	 * @param builtinUri
	 * @return
	 */
	abstract UnittedQuantityBuiltinHandlingType getUnittedQuantityBuiltinHandlingTypeOfBuiltinFromTranslator(String builtinUri);

	/**
	 * Method to get the UnittedQuantityBuiltinHandlingType of some common built-in comparison functions
	 * @param builtinType
	 * @return
	 */
	public static UnittedQuantityBuiltinHandlingType getUnittedQuantityBuiltinHandlingTypeOfCommonBuiltins(BuiltinType builtinType) {
		if (BuiltinElement.isComparisonBuiltin(builtinType)) {
			return UnittedQuantityBuiltinHandlingType.SameUnitsRequired;
		}
		if (builtinType.equals(BuiltinType.Minus) || builtinType.equals(BuiltinType.Plus)) {
			return UnittedQuantityBuiltinHandlingType.SameUnitsRequired;
		}
		else if (builtinType.equals(BuiltinType.Divide) || builtinType.equals(BuiltinType.Multiply)) {
			return UnittedQuantityBuiltinHandlingType.DifferentUnitsAllowedOrLeftOnly;
		}
		else if (builtinType.equals(BuiltinType.Power)) {
			return UnittedQuantityBuiltinHandlingType.LeftUnitsOnly;
		}
		// need to also handle average, max, min, 
		return UnittedQuantityBuiltinHandlingType.UnitsNotSupported;
	}
	
}
