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
package com.ge.research.sadl.reasoner;

import java.util.HashMap;
import java.util.Map;

import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;

/**
 * This class provides the interface for implementation classes that handle UnittedQuantity function arguments and 
 * return values during inference. It also provides a map of helper class names keyed by reasoner instance.
 * 
 * @author Andrew Crapo
 *
 */
public interface IUnittedQuantityInferenceHelper {
	// Map of helpers by reasoner
	static Map<Object, String> uqhelperMap = new HashMap<Object, String>();
	
	public enum BuiltinUnittedQuantityStatus {SingleArgument, SameUnitsRequired, DifferentUnitsAllowedOrLeftOnly, LeftUnitsOnly, UnitsNotSupported, StatusUnknown}
	// This enum identifies the categories of BuiltinElements WRT UnittedQuantity arguments.


	
	/**
	 * Method to add a helper class name for a given reasoner
	 * @param reasoner
	 * @param helper
	 * @return
	 */
	public static boolean addUnittedQuantityInferenceHelperClassname(Object reasoner, String helper) {
		uqhelperMap.put(reasoner, helper);
		return true;
	}
	
	/**
	 * Method to get a helper class name for a given reasoner
	 * @param reasoner
	 * @return
	 */
	public static String getUnittedQuantityInferenceHelperClassname(Object reasoner) {
		return uqhelperMap.get(reasoner);
	}

	/**
	 * Method to compute the units of the output of a binary operation that generates units, e.g., multiply and divide.
	 * @param reasonerContext -- an object containing all necessary information for interacting with the reasoner as needed to implement functionality
	 * @param operator
	 * @param arg1Units
	 * @param arg2Units
	 * @return units of operation results
	 * @throws UnittedQuantityHandlerException
	 */
	abstract Object combineUnits(Object reasonerContext, Object operator, Object arg1Units, Object arg2Units) throws UnittedQuantityHandlerException;

	/**
	 * Method to validate the operation WRT the units of the arguments, e.g., addition might require the same units.
	 * @param reasonerContext -- an object containing all necessary information for interacting with the reasoner as needed to implement functionality
	 * @param operatorType
	 * @param arg1Units
	 * @param arg2Units
	 * @return true if valid operation WRT units else false
	 * @throws UnittedQuantityHandlerException
	 */
	abstract boolean validateOperation(Object reasonerContext, BuiltinUnittedQuantityStatus operatorType, Object arg1Units, Object arg2Units) throws UnittedQuantityHandlerException;

	/**
	 * Method to report whether or not this handler supports unit conversions
	 * @param operator
	 * @param arg1Units
	 * @param arg2Units
	 * @return
	 */
	abstract boolean handlerSupportsUnitConversions(Object operator, Object arg1Units, Object arg2Units);
	
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
			Object arg2Units) throws UnittedQuantityHandlerException;
}
