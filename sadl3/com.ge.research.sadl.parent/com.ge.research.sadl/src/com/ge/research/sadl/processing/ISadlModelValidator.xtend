/************************************************************************
 * Copyright © 2007-2022 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.processing

import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.Constant

/**
 * Generic hook for 3rd party validators to participate in the processing of SADL resources
 *  
 */
interface ISadlModelValidator {
	// Pseudo operators for validation of binary operations
	public static final String ARGUMENT = "argument";	// used to compare an argument with the parameter declaration
	public static final String IS = "is";
	
	def boolean validate(BinaryOperation expression, StringBuilder errorMessageBuilder)
	def boolean isSkippedConstant(Constant expr)
}