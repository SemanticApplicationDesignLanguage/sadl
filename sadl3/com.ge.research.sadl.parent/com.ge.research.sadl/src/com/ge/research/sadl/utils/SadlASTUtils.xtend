/************************************************************************
 * Copyright 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.utils

import com.ge.research.sadl.sADL.SubjHasProp
import org.eclipse.emf.ecore.EObject

/**
 * Static utility class for SADL AST elements.
 * 
 * For instance, this class can be used to check whether an expression is a comma separated abbreviated expression.
 * Also, it provides some utilities for checking whether a model element represents a unit or not.
 * 
 * This class can be used without injection. 
 * 
 * @author akos.kitta
 */
class SadlASTUtils {

	/**
	 * {@code true} if the SADL model element argument is a comma separated abbreviated expression. Otherwise {@code false}.
	 */
	static def boolean isCommaSeparatedAbbreviatedExpression(EObject it) {
		return if(it instanceof SubjHasProp) comma else false;
	}

}
