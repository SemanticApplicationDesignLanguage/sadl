/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.diagnostics.Severity

import static org.eclipse.xtext.diagnostics.Severity.*

/**
 * Representation of a validation acceptor.
 * 
 * @author akos.kitta
 */
interface ValidationAcceptor {
	
	/**
	 * Shared NOOP validation acceptor.
	 */
	val NOOP = new ValidationAcceptor() {
		
		@Override
		override add(String message, EObject context, Severity severity) {
			// NOOP
		}
		
	}
	
	def void addInfo(String message, EObject context) {
		add(message, context, INFO)
	}
	
	def void addError(String message, EObject context) {
		add(message, context, ERROR)
	}
	
	def void addWarning(String message, EObject context) {
		add(message, context, WARNING)
	}
	
	def void add(String message, EObject context, Severity severity);
	
}
