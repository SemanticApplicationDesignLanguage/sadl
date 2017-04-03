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

/**
 * Extension interface of the validation acceptor.
 * 
 * @author akos.kitta
 */
interface ValidationAcceptorExt extends ValidationAcceptor {
	
	/**
	 * Returns with the number of errors processed by this acceptor.
	 */
	def int getErrorCount()
	
	/**
	 * Returns with the number of warning processed by this acceptor.
	 */	
	def int getWarningCount()
	
	/**
	 * Returns with the number of issues with info level processed by this acceptor.
	 */
	def int getInfoCount()
		
}
