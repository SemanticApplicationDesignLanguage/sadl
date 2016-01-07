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
 * $Last revised by: hathawa $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/05/05 13:27:28 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

public class QueryCancelledException extends Exception {
	private static final long serialVersionUID = 1L;

	public QueryCancelledException(String message) {
		super(message);
	}

	public QueryCancelledException(String message, Exception cause) {
		super(message, cause);		
	}
}
