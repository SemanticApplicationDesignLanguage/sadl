/************************************************************************
 * Copyright © 2007-2011 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.builder;

import com.ge.research.sadl.reasoner.ModelError.ErrorType;

/**
 * Class to capture errors encountered in translating Queries/Rules/Tests
 * to intermediate representation.
 * 
 * @author 200005201
 *
 */
public class IFTranslationError extends Exception {
	private static final long serialVersionUID = 1L;
	private Object errorLocation;
	private ErrorType errorType = ErrorType.ERROR;	// the default
	
	public IFTranslationError(String msg) {
		super(msg);
	}
	
	public IFTranslationError(String msg, Object location) {
		super(msg);
		setErrorLocation(location);
	}
	
	public IFTranslationError(String msg, Object location, ErrorType errType) {
		super(msg);
		setErrorLocation(location);
		setErrorType(errType);
	}
	

	private void setErrorLocation(Object errorLocation) {
		this.errorLocation = errorLocation;
	}

	public Object getErrorLocation() {
		return errorLocation;
	}

	public ErrorType getErrorType() {
		return errorType;
	}

	private void setErrorType(ErrorType errorType) {
		this.errorType = errorType;
	}
}
