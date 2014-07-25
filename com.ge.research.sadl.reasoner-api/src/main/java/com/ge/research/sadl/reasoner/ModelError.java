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
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

public class ModelError {
    public enum ErrorType{WARNING, ERROR}

    public String errorMsg;
	public ErrorType errorType;

	public ModelError(String msg, ErrorType etype) {
		setErrorMsg(msg);
		setErrorType(etype);
	}
	
	public boolean equals(Object me) {
		if (me instanceof ModelError && errorType.equals(((ModelError)me).errorType) &&
				errorMsg.equals(((ModelError)me).errorMsg)) {
			return true;
		}
		return false;
	}
	
    private void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    private void setErrorType(ErrorType errorType) {
        this.errorType = errorType;
    }

    public ErrorType getErrorType() {
        return errorType;
    }

    public String toString() {
    	return errorType.toString() + ": " + errorMsg;
    }

}