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
 * $Revision: 1.2 $ Last modified on   $Date: 2014/10/28 11:56:07 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.util.Objects;

public class ModelError {
    public enum ErrorType{INFO, WARNING, ERROR}

    public String errorMsg;
	public ErrorType errorType;
    private String hyperLinkFileName = null;


	public ModelError(String msg, ErrorType etype) {
		setErrorMsg(msg);
		setErrorType(etype);
	}
	
    public ModelError(String msg, ErrorType etype, String hyperlinkFilename) {
		setErrorMsg(msg);
		setErrorType(etype);
    	setHyperLinkFileName(hyperlinkFilename);
    }

    @Override
    public int hashCode() {
        return Objects.hash(errorMsg, errorType);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!(obj instanceof ModelError))
            return false;
        ModelError other = (ModelError) obj;
        return Objects.equals(errorMsg, other.errorMsg) && errorType == other.errorType;
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
    	if (getHyperLinkFileName() != null) {
    		return errorType.toString() + ": " + errorMsg + getHyperLinkFileName();
    	}
    	return errorType.toString() + ": " + errorMsg;
    }

	public String getHyperLinkFileName() {
		return hyperLinkFileName;
	}

	private void setHyperLinkFileName(String hyperLinkFileName) {
		this.hyperLinkFileName = hyperLinkFileName;
	}
}
