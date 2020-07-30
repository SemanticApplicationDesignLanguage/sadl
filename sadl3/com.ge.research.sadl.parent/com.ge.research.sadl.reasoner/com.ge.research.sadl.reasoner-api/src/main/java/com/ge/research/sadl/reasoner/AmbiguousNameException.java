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
 * $Revision: 1.2 $ Last modified on   $Date: 2014/05/05 13:27:28 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

public class AmbiguousNameException extends Exception {

    // Needed to avoid a warning.
    private static final long serialVersionUID = 1L;

    public AmbiguousNameException(String msg) {
        super(msg);
    }
    
    public AmbiguousNameException(String msg, Exception e) {
    	super(msg, e);
    }

}
