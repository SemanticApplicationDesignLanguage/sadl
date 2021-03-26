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
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:07 $
 ***********************************************************************/

package com.ge.research.sadl.model;

import com.ge.research.sadl.model.ConceptName.ConceptType;

public class PendingModelError extends ModelError {
    
    private ConceptName conceptName = null;
    private ConceptType conceptType = null;
    
    public PendingModelError(int argIdx, int lstIdx, ConceptName cName, ConceptType cType, String msg) {
        super(argIdx, lstIdx, ExistingNamePart.NAME, msg);
        setConceptName(cName);
        setConceptType(cType);
    }

    private void setConceptType(ConceptType conceptType) {
        this.conceptType = conceptType;
    }

    public ConceptType getConceptType() {
        return conceptType;
    }

    private void setConceptName(ConceptName conceptName) {
        this.conceptName = conceptName;
    }

    public ConceptName getConceptName() {
        return conceptName;
    }

}
