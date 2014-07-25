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

import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.hp.hpl.jena.ontology.OntResource;

public class PendingModelError extends ModelError {
    
    private ConceptName conceptName = null;
    private ConceptType conceptType = null;
    private List<AdditionalCheck> additionalChecks = null;
    
    public class AdditionalCheck {
    	private OntResource instance;
    	private OntResource requiredClass;
    	private String message;
    	private ErrorType severity;
    	
    	public boolean equals(Object pme) {
    		if (super.equals(pme)) {
    			if (pme instanceof PendingModelError) {
    				if (conceptName.equals(((PendingModelError)pme).getConceptName()) &&
    						conceptType.equals(((PendingModelError)pme).getConceptType())) {
    					return true;
    				}
    			}
    		}
    		return false;
    	}
    	
    	public AdditionalCheck(OntResource inst, OntResource clss, String msg, ErrorType errType) {
    		setInstance(inst);
    		setRequiredClass(clss);
    		setMessage(msg);
    		setSeverity(errType);
    	}

		private void setInstance(OntResource instance) {
			this.instance = instance;
		}

		public OntResource getInstance() {
			return instance;
		}

		private void setRequiredClass(OntResource requiredClass) {
			this.requiredClass = requiredClass;
		}

		public OntResource getRequiredClass() {
			return requiredClass;
		}

		private void setMessage(String message) {
			this.message = message;
		}

		public String getMessage() {
			return message;
		}

		private void setSeverity(ErrorType severity) {
			this.severity = severity;
		}

		public ErrorType getSeverity() {
			return severity;
		}
    }
    
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

	public void addAdditionalCheck(AdditionalCheck additionalCheck) {
		if(additionalChecks == null) {
			additionalChecks = new ArrayList<AdditionalCheck>();
		}
		additionalChecks.add(additionalCheck);
	}

	public List<AdditionalCheck> getAdditionalChecks() {
		return additionalChecks;
	}
}
