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
 * $Revision: 1.3 $ Last modified on   $Date: 2015/07/25 16:27:14 $
 ***********************************************************************/

package com.ge.research.sadl.model;

/**
 * This class encapsulated the information necessary to instantiate a class restriction in the OWL model. 
 * 
 * @author 200005201
 *
 */
public class ClassRestrictionCondition {
	// The types of class restriction supported
	public enum RestrictionType {ALLVALUES, SOMEVALUES, HASVALUE, CARDINALITY, MAXCARDINALIY, MINCARDINALITY}

	private RestrictionType restrictionType;
	private ConceptIdentifier restrictedToConcept;
	
	private Object restrictedToObject;
	private int restrictedToCardinality = -1;
	
	public ClassRestrictionCondition(RestrictionType rType, Object rObject) {
		restrictionType = rType;
		// Note: order matters in the following--ConceptName is a subclass of ConceptIdentifier so must be tested first
		if (rObject instanceof ConceptIdentifier) {
			restrictedToConcept = (ConceptIdentifier) rObject;
		}
		else {
			restrictedToObject = rObject;
		}
	}
	
	public ClassRestrictionCondition(RestrictionType rType, int card) {
		restrictionType = rType;
		restrictedToCardinality = card;
	}
	
	public ClassRestrictionCondition(RestrictionType rType, int card, ConceptIdentifier clsQualifier) {
		restrictionType = rType;
		restrictedToCardinality = card;
		restrictedToObject = clsQualifier;	// qualified cardinality
	}
	
	public RestrictionType getRestrictionType() {
		return restrictionType;
	}
	
	public ConceptIdentifier getRestrictedToConcept() {
		return restrictedToConcept;
	}
	
	public Object getRestrictedToObject() {
		return restrictedToObject;
	}
	
	public int getRestrictedToCardinality() {
		return restrictedToCardinality;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getRestrictionType().toString());
		if (restrictedToCardinality >= 0) {
			sb.append(" ");
			sb.append(restrictedToCardinality);
		}
		if (restrictedToObject != null) {
			sb.append(" ");
			sb.append(restrictedToObject.toString());
		}
		else if (restrictedToConcept != null) {
			sb.append(" ");
			sb.append(restrictedToConcept.toString());
		}
		return sb.toString();
	}
}
