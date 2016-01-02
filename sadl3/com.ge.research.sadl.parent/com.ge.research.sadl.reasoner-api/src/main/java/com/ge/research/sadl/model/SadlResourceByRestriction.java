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
 * $Revision: 1.2 $ Last modified on   $Date: 2015/07/25 16:27:14 $
 ***********************************************************************/
package com.ge.research.sadl.model;

/**
 * This class encapsulates the information needed to define an anonymous class restriction
 * @author 200005201
 *
 */
public class SadlResourceByRestriction extends ConceptIdentifier {

	private ConceptName onProperty;
	private ClassRestrictionCondition restrictCondition;
	
	public SadlResourceByRestriction(ConceptName _onProperty, ClassRestrictionCondition _restrictCondition) {
		onProperty = _onProperty;
		restrictCondition = _restrictCondition;
	}
	
	public ConceptName getOnProperty() {
		return onProperty;
	}
	
	public void setOnProperty(ConceptName onProperty) {
		this.onProperty = onProperty;
	}
	
	public ClassRestrictionCondition getRestrictCondition() {
		return restrictCondition;
	}
	
	public void setRestrictCondition(ClassRestrictionCondition restrictCondition) {
		this.restrictCondition = restrictCondition;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(onProperty.toString());
		sb.append(" ");
		sb.append(restrictCondition.toString());
		return sb.toString();
	}
}
