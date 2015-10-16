/************************************************************************
 * Copyright 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.8 $ Last modified on   $Date: 2014/11/11 14:31:33 $
 ***********************************************************************/
package com.ge.research.sadl.jena;

// should have no dependencies to Xtext, EMF or Eclipse
public class ModelManager {
	
	private String modelBaseURI;
	
	public void setModelBaseURI(String modelBaseURI) {
		this.modelBaseURI = modelBaseURI;
	}
	
	public String getModelBaseURI() {
		return modelBaseURI;
	}

}
