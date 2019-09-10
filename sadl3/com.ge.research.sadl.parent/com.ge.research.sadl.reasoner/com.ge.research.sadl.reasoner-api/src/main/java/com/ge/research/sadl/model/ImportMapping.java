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

import com.hp.hpl.jena.ontology.OntModel;

/**
 * This class encapsulates all of the information important to an
 * import: the publicURI, the actualURL, the prefix for this
 * import if it is known, the actual Jena OntModel, and the time
 * at which it was loaded.
 * 
 * @author crapo
 *
 */
public class ImportMapping {
	private String publicURI;
	private String actualURL;
	private String prefix;
	private OntModel model;
	
	public ImportMapping() {
		
	}
	
	public ImportMapping(String pubUri, String actUrl, String alias) {
		publicURI = pubUri;
		actualURL = actUrl;
		prefix = alias;
	}
	
	public void setPublicURI(String publicURI) {
		this.publicURI = publicURI;
	}
	
	public String getPublicURI() {
		return publicURI;
	}
	
	public void setActualURL(String actualURL) {
		this.actualURL = actualURL;
	}
	
	public String getActualURL() {
		return actualURL;
	}
	
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}
	
	public String getPrefix() {
		return prefix;
	}

	public OntModel getModel() {
		return model;
	}

	public void setModel(OntModel model) {
		this.model = model;
	}

}
