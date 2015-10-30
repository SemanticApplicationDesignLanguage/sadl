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

import java.util.ArrayList;
import java.util.List;

import com.hp.hpl.jena.ontology.OntModel;

// should have no dependencies to Xtext, EMF or Eclipse
public class ModelManager {
	
	private String modelBaseURI;
	private String modelAlias;
	private String modelActualURL;
	private List<Import> imports;
	
	private OntModel theJenaModel;
	
	public class Import {
		private String uri;
		private String alias = null;
		
		public Import(String _uri) {
			setUri(_uri);
		}
		
		public Import(String _uri, String _alias) {
			setUri(_uri);
			setAlias(_alias);
		}
		
		public String getUri() {
			return uri;
		}
		private void setUri(String uri) {
			this.uri = uri;
		}
		public String getAlias() {
			return alias;
		}
		private void setAlias(String alias) {
			this.alias = alias;
		}
		
		public boolean equals(Import otherImport) {
			if (otherImport.getUri().equals(getUri())) {
				return true;
			}
			return false;
		}
	}
	
	public void setModelBaseURI(String modelBaseURI) {
		this.modelBaseURI = modelBaseURI;
		
		// create a Jena model
		theJenaModel = null;  // ModelFactory....
	}
	
	public String getModelBaseURI() {
		return modelBaseURI;
	}
	
	public void addImport(String importURI, String alias) {
		if (imports == null) {
			imports = new ArrayList<Import>();
		}
		Import newImport = new Import(importURI, alias);
		if (!imports.contains(newImport)) {
			imports.add(newImport);
			
			// cause this Jena model to import each import so that the concepts are known within this OWL model
			theJenaModel.getOntology(getModelBaseURI()); // add import to model
			
		}
		
	}
	
	public List<Import> getOrderedImports() {
		return imports;
	}

	public String getModelAlias() {
		return modelAlias;
	}

	public void setModelAlias(String modelAlias) {
		this.modelAlias = modelAlias;
	}

	public String getModelActualURL() {
		return modelActualURL;
	}

	public void setModelActualURL(String modelActualURL) {
		this.modelActualURL = modelActualURL;
	}

}
