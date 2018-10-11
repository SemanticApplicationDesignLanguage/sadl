/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.jena;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.activation.DataSource;

import org.eclipse.emf.ecore.resource.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.owl2sadl.OwlToSadl;
import com.ge.research.sadl.processing.ISadlImportProcessor;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.OntModel;

public class JenaBasedSadlImportProcessor implements ISadlImportProcessor {
	private static final Logger logger = LoggerFactory.getLogger(JenaBasedSadlImportProcessor.class);

	private OntModel theJenaModel;
	
	enum AnnType {ALIAS, NOTE}
	
	@Inject DeclarationExtensions declarationExtensions;
	
    private List<String> errors = new ArrayList<String>();

	private String resultingDataSource;

	
	public JenaBasedSadlImportProcessor() {
		logger.debug("New " + this.getClass().getCanonicalName() + "' created");
	}
	/**
	 * For TESTING
	 * @return
	 */
	public OntModel getTheJenaModel() {
		return theJenaModel;
	}
	
	@Override
	public Object[] onImport(Resource resource, String projectPath) {
		try {
//			String policyFileName = UtilsForJena.getPolicyFilePathForProject(projectPath);
			String owlFilePath = resource.getURI().toFileString();
	    	File ffop = new File(owlFilePath);
//	    	String sfop = ffop.toURI().toString();
	        setResultingDataSource(null);
			OwlToSadl owlToSadl = new OwlToSadl(ffop); //sfop,policyFileName);
			setResultingDataSource(owlToSadl.getSadlModel());
		} catch (Exception e1) {
	        if (getResultingDataSource() == null) {
	            getErrors().add(e1.toString());
	        }
		}
		Object[] retval = new Object[2];
		try {
			retval[0] = getResultingDataSource();
		} catch (Throwable t) {
			t.printStackTrace();
		}
		retval[1] = getErrors();
		return retval;	
	}
	
	@Override
	public Object[] onImport(String owlContent) {
		try {
			OwlToSadl owlToSadl = new OwlToSadl(owlContent);
			setResultingDataSource(owlToSadl.getSadlModel());

		} catch (Exception e1) {
	        if (getResultingDataSource() == null) {
	            getErrors().add(e1.toString());
	        }
		}
		Object[] retval = new Object[2];
		retval[0] = getResultingDataSource();
		retval[1] = getErrors();
		return retval;
	}

	public List<String> getErrors() {
		return errors;
	}
	
	
	private void setResultingDataSource(String resultingDataSource) {
		this.resultingDataSource = resultingDataSource;
	}
	
	private String getResultingDataSource() {
		return resultingDataSource;
	}
}
