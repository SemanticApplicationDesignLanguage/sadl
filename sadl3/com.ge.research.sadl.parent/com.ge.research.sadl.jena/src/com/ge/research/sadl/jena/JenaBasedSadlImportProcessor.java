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
	public enum RangeValueType {CLASS_OR_DT, LIST, LISTS}
	
	@Inject DeclarationExtensions declarationExtensions;
	
    private List<String> errors = new ArrayList<String>();

	private DataSource resultingDataSource;

	
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
	public Object[] onImport(Resource resource, String policyFileName) {
		String owlFilePath = resource.getURI().toFileString();
    	File ffop = new File(owlFilePath);
    	String sfop = ffop.toURI().toString();
        setResultingDataSource(null);

		try {
			OwlToSadl owlToSadl = new OwlToSadl(sfop,policyFileName);
			setResultingDataSource(owlToSadl.getSadlModel());

		} catch (Exception e1) {
	        if (getResultingDataSource() == null) {
	            getErrors().add(e1.toString());
	        }
		}
		Object[] retval = new Object[2];
		try {
			retval[0] = getResultingDataSource() != null ? getResultingDataSource().getInputStream() : null;
		} catch (IOException e) {
			getErrors().add(e.getMessage());
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
		try {
			retval[0] = getResultingDataSource() != null ? getResultingDataSource().getInputStream() : null;
		} catch (IOException e) {
			getErrors().add(e.getMessage());
		}
		retval[1] = getErrors();
		return retval;
	}

	public List<String> getErrors() {
		return errors;
	}
	
	
	private void setResultingDataSource(DataSource resultingDataSource) {
		this.resultingDataSource = resultingDataSource;
	}
	
	private DataSource getResultingDataSource() {
		return resultingDataSource;
	}
}
