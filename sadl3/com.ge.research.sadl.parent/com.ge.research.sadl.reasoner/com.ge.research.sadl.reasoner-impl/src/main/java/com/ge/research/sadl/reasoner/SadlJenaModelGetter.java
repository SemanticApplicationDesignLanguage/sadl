/************************************************************************
 * Copyright 2007-2013 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.8 $ Last modified on   $Date: 2015/07/31 11:32:33 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ModelGetter;
import org.apache.jena.rdf.model.ModelReader;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.OWL;

/**
 * This class can be registered with the Jena OntModelSpec to provide loading
 * of imported models when the backend repository is Jena TDB
 */
public class SadlJenaModelGetter implements ModelGetter, ISadlJenaModelGetter {
    protected static final Logger logger = LoggerFactory.getLogger(SadlJenaModelGetter.class);

	private ModelGetter originalModelGetter = null;
	protected String tdbFolder = null;
    protected Dataset ds = null;
    
    private String format = null;
    private boolean addMissingModelToTDB = false;
    
    private IConfigurationManager configurationManager = null;
    
    /**
     * This constructor should be called when the repository format is not known
     * @param _tdbFolder
     */
    public SadlJenaModelGetter(IConfigurationManager configMgr, String _tdbFolder) {
    	configurationManager = configMgr;
    	File tdbFile = _tdbFolder != null ? new File(_tdbFolder) : null;
    	if (tdbFile != null && tdbFile.exists()) {
    		setFormat(SadlSerializationFormat.JENA_TDB_FORMAT);	// if the caller doesn't tell us the format
    													// and the TDB folder exists, use it
    	}
    	else {
    		setFormat(SadlSerializationFormat.RDF_XML_ABBREV_FORMAT);
    	}
    	setTdbFolder(_tdbFolder);
     	if (originalModelGetter == null) {
    		// save original (file-based) importModelGetter
    		originalModelGetter = OntModelSpec.getDefaultSpec(OWL.getURI()).getImportModelGetter();
    	}
    }

    /**
     * This constructor should be called when the repository format is known
     * @param _tdbFolder
     * @param format
     */
    public SadlJenaModelGetter(IConfigurationManager configMgr, String _tdbFolder, String _format) {
    	configurationManager = configMgr;
    	setFormat(_format);
    	setTdbFolder(_tdbFolder);

		if (originalModelGetter == null) {
    		// we haven't already gotten the original (file-based) importModelGetter so get it
    		originalModelGetter = OntModelSpec.getDefaultSpec(OWL.getURI()).getImportModelGetter();
     	}
    }
    
    
    /* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#configureToModel(org.apache.jena.ontology.OntModel)
	 */
    public boolean configureToModel(OntModel m) {
    	ModelGetter mg = m.getSpecification().getImportModelGetter();
    	if (mg.equals(this)) {
    		return false;
    	}
    	else if (mg.getClass().equals(this.getClass())) {
    		return false;
    	}
    	else if (originalModelGetter == null) {
    		originalModelGetter = mg;
    		m.getSpecification().setImportModelGetter(this);
    		return true;
    	}
    	else {
    		m.getSpecification().setImportModelGetter(this);
    		return true;
    	}
    }
    
    /* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#sync()
	 */
    public boolean sync() {
    	if (ds != null) {
    		TDB.sync(ds);
    		return true;
    	}
    	return false;
    }
    
    /* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#close()
	 */
    public boolean close() {
    	if (ds != null) {
    		TDB.sync(ds);
    		ds.close();
    		ds = null;
    		return true;
    	}
    	return false;
    }
    
    /**
     * Call this method to get a model by it's public URI
     */
    public Model getModel( String uri ) {
    	if (ds != null && ds.containsNamedModel(uri)) {
    		Model m = ds.getNamedModel(uri);
    		if (m != null) {
    			return m;
    		}
    		else if (originalModelGetter != null) {
    			m = originalModelGetter.getModel(uri);
    			if (m != null) {
    				if (addMissingModelToTDB) {
    					ds.addNamedModel( uri, m );
    					TDB.sync(m);
    				}
    				return m;
    			}
     		}
    	}
    	else if (originalModelGetter != null) {
    		Model m = originalModelGetter.getModel(uri);
    		if (m != null) {
    			return m;
    		}
    	}
		return null;
    }

    /**
     * Call this method to get a model by its public URI and if necessary read it using the specified ModelReader
     */
    public Model getModel( String uri, ModelReader loadIfAbsent ) {
    	boolean addToTDB = addMissingModelToTDB;
    	Model m = null;
    	if (uri.equals(IConfigurationManager.ServicesConfigurationURI)) {
    		// this is a special case--it is always left as an OWL file in RDF/XML format
    		addToTDB = false;
    	}
    	else if (getFormat().equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
    		// try TDB first
    		m = getModel(uri);
    	}
    	if (m == null && loadIfAbsent != null) {
            String altUrl = configurationManager.getJenaDocumentMgr().doAltURLMapping(uri);
            if (altUrl != null && altUrl.equals(uri)) {
            	// try for a dependent project
            	try {
					String dpAltUrl = configurationManager.getAltUrlFromPublicUri(uri);
					if (dpAltUrl != null) {
						altUrl = dpAltUrl;
					}
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
            }
            if (altUrl != null && altUrl.endsWith(".TDB/")) {
            	try {
            		SadlUtils su = new SadlUtils();
            		Dataset tmpds = TDBFactory.createDataset(su.fileUrlToFileName(altUrl));
            		tmpds.begin(ReadWrite.READ);
					m = tmpds.getDefaultModel();
					tmpds.end();
				} catch (MalformedURLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
            }
            else {
                m = ModelFactory.createDefaultModel();
	            loadIfAbsent.readModel( m, altUrl != null ? altUrl : uri );
	            if (addToTDB && ds != null && getFormat().equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
	            	ds.begin(ReadWrite.WRITE);
	            	ds.addNamedModel( uri, m );
	            	ds.commit();
	            	ds.end();
	            	TDB.sync(ds);
	            }
            }
        }

        return m;
    }
    
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#getOntModel(java.lang.String)
	 */
    public OntModel getOntModel(String publicUri, String altUrl, String format) {
    	Model m = getModel(publicUri, new ModelReader() {
            @Override
            public Model readModel(Model toRead, String URL) {
               RDFDataMgr.read(toRead, URL);
               return toRead;
            }
         });
    	if (m == null) {
			m = ModelFactory.createOntologyModel(configurationManager.getOntModelSpec(null));
			m.read(altUrl, format);			
    	}
    	if (m instanceof OntModel) {
    		return (OntModel)m;
    	}
    	else {
    		configurationManager.getOntModelSpec(null).setImportModelGetter(this);
    		return ModelFactory.createOntologyModel(configurationManager.getOntModelSpec(null), m);
    	}
    }

    /* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#setAddMissingModelToTDB(boolean)
	 */
	public void setAddMissingModelToTDB(boolean addMissingModelToTDB) {
		this.addMissingModelToTDB = addMissingModelToTDB;
	}

	public String getTdbFolder() {
		return tdbFolder;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#setTdbFolder(java.lang.String)
	 */
	public void setTdbFolder(String _tdbFolder) {
    	if (ds != null) {
    		// we have an open TDB Dataset
    		if (getTdbFolder() != null && !_tdbFolder.equals(getTdbFolder())) {
    			// the TDB location has changed
    			TDB.sync(ds);
    			ds.close();
    			ds = TDBFactory.createDataset( _tdbFolder );
    		}
    	}
    	else if (getFormat().equals(SadlSerializationFormat.JENA_TDB_FORMAT)){
			ds = TDBFactory.createDataset( _tdbFolder );
    	}
		tdbFolder = _tdbFolder;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#modelExists(java.lang.String, java.lang.String)
	 */
	public boolean modelExists(String publicUri, String altUrl) throws MalformedURLException {
		if (ds != null) {
			if (ds.containsNamedModel(publicUri)) {
				return true;
			}
			if (logger.isDebugEnabled()) {
				Iterator<String> modeliter = ds.listNames();
				while (modeliter.hasNext()) {
					logger.debug("Dataset contains model '" + modeliter.next() + "'");
				}
			}
		}
		if (altUrl.startsWith("http:")) {
			URL dataURL = new URL(altUrl);
			URLConnection uc;
			try {
				uc = dataURL.openConnection();
				uc.connect();
				return true;
			} catch (IOException e) {
				return false;
			}
//			// is there some way to test this now?
//			return true;
		}
		SadlUtils su = new SadlUtils();
		File f = new File(su.fileUrlToFileName(altUrl));
		if (f.exists()) {
			return true;
		}
		return false;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

}
