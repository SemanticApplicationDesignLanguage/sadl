/************************************************************************
 * Copyright 2007-2016 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.jena.inference;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.utils.SadlUtils;

/**
 * This class can be registered with the Jena OntModelSpec to provide loading
 * of imported models when the backend repository is Jena TDB
 */
public class SadlJenaModelGetter implements ModelGetter, ISadlJenaModelGetter {
    protected static final Logger logger = LoggerFactory.getLogger(SadlJenaModelGetter.class);

	private ModelGetter originalModelGetter = null;
	private String modelFolder = null;
	protected String tdbFolder = null;
    protected Dataset ds = null;
    
    private String format = null;
    private boolean addMissingModelToTDB = false;
    
    private IConfigurationManager configurationManager = null;
	
	public static final String ServicesConfigurationURI = "http://com.ge.research.sadl/sadlserver/Services";

	    private OntModelSpec modelSpec = null;
    
    /**
     * This constructor should be called when the repository format is not known
     * @param _tdbFolder
     * @throws ConfigurationException 
     */
    public SadlJenaModelGetter(OntModelSpec spec, String modelFolder) throws ConfigurationException {
    	this.modelFolder = modelFolder;
    	File modelFolderFile = new File(modelFolder);
    	if (!modelFolderFile.exists()) {
    		throw new ConfigurationException("Invalid model folder (" + modelFolder + ")passed to SadlJenaModelGetter");
    	}
    	modelSpec = spec;
    	File tdbFile = new File(getTdbFolder());
    	if (tdbFile.exists()) {
    		setFormat(SadlSerializationFormat.JENA_TDB_FORMAT);	// if the caller doesn't tell us the format
    													// and the TDB folder exists, use it
    	}
    	else {
    		setFormat(SadlSerializationFormat.RDF_XML_ABBREV_FORMAT);
    	}
     	if (originalModelGetter == null) {
    		// save original (file-based) importModelGetter
    		originalModelGetter = spec.getImportModelGetter();
    	}
    }

    /**
     * This constructor should be called when the repository format is known
     * @param _tdbFolder
     * @param format
     */
    public SadlJenaModelGetter(OntModelSpec configMgr, String _tdbFolder, String _format) {
//    	configurationManager = configMgr;
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
    	if (uri.equals(ServicesConfigurationURI)) {
    		// this is a special case--it is always left as an OWL file in RDF/XML format
    		addToTDB = false;
    	}
    	else if (getFormat().equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
    		// try TDB first
    		m = getModel(uri);
    	}
    	if (m == null && loadIfAbsent != null) {
            String altUrl = modelSpec.getDocumentManager().doAltURLMapping(uri);
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
//            loadUserDefinedDataTypes(uri, altUrl);
        }

        return m;
    }
    
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#getOntModel(java.lang.String)
	 */
    public OntModel getOntModel(String publicUri, String altUrl, String format) {
    	Model m = getModel(publicUri, new ModelReader() {
            @Override
            public Model readModel(Model toRead, String url) {
            	RDFDataMgr.read(toRead, url);
            	return toRead;
            }
         });
    	if (m == null) {
			m = ModelFactory.createOntologyModel(modelSpec);
			m.read(altUrl, format);			
    	}
    	if (m instanceof OntModel) {
    		return (OntModel)m;
    	}
    	else {
    		modelSpec.setImportModelGetter(this);
    		return ModelFactory.createOntologyModel(modelSpec, m);
    	}
    }

    /* (non-Javadoc)
	 * @see com.ge.research.sadl.reasoner.ISadlJenaModelGetter#setAddMissingModelToTDB(boolean)
	 */
	public void setAddMissingModelToTDB(boolean addMissingModelToTDB) {
		this.addMissingModelToTDB = addMissingModelToTDB;
	}

	public String getTdbFolder() {
		if (tdbFolder == null) {
			tdbFolder = getModelFolder() + "/TDB";
		}
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

//    private void loadUserDefinedDataTypes(String uri, String altUrl) {
//		String uddtFolder;
//		try {
//			uddtFolder = getModelXsdFolder(uri, altUrl);
//			if (uddtFolder != null) {
//				File uddtFolderFile = new File(uddtFolder);
//				if (uddtFolderFile.exists() && uddtFolderFile.isDirectory()) {
//					File[] xsdFiles = uddtFolderFile.listFiles();
//					for (int i = 0; i < xsdFiles.length; i++) {
//						FileReader fr = new FileReader(xsdFiles[i]);
//				        XSDDatatype.loadUserDefined(uri, fr, null, TypeMapper.getInstance());				
//					}
//				}
//			}
//		} catch (MalformedURLException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (FileNotFoundException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//	}

	private String getModelXsdFolder(String uri, String altUrl) throws MalformedURLException {
		String uriModified = uri.substring(7);
		SadlUtils su = new SadlUtils();
		String fn = su.fileUrlToFileName(altUrl);
		if (fn.endsWith(".TDB/")) {
			// TODO need to handle TD B
			fn = new File(fn).getParent();
		}
		File f = new File(fn);
		if (f.exists()) {
			return f.getParent() + File.separator + uriModified;			
		}
		return null;
	}

	public String getTdbFolder(String modelFolder) throws IOException {
		return getModelFolder() + "/TDB";
	}

	private String getModelFolder() {
		return modelFolder;
	}

	private void setModelFolder(String modelFolder) {
		this.modelFolder = modelFolder;
	}

	@Override
	public HashMap<String, Map> getImportHierarchy(IConfigurationManager configMgr, String modelUri) throws ConfigurationException {
		configurationManager = configMgr;
		String modelUrl = configMgr.getAltUrlFromPublicUri(modelUri);
		OntModel model = getOntModel(modelUri, modelUrl, null);
		Set<String> imports = model.listImportedOntologyURIs();
		if (imports != null) {
			HashMap<String, Map> thisMap = new HashMap<String,Map>();
			Iterator<String> impitr = imports.iterator();
			while (impitr.hasNext()) {
				String importUri = impitr.next();
				thisMap.put(importUri, getImportHierarchy(configMgr, importUri));
			}
			return thisMap;
		}
		return null;
	}


}
