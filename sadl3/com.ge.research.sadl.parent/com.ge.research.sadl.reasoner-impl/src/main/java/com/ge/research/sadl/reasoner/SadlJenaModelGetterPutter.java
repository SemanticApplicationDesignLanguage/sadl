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

package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.tdb.TDB;
import com.hp.hpl.jena.tdb.TDBFactory;

/**
 * This class extends the base class by adding the ability to save a model to the backend repository
 * 
 * @author 200005201
 *
 */
public class SadlJenaModelGetterPutter extends SadlJenaModelGetter {
	
    /**
     * This constructor should be called when the repository format is not known
     * @param _tdbFolder
     * @throws ConfigurationException 
     * @throws IOException 
     */
    public SadlJenaModelGetterPutter(IConfigurationManager configMgr, String modelFolder) throws ConfigurationException, IOException {
    	super(configMgr, modelFolder);
    	setAddMissingModelToTDB(false);
    }
    
    /**
     * This constructor should be called when the repository format is known
     * @param _tdbFolder
     * @param format
     */
    public SadlJenaModelGetterPutter(IConfigurationManager configMgr, String _tdbFolder, String format) {
    	super(configMgr, _tdbFolder, format);
    	setAddMissingModelToTDB(true);
    }
    	
    /**
     * Call this method to remove all named models from a TDB repository
     * @return
     * @throws ConfigurationException
     */
    public int clean() throws ConfigurationException {
    	if (ds != null) {
    		int cnt = 0;
			List<String> modelsToRemove = new ArrayList<String>();
    		Iterator<String> namedModelsItr = ds.listNames();
    		while (namedModelsItr != null && namedModelsItr.hasNext()) {
    			String modelname = namedModelsItr.next();
    			modelsToRemove.add(modelname);
    		}
    		for (int i = 0; i < modelsToRemove.size(); i++) {
    			try {
   					ds.removeNamedModel(modelsToRemove.get(i));
   					cnt++;
    			}
    			catch (Throwable t) {
    				throw new ConfigurationException("Unexpected exception: " + t.getMessage());
    			}
    		}
    		return cnt;
    	}
    	return 0;
    }
    
    private boolean removeTdbFolder() {
    	if (tdbFolder != null) {
    		boolean status = true;
    		File folder = new File(tdbFolder);
    		if (folder.exists()) {
    			SadlUtils su = new SadlUtils();
    			if (!su.recursiveDelete(folder)) {
    				status = false;
    			}
    			return status;
    		}
    	}
    	return false;
    }

    /**
	 * Call this method to save a model to the repository
	 * @param m -- the Model to be saved
	 * @param modelNamespace -- namespace of the model, needed to write some formats
	 * @param publicUri -- the URI naming the model
	 * @param owlFilename -- the altUrl used for writing to a file-based repository
	 * @param format -- the format to be used for this particular save--can be non-TDB for service files 
	 * @return true if saved else false
	 * @throws IOException 
	 */
	public synchronized boolean saveModel(Model m, String modelNamespace, String publicUri, String owlFilename, String format) throws IOException {
    	if (format.equals(IConfigurationManager.JENA_TDB)) {
    		if (ds == null) {
    			setTdbFolder(getTdbFolder());
    		}
    		Model modelToRemove = ds.getNamedModel(publicUri);
    		if (modelToRemove != null) {
    			ds.replaceNamedModel(publicUri, m);
    		}
    		else {
    			// just add the base model; the imported models should be separately stored in the repository Dataset
    			if (m instanceof OntModel) {
    				ds.addNamedModel(publicUri, ((OntModel)m).getBaseModel());	 
    			}
    			else {
    				ds.addNamedModel(publicUri, m);	 
    			}
    		}
    		TDB.sync(ds);
    		File mfile = new File(owlFilename);
    		if (mfile.exists()) {
    			// the OWL file exists and we just saved the OWL model to TDB, so delete the OWL file
    			if (!mfile.delete()) {
    				logger.error("Failed to delete OWL file '" + owlFilename + "' after saving it to TDB");
    			}
    		}
    	}
    	else {
			FileOutputStream fps = new FileOutputStream(owlFilename);
			RDFWriter rdfw = m.getWriter(format);
			// NTripleWriter.setProperty always throws UnknownPropertyException;
			// ditto for N3.
			if (format.startsWith("RDF/XML")) {
				rdfw.setProperty("xmlbase", publicUri);
				rdfw.setProperty("relativeURIs", "");
				rdfw.setProperty("minimalPrefixes", true);
			}

			m.setNsPrefix("", modelNamespace);
			if (m instanceof OntModel) {
				rdfw.write(((OntModel)m).getBaseModel(), fps, publicUri);
			}
			else {
				rdfw.write(m, fps, publicUri);
			}
			fps.close();
			
			boolean tdbFolderExists = false;
			if (tdbFolder != null) {
				File tdbFile = new File(tdbFolder);
				tdbFolderExists = tdbFile.exists();
			}
			if (!tdbFolderExists && ds != null) {
				ds.close();
				ds = null;
			}
			if (tdbFolderExists && ds == null) {
	    		ds = TDBFactory.createDataset( tdbFolder );
			}
			if (ds != null) {
				// we just saved the OWL model to a file so if it is also in TDB remove it from TDB
				ds.removeNamedModel(publicUri);
				TDB.sync(ds);
				int cnt = 0;
				Iterator<String> nameItr = ds.listNames();
				while (nameItr.hasNext()) {
					nameItr.next();
					cnt++;
				}
				if (cnt == 0) {
					// there are no models left--try to delete the TDB folder
					ds.close();
					ds = null;
					removeTdbFolder();
				}
			}
    	}
		return true;
	}
}
