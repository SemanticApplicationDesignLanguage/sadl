package com.ge.research.sadl.model.persistence;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDB;

import com.ge.research.sadl.model.persistence.ISadlModelGetterPutter;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.TranslationException;

/**
 * Strategy: all read calls to SadlJenaTDBGetter check and if needed open READ transaction. 
 * Putter checks for transaction and if true ends, opens WRITE transaction, does save, ends, 
 * reopens READ transaction.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class SadlJenaTDBGetterPutter extends SadlJenaTDBGetter implements ISadlModelGetterPutter {

	public SadlJenaTDBGetterPutter(IConfigurationManager mgr, String fmt) throws IOException {
		super(mgr, fmt);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String owlFileName, String format)
			throws TranslationException {
		try {
			if (ds.isInTransaction()) {
				ds.end();
			}
	    	ds.begin(ReadWrite.WRITE);
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
			ds.commit();
			TDB.sync(ds);
		}
		catch (Exception e) {
			throw new TranslationException(e.getMessage(), e);
		}
		finally {
			ds.end();
			ds.begin(ReadWrite.READ);
		}
		if (owlFileName.endsWith(".tdb")) {
			File existingOwlFile = findExistingOwlFilename(owlFileName);
			if (existingOwlFile !=  null) {
				// the OWL file exists and we just saved the OWL model to TDB, so delete the OWL file
				if (!existingOwlFile.delete()) {
					try {
						logger.error("Failed to delete OWL file '" + existingOwlFile.getCanonicalPath() + "' after saving it to TDB");
					} catch (IOException e) {
						// not worth doing anything?
					}
				}
			}
		}
		return true;
	}
	
	/**
	 * Method to find a left-over OWL file from non-TDB if one exists
	 * @param owlFileName
	 * @return
	 */
	private File findExistingOwlFilename(String owlFileName) {
		int idx = owlFileName.lastIndexOf(".");
		if (idx > 0) {
			String baseFN = owlFileName.substring(0, idx + 1);
			List<String> extensions = SadlPersistenceFormat.getOwlFileExtensions();
			for (String ext : extensions) {
				File f = new File(baseFN + ext);
				if (f.exists()) {
					return f;
				}
			}
		}
		return null;
	}

	@Override
	public boolean removeModel(String uri) throws IOException {
		if (getConfigMgr() instanceof IConfigurationManagerForEditing) {
			try {
				if (ds.containsNamedModel(uri)) {
					if (ds.isInTransaction()) {
						ds.end();
					}
					ds.begin(ReadWrite.WRITE);
					ds.removeNamedModel(uri);
					ds.commit();
					TDB.sync(ds);
					return true;
				}
			}
			finally {
				ds.end();
				ds.begin(ReadWrite.READ);
			}
		} else {
			throw new IOException("Can't remove models with the current ConfigurationManager class: " + getConfigMgr().getClass().getCanonicalName());
		}
		return false;
	}

    /**
     * Call this method to remove all named models from a TDB repository
     * @return
     * @throws ConfigurationException
     */
	@Override
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
					if (ds.isInTransaction()) {
						ds.end();
					}
					ds.begin(ReadWrite.WRITE);
					ds.removeNamedModel(modelsToRemove.get(i));
					cnt++;
				}
				catch (Throwable t) {
					throw new ConfigurationException("Unexpected exception: " + t.getMessage());
				}
				finally {
					ds.commit();
					TDB.sync(ds);
					ds.end();
					ds.begin(ReadWrite.READ);
				}
			}
			return cnt;
		}
		return 0;
	}
    
}
