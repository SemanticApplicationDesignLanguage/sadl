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
import com.ge.research.sadl.reasoner.TranslationException;

public class SadlJenaTDBGetterPutter extends SadlJenaTDBGetter implements ISadlModelGetterPutter {

	public SadlJenaTDBGetterPutter(IConfigurationManager mgr, String fmt) throws IOException {
		super(mgr, fmt);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String OwlFileName, String format)
			throws TranslationException {
		try {
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
		}
		File mfile = new File(OwlFileName);
		if (mfile.exists()) {
			// the OWL file exists and we just saved the OWL model to TDB, so delete the OWL file
			if (!mfile.delete()) {
				logger.error("Failed to delete OWL file '" + OwlFileName + "' after saving it to TDB");
			}
		}
		return true;
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
    
}
