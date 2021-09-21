package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.IOException;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDB;

public class SadlJenaTDBGetterPutter extends SadlJenaTDBGetter implements ISadlModelGetterPutter {

	public SadlJenaTDBGetterPutter(IConfigurationManager mgr, String fmt) throws IOException {
		super(mgr, fmt);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String OwlFileName, String format)
			throws TranslationException {
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
		File mfile = new File(OwlFileName);
		if (mfile.exists()) {
			// the OWL file exists and we just saved the OWL model to TDB, so delete the OWL file
			if (!mfile.delete()) {
				logger.error("Failed to delete OWL file '" + OwlFileName + "' after saving it to TDB");
			}
		}
		return true;
	}

}
