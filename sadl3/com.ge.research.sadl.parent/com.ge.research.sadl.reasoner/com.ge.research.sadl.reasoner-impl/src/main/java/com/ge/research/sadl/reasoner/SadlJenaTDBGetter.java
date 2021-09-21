package com.ge.research.sadl.reasoner;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelReader;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.OWL;

public class SadlJenaTDBGetter extends SadlModelGetter {

	protected String tdbFolder = null;
    protected Dataset ds = null;
    
    public SadlJenaTDBGetter(IConfigurationManager mgr, String fmt) throws IOException {
    	super(mgr, fmt);
    	tdbFolder = mgr.getModelFolder()  + "/TDB";
     	if (getOriginalModelGetter() == null) {
    		// save original (file-based) importModelGetter
    		setOriginalModelGetter(OntModelSpec.getDefaultSpec(OWL.getURI()).getImportModelGetter());
    	}
     	ds = TDBFactory.createDataset(tdbFolder);
     	ds.begin(ReadWrite.READ);
     	ds.end();
	}
	
	@Override
	public Model getModel(String uri) {
		if(ds.containsNamedModel(uri)) {
			return ds.getNamedModel(uri);
		}
		return null;
	}

	@Override
	public Model getModel(String URL, ModelReader loadIfAbsent) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean modelExists(String uri) throws ConfigurationException, MalformedURLException {
		if (ds.containsNamedModel(uri)) {
			return true;
		}
		return false;
	}

	@Override
	public List<String> getAvailableModelNames() {
		Iterator<String> existingNames = ds.listNames();
		if (existingNames.hasNext()) {
			List<String> names = new ArrayList<String>();
			for (String name : names) {
				names.add(name);
			}
			return names;
		}
		return null;
	}

}
