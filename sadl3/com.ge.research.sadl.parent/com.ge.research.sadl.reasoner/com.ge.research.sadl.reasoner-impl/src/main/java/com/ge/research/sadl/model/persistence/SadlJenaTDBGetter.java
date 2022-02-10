package com.ge.research.sadl.model.persistence;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ModelReader;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.vocabulary.OWL;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;

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
	}
	
	@Override
	public Model getModel(String uri) {
     	if (!ds.isInTransaction()) {
     		ds.begin(ReadWrite.READ);
     	}
     	try {
			if(ds.containsNamedModel(uri)) {
				Model m = ds.getNamedModel(uri);
				return m;
			}
     	}
     	finally {
//     		ds.end();
     	}
		return null;
	}

	@Override
	public OntModel getOntModel(String uri) {
		Model m = getModel(uri);
		if (m instanceof OntModel) {
			return (OntModel)m;
		}
		else {
	     	if (!ds.isInTransaction()) {
	     		ds.begin(ReadWrite.READ);
	     	}
	     	try {
	     		getConfigMgr().getOntModelSpec(null).setImportModelGetter(this);
	     		OntModel om = ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null), m);
				om.getDocumentManager().setProcessImports(true);
				om.loadImports();
				return om;
	     	}
	     	finally {
//	     		ds.end();
	     	}
		}
	}
	
	@Override
	public Model getModel(String uri, ModelReader loadIfAbsent) {
     	if (!ds.isInTransaction()) {
     		ds.begin(ReadWrite.READ);
     	}
     	try {
			if (ds.containsNamedModel(uri)) {
				return ds.getNamedModel(uri);
			}
     	}
     	finally {
//    		ds.end();
     	}
		return null;
	}
	
	@Override
	public boolean modelExists(String uri) throws ConfigurationException, MalformedURLException {
     	if (!ds.isInTransaction()) {
     		ds.begin(ReadWrite.READ);
     	}
     	try {
			if (ds.containsNamedModel(uri)) {
				return true;
			}
     	}
     	finally {
//    		ds.end();
     	}
		return false;
	}

	@Override
	public List<String> getAvailableModelNames() {
     	if (!ds.isInTransaction()) {
     		ds.begin(ReadWrite.READ);
     	}
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
