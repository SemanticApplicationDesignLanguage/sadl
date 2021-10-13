package com.ge.research.sadl.model.persistence;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
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
import com.ge.research.semtk.resultSet.Table;
import com.ge.research.semtk.sparqlX.FusekiSparqlEndpointInterface;
import com.ge.research.semtk.sparqlX.SparqlEndpointInterface;

public class SadlJenaSemTKGetter extends SadlModelGetter {

	protected String semtkURL = null;
	
    protected String allGraphsQry = "SELECT DISTINCT ?graphs WHERE { GRAPH ?graphs { ?s ?p ?o }}"; 
    protected String retrieveGraphQry = "CONSTRUCT {?s ?p ?o} FROM <http://aske.ge.com/turbo> WHERE {?s ?p ?o}";
    
    
    public SadlJenaSemTKGetter(IConfigurationManager mgr, String fmt)  {
    	super(mgr, fmt);
    	if (fmt != SadlPersistenceFormat.SEMTK_FORMAT) {
//    		throw (new IOException());
    	}
//    	tdbFolder = mgr.getModelFolder()  + "/TDB";
    	
    	
    	
     	if (getOriginalModelGetter() == null) {
    		// save original (file-based) importModelGetter
    		setOriginalModelGetter(OntModelSpec.getDefaultSpec(OWL.getURI()).getImportModelGetter());
    	}
//     	ds = TDBFactory.createDataset(tdbFolder);
	}
	
//	@Override
	public Model getModel(String uri) {
		try {
			if(modelExists(uri)) {
				String modelStr = getNamedModel(uri);
				// create an empty model
				InputStream is = new ByteArrayInputStream(modelStr.getBytes());
			 	Model model = ModelFactory.createDefaultModel();
			 	model.read(is,null);
				return model;
			}
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		return null;
	}

	private String getNamedModel(String uri) {
		//TODO
		try {
			FusekiSparqlEndpointInterface sei = new FusekiSparqlEndpointInterface("http://leb1acdev.hpc.ge.com:3030/ML4M", "http://kdl.ge.com/IPD");
			String res = sei.executeQueryToRdf(retrieveGraphQry);
			return res;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

//	@Override
	public OntModel getOntModel(String uri) {
		Model m = getModel(uri);
		if (m instanceof OntModel) {
			return (OntModel)m;
		}
		else {
//			ds.begin(ReadWrite.READ);
			getConfigMgr().getOntModelSpec(null).setImportModelGetter(this);
			OntModel om = ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null), m);
//			om.getDocumentManager().setProcessImports(true);
//			om.loadImports();
//			ds.end();
			return om;
		}
	
	}
//	@Override
	public Model getModel(String URL, ModelReader loadIfAbsent) {
		return null;
	}
	
//	@Override
	public boolean modelExists(String uri) throws MalformedURLException  {
		List<String> models = getAvailableModelNames();
		if (models.contains(uri)) {
			return true;
		}
		return false;
	}

//	@Override
	public List<String> getAvailableModelNames()  {
		try {
			FusekiSparqlEndpointInterface sei = new FusekiSparqlEndpointInterface("http://leb1acdev.hpc.ge.com:3030/ML4M", "http://kdl.ge.com/IPD");
			Table res = sei.executeQueryToTable(allGraphsQry);
			List<String> models = new ArrayList<String>();

			for (int i=0; i<res.getNumRows(); i++) {
				models.add(res.getCell(i, 0));
			}
			return models;
		} catch (Exception e) {
			e.printStackTrace();
		}

//		if (existingNames.hasNext()) {
//			List<String> names = new ArrayList<String>();
//			for (String name : names) {
//				names.add(name);
//			}
//			return names;
//		}
		return null;
	}

}
