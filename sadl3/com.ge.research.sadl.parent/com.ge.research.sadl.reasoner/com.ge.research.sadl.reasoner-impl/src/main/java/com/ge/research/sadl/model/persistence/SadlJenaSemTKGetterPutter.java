package com.ge.research.sadl.model.persistence;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.tdb.TDB;

import com.ge.research.sadl.model.persistence.ISadlModelGetterPutter;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.semtk.sparqlX.FusekiSparqlEndpointInterface;

public class SadlJenaSemTKGetterPutter extends SadlJenaSemTKGetter implements ISadlModelGetterPutter {
    
	public SadlJenaSemTKGetterPutter(IConfigurationManager mgr, String format) {
		super(mgr, format);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String owlFileName, String format) throws TranslationException, MalformedURLException {
		
		// Check if model exists in store
		//Model modelToRemove = 
		// If  model exists, we delete it
			if (modelExists(publicUri)) {
				try {
					removeModel(publicUri);
				} catch (ConfigurationException | IOException | URISyntaxException e1) {
					e1.printStackTrace();
				}
			

			// Then upload new model
				
//				ModelFactory.createOntologyModel(getCurrentCodeModel().getSpecification(), inferredModel);
				//get string from m, then get byte[]
				RDFWriter rdfw = m.getWriter(format);
				if (format.startsWith("RDF/XML")) {
					rdfw.setProperty("xmlbase", publicUri); //need to strip #?
					rdfw.setProperty("relativeURIs", "");
				}
				OntModel om = (OntModel) m;
				om.setNsPrefix("", modelNamespace);
				ByteArrayOutputStream os = new ByteArrayOutputStream();
				rdfw.write(om.getBaseModel(), os, publicUri);

				try {
					FusekiSparqlEndpointInterface sei = new FusekiSparqlEndpointInterface("http://leb1acdev.hpc.ge.com:3030/ML4M", publicUri);
					sei.uploadOwlModelNoClear(os.toByteArray());
				} catch (Exception e) {
					e.printStackTrace();
				}
				return true;
			}
		

		// If there was an owlfile, e.g. because we just switch persistence format, delete it.
//		File mfile = new File(owlFileName);
//		if (mfile.exists()) {
//			// the OWL file exists and we just uploaded the model, so delete the OWL file
//			if (!mfile.delete()) {
//				logger.error("Failed to delete OWL file '" + owlFileName + "' after saving it to TDB");
//			}
//		}
		return false;
	}

	@Override
	public boolean removeModel(String uri) throws ConfigurationException, IOException, URISyntaxException {
		if (getConfigMgr() instanceof IConfigurationManagerForEditing) {
			// Check if model exists in store
			//Model modelToRemove = 
			// If  model exists, we delete it
			try {
				FusekiSparqlEndpointInterface sei = new FusekiSparqlEndpointInterface("http://leb1acdev.hpc.ge.com:3030/ML4M", uri);
				sei.clearGraph();

				return true;
			
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			throw new IOException("Can't remove models with the current ConfigurationManager class: " + getConfigMgr().getClass().getCanonicalName());
		}
		return false;
	}
	
	@Override
	public int clean() throws ConfigurationException {
		//delete all graphs
		List<String> graphList = getAvailableModelNames();
		
		// delete each one
		return 0;
	}

}
