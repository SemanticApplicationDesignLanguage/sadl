package com.ge.research.sadl.reasoner;

import java.io.File;
import java.net.MalformedURLException;
import java.util.List;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ModelReader;

import com.ge.research.sadl.reasoner.utils.SadlUtils;

public class SadlJenaFileGetter extends SadlModelGetter {

	public SadlJenaFileGetter(IConfigurationManager mgr, String fmt) {
		super(mgr, fmt);
	}
	
	@Override
	public Model getModel(String uri) {
		if (getOriginalModelGetter() != null) {
    		Model m = getOriginalModelGetter().getModel(uri);
    		if (m != null) {
    			return m;
    		}
    	}
		return null;
	}

	@Override
	public Model getModel(String uri, ModelReader loadIfAbsent) {
		Model m = null;
    	if (loadIfAbsent != null) {
            String altUrl = getModelSpec().getDocumentManager().doAltURLMapping(uri);
            m = ModelFactory.createDefaultModel();
            loadIfAbsent.readModel( m, altUrl != null ? altUrl : uri );
        }

        return m;
	}

	@Override
	public boolean modelExists(String namespace) throws ConfigurationException, MalformedURLException {
		String publicUri;
		if (namespace.endsWith("#")) {
			publicUri = namespace.substring(0, namespace.length() - 1);
		}
		else {
			publicUri = namespace;
		}
		String altUrl = getConfigMgr().getAltUrlFromPublicUri(publicUri);
		if (altUrl == null) {
			altUrl = publicUri;	// by default, try to find at the URI
		}
		SadlUtils su = new SadlUtils();
		File f = new File(su.fileUrlToFileName(altUrl));
		if (f.exists()) {
			return true;
		}
		return false;
	}

	@Override
	public List<String> getAvailableModelNames() {
		// TODO Auto-generated method stub
		return null;
	}

}
