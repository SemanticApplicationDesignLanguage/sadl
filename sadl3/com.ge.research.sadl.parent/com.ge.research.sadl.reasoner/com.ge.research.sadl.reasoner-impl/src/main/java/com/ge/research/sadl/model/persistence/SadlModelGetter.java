package com.ge.research.sadl.model.persistence;

import java.io.ByteArrayOutputStream;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ModelGetter;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.vocabulary.OWL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.persistence.ISadlModelGetter;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.TranslationException;

abstract public class SadlModelGetter implements ModelGetter, ISadlModelGetter {
	protected static final Logger logger = LoggerFactory.getLogger(SadlModelGetter.class);

	private IConfigurationManager configMgr = null;
	private ModelGetter originalModelGetter;
    private OntModelSpec modelSpec = null;
    private String format = null;

	public SadlModelGetter(IConfigurationManager mgr, String fmt) {
		setConfigMgr(mgr);
		format = fmt;
		modelSpec = mgr.getOntModelSpec(null);
		originalModelGetter = OntModelSpec.getDefaultSpec(OWL.getURI()).getImportModelGetter();
}
	
	@Override
	public String getFormat() {
		return format;
	}

	public IConfigurationManager getConfigMgr() {
		return configMgr;
	}

	protected void setConfigMgr(IConfigurationManager configMgr) {
		this.configMgr = configMgr;
	}

	protected ModelGetter getOriginalModelGetter() {
		return originalModelGetter;
	}

	protected void setOriginalModelGetter(ModelGetter originalModelGetter) {
		this.originalModelGetter = originalModelGetter;
	}

	protected OntModelSpec getModelSpec() {
		return modelSpec;
	}

	protected void setModelSpec(OntModelSpec modelSpec) {
		this.modelSpec = modelSpec;
	}

	@Override
	public CharSequence getModelAsString(Model m, String prefix, String modelName, String format, Charset charset)
			throws TranslationException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		if (prefix != null) {
			if (m instanceof OntModel) {
				((OntModel) m).getBaseModel().setNsPrefix(prefix, modelName);
				if (prefix.length() > 0) {
					// also add the empty string prefix to enable finding the URI of this model from the OWL file
					((OntModel) m).getBaseModel().setNsPrefix("", modelName);
				}
			}
		}
		RDFDataMgr.write(out, m, SadlPersistenceFormat.getRDFFormat(format));
		CharSequence seq = new String(out.toByteArray(), charset);
		return seq;
	}

	@Override
	public OntModel getOntModel(String uri) {
		Model m = getModel(uri);
		if (m instanceof OntModel) {
			return (OntModel)m;
		}
		else {
			getConfigMgr().getOntModelSpec(null).setImportModelGetter(this);
			return ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null), m);
		}
	
	}

	@Override
	public HashMap<String, Map> getImportHierarchy(String modelUri) throws ConfigurationException {
		String modelUrl = getConfigMgr().getAltUrlFromPublicUri(modelUri);
		OntModel model = getOntModel(modelUri);
		Set<String> imports = model.listImportedOntologyURIs();
		if (imports != null) {
			HashMap<String, Map> thisMap = new HashMap<String,Map>();
			Iterator<String> impitr = imports.iterator();
			while (impitr.hasNext()) {
				String importUri = impitr.next();
				thisMap.put(importUri, getImportHierarchy(importUri));
			}
			return thisMap;
		}
		return null;
	}

}
