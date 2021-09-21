package com.ge.research.sadl.reasoner;

import java.io.IOException;

import org.apache.jena.rdf.model.Model;

public class SadlJenaTDBGetterPutter extends SadlJenaTDBGetter implements ISadlModelGetterPutter {

	public SadlJenaTDBGetterPutter(IConfigurationManager mgr, String fmt) throws IOException {
		super(mgr, fmt);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String OwlFileName, String format)
			throws TranslationException {
		// TODO Auto-generated method stub
		return false;
	}

}
