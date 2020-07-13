package com.ge.research.sadl.reasoner;

import java.io.IOException;

import org.slf4j.Logger;

import org.apache.jena.ontology.OntDocumentManager.ReadFailureHandler;
import org.apache.jena.rdf.model.Model;

public class SadlReadFailureHandler implements ReadFailureHandler {
	Logger logger;
	private ConfigurationManager sadlConfigMgr = null;
	
	public SadlReadFailureHandler(Logger logger) {
		this.logger = logger;
	}

	public void handleFailedRead(String url, Model model, Exception e) {
		IOException e2 = new IOException("Failed to read model '" + url + "': " + e.getMessage(), e);
		getSadlConfigMgr().setReadError(e2.getMessage());
		logger.error(e2.getMessage());
//		try {
//			throw e2;
//		} catch (IOException e1) {
//			e1.printStackTrace();
//		}
	}

	public ConfigurationManager getSadlConfigMgr() {
		return sadlConfigMgr;
	}

	public void setSadlConfigMgr(ConfigurationManager sadlConfigMgr) {
		this.sadlConfigMgr = sadlConfigMgr;
	}

}
