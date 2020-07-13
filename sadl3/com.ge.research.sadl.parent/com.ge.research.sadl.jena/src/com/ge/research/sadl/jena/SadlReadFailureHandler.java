/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
package com.ge.research.sadl.jena;

import java.io.IOException;

import org.slf4j.Logger;

import org.apache.jena.ontology.OntDocumentManager.ReadFailureHandler;
import org.apache.jena.rdf.model.Model;

public class SadlReadFailureHandler implements ReadFailureHandler {
	Logger logger;
//	private ConfigurationManager sadlConfigMgr = null;
	
	public SadlReadFailureHandler(Logger logger) {
		this.logger = logger;
	}

	public void handleFailedRead(String url, Model model, Exception e) {
		IOException e2 = new IOException("Failed to read model '" + url + "': " + e.getMessage(), e);
//		getSadlConfigMgr().setReadError(e2.getMessage());
		logger.error(e2.getMessage());
//		try {
//			throw e2;
//		} catch (IOException e1) {
//			e1.printStackTrace();
//		}
	}

//	public ConfigurationManager getSadlConfigMgr() {
//		return sadlConfigMgr;
//	}
//
//	public void setSadlConfigMgr(ConfigurationManager sadlConfigMgr) {
//		this.sadlConfigMgr = sadlConfigMgr;
//	}

}
