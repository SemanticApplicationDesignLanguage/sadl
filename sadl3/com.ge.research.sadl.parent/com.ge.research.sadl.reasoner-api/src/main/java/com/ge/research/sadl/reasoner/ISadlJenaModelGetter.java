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
package com.ge.research.sadl.reasoner;

import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.hp.hpl.jena.ontology.OntModel;

public interface ISadlJenaModelGetter {

	public abstract boolean configureToModel(OntModel m);

	/**
	 * Call this method to flush changes to the repository (if needed) 
	 * @return
	 */
	public abstract boolean sync();

	/**
	 * Call this method to close the repository
	 * @return
	 */
	public abstract boolean close();

	/**
	 * Call this method to get an OntModel with a given URI
	 * @param publicUri
	 * @param altUrl
	 * @param format
	 * @return
	 */
	public abstract OntModel getOntModel(String publicUri, String altUrl, String format);

	/**
	 * Call this method to turn on (true) or off (false) the addition of missing models to a TDB backend repository 
	 */
	public abstract void setAddMissingModelToTDB(boolean addMissingModelToTDB);

	public abstract void setTdbFolder(String _tdbFolder);

	public abstract boolean modelExists(String publicUri, String altUrl)
			throws MalformedURLException;

	public abstract String getFormat();

	public abstract void setFormat(String format);

	/**
	 * Method to return the import hierarchy of the specified model
	 * @param modelUrl -- actual URL of the model for which imports are desired
	 * @return configMgr -- an implementation of IConfigurationManager that will be used to resolve mappings
	 * @return -- import hierarch as a Map. The import URI is the key and a Map of that models imports else null if it has none is the value.
	 * @throws ConfigurationException 
	 */
	public abstract HashMap<String, Map> getImportHierarchy(IConfigurationManager configMgr, String modelUri) throws ConfigurationException;

}