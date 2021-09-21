/************************************************************************
 * Copyright © 2021 - Natural Semantics, LLC. All Rights Reserved.
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
import java.nio.charset.Charset;
import java.util.List;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelGetter;

public interface ISadlModelGetter extends ModelGetter{
	
	/**
	 * Method to get the format being used in a SADL knowledge base to persist OWL files.
	 * @return--the specified format
	 */
	public abstract String getFormat();

	/**
	 * Method to determine if the current persistence repository has a model with this namespace
	 * @param namespace
	 * @return
	 * @throws ConfigurationException 
	 * @throws MalformedURLException 
	 */
	public abstract boolean modelExists(String namespace) throws ConfigurationException, MalformedURLException;

	/**
	 * Method to serialize a model as a String
	 * @param m
	 * @param prefix
	 * @param modelName
	 * @param format
	 * @param charset
	 * @return
	 * @throws TranslationException 
	 */
	CharSequence getModelAsString(Model m, String prefix, String modelName, String format, Charset charset) throws TranslationException;

	/**
	 * Method to get a list of all of the model names (URIs, names of named graphs) that are available in this persisence repository
	 * @return
	 */
	public abstract List<String> getAvailableModelNames();
	
}
