package com.ge.research.sadl.model.persistence;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;

import org.apache.jena.rdf.model.Model;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.TranslationException;

public interface ISadlModelGetterPutter extends ISadlModelGetter {
	
	/**
	 * Method to persist a Jena Model.
	 * @param m--the Jena Model to persist
	 * @param modelNamespace--the namespace of the model (SADL assumes a model has a single namespace)
	 * @param publicUri--the model namespace with a "#" appended
	 * @param OwlFileName--for file-based persistence, the OWL filename; otherwise null
	 * @param format--the format used for persistence as defined in SadlPersistenceFormat
	 * @throws TranslationException 
	 * @throws IOException 
	 * @return--true if successfully saved else false
	 */
	public abstract boolean saveModel(Model m, String modelNamespace, String publicUri, String OwlFileName, String format) throws TranslationException, IOException, MalformedURLException;

	/**
	 * Model to remove a Jena model from the persistent repository
	 * @param uri
	 * @return
	 * @throws MalformedURLException 
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	boolean removeModel(String uri) throws MalformedURLException, ConfigurationException, IOException, URISyntaxException;

	/**
	 * Call this method to remove all named models from a TDB repository
	 * @return
	 * @throws ConfigurationException
	 */
	public abstract int clean() throws ConfigurationException;

}
