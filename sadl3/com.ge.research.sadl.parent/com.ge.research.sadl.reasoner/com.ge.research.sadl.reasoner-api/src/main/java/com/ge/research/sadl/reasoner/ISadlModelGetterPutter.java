package com.ge.research.sadl.reasoner;

import java.io.IOException;

import org.apache.jena.rdf.model.Model;

public interface ISadlModelGetterPutter extends ISadlModelGetter{
	
	/**
	 * Method to persist a Jena Model.
	 * @param m--the Jena Model to persist
	 * @param modelNamespace--the namespace of the model (SADL assumes a model has a single namespace)
	 * @param publicUri--the model namespace with a "#" appended
	 * @param OwlFileName--for file-based persistence, the OWL filename; otherwise null
	 * @param format--the format used for persistence as defined in SadlSerializationFormat
	 * @throws TranslationException 
	 * @throws IOException 
	 * @return--true if successfully saved else false
	 */
	public abstract boolean saveModel(Model m, String modelNamespace, String publicUri, String OwlFileName, String format) throws TranslationException, IOException;

}
