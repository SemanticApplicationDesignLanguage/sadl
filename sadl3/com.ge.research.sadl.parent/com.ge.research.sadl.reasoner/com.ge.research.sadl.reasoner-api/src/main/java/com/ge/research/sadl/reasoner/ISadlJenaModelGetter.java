package com.ge.research.sadl.reasoner;

import java.net.MalformedURLException;

import org.apache.jena.ontology.OntModel;

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

}