package com.ge.research.sadl.jena;

import java.io.IOException;
import java.net.URISyntaxException;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.hp.hpl.jena.ontology.OntModel;

public interface IMetricsProcessor {

	boolean saveMetrics(String format) throws IOException, ConfigurationException, URISyntaxException;

	ResultSet queryProjectMetrics(String queryName, String modelFolder);

	boolean addMarker(String subjectUri, String markerClassUri, String markerTypeUri);
	
	boolean addControlledOrMonitoredProperty(String specName, String propertyUri);

	boolean addEffectiveRangeAndDomain(String specName, String className, String property, String range, boolean isList);

	void setMetricsTargetModel(OntModel metricsTargetModel);

	String getBaseUri();

}