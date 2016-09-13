package com.ge.research.sadl.jena;

import java.io.IOException;
import java.net.URISyntaxException;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ResultSet;

public interface IMetricsProcessor {

	boolean saveMetrics(String format) throws IOException, ConfigurationException, URISyntaxException;

	ResultSet queryProjectMetrics(String queryName, String modelFolder);

	boolean addMarker(String subjectUri, String markerClassUri, String markerTypeUri);

	boolean addControlledVariable(String specName, String propertyUri);

	boolean addMonitoredVariable(String specName, String propertyUri);

	boolean addControlledOrMonitoredVariable(String specName, String propertyUri, boolean isControlled);

}