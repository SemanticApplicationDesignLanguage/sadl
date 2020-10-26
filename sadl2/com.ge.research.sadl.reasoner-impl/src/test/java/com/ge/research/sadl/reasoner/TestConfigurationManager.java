package com.ge.research.sadl.reasoner;

import static org.junit.Assert.*;

import java.util.Collections;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

public class TestConfigurationManager extends EclipseOrMavenJUnitTest {

	private String modelFolder1;
	private String modelFolder2;
	private String kbroot;

	@Before
	public void setUp() throws Exception {
		kbroot = getKbRoot();
		modelFolder1 = kbroot + "/Model1/OwlModels";
		modelFolder2 = kbroot + "/Model2/OwlModels";
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
	}

	@Test
	public void testConfigurationManager() throws ConfigurationException {
		String modelUri1 = "http://sadl.org/SadlLinking1/RuleBasedRelations";
		String modelUri2 = "http://sadl.org/SadlLinking2/AdamsFamily";
		String genBaseUri = "http://sadl.org/SadlLinking1/GenealogyBase";
		IConfigurationManager icm1 = new ConfigurationManager(modelFolder1, IConfigurationManager.RDF_XML_ABBREV_FORMAT);
		assertNotNull(icm1);
		IConfigurationManager icm2 = new ConfigurationManager(modelFolder2, IConfigurationManager.RDF_XML_ABBREV_FORMAT);
		assertNotNull(icm2);
		String altUri1 = icm1.getAltUrlFromPublicUri(modelUri1);
		assertNotNull(altUri1);
		String altUri2 = icm2.getAltUrlFromPublicUri(modelUri2);
		assertNotNull(altUri2);
		String genAltUri1 = icm1.getAltUrlFromPublicUri(genBaseUri);
		assertNotNull(genAltUri1);
		String genAltUri2 = icm2.getAltUrlFromPublicUri(genBaseUri);
		assertEquals(genAltUri2, genBaseUri);
	}

}
