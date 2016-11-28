package com.ge.research.sadl.jena.inference;

import static org.junit.Assert.*;

import java.io.File;
import java.net.URL;

import org.junit.Test;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.hp.hpl.jena.rdf.model.Model;

public class TestShapesInference {

	@Test
	public void test() throws ConfigurationException, ReasonerNotFoundException, QueryParseException, QueryCancelledException, InvalidNameException {
		URL dataModelsFolder = ClassLoader.getSystemResource("TestModels");
		String kbid = dataModelsFolder.getFile() + "/Shapes";
		String modelName = "http://sadl.org/Test.sadl";
		IConfigurationManager configmgr = ConfigurationManagerFactory.getConfigurationManager(kbid, null);
		IReasoner reasoner = configmgr.getReasoner();
		if (!reasoner.isInitialized()) {
			reasoner.setConfigurationManager(configmgr);
			reasoner.initializeReasoner(kbid, modelName, IConfigurationManager.RDF_XML_ABBREV_FORMAT);
		}
		String query = reasoner.prepareQuery("select ?s ?p ?v where {?s <rdf:type> <Shape> . ?s ?p ?v}");
		ResultSet rs = reasoner.ask(query);
		assertNotNull(rs);
		assertTrue(rs.toString().indexOf("\"http://sadl.org/Test.sadl#MyNapkin\",\"http://sadl.org/concepts.sadl#area\",32.0") > 0);
		assertTrue(rs.toString().indexOf("\"http://sadl.org/Test.sadl#MyPlate\",\"http://sadl.org/concepts.sadl#area\",314.1592741012573") > 0);
		Object im = reasoner.getInferredModel(false);
		if (im instanceof Model) {
			((Model)im).write(System.out);
		}
		System.out.println(rs != null ? rs.toString() : "No results found");
	}

}
