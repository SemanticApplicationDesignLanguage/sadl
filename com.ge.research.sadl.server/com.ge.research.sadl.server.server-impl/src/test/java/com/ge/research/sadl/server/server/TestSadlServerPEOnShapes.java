package com.ge.research.sadl.server.server;

import static org.junit.Assert.*;

import java.util.Collections;
import java.util.List;

import junit.framework.TestCase;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.server.ISadlServerMD;
import com.ge.research.sadl.server.NamedServiceNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;

/**
 * @author 200005201
 * 
 * This test case works with the simple Shapes ontology.
 * The pre-existing ontology in DataModels/ShapesDemo includes shapes-top,
 * 	shapes-specfic, and shapes-rules. 
 *  1) The test case adds a triangle class and a rule to calculate 
 *  	area of triangles in a triangle model and persists it as a named service. 
 *  2) It then creates an instance model (scenario) of various shapes and 
 *  	persists it as a named service.
 *  3) It then executes queries against the persisted scenario
 *
 */

public class TestSadlServerPEOnShapes extends TestCase {
	private String modelFolder;
	private String modelName;
	private String instNS = "http://edu.uiuc/mobius/advise#";
	
	private String baseOntologyNamedService = "Shapes";
	private String augmentedOntologyNamedService = "TrianglesAdded";
	private String specificScenarioNamedService = "MyPersistedScenario";
	
	@Before
	public void setUp() throws Exception {
		super.setUp();
		modelFolder = ClassLoader.getSystemResource("DataModels/ShapesDemo").getFile();
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		modelName = "http://www.illinois.edu/advise/SmallExample";
	}



	@Test
	public void test0() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
		// demonstrate ability to access the base named service
		ISadlServerMD srvr = new SadlServerMDImpl();
		srvr.setKbaseRoot(modelFolder);
		String session = srvr.selectServiceModel(baseOntologyNamedService);
		assertNotNull(session);
		String qry = srvr.prepareQuery("select ?sc where {?sc <rdfs:subClassOf> <Shape>}");
		ResultSet rs = srvr.query(qry);
		assertNotNull(rs);
		assertTrue(rs.toString().contains("Rectangle"));
	}

}
