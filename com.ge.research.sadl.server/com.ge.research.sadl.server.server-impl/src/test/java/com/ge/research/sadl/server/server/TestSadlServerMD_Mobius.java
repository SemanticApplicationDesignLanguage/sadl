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
 * This test case works with the UIUC Mobius ontology.
 * The pre-existing ontology in DataModels/mobiusyyyymmdd includes ,
 *
 */

public class TestSadlServerMD_Mobius extends TestCase {
	private String modelFolder;
	private String modelName;
	private String instNS = "http://edu.uiuc/mobius/advise#";
	
	private String baseOntologyNamedService = "RecloserExample";
	private String augmentedOntologyNamedService = "TrianglesAdded";
	private String specificScenarioNamedService = "MyPersistedScenario";
	
	@Before
	public void setUp() throws Exception {
		super.setUp();
//		modelFolder = ClassLoader.getSystemResource("DataModels/mobius20160211").getFile();
		modelFolder = "E:/sadl/workspace-sadl/Mobius2.new/OwlModels";
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		modelName = "http://www.mobius.illinois.edu/advise/ont/core/Queries";
	}



	@Test
	public void test0() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
		// demonstrate ability to access the base named service
		ISadlServerMD srvr = new SadlServerMDImpl();
		srvr.setKbaseRoot(modelFolder);
		String session = srvr.selectServiceModel(baseOntologyNamedService);
		assertNotNull(session);
		String qry = srvr.prepareQuery("select ?et where {?et <urn:x-hp-direct-predicate:http_//www.w3.org/2000/01/rdf-schema#subClassOf> <http://www.mobius.illinois.edu/advise/ont/core/System#Performer> }");
		ResultSet rs = srvr.query(qry);
		assertNotNull(rs);
		System.out.println(rs.toStringWithIndent(5));
		
		qry = srvr.prepareQuery("select X where rdf(X,'http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.mobius.illinois.edu/advise/ont/core/System#Performer')");
		rs = srvr.query(qry);
		assertNotNull(rs);
		System.out.println(rs.toStringWithIndent(5));
	}

}
