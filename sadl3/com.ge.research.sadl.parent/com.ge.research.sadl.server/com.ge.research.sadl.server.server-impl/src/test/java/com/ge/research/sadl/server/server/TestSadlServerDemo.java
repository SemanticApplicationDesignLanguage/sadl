/************************************************************************
 * Copyright (c) 2007-2014 - General Electric Company, All Rights Reserved
 *
 * Project: SADL Knowledge Server
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language. 
 * 
 * The SADL Knowledge Server is a set of Java classes implementing 
 * a service interface for deploying ontology-based knowledge bases
 * for use in a client-server environment.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/09 14:06:51 $
 ***********************************************************************/

package com.ge.research.sadl.server.server;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;
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
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.server.ISadlServer;
import com.ge.research.sadl.server.ISadlServerPE;
import com.ge.research.sadl.server.NamedServiceNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;

public class TestSadlServerDemo extends TestCase {

	private String kbaseRoot;
	private String modelFolder;
	
	private String shapesMN;
	private String ruleMN;
	private String test1MN;
	private String extendedMN;
	private String clientScenarioMN = "http://sadl.org/Shapes/clientdata";
	private String shapesNS;
	private String rectangleNS;
	private String ruleNS;
	private String test1NS;
	private String extendedNS;
	private String clientScenarioNS;
	private String clientScenearioFileName = "ClientData.owl";
	private String clientScenarioGlobalPrefix = "clientdata";
	private String newTestNS = "http://sadl.org/Shapes/NewTest#";
	
	private String serverSideScenario = "Test1Scenario";
	private String clientScenarioService = "ClientScenario";
//	private String initialNamedService = "SSShapes";
	private String initialNamedService = "Shapes";
	private String toBeCreatedNamedService = "SSShapesExtended";
	
	@Before
	public void setUp() throws Exception {
		super.setUp();
		kbaseRoot = ClassLoader.getSystemResource("DataModels").getFile();
//		modelFolder = ClassLoader.getSystemResource("DataModels/Advise2").getFile();
//		modelFolder = ClassLoader.getSystemResource("DataModels/ShapesSadlServerTest/OwlModels").getFile();
		modelFolder = ClassLoader.getSystemResource("DataModels/ShapesDemo/OwlModels").getFile();
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		shapesMN = "http://sadl.org/Shapes/Shapes";
		ruleMN = "http://sadl.org/Shapes/Rule";
		test1MN = "http://sadl.org/Shapes/Test1";
		extendedMN = "http://sadl.org/Shapes/MoreShapes";
		shapesNS = shapesMN + "#";
		rectangleNS = "http://sadl.org/Shapes/Rectangle#";
		ruleNS = ruleMN + "#";
		test1NS = test1MN + "#";	
		extendedNS = extendedMN + "#";
		clientScenarioNS = clientScenarioMN + "#";
	}
	
	@Test
	public void testServerVersionInfo() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, InvalidNameException, QueryCancelledException, QueryParseException, IOException {
		// get an instance of the server
		ISadlServer srvr = new SadlServerImpl(kbaseRoot);
		assertNotNull(srvr);
		// get server version
		String sv = srvr.getServiceVersion();
		assertNotNull(sv);
		System.out.println("Server version: " + sv);
		
//		// select a service model--this is necessary to enable creation of a reasoner--and then get the reasoner version
//		assertNotNull(srvr.selectServiceModel(serverSideScenario));
//		String rv = srvr.getReasonerVersion();
//		assertNotNull(rv);
//		System.out.println("Reasoner version: " + rv);
//		
//		// get the version of the ontology (model) specified in the named service
//		String modelName = srvr.getModelName();
//		String vqry = srvr.prepareQuery("select ?ver where {<" + modelName + "> <owl:versionInfo> + ?ver}");
//		ResultSet rs = srvr.query(vqry);
//		assertNotNull(rs);
//		System.out.println("Server-side scenario ontology (" + modelName + ") version: " + rs.getResultAt(0, 0));
//		
//		// get the names of all imported models and their versions
//		String ivqry = srvr.prepareQuery("select ?impont ?impver where {<" + modelName + "> <owl:imports>+ ?impont . ?impont <owl:versionInfo> ?impver}");
//		rs = srvr.query(ivqry);
//		assertNotNull(rs);
//		System.out.println("Imported ontologies and versions: " + rs.toStringWithIndent(5));
	}

	@Test
	public void testSadlServerServerSideScenario() throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, NamedServiceNotFoundException, IOException, InvalidNameException, QueryCancelledException, QueryParseException {
		ISadlServer srvr = new SadlServerImpl(kbaseRoot);
		assertNotNull(srvr);
		
//		assertNotNull(srvr.selectServiceModel(serverSideScenario));
//		assertTrue(test1MN.equals(srvr.getModelName()));
//		
//		String qry = srvr.prepareQuery("select ?shape ?area where {?shape <area> ?area}");
//		ResultSet rs = srvr.query(qry);
//		assertNotNull(rs);
//		assertTrue(rs.getRowCount() > 0);
//		String[] cols = rs.getColumnNames();
//		assertTrue(cols[0].equals("shape"));
//		assertTrue(cols[1].equals("area"));
//		assertTrue(rs.getResultAt(0, 0).equals(test1NS+"MyRect"));
//		assertTrue(rs.getResultAt(0, 1).equals(13.75));
	}

	@Test
	public void testSadlServerClienSideScenario() throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, NamedServiceNotFoundException, InvalidNameException, IOException, TripleNotFoundException, QueryCancelledException {
//		ISadlServer srvr = new SadlServerImpl(kbaseRoot);
//		assertNotNull(srvr);
//		Map<String, String[]> map = srvr.getServiceNameMap();
//		int x = map.size();
//		assertNotNull(srvr.selectServiceModel(initialNamedService));
//		String newRectUri = clientScenarioNS + "Rect" + System.currentTimeMillis();
//		String instUri = srvr.createInstance(newRectUri, rectangleNS + "Rectangle");
//		assertTrue(instUri.equals(newRectUri));
//		assertTrue(srvr.addTriple(instUri, rectangleNS + "height", 10));
//		assertTrue(srvr.addTriple(instUri, rectangleNS + "width", 12));
//		ResultSet rs = srvr.ask(instUri, shapesNS + "area", null);
//		assertNotNull(rs);
//		assertEquals(rs.getResultAt(0, 0),120.0);
	}

	@Test
	public void testSadlServerClienSideScenarioWithPersistence() throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, NamedServiceNotFoundException, InvalidNameException, IOException, TripleNotFoundException, QueryCancelledException {
		ISadlServerPE srvr = new SadlServerPEImpl(kbaseRoot);
		assertNotNull(srvr);
		assertNotNull(srvr.selectServiceModel(initialNamedService));
//		String newRectUri = clientScenarioNS + "Rect" + System.currentTimeMillis();
//		String instUri = srvr.createInstance(newRectUri, rectangleNS + "Rectangle");
//		assertTrue(instUri.equals(newRectUri));
//		assertTrue(srvr.addTriple(instUri, rectangleNS + "height", 10));
//		assertTrue(srvr.addTriple(instUri, rectangleNS + "width", 12));
//		ResultSet rs = srvr.ask(instUri, shapesNS + "area", null);
//		assertNotNull(rs);
//		assertEquals(rs.getResultAt(0, 0),120.0);
//		assertTrue(srvr.persistInstanceModel(clientScenearioFileName, clientScenarioGlobalPrefix));
//		
//		ISadlServer srvr2 = new SadlServerImpl(kbaseRoot);
//		assertNotNull(srvr2.selectServiceModel(modelFolder, clientScenarioMN));
//		ResultSet rs2 = srvr.ask(instUri, shapesNS + "area", null);
//		assertNotNull(rs2);
//		assertEquals(rs2.getResultAt(0, 0),120.0);
	}

//	@Test
//	public void testSadlServerClienSideScenarioPersistencedAsNamedService() throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, NamedServiceNotFoundException, InvalidNameException, IOException, TripleNotFoundException, QueryCancelledException {
//		ISadlServerPE srvr = new SadlServerPEImpl(kbaseRoot);
//		assertNotNull(srvr);
//		assertNotNull(srvr.selectServiceModel(initialNamedService));
//		String newRectUri = clientScenarioNS + "Rect" + System.currentTimeMillis();
//		String instUri = srvr.createInstance(newRectUri, rectangleNS + "Rectangle");
//		assertTrue(instUri.equals(newRectUri));
//		assertTrue(srvr.addTriple(instUri, rectangleNS + "height", 10));
//		assertTrue(srvr.addTriple(instUri, rectangleNS + "width", 12));
//		ResultSet rs = srvr.ask(instUri, shapesNS + "area", null);
//		assertNotNull(rs);
//		assertEquals(rs.getResultAt(0, 0),120.0);
//		assertTrue(srvr.createServiceModel(modelFolder, clientScenarioService, clientScenarioMN, clientScenearioFileName, clientScenarioGlobalPrefix));
//		
//		ISadlServer srvr2 = new SadlServerImpl(kbaseRoot);
//		assertNotNull(srvr2.selectServiceModel(clientScenarioService));
//		ResultSet rs2 = srvr.ask(instUri, shapesNS + "area", null);
//		assertNotNull(rs2);
//		assertEquals(rs2.getResultAt(0, 0),120.0);
//	}

//	@Test
//	public void testSadlServerPENewModel() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException {
//		ISadlServerPE srvr = new SadlServerPEImpl(kbaseRoot);
//		assertNotNull(srvr);
//		assertNotNull(srvr.selectServiceModel(initialNamedService));
//		srvr.createServiceModel(kbid, serviceName, modelName, owlFileName)
//	}

	private static String writeDataSourceToString(DataSource out) {
		InputStream is = null;
		try {
			is = out.getInputStream();
			java.util.Scanner s = new java.util.Scanner(is);
			s.useDelimiter("\\A");
		    return s.hasNext() ? s.next() : "";
		} catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			if (is != null) {
			    try {
					is.close();
				} catch (IOException e) {
					e.printStackTrace();
				}				
			}
		}
		return null;
	}

}
