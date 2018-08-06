package com.ge.research.sadl.server.server;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.server.ISadlServerMD;
import com.ge.research.sadl.server.NamedServiceNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;

import junit.framework.TestCase;

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
		modelFolder = "D:/sadl/workspace-sadl/Mobius2/OwlModels";
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		modelName = "http://www.mobius.illinois.edu/advise/ont/core/Queries";
	}

	@Ignore
	@Test
	public void testWithJenaReasoner() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
		// demonstrate ability to access the base named service
		modelFolder = "D:/sadl/workspace-sadl/Mobius2.new/OwlModels";
		ISadlServerMD srvr = new SadlServerMDImpl();
		srvr.setKbaseRoot(modelFolder);
		List<ConfigurationItem> preferences = new ArrayList<ConfigurationItem>();
		String[] categoryTree = new String[1];
		categoryTree[0] = "http://com.ge.research.sadl.configuration#ReasonerSpec";
		ConfigurationItem ci = new ConfigurationItem(categoryTree);
		NameValuePair nvp = ci.new NameValuePair("reasonerClassName", "com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin");
		ci.addNameValuePair(nvp);
		preferences.add(ci);
		String session = srvr.selectServiceModel(baseOntologyNamedService, preferences);
		assertNotNull(session);
		String qry = srvr.prepareQuery("select ?et where {?et <urn:x-hp-direct-predicate:http_//www.w3.org/2000/01/rdf-schema#subClassOf> <http://www.mobius.illinois.edu/advise/ont/core/System#Performer> }");
		ResultSet rs = srvr.query(qry);
		assertNotNull(rs);
		System.out.println(rs.toStringWithIndent(5));
	}

//	@Ignore
//	@Test
//	public void testWithSWIPrologReasoner() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
//		// demonstrate ability to access the base named service
//		modelFolder = "D:/sadl/workspace-sadl/Mobius2.new/OwlModels";
//		ISadlServerMD srvr = new SadlServerMDImpl();
//		srvr.setKbaseRoot(modelFolder);
//		List<ConfigurationItem> preferences = new ArrayList<ConfigurationItem>();
//		String[] categoryTree = new String[1];
//		categoryTree[0] = "http://com.ge.research.sadl.configuration#ReasonerSpec";
//		ConfigurationItem ci = new ConfigurationItem(categoryTree);
//		NameValuePair nvp = ci.new NameValuePair("reasonerClassName", "com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin");
//		ci.addNameValuePair(nvp);
//		preferences.add(ci);
//		String session = srvr.selectServiceModel(baseOntologyNamedService, preferences);
//		assertNotNull(session);
//		String qry = srvr.prepareQuery("select X where rdf(X,'http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.mobius.illinois.edu/advise/ont/core/System#Performer')");
//		ResultSet rs = srvr.query(qry);
//		assertNotNull(rs);
//		System.out.println(rs.toStringWithIndent(5));
//		
//	}
//
//	@Ignore
//	@Test
//	public void test1() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
//		// demonstrate ability to get correct query answer
//		modelFolder = "DS:/sadl/workspace-sadl/Mobius2.new/OwlModels";
//		String modelName = "http://www.mobius.illinois.edu/advise/ont/core/RecloserSimple1";
//		ISadlServerMD srvr = new SadlServerMDImpl();
//		srvr.setKbaseRoot(modelFolder);
//		String session = srvr.selectServiceModel(modelFolder, modelName);
//		assertNotNull(session);
//		String qry = srvr.prepareQuery("select Sk P where minSkillProficiency('http://www.mobius.illinois.edu/advise/ont/core/Attack#AdminModifyFWOpen',Sk,P)");
//		ResultSet rs = srvr.query(qry);
//		assertNotNull(rs);
//		System.out.println(rs.toStringWithIndent(5));		
//	}
	
	@Test
	public void testCreateAndDelete() throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException, InvalidNameException, IOException, TripleNotFoundException, QueryCancelledException, URISyntaxException {
		String modelName = "http://www.mobius.illinois.edu/advise/ont/core/InitInstanceModel";
		ISadlServerMD srvr = new SadlServerMDImpl();
		assertNotNull(srvr);
		assertNotNull(srvr.selectServiceModel(modelFolder, modelName));
		String instMn = "http://www.mobius.illinois.edu/advise/ont/inst1";
		String instNs = instMn + "#";
		String newUri = srvr.getUniqueInstanceUri(instMn, instNs, "Device");
		String newInst = srvr.createInstance(instMn, newUri, "http://www.mobius.illinois.edu/advise/ont/core/System#Device");
		assertNotNull(newInst);
		assertEquals(newUri, newInst);
		assertFalse(srvr.updateRdfsLabel(instMn, newUri, "Ken's Device", null));
		assertTrue(srvr.addTriple(instMn, newInst, "http://www.mobius.illinois.edu/advise/ont/core/System#loginPolicyImpact", 0.0));
		System.out.println(srvr.ask(instMn, newUri, null, null));
		assertTrue(srvr.deleteTriple(instMn, newUri, null, null));
		assertFalse(srvr.deleteTriple(instMn, null, null, newInst));
		System.out.println(srvr.ask(instMn, newUri, null, null));
		System.out.println(srvr.ask(instMn, null, null, newUri));
		assertNotNull(srvr.getErrors());
		String owlInstanceFileName = "file:///c:/tmp/inst1.owl";
		String globalPrefix = "inst1";
		assertTrue(srvr.persistInstanceModel(instMn, owlInstanceFileName, globalPrefix));
	}
}
