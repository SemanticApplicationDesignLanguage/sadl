package com.ge.research.sadl.server.server;

import static org.junit.Assert.*;

import java.io.File;
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

public class TestTest extends TestCase {

	private String modelFolder;
	private String modelName;
	private String adversaryNS;
	private String systemNS;
	private String instNS = "http://edu.uiuc/mobius/advise#";

	@Before
	public void setUp() throws Exception {
		super.setUp();
//		modelFolder = ClassLoader.getSystemResource("DataModels/Advise2").getFile();
		modelFolder = ClassLoader.getSystemResource("DataModels/Mobius2/OwlModels").getFile();
//		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
//		loggers.add(LogManager.getRootLogger());
//		for ( Logger logger : loggers ) {
////		    logger.setLevel(Level.OFF);
//			logger.setLevel(Level.DEBUG);
//		}
		adversaryNS = "http://www.mobius.illinois.edu/advise/ont/core/Adversary";
		systemNS = "http://www.mobius.illinois.edu/advise/ont/core/System";
		modelName = "http://www.mobius.illinois.edu/advise/ont/core/SmallExample";
	}

	@Test
	public void testByKbModName() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
		ISadlServerMD srvr1 = new SadlServerMDImpl(modelFolder, modelName);
		assertNotNull(srvr1);
		String qstr = "select ?v where {?r <owl:onProperty> <hasResourceLevel> . ?r <owl:hasValue> ?v}";
		qstr = srvr1.prepareQuery(qstr);
		ResultSet rs = srvr1.query(qstr);
		assertNotNull(rs);
		System.out.println(rs.toString());
	}

	@Test
	public void testByServiceName() throws ConfigurationException, ReasonerNotFoundException, NamedServiceNotFoundException, SessionNotFoundException, QueryCancelledException, QueryParseException, InvalidNameException {
		ISadlServerMD srvr1 = new SadlServerMDImpl();
		assertNotNull(srvr1);
		File mf = new File(modelFolder);
		String kbroot = mf.getParent();
		srvr1.setKbaseRoot(kbroot);
		assertNotNull(srvr1.selectServiceModel("SmallExample"));
		String qstr = "select ?v where {?r <owl:onProperty> <hasResourceLevel> . ?r <owl:hasValue> ?v}";
		qstr = srvr1.prepareQuery(qstr);
		ResultSet rs = srvr1.query(qstr);
		assertNotNull(rs);
		System.out.println(rs.toString());
	}

}
