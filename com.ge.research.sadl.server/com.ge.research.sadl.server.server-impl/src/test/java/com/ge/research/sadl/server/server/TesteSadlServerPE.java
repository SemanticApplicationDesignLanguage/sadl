/**
 * 
 */
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
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.server.ISadlServerMD;
import com.ge.research.sadl.server.SessionNotFoundException;

/**
 * @author 200005201
 * 
 */
public class TesteSadlServerPE extends TestCase {

	private String modelFolder;
	private String modelName;
	private String instNS = "http://edu.uiuc/mobius/advise#";
	
	@Before
	public void setUp() throws Exception {
		super.setUp();
		modelFolder = ClassLoader.getSystemResource("DataModels/Advise2").getFile();
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		modelName = "http://www.illinois.edu/advise/SmallExample";
	}


//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#loadData(java.lang.String)}.
//	 */
//	@Test
//	public void testLoadData() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#sendData(javax.activation.DataSource)}.
//	 */
//	@Test
//	public void testSendDataDataSource() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#sendData(javax.activation.DataSource, java.lang.String)}.
//	 */
//	@Test
//	public void testSendDataDataSourceString() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addTriple(java.lang.String, java.lang.String, java.lang.Object)}.
//	 */
//	@Test
//	public void testAddTripleStringStringObject() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#deleteTriple(java.lang.String, java.lang.String, java.lang.Object)}.
//	 */
//	@Test
//	public void testDeleteTripleStringStringObject() {
//		fail("Not yet implemented");
//	}

	/**
	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#reset()}.
	 * @throws SessionNotFoundException 
	 * @throws ReasonerNotFoundException 
	 * @throws ConfigurationException 
	 */
	@Test
	public void testReset() throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException {
		ISadlServerMD srvr = new SadlServerMDImpl(modelFolder, modelName);
		assertNotNull(srvr);
		assertTrue(srvr.reset());
	}

//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#setInstanceDataNamespace(java.lang.String)}.
//	 */
//	@Test
//	public void testSetInstanceDataNamespace() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#createInstance(java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testCreateInstanceStringString() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#getModelNamespace(java.lang.String)}.
//	 */
//	@Test
//	public void testGetModelNamespace() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#SadlServerPEImpl()}.
//	 */
//	@Test
//	public void testSadlServerPEImpl() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#SadlServerPEImpl(java.lang.String)}.
//	 */
//	@Test
//	public void testSadlServerPEImplString() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#createServiceModel(java.lang.String, java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testCreateServiceModel() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#persistInstanceModel(java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testPersistInstanceModelStringString() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#persistInstanceModel(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testPersistInstanceModelStringStringString() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#persistChangesToServiceModels()}.
//	 */
//	@Test
//	public void testPersistChangesToServiceModels() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addTriple(java.lang.String, java.lang.String, java.lang.String, java.lang.Object)}.
//	 */
//	@Test
//	public void testAddTripleStringStringStringObject() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#deleteTriple(java.lang.String, java.lang.String, java.lang.String, java.lang.Object)}.
//	 */
//	@Test
//	public void testDeleteTripleStringStringStringObject() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#getUniqueInstanceUri(java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testGetUniqueInstanceUri() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addInstance(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddInstance() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addClass(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddClass() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addOntProperty(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddOntProperty() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addOntPropertyDomainClass(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddOntPropertyDomainClass() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addObjectPropertyRangeClass(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddObjectPropertyRangeClass() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#setDatatypePropertyRange(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testSetDatatypePropertyRange() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addAllValuesFromRestriction(java.lang.String, java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddAllValuesFromRestriction() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addSomeValuesFromRestriction(java.lang.String, java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddSomeValuesFromRestriction() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addHasValueRestriction(java.lang.String, java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddHasValueRestriction() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addCardinalityRestriction(java.lang.String, java.lang.String, java.lang.String, int)}.
//	 */
//	@Test
//	public void testAddCardinalityRestriction() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addMinCardinalityRestriction(java.lang.String, java.lang.String, java.lang.String, int)}.
//	 */
//	@Test
//	public void testAddMinCardinalityRestriction() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addMaxCardinalityRestriction(java.lang.String, java.lang.String, java.lang.String, int)}.
//	 */
//	@Test
//	public void testAddMaxCardinalityRestriction() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#setConfigurationMgr(com.ge.research.sadl.reasoner.ConfigurationManagerForEditing)}.
//	 */
//	@Test
//	public void testSetConfigurationMgrConfigurationManagerForEditing() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#getConfigurationMgr()}.
//	 */
//	@Test
//	public void testGetConfigurationMgr() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#getErrors()}.
//	 */
//	@Test
//	public void testGetErrors() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#createInstance(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testCreateInstanceStringStringString() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addRule(java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddRule() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#deleteModel(java.lang.String)}.
//	 */
//	@Test
//	public void testDeleteModel() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addExistingModel(java.lang.String, java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddExistingModel() {
//		fail("Not yet implemented");
//	}
//
//	/**
//	 * Test method for {@link com.ge.research.sadl.server.server.SadlServerPEImpl#addImport(java.lang.String, java.lang.String)}.
//	 */
//	@Test
//	public void testAddImport() {
//		fail("Not yet implemented");
//	}
//
}
