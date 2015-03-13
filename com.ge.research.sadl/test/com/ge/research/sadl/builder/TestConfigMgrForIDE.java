package com.ge.research.sadl.builder;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;

public class TestConfigMgrForIDE {

	private String testModelsFolder = null;
	
	@Before
	public void setUp() throws Exception {
		File curdir = new File(".");
		File testFolder = new File(curdir.getCanonicalPath() + "/TestSadlIdeTestProject/OwlModels");
		if (!testFolder.exists()) {
			fail("Test folder '" + testFolder.getCanonicalPath() + "' does not exist.");
		}
		testModelsFolder = testFolder.getCanonicalPath();
	}

	@Test
	public void testGetConfigMgr() throws ConfigurationException {
		IConfigurationManagerForIDE confMgr = new ConfigurationManagerForIDE(testModelsFolder, null);
		assertNotNull(confMgr);
	}
	
	@Test
	public void testIsSadlDerived() throws ConfigurationException, MalformedURLException {
		String publicUri1 = "http://sadl.imp/familyrelationships";
		String publicUri2 = "http://research.ge.com/Acuity/aulo.owl";
		IConfigurationManagerForIDE confMgr = new ConfigurationManagerForIDE(testModelsFolder, null);
		assertNotNull(confMgr);
		assertTrue(confMgr.isSadlDerived(publicUri1));
		assertFalse(confMgr.isSadlDerived(publicUri2));
	}
	
	@Test
	public void testGetModelImports() throws ConfigurationException, IOException, InvalidNameException {
		String publicUri1 = "http://sadl.imp/familyrelationships";
		String publicUri2 = "http://sadl.imp/shapes_test";
		ConceptType cType = null;
		Scope scope = Scope.LOCALONLY;
		IConfigurationManagerForIDE confMgr = new ConfigurationManagerForIDE(testModelsFolder, null);
		assertNotNull(confMgr);
		List<ConceptName> names2 = confMgr.getNamedConceptsInModel(publicUri2, cType, scope);
		assertNotNull(names2);
		
		Map<String, String> importsAndPrefixes1 = confMgr.getImports(publicUri1, Scope.LOCALONLY);
		assertNull(importsAndPrefixes1);
		Map<String, String> importsAndPrefixes2 = confMgr.getImports(publicUri2, Scope.LOCALONLY);
		assertNotNull(importsAndPrefixes2);
		assertTrue(importsAndPrefixes2.size() == 2);
		Iterator<String> importUriItr = importsAndPrefixes2.keySet().iterator();
		while (importUriItr.hasNext()) {
			String importUri = importUriItr.next();
			List<ConceptName> names = confMgr.getNamedConceptsInModel(importUri, cType, scope);
			assertNotNull(names);
		}
		
		displayImportsAndNamedConcepts(confMgr, cType, scope, publicUri2);
	}

	private void displayImportsAndNamedConcepts(IConfigurationManagerForIDE confMgr, ConceptType cType, Scope scope, String publicUri) throws InvalidNameException, ConfigurationException, IOException {
		System.out.println("Model URI: " + publicUri);
		List<ConceptName> names = confMgr.getNamedConceptsInModel(publicUri, cType, scope);
		if (names != null) {
			Iterator<ConceptName> cnitr = names.iterator();
			while (cnitr.hasNext()) {
				System.out.println("     Defines concept: " + cnitr.next().toFQString());
			}
		}
		Map<String, String> importsAndPrefixes = confMgr.getImports(publicUri, Scope.LOCALONLY);
		if (importsAndPrefixes != null && importsAndPrefixes.size() > 0) {
			Iterator<String> importUriItr = importsAndPrefixes.keySet().iterator();
			while (importUriItr.hasNext()) {
				String importUri = importUriItr.next();
				displayImportsAndNamedConcepts(confMgr, cType, scope, importUri);
			}
		}
	}
}
