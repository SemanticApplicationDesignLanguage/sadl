package com.ge.research.sadl.jena.inference;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.model.SadlUnionClass;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.utils.ResourceManager;
import org.apache.jena.ontology.OntModelSpec;

public class TestGetModelImports {
	
	private String kbroot = null;
	
	@Before
	public void setUp() throws Exception {
		kbroot = ClassLoader.getSystemResource("TestModels").getFile();
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		System.out.println("kbroot: " + kbroot);
		File check = new File(kbroot);
		if (!check.exists()) {
			throw new IOException("kbroot '" + kbroot + "' does not exist. Something is wrong.");
		}
	}

//	@Ignore("https://github.com/crapo/sadlos2/issues/332")
	@Test
	public void test() throws ConfigurationException, URISyntaxException {
		String modelFolder = kbroot + "/Shapes";
		String modelName = "Test.owl";
		Map<String,Map>imports = getImportHierarch(modelFolder, modelName);
		System.out.println("Import hierarchy for '" + modelName + "'");
		displayImportMap(imports, 1);
	}
	
	/**
	 * Method to get the import hierarchy of a given OWL model in a SADL model folder
	 * @param modelFolder -- the location of the SADL OwlModels folder
	 * @param owlModelName -- the name of the OWL model in the model folder for which imports is desired
	 * @return -- a Map with key the URI of an import, the value a Map of the imported models imports 
	 * @throws ConfigurationException 
	 * @throws URISyntaxException 
	 */
	private Map<String, Map> getImportHierarch(String modelFolder, String owlModelName) throws ConfigurationException, URISyntaxException {
		IConfigurationManager configMgr = new ConfigurationManager(modelFolder, null);
		ISadlJenaModelGetter getter = new SadlJenaModelGetter(OntModelSpec.OWL_MEM, modelFolder);
		String modelUrl = new SadlUtils().fileNameToFileUrl(modelFolder + "/" + owlModelName);
		Map<String,Map> imports = getter.getImportHierarchy(configMgr, configMgr.getPublicUriFromActualUrl(modelUrl));
		return imports;
	}

	private void displayImportMap(Map<String, Map> imports, int level) {
		if (imports != null) {
			Iterator<String> itr = imports.keySet().iterator();
			while (itr.hasNext()) {
				String key = itr.next();
				Map val = imports.get(key);
				for (int i = 0; i < level; i++) {
					System.out.print("\t");
				}
				System.out.println(key);
				displayImportMap(val, level + 1);
			}
		}
	}

}
