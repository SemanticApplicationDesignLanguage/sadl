/**
 * 
 */
package com.ge.research.sadl.jena.importer;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collections;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.importer.AbortDataRowException;
import com.ge.research.sadl.importer.TemplateException;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;

/**
 * @author 200005201
 *
 */
public class CsvImporterDocExampleTest {

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


	@Ignore("https://github.com/crapo/sadlos2/issues/332")
	@Test
	public void testTemplateBasedImport_01() throws URISyntaxException, IOException, ConfigurationException, TemplateException, InvalidNameException, AbortDataRowException, QueryCancelledException, ReasonerNotFoundException {
		String kbasePath = kbroot + File.separator + "CsvImporterDocExample";
		String modelPath = kbasePath  + File.separator + "OwlModels";
		String templatePath = "file:/" + kbasePath + File.separator + "mapping.tmpl";
		String dataPath = kbasePath + File.separator + "testdata.csv";
		
		CsvImporter imp = new CsvImporter();
		imp.setModelFolder(modelPath);
		imp.setImportFilename(dataPath, true);
		imp.setTemplates(templatePath);
		imp.setOwlModelFormat(SadlSerializationFormat.N3_FORMAT);
		long numRowsImported = imp.processImport();
		System.out.println("Number Triples Imported: " + numRowsImported);
		imp.getOwlModel().write(System.out, "N3");
		assertEquals(52, numRowsImported);
	}

}
