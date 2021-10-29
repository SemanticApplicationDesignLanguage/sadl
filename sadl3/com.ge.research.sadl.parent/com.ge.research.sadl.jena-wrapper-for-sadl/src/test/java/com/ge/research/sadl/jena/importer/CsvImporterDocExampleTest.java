/**
 * 
 */
package com.ge.research.sadl.jena.importer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.importer.AbortDataRowException;
import com.ge.research.sadl.importer.TemplateException;
import com.ge.research.sadl.model.persistence.SadlPersistenceFormat;
import com.ge.research.sadl.reasoner.ConfigurationException;
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
		imp.setOwlModelFormat(SadlPersistenceFormat.N3_FORMAT);
		long numRowsImported = imp.processImport();
		System.out.println("Number Triples Imported: " + numRowsImported);
		imp.getOwlModel().write(System.out, "N3");
		assertEquals(52, numRowsImported);
	}
	
	@Test
	public void testStringSplitter() {
		String str = "A|B|C|The Steading+|Keir Allan+|Braco|E";
		String[] split = CsvImporter.splitStringWithEscape("|", "+", str);
		StringBuilder sb = new StringBuilder();
		int cntr = 0;
		for (String s : split) {
			System.out.println(s);
			if (cntr++ > 0) {
				sb.append(",");
			}
			sb.append(s);
		}
		assertTrue(sb.toString().equals("A,B,C,The Steading+|Keir Allan+|Braco,E"));
	}
	
	@Test
	public void testStringSplitter02() {
		String str = "\" \":_";
		String[] split = CsvImporter.splitStringWithEscape(":", "'", str);
		assertTrue(split.length == 2);
		assertTrue(split[0].equals("\" \""));
		assertTrue(split[1].equals("_"));
	}

	@Test
	public void testStringSplitter03() {
		String str = "\" \":_,&:_AND_";
		String[] split1 = CsvImporter.splitStringWithEscape(",", "'", str);
		assertTrue(split1.length == 2);
		assertTrue(split1[0].equals("\" \":_"));
		assertTrue(split1[1].equals("&:_AND_"));
		String[] split1a = CsvImporter.splitStringWithEscape(":", "'", split1[0]);
		assertTrue(split1a.length == 2);
		assertTrue(split1a[0].equals("\" \""));
		assertTrue(split1a[1].equals("_"));
		String[] split1b = CsvImporter.splitStringWithEscape(":", "'", split1[1]);
		assertTrue(split1b.length == 2);
		assertTrue(split1b[0].equals("&"));
		assertTrue(split1b[1].equals("_AND_"));
	}
	
	@Test
	public void testStringSplitter04() {
		String str = "\"1\" : true,\"\" : false";
		String[] split1 = CsvImporter.splitStringWithEscape(",", "'", str);
		assertTrue(split1.length == 2);
		assertTrue(split1[0].equals("\"1\" : true"));
		assertTrue(split1[1].equals("\"\" : false"));
		String[] split1a = CsvImporter.splitStringWithEscape(":", "'", split1[0]);
		assertTrue(split1a.length == 2);
		assertTrue(split1a[0].equals("1"));
		assertTrue(split1a[1].equals("true"));
		String[] split1b = CsvImporter.splitStringWithEscape(":", "'", split1[1]);
		assertTrue(split1b.length == 2);
		assertTrue(split1b[0].equals(""));
		assertTrue(split1b[1].equals("false"));
	}
	
	@Test
	public void testStringSplitter05() {
		String str = "'\"1\" : true,\"\" : false";
		String[] split1 = CsvImporter.splitStringWithEscape(",", "'", str);
		assertTrue(split1.length == 2);
		assertTrue(split1[0].equals("'\"1\" : true"));
		assertTrue(split1[1].equals("\"\" : false"));
		String[] split1a = CsvImporter.splitStringWithEscape(":", "'", split1[0]);
		assertTrue(split1a.length == 2);
		assertTrue(CsvImporter.removeDelimiters(split1a[0],"'").equals("\"1\""));
		assertTrue(CsvImporter.removeDelimiters(split1a[1],"'").equals("true"));
		String[] split1b = CsvImporter.splitStringWithEscape(":", "'", split1[1]);
		assertTrue(split1b.length == 2);
		assertTrue(CsvImporter.removeDelimiters(split1b[0], "'").equals(""));
		assertTrue(CsvImporter.removeDelimiters(split1b[1], "'").equals("false"));
	}

	@Test
	public void testStringSplitter06() {
		String str = "\" \":_";
		String[] split1 = CsvImporter.splitStringWithEscape(",", "'", str);
		assertTrue(split1.length == 1);
		assertTrue(split1[0].equals("\" \":_"));
		String[] split1a = CsvImporter.splitStringWithEscape(":", "'", split1[0]);
		assertTrue(split1a.length == 2);
		assertTrue(CsvImporter.removeDelimiters(split1a[0],"'").equals(" "));
		assertTrue(CsvImporter.removeDelimiters(split1a[1],"'").equals("_"));
	}
	
	@Test
	public void testStringSplitter07() {
		String str = "'\"1'\" : true,\"\" : false";
		String[] split1 = CsvImporter.splitStringWithEscape(",", "'", str);
		assertTrue(split1.length == 2);
		assertTrue(split1[0].equals("'\"1'\" : true"));
		assertTrue(split1[1].equals("\"\" : false"));
		String[] split1a = CsvImporter.splitStringWithEscape(":", "'", split1[0]);
		assertTrue(split1a.length == 2);
		assertTrue(CsvImporter.removeDelimiters(split1a[0],"'").equals("\"1\""));
		assertTrue(CsvImporter.removeDelimiters(split1a[1],"'").equals("true"));
		String[] split1b = CsvImporter.splitStringWithEscape(":", "'", split1[1]);
		assertTrue(split1b.length == 2);
		assertTrue(CsvImporter.removeDelimiters(split1b[0], "'").equals(""));
		assertTrue(CsvImporter.removeDelimiters(split1b[1], "'").equals("false"));
	}

	@Test
	public void testStringSplitter08() {
		String str = "\"''\":_";		// replace a single quote with an underscore
		String[] split1 = CsvImporter.splitStringWithEscape(",", "'", str);
		assertTrue(split1.length == 1);
		assertTrue(split1[0].equals("\"''\":_"));
		String[] split1a = CsvImporter.splitStringWithEscape(":", "'", split1[0]);
		assertTrue(split1a.length == 2);
		String split1ap = CsvImporter.removeDelimiters(split1a[0],"'");
		assertTrue(split1ap.equals("'"));
		assertTrue(CsvImporter.removeDelimiters(split1a[1],"'").equals("_"));
	}
	
}
