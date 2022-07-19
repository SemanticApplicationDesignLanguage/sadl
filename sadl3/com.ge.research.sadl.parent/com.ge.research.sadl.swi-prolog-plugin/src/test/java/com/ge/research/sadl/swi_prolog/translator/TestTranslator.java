package com.ge.research.sadl.swi_prolog.translator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.net.URL;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.ModelFactory;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.TranslationException;

public class TestTranslator {

	private String kbroot;

	@Before
	public void setUp() throws Exception {
		URL dataModelsFolder = ClassLoader.getSystemResource("DataModels");
		kbroot = dataModelsFolder.getFile();
		System.out.println("File kbase root: " + kbroot);
	}

	@Test
	public void testTranslateRule_01() throws TranslationException {
		ITranslator trans = new SWIPrologTranslatorPlugin();
		OntModel om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		String altUrl = kbroot + "/Shapes/OwlModels/Shapes.owl";
		String format = "RDF/XML";
		om.read(altUrl, format);
		Rule rule = new Rule("TestRule");
		
		String stat = trans.translateRule(om, "http://sadl.org/Shapes.owl", rule);
	}

	
	@Test
	public void testTranslateRule_02() throws TranslationException {
		ITranslator trans = new SWIPrologTranslatorPlugin();
		OntModel om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		String altUrl = kbroot + "/Likes/OwlModels/test.owl";
		String format = "RDF/XML";
		om.read(altUrl, format);
		Rule rule = new Rule("TestRule");
		
		String stat = trans.translateRule(om, "http://sadl.org/test.sadl", rule);
	}
	
	@Ignore
	@Test
	public void testTranslateQuery() {
		ITranslator trans = new SWIPrologTranslatorPlugin();
		fail("Not yet implemented");
	}

	@Test
	public void testGetReasonerFamily() {
		ITranslator trans = new SWIPrologTranslatorPlugin();
		assertEquals(trans.getReasonerFamily(), "SWI-Prolog-Based");
	}

	@Ignore
	@Test
	public void testPrepareQuery() {
		ITranslator trans = new SWIPrologTranslatorPlugin();
		fail("Not yet implemented");
	}

}
