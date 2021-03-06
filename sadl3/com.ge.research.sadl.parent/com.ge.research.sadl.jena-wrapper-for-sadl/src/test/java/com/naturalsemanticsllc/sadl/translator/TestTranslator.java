package com.naturalsemanticsllc.sadl.translator;

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
	public void testTranslateRule() throws TranslationException {
		ITranslator trans = new JenaAugmentedTranslatorPlugin();
		OntModel om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		String altUrl = kbroot + "/Shapes/OwlModels/Shapes.owl";
		String format = "RDF/XML";
		om.read(altUrl, format);
		Rule rule = new Rule("TestRule");
		
		String stat = trans.translateRule(om, "http://sadl.org/Shapes.owl", rule);
	}

	@Ignore
	@Test
	public void testTranslateQuery() {
		ITranslator trans = new JenaAugmentedTranslatorPlugin();
		fail("Not yet implemented");
	}

	@Test
	public void testGetReasonerFamily() {
		ITranslator trans = new JenaAugmentedTranslatorPlugin();
		assertEquals(trans.getReasonerFamily(), "SWI-Prolog-Based");
	}

	@Ignore
	@Test
	public void testPrepareQuery() {
		ITranslator trans = new JenaAugmentedTranslatorPlugin();
		fail("Not yet implemented");
	}

}
