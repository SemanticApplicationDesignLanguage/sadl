package com.ge.research.sadl.swi_prolog.translator;

import static org.junit.Assert.*;

import java.net.URL;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;

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
		ITranslator trans = new SWIPrologTranslatorPlugin();
		OntModel om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		String altUrl = kbroot + "/Shapes/OwlModels/Shapes.owl";
		String format = "RDF/XML";
		om.read(altUrl, format);
		Rule rule = new Rule("TestRule");
		
		String stat = trans.translateRule(om, rule);
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
