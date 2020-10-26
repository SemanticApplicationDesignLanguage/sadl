package com.ge.research.sadl.swi_prolog_reasoner;

import static org.junit.Assert.*;

import java.net.URL;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin;

public class TestReasoner {

	private String kbroot;

	@Before
	public void setUp() throws Exception {
		URL dataModelsFolder = ClassLoader.getSystemResource("DataModels");
		kbroot = dataModelsFolder.getFile();
		System.out.println("File kbase root: " + kbroot);
	}

	@Test
	public void testGetReasonerCategory() {
		IReasoner rsrnr = new SWIPrologReasonerPlugin();
		assertEquals(rsrnr.getConfigurationCategory(), "SWI-Prolog-Reasoner");
	}
	
	@Test
	public void testGetReasonerFamily() {
		IReasoner rsrnr = new SWIPrologReasonerPlugin();
		assertEquals(rsrnr.getReasonerFamily(), "SWI-Prolog-Based");
	}
	
	@Ignore
	@Test
	public void testInitializeReasoner() throws ReasonerNotFoundException, ConfigurationException, TripleNotFoundException {
		IReasoner rsrnr = new SWIPrologReasonerPlugin();
		String kbIdentifier = kbroot + "/Shapes/OwlModels";
		String modName = "http://sadl.org/Shapes/Test";
		String repoType = null;
		((SWIPrologReasonerPlugin)rsrnr).setTranslatorPrologFolder(kbroot + "/Prolog");
		((SWIPrologReasonerPlugin)rsrnr).setPortNumber("5000");
		rsrnr.initializeReasoner(kbIdentifier, modName, repoType);
		ResultSet rs = rsrnr.ask(null, null, null);
		System.out.println(rs.toString());
	}	

	@Test
	public void testXXXy() {
		IReasoner rsrnr = new SWIPrologReasonerPlugin();
	}	

}
