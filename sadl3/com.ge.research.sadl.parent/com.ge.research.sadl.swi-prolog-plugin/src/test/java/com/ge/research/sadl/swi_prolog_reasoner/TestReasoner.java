package com.ge.research.sadl.swi_prolog_reasoner;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
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
	
//	@Ignore
	@Test
	public void testInitializeReasoner() throws ReasonerNotFoundException, ConfigurationException, TripleNotFoundException {
		String kbIdentifier = kbroot + "/Shapes/OwlModels";
		IConfigurationManager cm = new ConfigurationManager(kbIdentifier, null);
		String prologReasonerClassName = "com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin";
		IReasoner rsrnr = cm.getOtherReasoner(prologReasonerClassName);
		String modName = "http://sadl.org/Shapes/Test";
		String repoType = null;
//		((SWIPrologReasonerPlugin)rsrnr).setTranslatorPrologFolder(kbroot + "/Prolog");
		((SWIPrologReasonerPlugin)rsrnr).setPortNumber("5000");
		rsrnr.initializeReasoner(kbIdentifier, modName, repoType);
		ResultSet rs = rsrnr.ask(null, null, null);
		assertNotNull(rs);
		System.out.println(rs.toString());
		assertTrue(rs.getRowCount() > 0);
	}	

	@Test
	public void testShapes01() throws ConfigurationException, ReasonerNotFoundException, IOException, TripleNotFoundException, QueryParseException, QueryCancelledException {
		String kbIdentifier = kbroot + "/Shapes/OwlModels";
		IConfigurationManager cm = new ConfigurationManager(kbIdentifier, null);
		String prologReasonerClassName = "com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin";
		IReasoner rsrnr = cm.getOtherReasoner(prologReasonerClassName);
		String modName = "http://sadl.org/Shapes/Test";
		String repoType = null;
//		((SWIPrologReasonerPlugin)rsrnr).setTranslatorPrologFolder(kbroot + "/Prolog");
		((SWIPrologReasonerPlugin)rsrnr).setPortNumber("5000");
		rsrnr.initializeReasoner(kbIdentifier, modName, repoType);
//		boolean status = rsrnr.loadInstanceData(cm.getAltUrlFromPublicUri(modName));
		ResultSet rs = rsrnr.ask(null, null, null);
		assertNotNull(rs);
		System.out.println(rs.toString());
		assertTrue(rs.getRowCount() > 0);
		
		System.out.println("Now more specific query:");
//		ResultSet rs2 = rsrnr.ask(null, "http://sadl.org/Shapes/Shapes#area", null);
		ResultSet rs2 = rsrnr.ask("http://sadl.org/Shapes/Test#MyRect", null, null);
		assertNotNull(rs2);
		System.out.println(rs2.toString());
		ResultSet rs3 = rsrnr.ask("listing(area).");
		assertNotNull(rs3);
		System.out.println(rs3.toString());
	}	

	@Test
	public void testGetPredicates() throws ConfigurationException, ReasonerNotFoundException, IOException, TripleNotFoundException, QueryParseException, QueryCancelledException {
		String kbIdentifier = kbroot + "/Shapes/OwlModels";
		IConfigurationManager cm = new ConfigurationManager(kbIdentifier, null);
		String prologReasonerClassName = "com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin";
		IReasoner rsrnr = cm.getOtherReasoner(prologReasonerClassName);
		String modName = "http://sadl.org/Shapes/Test";
		String repoType = null;
//		((SWIPrologReasonerPlugin)rsrnr).setTranslatorPrologFolder(kbroot + "/Prolog");
		((SWIPrologReasonerPlugin)rsrnr).setPortNumber("5000");
		rsrnr.initializeReasoner(kbIdentifier, modName, repoType);
//		boolean status = rsrnr.loadInstanceData(cm.getAltUrlFromPublicUri(modName));
		ResultSet rs = rsrnr.ask("select X Y where current_predicate(X/Y)");
		assertNotNull(rs);
		System.out.println(rs.toString());
		assertTrue(rs.getRowCount() > 0);
		// sort rows
		Arrays.sort(rs.getData(), new Comparator<Object[]>(){  
		    @Override  
		    public int compare(Object[] row1, Object[] row2){  
//		         return apple1.weight - apple2.weight;  
		    	 return row1[0].toString().compareTo(row2[0].toString());
		    }  
		}); 
		System.out.println(rs.toString());
		List<String> signatures = new ArrayList<String>();
		String lastRowPredicate = rs.getResultAt(0, 0).toString();
		int lastRowArity = Integer.parseInt(rs.getResultAt(0, 1).toString());
		int dupCntr = 0; 
		StringBuilder sb = new StringBuilder();
		for (int i = 1; i < rs.getRowCount(); i++) {
			String pred = rs.getResultAt(i, 0).toString();
			int arity = Integer.parseInt(rs.getResultAt(i, 1).toString());
			if (!pred.equals(lastRowPredicate) || i == rs.getRowCount() - 1) {
				if (Character.isAlphabetic(lastRowPredicate.charAt(0)) && lastRowPredicate.matches("^[a-zA-Z_]+[a-zA-Z0-9_\\-%~]*")) {
					// valid name
					sb.append("External ");
					sb.append(lastRowPredicate);
					sb.append("(");
					if (dupCntr > 0) {
						sb.append("--");
					}
					else {
						for (int j = 0; j <= lastRowArity; j++) {
							if (j > 0) {
								sb.append(",");
							}
							sb.append("string ");	// assume all arguments are strings for now
							sb.append("PV" + j);
						}
					}
					sb.append(") returns -- : \"com.ge.research.sadl.swi-prolog.predicate#");
					sb.append(lastRowPredicate);
					sb.append("\".");
					signatures.add(sb.toString());
					System.out.println(sb.toString());
					sb.setLength(0);
				}
				dupCntr = 0;
			}
			else {
				dupCntr++;
			}
			lastRowPredicate = pred;
			lastRowArity = arity;
		}
		
		assertTrue(signatures.size() > 0);
		assertTrue(signatures.contains("External holds(--) returns -- : \"com.ge.research.sadl.swi-prolog.predicate#holds\"."));
		

//		List<BuiltinInfo> bilst = rsrnr.getImplicitBuiltins();
//		if (bilst != null) {
//			for (BuiltinInfo bi : bilst) {
//				System.out.println(bi.toString());
//			}
//		}
	}	
}
