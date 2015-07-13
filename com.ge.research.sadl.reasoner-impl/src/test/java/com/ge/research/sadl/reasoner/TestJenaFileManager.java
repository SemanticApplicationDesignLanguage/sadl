package com.ge.research.sadl.reasoner;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.FileManager;

public class TestJenaFileManager extends EclipseOrMavenJUnitTest {

	private String modelFolder1;
	private String modelFolder2;
	private String kbroot;

	@Before
	public void setUp() throws Exception {
		kbroot = getKbRoot();
		modelFolder1 = kbroot + "/Model1/OwlModels";
		modelFolder2 = kbroot + "/Model2/OwlModels";
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
	}

	@Test
	public void test() throws ConfigurationException, IOException {
		String mappingFile1 = modelFolder1 + "/ont-policy.rdf";
		String mappingFile2 = modelFolder2 + "/ont-policy.rdf";
		Model mappingModel1 = loadMappingFile(mappingFile1);
		Model mappingModel2 = loadMappingFile(mappingFile2);
		String modelUri1 = "http://sadl.org/SadlLinking1/RuleBasedRelations";
		String modelUri2 = "http://sadl.org/SadlLinking2/AdamsFamily";
		String genBaseUri = "http://sadl.org/SadlLinking1/GenealogyBase";
		
		boolean m2ProjectDependsOn_m1Project = false;

		OntDocumentManager odm1 = new OntDocumentManager(mappingModel1);
		OntDocumentManager odm2;
		
		if (m2ProjectDependsOn_m1Project) {
			odm2 = odm1;
			odm2.setMetadataSearchPath(mappingFile2, false);
		}
		else {
			odm2 = new OntDocumentManager(mappingModel2);
		}
		
		FileManager fm1 = odm1.getFileManager();
		FileManager fm2 = odm2.getFileManager();

		if (fm1.equals(fm2)) {
			System.out.println("FileManagers are the same");
		}
		else {
			System.out.println("FileManagers are unique to each OntDocumentManager");
		}
		
		OntModel m1 = odm1.getOntology(modelUri1, OntModelSpec.OWL_DL_MEM);
		odm2.setProcessImports(true);
		OntModel m2 = odm2.getOntology(modelUri2, OntModelSpec.OWL_DL_MEM);
		m2.loadImports();
		String jqauri = modelUri2 + "#JohnQuincyAdams";
		Individual jqar = m2.getIndividual(jqauri);
		StmtIterator sitr = m2.listStatements(jqar, (Property)null, (RDFNode)null);
		while (sitr.hasNext()) {
			System.out.println(sitr.nextStatement().toString());
		}
		OntClass cls = m2.getOntClass(genBaseUri + "#Man");
//		assertNotNull(cls);
//		OntClass supercls = cls.listEquivalentClasses().next();
//		assertNotNull(supercls);
//		System.out.println("Man is a subclass of " + (supercls.isURIResource() ? supercls.getURI() : "an anon class."));

	}

	protected Model loadMappingFile(String mappingFN) throws ConfigurationException, IOException {
		if (mappingFN != null) {
			File mappingFile = new File(mappingFN);
			if (mappingFile.exists()) {
				// load mapping info from file
				Model m = ModelFactory.createDefaultModel() ;
			    InputStream in = FileManager.get().open(mappingFile.getCanonicalPath());
			    if (in == null) {
			    	throw new IllegalArgumentException("File: " + mappingFN + " not found");
			    }
				try {
					m.read(in, "");
					return m;
				}
				catch (Throwable t) {
					t.printStackTrace();
					System.err.println("Failed to read mapping file '" + mappingFN + "': " + t.getLocalizedMessage());
				}
			}
			else {
				System.out.println("Model file '" + mappingFN + "' does not exist.");
			} 
		}
		throw new IOException("Null mapping file name");
	}
}
