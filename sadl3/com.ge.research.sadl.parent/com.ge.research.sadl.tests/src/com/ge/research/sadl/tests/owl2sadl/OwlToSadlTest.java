package com.ge.research.sadl.tests.owl2sadl;

import static org.junit.Assert.*;

import java.net.URL;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.ge.research.sadl.owl2sadl.OwlToSadl;
import com.ge.research.sadl.tests.external.ExternalResourceContentHelper;

public class OwlToSadlTest {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void test01() throws Exception {
		String owlModelContent = "<rdf:RDF\n" + 
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" + 
				"    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n" + 
				"    xmlns:Shapes=\"http://sadl.org/Shapes.sadl#\"\n" + 
				"    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n" + 
				"    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n" + 
				"  xml:base=\"http://sadl.org/Shapes.sadl\">\n" + 
				"  <owl:Ontology rdf:about=\"\">\n" + 
				"    <rdfs:comment xml:lang=\"en\">This ontology is a simple representation of 2D shapes.</rdfs:comment>\n" + 
				"  </owl:Ontology>\n" + 
				"  <owl:Class rdf:ID=\"Shape\"/>\n" + 
				"  <owl:Class rdf:ID=\"Circle\">\n" + 
				"    <rdfs:subClassOf rdf:resource=\"#Shape\"/>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:DatatypeProperty rdf:ID=\"radius\">\n" + 
				"    <rdfs:domain rdf:resource=\"#Circle\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#float\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:DatatypeProperty rdf:ID=\"area\">\n" + 
				"    <rdfs:domain rdf:resource=\"#Shape\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#float\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:AnnotationProperty rdf:ID=\"comment\"/>\n" + 
				"  <Shapes:Circle rdf:ID=\"MyCircle\">\n" + 
				"    <Shapes:radius rdf:datatype=\"http://www.w3.org/2001/XMLSchema#float\"\n" + 
				"    >3.0</Shapes:radius>\n" + 
				"  </Shapes:Circle>\n" + 
				"</rdf:RDF>\n";
		OwlToSadl o2s = new OwlToSadl(owlModelContent);
		o2s.setVerboseMode(true);
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/Shapes.sadl\" alias Shapes\n" + 
				"    (note \"This ontology is a simple representation of 2D shapes.\").\n" + 
				"\n" + 
				"// This ontology is a simple representation of 2D shapes.@en.\n" + 
				"\n" + 
				"\n" + 
				"// No Errors\n" + 
				"\n" + 
				"\n" + 
				"// No Warnings\n" + 
				"\n" + 
				"\n" + 
				"// No Info output\n" + 
				"\n" + 
				"\n" + 
				"// Ontologies:\n" + 
				"//    http://sadl.org/Shapes.sadl\n" + 
				"\n" + 
				"\n" + 
				"// Datatype Declarations:\n" + 
				"\n" + 
				"\n" + 
				"// Annotation Properties:\n" + 
				"comment is a type of annotation.\n" + 
				"\n" + 
				"\n" + 
				"// RDF Properties:\n" + 
				"\n" + 
				"\n" + 
				"// Object properties without specified range:\n" + 
				"\n" + 
				"\n" + 
				"// Datatype properties without specified range:\n" + 
				"\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"Circle is a type of Shape,\n" + 
				"    described by radius with values of type float.\n" + 
				"Shape is a class,\n" + 
				"    described by area with values of type float.\n" + 
				"\n" + 
				"\n" + 
				"// Other object Properties:\n" + 
				"\n" + 
				"\n" + 
				"// Other datatype Properties:\n" + 
				"\n" + 
				"\n" + 
				"// Individuals:\n" + 
				"MyCircle is a Circle,\n" + 
				"    has radius 3.0 .\n" + 
				"\n" + 
				"\n" + 
				"// Other restrictions:\n" +
				"\n" + 
				"\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#comment, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.w3.org/2002/07/owl#AnnotationProperty]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#radius, http://www.w3.org/2000/01/rdf-schema#range, http://www.w3.org/2001/XMLSchema#float]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#radius, http://www.w3.org/2000/01/rdf-schema#domain, http://sadl.org/Shapes.sadl#Circle]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#radius, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.w3.org/2002/07/owl#DatatypeProperty]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl, http://www.w3.org/2000/01/rdf-schema#comment, \"This ontology is a simple representation of 2D shapes.\"@en]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.w3.org/2002/07/owl#Ontology]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#Shape, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.w3.org/2002/07/owl#Class]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#area, http://www.w3.org/2000/01/rdf-schema#range, http://www.w3.org/2001/XMLSchema#float]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#area, http://www.w3.org/2000/01/rdf-schema#domain, http://sadl.org/Shapes.sadl#Shape]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#area, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.w3.org/2002/07/owl#DatatypeProperty]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#MyCircle, http://sadl.org/Shapes.sadl#radius, \"3.0\"^^http://www.w3.org/2001/XMLSchema#float]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#MyCircle, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://sadl.org/Shapes.sadl#Circle]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#Circle, http://www.w3.org/2000/01/rdf-schema#subClassOf, http://sadl.org/Shapes.sadl#Shape]\n" + 
				"// Processed statement: [http://sadl.org/Shapes.sadl#Circle, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.w3.org/2002/07/owl#Class]\n";
		System.out.print(sadlModelContent);
		assertEquals(expected, sadlModelContent);
	}

	@Test
	public void test02() throws Exception {
		String owlModelContent = "<rdf:RDF\n" + 
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" + 
				"    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n" + 
				"    xmlns:Shapes=\"http://sadl.org/Shapes.sadl#\"\n" + 
				"    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n" + 
				"    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n" + 
				"  xml:base=\"http://sadl.org/Shapes.sadl\">\n" + 
				"  <owl:Ontology rdf:about=\"\">\n" + 
				"    <rdfs:comment xml:lang=\"en\">This ontology is a simple representation of 2D shapes.</rdfs:comment>\n" + 
				"  </owl:Ontology>\n" + 
				"  <owl:Class rdf:ID=\"Shape\"/>\n" + 
				"  <owl:Class rdf:ID=\"Circle\">\n" + 
				"    <rdfs:subClassOf rdf:resource=\"#Shape\"/>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:DatatypeProperty rdf:ID=\"radius\">\n" + 
				"    <rdfs:domain rdf:resource=\"#Circle\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#float\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:DatatypeProperty rdf:ID=\"area\">\n" + 
				"    <rdfs:domain rdf:resource=\"#Shape\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#float\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:AnnotationProperty rdf:ID=\"comment\"/>\n" + 
				"  <Shapes:Circle rdf:ID=\"MyCircle\">\n" + 
				"    <Shapes:radius rdf:datatype=\"http://www.w3.org/2001/XMLSchema#float\"\n" + 
				"    >3.0</Shapes:radius>\n" + 
				"  </Shapes:Circle>\n" + 
				"</rdf:RDF>\n";
		OwlToSadl o2s = new OwlToSadl(owlModelContent);
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/Shapes.sadl\" alias Shapes\n" + 
				"    (note \"This ontology is a simple representation of 2D shapes.\").\n" + 
				"\n" + 
				"\n" + 
				"\n" + 
				"// Annotation Properties:\n" + 
				"comment is a type of annotation.\n" + 
				"\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"Circle is a type of Shape,\n" + 
				"    described by radius with values of type float.\n" + 
				"Shape is a class,\n" + 
				"    described by area with values of type float.\n" + 
				"\n" + 
				"\n" + 
				"// Individuals:\n" + 
				"MyCircle is a Circle,\n" + 
				"    has radius 3.0 .\n";
		System.out.print(sadlModelContent);
		assertEquals(expected, sadlModelContent);
	}

	@Ignore
	@Test
	public void test03() throws Exception {
		URL owlUrl = ExternalResourceContentHelper.getURL("Shapes/concepts.owl");
//		URL pfUrl = ExternalResourceContentHelper.getURL("Shapes/ont-policy.rdf");
		OwlToSadl o2s = new OwlToSadl(owlUrl); //, pfUrl.toString());
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/concepts.sadl\" alias concepts\n" + 
				"    (note \"This ontology was created from a SADL file 'concepts.sadl' and should not be directly edited.\")\n" + 
				"    (note \"This model was generated from the OWL model in file 'concepts.owl'\").\n" + 
				"\n" + 
				"\n" + 
				"\n" + 
				"// Datatype Declarations:\n" + 
				"ColorComponent is a type of int [0,255].\n" + 
				"\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"Color is a class,\n" + 
				"    described by blue with values of type ColorComponent,\n" + 
				"    described by green with values of type ColorComponent,\n" + 
				"    described by red with values of type ColorComponent.\n" + 
				"Shape is a class,\n" + 
				"    described by area with values of type float,\n" + 
				"    described by color with values of type Color.\n" + 
				"\n" + 
				"\n" + 
				"// Individuals:\n" + 
				"Blue is a Color,\n" + 
				"    has red 0,\n" + 
				"    has green 0,\n" + 
				"    has blue 255 .\n" + 
				"Green is a Color,\n" + 
				"    has red 0,\n" + 
				"    has green 255,\n" + 
				"    has blue 0 .\n" + 
				"Red is a Color,\n" + 
				"    has red 255,\n" + 
				"    has green 0,\n" + 
				"    has blue 0 .\n" + 
				"Yellow is a Color,\n" + 
				"    has red 255,\n" + 
				"    has green 255,\n" + 
				"    has blue 0 .\n";
		System.out.print(sadlModelContent);
		assertEquals(expected, sadlModelContent);
	}
	
	@Ignore("https://github.com/crapo/sadlos2/issues/372")
	@Test
	public void test04() throws Exception {
		String owlModelContent = ExternalResourceContentHelper.getContent("Shapes/specificshapes.owl");
		OwlToSadl o2s = new OwlToSadl(owlModelContent);
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/specificshapes.sadl\" alias specificshapes\n" + 
				"    (note \"This ontology was created from a SADL file 'specificshapes.sadl' and should not be directly edited.\").\n" + 
				"\n" + 
				"import \"http://sadl.org/concepts.sadl\" as concepts.\n" + 
				"\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"Circle is a type of concepts:Shape,\n" + 
				"    described by radius with values of type float.\n" + 
				"Rectangle is a type of concepts:Shape,\n" + 
				"    described by height with values of type float,\n" + 
				"    described by width with values of type float.\n";
		System.out.print(sadlModelContent);
		assertEquals(expected, sadlModelContent);
	}
	
}
