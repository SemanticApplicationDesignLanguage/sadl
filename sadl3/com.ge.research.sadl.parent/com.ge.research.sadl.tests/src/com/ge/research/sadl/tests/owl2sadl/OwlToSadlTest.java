package com.ge.research.sadl.tests.owl2sadl;

import java.net.URL;

import org.junit.Before;
import org.junit.Test;

import com.ge.research.sadl.owl2sadl.OwlImportException;
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
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/Shapes.sadl");
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
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/Shapes.sadl");
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

//	@Ignore
	@Test
	public void test03() throws Exception {
		URL owlUrl = ExternalResourceContentHelper.getURL("Shapes/concepts.owl");
//		URL pfUrl = ExternalResourceContentHelper.getURL("Shapes/ont-policy.rdf");
		OwlToSadl o2s = new OwlToSadl(owlUrl, "http://sadl.org/concepts.sadl"); //, pfUrl.toString());
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
	
//	@Ignore("https://github.com/crapo/sadlos2/issues/372")
	@Test
	public void test04() throws Exception {
		String owlModelContent = ExternalResourceContentHelper.getContent("Shapes/specificshapes.owl");
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/specificshapes.sadl");
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/specificshapes.sadl\" alias specificshapes\n" + 
				"    (note \"This ontology was created from a SADL file 'specificshapes.sadl' and should not be directly edited.\").\n" + 
				"\n" + 
				"import \"http://sadl.org/concepts.sadl\" as concepts.\n" + 
				"\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"Circle is a type of Shape,\n" + 
				"    described by radius with values of type float.\n" + 
				"Rectangle is a type of Shape,\n" + 
				"    described by height with values of type float,\n" + 
				"    described by width with values of type float.\n";
		System.out.print(sadlModelContent);
		assertEquals(expected, sadlModelContent);
	}
	
	@Test
	public void test05() throws Exception {
		String owlModelContent = "<rdf:RDF\r\n" + 
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\r\n" + 
				"    xmlns:IntersectionClass=\"http://sadl.org/IntersectionClass.sadl#\"\r\n" + 
				"    xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel\"\r\n" + 
				"    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\r\n" + 
				"    xmlns:sadlimplicitmodel=\"http://sadl.org/sadlimplicitmodel\"\r\n" + 
				"    xmlns:builtinfunctions=\"http://sadl.org/builtinfunctions\"\r\n" + 
				"    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\r\n" + 
				"    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\r\n" + 
				"  xml:base=\"http://sadl.org/IntersectionClass.sadl\">\r\n" + 
				"  <owl:Ontology rdf:about=\"\">\r\n" + 
				"    <owl:imports rdf:resource=\"builtinfunctions\"/>\r\n" + 
				"    <owl:imports rdf:resource=\"sadlimplicitmodel\"/>\r\n" + 
				"    <owl:imports rdf:resource=\"sadlbasemodel\"/>\r\n" + 
				"    <rdfs:comment xml:lang=\"en\">This ontology was created from a SADL file 'IntersectionClass.sadl' and should not be directly edited.</rdfs:comment>\r\n" + 
				"  </owl:Ontology>\r\n" + 
				"  <owl:Class rdf:ID=\"Refreshment\"/>\r\n" + 
				"  <owl:Class rdf:ID=\"Nourishment\"/>\r\n" + 
				"  <owl:Class rdf:ID=\"GoodChoice\">\r\n" + 
				"    <rdfs:subClassOf>\r\n" + 
				"      <owl:Class>\r\n" + 
				"        <owl:intersectionOf rdf:parseType=\"Collection\">\r\n" + 
				"          <owl:Class rdf:about=\"#Nourishment\"/>\r\n" + 
				"          <owl:Class rdf:about=\"#Refreshment\"/>\r\n" + 
				"        </owl:intersectionOf>\r\n" + 
				"      </owl:Class>\r\n" + 
				"    </rdfs:subClassOf>\r\n" + 
				"  </owl:Class>\r\n" + 
				"</rdf:RDF>";
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/IntersectionClass.sadl");
//		o2s.setVerboseMode(true);
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/IntersectionClass.sadl\" alias IntersectionClass\n" + 
				"    (note \"This ontology was created from a SADL file 'IntersectionClass.sadl' and should not be directly edited.\").\n" + 
				"\n" + 
				"\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"GoodChoice is a type of {Nourishment and Refreshment}.\n" + 
				"Nourishment is a class.\n" + 
				"Refreshment is a class.\n" + 
				"";
		System.out.print(sadlModelContent);
		assertEquals(expected, sadlModelContent);
	}
	
	@Test
	public void test06() throws OwlImportException {
		String owlModelContent = 
				"<rdf:RDF\r\n" + 
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\r\n" + 
				"    xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel\"\r\n" + 
				"    xmlns:dfa=\"http://sadl.org/derivedFromAnnotation.sadl#\"\r\n" + 
				"    xmlns:j.0=\"http://sadl.org/sadllistmodel#\"\r\n" + 
				"    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\r\n" + 
				"    xmlns:sadlimplicitmodel=\"http://sadl.org/sadlimplicitmodel\"\r\n" + 
				"    xmlns:builtinfunctions=\"http://sadl.org/builtinfunctions\"\r\n" + 
				"    xmlns:j.1=\"http://sadl.org/sadlimplicitmodel#\"\r\n" + 
				"    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\r\n" + 
				"    xmlns:sadllistmodel=\"http://sadl.org/sadllistmodel\"\r\n" + 
				"    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\r\n" + 
				"  xml:base=\"http://sadl.org/derivedFromAnnotation.sadl\">\r\n" + 
				"  <owl:Ontology rdf:about=\"\">\r\n" + 
				"    <owl:imports rdf:resource=\"sadllistmodel\"/>\r\n" + 
				"    <owl:imports rdf:resource=\"builtinfunctions\"/>\r\n" + 
				"    <owl:imports rdf:resource=\"sadlimplicitmodel\"/>\r\n" + 
				"    <owl:imports rdf:resource=\"sadlbasemodel\"/>\r\n" + 
				"    <rdfs:comment xml:lang=\"en\">This ontology was created from a SADL file 'derivedFromAnnotation.sadl' and should not be directly edited.</rdfs:comment>\r\n" + 
				"  </owl:Ontology>\r\n" + 
				"  <owl:Class rdf:nodeID=\"A0\">\r\n" + 
				"    <rdfs:subClassOf>\r\n" + 
				"      <owl:Restriction>\r\n" + 
				"        <owl:allValuesFrom rdf:nodeID=\"A0\"/>\r\n" + 
				"        <owl:onProperty rdf:resource=\"sadllistmodel#rest\"/>\r\n" + 
				"      </owl:Restriction>\r\n" + 
				"    </rdfs:subClassOf>\r\n" + 
				"    <rdfs:subClassOf>\r\n" + 
				"      <owl:Restriction>\r\n" + 
				"        <owl:allValuesFrom rdf:resource=\"sadlimplicitmodel#DataDescriptor\"/>\r\n" + 
				"        <owl:onProperty rdf:resource=\"sadllistmodel#first\"/>\r\n" + 
				"      </owl:Restriction>\r\n" + 
				"    </rdfs:subClassOf>\r\n" + 
				"    <rdfs:subClassOf rdf:resource=\"sadllistmodel#List\"/>\r\n" + 
				"  </owl:Class>\r\n" + 
				"  <j.1:ExternalEquation rdf:ID=\"eq2\">\r\n" + 
				"    <j.1:externalURI rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"\r\n" + 
				"    >uri22</j.1:externalURI>\r\n" + 
				"    <j.1:returnTypes rdf:parseType=\"Resource\">\r\n" + 
				"      <j.0:first>\r\n" + 
				"        <j.1:DataDescriptor>\r\n" + 
				"          <j.1:dataType>http://www.w3.org/2001/XMLSchema#float</j.1:dataType>\r\n" + 
				"        </j.1:DataDescriptor>\r\n" + 
				"      </j.0:first>\r\n" + 
				"      <rdf:type rdf:nodeID=\"A0\"/>\r\n" + 
				"    </j.1:returnTypes>\r\n" + 
				"    <j.1:derivedFrom>\r\n" + 
				"      <j.1:ExternalEquation rdf:ID=\"eq1\">\r\n" + 
				"        <j.1:externalURI rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"\r\n" + 
				"        >uri12</j.1:externalURI>\r\n" + 
				"        <j.1:returnTypes rdf:parseType=\"Resource\">\r\n" + 
				"          <j.0:first>\r\n" + 
				"            <j.1:DataDescriptor>\r\n" + 
				"              <j.1:dataType>http://www.w3.org/2001/XMLSchema#float</j.1:dataType>\r\n" + 
				"            </j.1:DataDescriptor>\r\n" + 
				"          </j.0:first>\r\n" + 
				"          <rdf:type rdf:nodeID=\"A0\"/>\r\n" + 
				"        </j.1:returnTypes>\r\n" + 
				"      </j.1:ExternalEquation>\r\n" + 
				"    </j.1:derivedFrom>\r\n" + 
				"  </j.1:ExternalEquation>\r\n" + 
				"</rdf:RDF>\r\n";
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/derivedFromAnnotation.sadl");
//		o2s.setVerboseMode(true);
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/derivedFromAnnotation.sadl\" alias dfa\n" + 
				"    (note \"This ontology was created from a SADL file 'derivedFromAnnotation.sadl' and should not be directly edited.\").\n" + 
				"\n" + 
				"\n" + 
				"\n" + 
				"// Individuals:\n" + 
				"External eq1() returns float: \"uri12\".\n" + 
				"External eq2 (derivedFrom eq1) () returns float: \"uri22\".";
		System.out.print(sadlModelContent);
		assertEquals(expected.trim(), sadlModelContent.trim());
	}
	
	@Test
	public void test07() throws OwlImportException {
		String owlModelContent = "<rdf:RDF\r\n" + 
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\r\n" + 
				"    xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel\"\r\n" + 
				"    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\r\n" + 
				"    xmlns:sadlimplicitmodel=\"http://sadl.org/sadlimplicitmodel\"\r\n" + 
				"    xmlns:builtinfunctions=\"http://sadl.org/builtinfunctions\"\r\n" + 
				"    xmlns:OwlFileToTestOwl2Sadl=\"http://sadl.org/OwlFileToTestOwl2Sadl.sadl#\"\r\n" + 
				"    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\r\n" + 
				"    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\r\n" + 
				"  xml:base=\"http://sadl.org/OwlFileToTestOwl2Sadl.sadl\">\r\n" + 
				"  <owl:Ontology rdf:about=\"http://sadl.org/OwlFileToTestOwl2Sadl.sadl\">\r\n" + 
				"    <owl:imports rdf:resource=\"builtinfunctions\"/>\r\n" + 
				"    <owl:imports rdf:resource=\"sadlimplicitmodel\"/>\r\n" + 
				"    <rdfs:comment xml:lang=\"en\">This ontology was created from a SADL file 'OwlFileToTestOwl2Sadl.sadl' and should not be directly edited.</rdfs:comment>\r\n" + 
				"  </owl:Ontology>\r\n" + 
				"  <owl:Class rdf:about=\"OwlFileToTestOwl2Sadl.sadl#Class1\"/>\r\n" + 
				"  <owl:Class rdf:about=\"OwlFileToTestOwl2Sadl.sadl#Class2\"/>\r\n" + 
				"  <owl:ObjectProperty rdf:about=\"OwlFileToTestOwl2Sadl.sadl#op\">\r\n" + 
				"    <rdfs:domain rdf:resource=\"OwlFileToTestOwl2Sadl.sadl#Class1\"/>\r\n" + 
				"    <rdfs:range rdf:resource=\"OwlFileToTestOwl2Sadl.sadl#Class2\"/>\r\n" + 
				"  </owl:ObjectProperty>\r\n" + 
				"  <owl:DatatypeProperty rdf:about=\"OwlFileToTestOwl2Sadl.sadl#dps\">\r\n" + 
				"    <rdfs:domain rdf:resource=\"OwlFileToTestOwl2Sadl.sadl#Class1\"/>\r\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\r\n" + 
				"  </owl:DatatypeProperty>\r\n" + 
				"  <owl:DatatypeProperty rdf:about=\"OwlFileToTestOwl2Sadl.sadl#dpi\">\r\n" + 
				"    <rdfs:domain rdf:resource=\"OwlFileToTestOwl2Sadl.sadl#Class1\"/>\r\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\r\n" + 
				"  </owl:DatatypeProperty>\r\n" + 
				"  <owl:DatatypeProperty rdf:about=\"OwlFileToTestOwl2Sadl.sadl#dpf\">\r\n" + 
				"    <rdfs:domain rdf:resource=\"OwlFileToTestOwl2Sadl.sadl#Class1\"/>\r\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#float\"/>\r\n" + 
				"  </owl:DatatypeProperty>\r\n" + 
				"  <owl:DatatypeProperty rdf:about=\"OwlFileToTestOwl2Sadl.sadl#dpdt\">\r\n" + 
				"    <rdfs:domain rdf:resource=\"OwlFileToTestOwl2Sadl.sadl#Class1\"/>\r\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#dateTime\"/>\r\n" + 
				"  </owl:DatatypeProperty>\r\n" + 
				"  <OwlFileToTestOwl2Sadl:Class1 rdf:about=\"OwlFileToTestOwl2Sadl.sadl#Cls1Inst\">\r\n" + 
				"    <OwlFileToTestOwl2Sadl:dpdt rdf:datatype=\"http://www.w3.org/2001/XMLSchema#dateTime\"\r\n" + 
				"    >1951-07-19T00:00:00-04:00</OwlFileToTestOwl2Sadl:dpdt>\r\n" + 
				"    <OwlFileToTestOwl2Sadl:dpi rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\"\r\n" + 
				"    >23</OwlFileToTestOwl2Sadl:dpi>\r\n" + 
				"    <OwlFileToTestOwl2Sadl:dpf rdf:datatype=\"http://www.w3.org/2001/XMLSchema#float\"\r\n" + 
				"    >3.1416</OwlFileToTestOwl2Sadl:dpf>\r\n" + 
				"    <OwlFileToTestOwl2Sadl:dps>Hello world</OwlFileToTestOwl2Sadl:dps>\r\n" + 
				"    <OwlFileToTestOwl2Sadl:op>\r\n" + 
				"      <OwlFileToTestOwl2Sadl:Class2 rdf:about=\"OwlFileToTestOwl2Sadl.sadl#Cls2Inst\"/>\r\n" + 
				"    </OwlFileToTestOwl2Sadl:op>\r\n" + 
				"  </OwlFileToTestOwl2Sadl:Class1>\r\n" + 
				"</rdf:RDF>";
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/OwlFileToTestOwl2Sadl.sadl");
//		o2s.setVerboseMode(true);
		String sadlModelContent = o2s.getSadlModel();
		String expected = "uri \"http://sadl.org/OwlFileToTestOwl2Sadl.sadl\" alias OwlFileToTestOwl2Sadl\r\n" + 
				"    (note \"This ontology was created from a SADL file 'OwlFileToTestOwl2Sadl.sadl' and should not be directly edited.\").\r\n" + 
				"\r\n" + 
				"\r\n" + 
				"\r\n" + 
				"// Class definitions:\r\n" + 
				"Class1 is a class,\r\n" + 
				"    described by dpdt with values of type dateTime,\r\n" + 
				"    described by dpf with values of type float,\r\n" + 
				"    described by dpi with values of type int,\r\n" + 
				"    described by dps with values of type string,\r\n" + 
				"    described by op with values of type Class2 .\r\n" + 
				"Class2 is a class.\r\n" + 
				"\r\n" + 
				"\r\n" + 
				"// Individuals:\r\n" + 
				"Cls1Inst is a Class1,\r\n" + 
				"    has op Cls2Inst,\r\n" + 
				"    has dps \"Hello world\",\r\n" + 
				"    has dpf 3.1416,\r\n" + 
				"    has dpi 23,\r\n" + 
				"    has dpdt \"1951-07-19T00:00:00-04:00\".\r\n" + 
				"Cls2Inst is a Class2 .\r\n";
		System.out.print(sadlModelContent);
		assertEquals(expected.trim(), sadlModelContent.trim());
	}
	
	@Test
	public void test08() throws OwlImportException {
		String owlModelContent = 
				"<rdf:RDF\n" + 
				"xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" + 
				"xmlns:builtinfunctions=\"http://sadl.org/builtinfunctions#\"\n" + 
				"xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n" + 
				"xmlns:sadlimplicitmodel=\"http://sadl.org/sadlimplicitmodel#\"\n" + 
				"xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n" + 
				"xmlns:owlimp=\"http://sadl.org/testOwlimport.sadl#\"\n" + 
				"xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel#\"\n" + 
				"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n" + 
				"xml:base=\"http://sadl.org/testOwlimport.sadl\">\n" + 
				"<owl:Ontology rdf:about=\"\">\n" + 
				"<owl:imports rdf:resource=\"builtinfunctions\"/>\n" + 
				"<owl:imports rdf:resource=\"sadlimplicitmodel\"/>\n" + 
				"<owl:imports rdf:resource=\"sadlbasemodel\"/>\n" + 
				"</owl:Ontology>\n" + 
				"<owl:Class rdf:ID=\"C\"/>\n" + 
				"<owl:DatatypeProperty rdf:ID=\"description\">\n" + 
				"<rdfs:domain rdf:resource=\"#C\"/>\n" + 
				"<rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n" + 
				"</owl:DatatypeProperty>\n" + 
				"<owlimp:C rdf:about=\"aninstance\">\n" + 
				"<owlimp:description>sample instance</owlimp:description>\n" + 
				"</owlimp:C>\n" + 
				"</rdf:RDF>";
		OwlToSadl o2s = new OwlToSadl(owlModelContent, "http://sadl.org/testOwlimport.sadl");
//		o2s.setVerboseMode(true);
		String sadlModelContent = o2s.getSadlModel();
		String expected = 
				"uri \"http://sadl.org/testOwlimport.sadl\" alias owlimp.\n" + 
				"\n" + 
				"// Class definitions:\n" + 
				"C is a class,\n" + 
				"    described by description with values of type string.\n" + 
				"\n" + 
				"// Individuals:\n" + 
				"aninstance is a C,\n" + 
				"    has description \"sample instance\".\n" + 
				"\n";
		System.out.print(sadlModelContent);
		assertEquals(expected.trim(), sadlModelContent.trim());
	}
	
	private void assertEquals(Object expected, Object actual) {
		String s1 = expected.toString().replace("\r", "");
		s1 = s1.replace("\n", "");
		String s2 = actual.toString().replace("\r", "");
		s2 = s2.replace("\n", "");
		org.junit.Assert.assertEquals(s1, s2);	
	}
}
