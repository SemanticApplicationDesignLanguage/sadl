package com.ge.research.sadl.tests.external

import com.ge.research.sadl.tests.AbstractLinkingTest
import org.junit.Test
import org.junit.Assert
import com.google.inject.Inject
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlResource
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import com.ge.research.sadl.resource.ResourceDescriptionStrategy
import org.eclipse.xtext.resource.IResourceDescription
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.emf.ecore.resource.Resource
import org.junit.Ignore

class ExternalEmfResourceTest extends AbstractLinkingTest {
	
	@Inject extension DeclarationExtensions
	@Inject ValidationTestHelper validator
	@Inject IResourceDescription.Manager mnr
	@Inject IQualifiedNameConverter converter
	
	@Ignore
	@Test def void testOwl() {
		val owl = '''
			<rdf:RDF
			    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
			    xmlns:owl="http://www.w3.org/2002/07/owl#"
			    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
			    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
			  xml:base="http://assert/Properties">
			  <owl:Class rdf:ID="Foo"/>
			  <owl:ObjectProperty rdf:ID="myProperty"/>
			</rdf:RDF>
		'''.owl
		val sadlFile = '''
			uri "http://sadl.org/Tests/Import" alias imp.
			import "http://assert/Properties".
			
			Bar is a type of Foo.
		'''.sadl
		
		validator.assertNoErrors(sadlFile)
		
		val desc = mnr.getResourceDescription(owl)
		val exported = desc.exportedObjects.toList
		Assert.assertEquals("http://assert/Properties", converter.toString(exported.get(0).name))
		Assert.assertEquals("http://assert/Properties:Foo", converter.toString(exported.get(1).name))
		Assert.assertEquals("http://assert/Properties:myProperty", converter.toString(exported.get(2).name))
	}
	
	@Ignore
	@Test def void testNtFormat() {
		'''
			<http://sadl.org/test.sadl#relation1> <http://www.w3.org/2000/01/rdf-schema#domain> <http://sadl.org/test.sadl#Thingy> .
			<http://sadl.org/test.sadl#relation1> <http://www.w3.org/2000/01/rdf-schema#range> <http://sadl.org/test.sadl#Thingy> .
			<http://sadl.org/test.sadl#relation1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#ObjectProperty> .
			<http://sadl.org/test.sadl#Thingy> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> .
			<http://sadl.org/test.sadl#attribute1> <http://www.w3.org/2000/01/rdf-schema#domain> <http://sadl.org/test.sadl#Thingy> .
			<http://sadl.org/test.sadl#attribute1> <http://www.w3.org/2000/01/rdf-schema#range> <http://www.w3.org/2001/XMLSchema#int> .
			<http://sadl.org/test.sadl#attribute1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#DatatypeProperty> .
			<http://sadl.org/test.sadl#MyThingy> <http://sadl.org/test.sadl#relation1> <http://sadl.org/test.sadl#OtherThingy> .
			<http://sadl.org/test.sadl#MyThingy> <http://sadl.org/test.sadl#attribute1> "23"^^<http://www.w3.org/2001/XMLSchema#int> .
			<http://sadl.org/test.sadl#MyThingy> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/test.sadl#Thingy> .
			<http://sadl.org/test.sadl> <http://www.w3.org/2002/07/owl#imports> <http://sadl.org/sadlbasemodel> .
			<http://sadl.org/test.sadl> <http://www.w3.org/2000/01/rdf-schema#comment> "This ontology was created from a SADL file 'test.sadl' and should not be directly edited."@en .
			<http://sadl.org/test.sadl> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Ontology> .
			<http://sadl.org/test.sadl#OtherThingy> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/test.sadl#Thingy> .
		'''.nt.assertContents
	}
	
	@Ignore
	@Test def void testOwlFormat() {
		'''
			<rdf:RDF
			    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
			    xmlns:test="http://sadl.org/test.sadl#"
			    xmlns:owl="http://www.w3.org/2002/07/owl#"
			    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
			    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
			  xml:base="http://sadl.org/test.sadl">
			  <owl:Ontology rdf:about="">
			    <owl:imports rdf:resource="sadlbasemodel"/>
			    <rdfs:comment xml:lang="en">This ontology was created from a SADL file 'test.sadl' and should not be directly edited.</rdfs:comment>
			  </owl:Ontology>
			  <owl:Class rdf:ID="Thingy"/>
			  <owl:ObjectProperty rdf:ID="relation1">
			    <rdfs:domain rdf:resource="#Thingy"/>
			    <rdfs:range rdf:resource="#Thingy"/>
			  </owl:ObjectProperty>
			  <owl:DatatypeProperty rdf:ID="attribute1">
			    <rdfs:domain rdf:resource="#Thingy"/>
			    <rdfs:range rdf:resource="http://www.w3.org/2001/XMLSchema#int"/>
			  </owl:DatatypeProperty>
			  <test:Thingy rdf:ID="MyThingy">
			    <test:relation1>
			      <test:Thingy rdf:ID="OtherThingy"/>
			    </test:relation1>
			    <test:attribute1 rdf:datatype="http://www.w3.org/2001/XMLSchema#int"
			    >23</test:attribute1>
			  </test:Thingy>
			</rdf:RDF>
		'''.owl.assertContents
	}
	
	@Ignore
	@Test def void testN3Format() {
		'''
			@base          <http://sadl.org/test.sadl> .
			@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
			@prefix test:  <http://sadl.org/test.sadl#> .
			@prefix owl:   <http://www.w3.org/2002/07/owl#> .
			@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
			@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
			
			test:relation1  a    owl:ObjectProperty ;
			        rdfs:domain  test:Thingy ;
			        rdfs:range   test:Thingy .
			
			test:Thingy  a  owl:Class .
			
			test:attribute1  a   owl:DatatypeProperty ;
			        rdfs:domain  test:Thingy ;
			        rdfs:range   xsd:int .
			
			test:MyThingy  a         test:Thingy ;
			        test:attribute1  "23"^^xsd:int ;
			        test:relation1   test:OtherThingy .
			
			<>      a             owl:Ontology ;
			        rdfs:comment  "This ontology was created from a SADL file 'test.sadl' and should not be directly edited."@en ;
			        owl:imports   <sadlbasemodel> .
			
			test:OtherThingy  a  test:Thingy .
		'''.n3.assertContents
	}
	
	protected def void assertContents(Resource resource) {
		val sadlFile = '''
			uri "http://sadl.org/Tests/External".
			import "http://sadl.org/test.sadl".
			
			Bar is a Thingy.
		'''.sadl
		
		validator.assertNoErrors(sadlFile)
		
		val desc = mnr.getResourceDescription(resource)
		val exported = desc.exportedObjects.toList
		Assert.assertEquals("http://sadl.org/test.sadl", converter.toString(exported.get(0).name))
		Assert.assertEquals("http://sadl.org/test.sadl:Thingy", converter.toString(exported.get(1).name))
		Assert.assertEquals("http://sadl.org/test.sadl:MyThingy", converter.toString(exported.get(2).name))
		Assert.assertEquals("http://sadl.org/test.sadl:attribute1", converter.toString(exported.get(3).name))
		Assert.assertEquals("http://sadl.org/test.sadl:OtherThingy", converter.toString(exported.get(4).name))
		Assert.assertEquals("http://sadl.org/test.sadl:relation1", converter.toString(exported.get(5).name))
		Assert.assertEquals(6, exported.size)
	}
}