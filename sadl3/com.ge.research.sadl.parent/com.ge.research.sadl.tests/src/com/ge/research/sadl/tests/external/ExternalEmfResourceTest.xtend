/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.tests.external

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.resource.IResourceDescription
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Assert
import org.junit.Test
import org.junit.Ignore
import java.util.ArrayList

class ExternalEmfResourceTest extends AbstractLinkingTest {
	
	@Inject ValidationTestHelper validator
	@Inject IResourceDescription.Manager mnr
	@Inject IQualifiedNameConverter converter
	
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
		'''.owl;
		val sadlFile = '''
			uri "http://sadl.org/Tests/Import" alias imp.
			import "http://assert/Properties".
			
			Bar is a type of Foo.
		'''.sadl
		
		validator.assertNoErrors(sadlFile)
		val expectedExportedNames = #{
			'http://assert/Properties',
			'http://assert/Properties:Foo',
			'http://assert/Properties:myProperty'};
		val actualExportedNames = mnr.getResourceDescription(owl).exportedObjects.map[converter.toString(name)].toSet;
		Assert.assertEquals(expectedExportedNames, actualExportedNames);
	}
	
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
	
	@Test def void testN3Format() {
		val content = '''
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
		''';
		
		content.n3.assertContents;
	}
	
	@Test def void GH_201() {
		ExternalResourceContentHelper.getContent('aulo.owl').owl;
		ExternalResourceContentHelper.getContent('apvf.owl').owl;
		'''
			uri "http://sadl.org/base.sadl".
			Shape is a class.
		'''.sadl;
		'''
			uri "http://sadl.org/extension.sadl" alias extension.

			import "http://research.ge.com/Acuity/apvf.owl".
			import "http://sadl.org/base.sadl".
			
			Circle is a Shape. 
			FooBar is a type of AcuityController.
			MyHero is an ArtificialAgent.
		'''.sadl.assertNoErrors;
	}
	
	@Ignore
	@Test def void testSadlBaseModel() {
		'''
			<rdf:RDF
				xmlns="http://sadl.org/sadlbasemodel#"
			    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
			    xmlns:owl="http://www.w3.org/2002/07/owl#"
			    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
			    xmlns:sadlbasemodel="http://sadl.org/sadlbasemodel#"
			    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
			  xml:base="http://sadl.org/sadlbasemodel">
			  <owl:Ontology rdf:about="">
			    <rdfs:comment xml:lang="en">Base model for SADL. These concepts can be used without importing.</rdfs:comment>
			  </owl:Ontology>
			  <owl:Class rdf:ID="ExternalEquation"/>
			  <owl:Class rdf:ID="Equation"/>
			  <owl:DatatypeProperty rdf:ID="location">
			    <rdfs:range rdf:resource="http://www.w3.org/2001/XMLSchema#string"/>
			    <rdfs:domain rdf:resource="#ExternalEquation"/>
			  </owl:DatatypeProperty>
			  <owl:DatatypeProperty rdf:ID="externalURI">
			    <rdfs:range rdf:resource="http://www.w3.org/2001/XMLSchema#anyURI"/>
			    <rdfs:domain rdf:resource="#ExternalEquation"/>
			  </owl:DatatypeProperty>
			  <owl:DatatypeProperty rdf:ID="expression">
			    <rdfs:range rdf:resource="http://www.w3.org/2001/XMLSchema#string"/>
			    <rdfs:domain rdf:resource="#Equation"/>
			  </owl:DatatypeProperty>
			</rdf:RDF>
		'''.owl.explore
	}
	
	protected def void explore(Resource resource) {
		val sadlFile = '''
			uri "http://sadl.org/Tests/External".
			
			Bar is an ^Equation.
		'''.sadl
		
		validator.assertNoErrors(sadlFile)
		
		val desc = mnr.getResourceDescription(resource)
		val exported = desc.exportedObjects.toList
		for (exp : exported) {
			print(converter.toString(exp.name))
		}
		
//		Assert.assertEquals("http://sadl.org/test.sadl", converter.toString(exported.get(0).name))
		
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
		Assert.assertTrue(exported.size == 6)
		var content = new ArrayList<String>()
		for (var i = 0; i < 6; i++) {
			content.add(converter.toString(exported.get(i).name))
		}
		Assert.assertTrue(content.contains("http://sadl.org/test.sadl"))
		Assert.assertTrue(content.contains("http://sadl.org/test.sadl:Thingy"))
		Assert.assertTrue(content.contains("http://sadl.org/test.sadl:MyThingy"))
		Assert.assertTrue(content.contains("http://sadl.org/test.sadl:attribute1"))
		Assert.assertTrue(content.contains("http://sadl.org/test.sadl:OtherThingy"))
		Assert.assertTrue(content.contains("http://sadl.org/test.sadl:relation1"))
	}
}