/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.model.CircularDefinitionException
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class DeclarationExtensionsTest {
	@Inject extension ParseHelper<SadlModel>
	@Inject extension DeclarationExtensions
	
	@Test def void testConceptUri() {
		val model = '''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen. 
			
			datatype2 is a type of string length 1-4.
			datatype3 is a type of string length 1-*.
			
			SomeClass is a class, described by ident with values of type datatype1.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals("http://sadl.org/TestRequrements/StringLength#datatype2", name2resource.get('datatype2').conceptUri)
		assertEquals("http://sadl.org/TestRequrements/StringLength#datatype3", name2resource.get('datatype3').conceptUri)
		assertEquals("http://sadl.org/TestRequrements/StringLength#SomeClass", name2resource.get('SomeClass').conceptUri)
	}
	
	@Test def void testGetOntConceptType_01() {
		val model = '''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen. 
			
			datatype2 is a type of string length 1-4 .
			
			SomeClass is a class, described by ident with values of type datatype1.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals(OntConceptType.DATATYPE, name2resource.get('datatype2').ontConceptType)
		assertEquals(OntConceptType.CLASS, name2resource.get('SomeClass').ontConceptType)
		
	}
	
	@Test def void testGetOntConceptType_02() {
		val model = '''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen. 
			
			over12 is a type of int [12,].				// an int >= 12
			clothingsize is a type of {int or string}.	// either an int or a string
			enumeratedheight is a type of string {"short", "medium", "tall"}.	// enumeration of 3 possible string values
			SSN is a type of string "[0-9]{3}-[0-9]{2}-[0-9]{4}".
			year is a type of int length 4 .
		'''.parse
		
		model.eAllContents.filter(SadlResource).forEach [
			assertEquals(concreteName, OntConceptType.DATATYPE, ontConceptType)
		]
	}
	
	@Test def void testGetOntConceptType_03() {
		val model = '''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			Person is a class, 
				described by spouse with a single value of type Person,
				described by friend with values of type Person,
				described by age with a single value of type decimal,
				described by likes with values of type string.
			Birth is a class 
				described by child with values of type Person,
				described by mother with a single value of type Person,
				described by location with a single value of type Location,
				described by ^when with a single value of type dateTime,
				described by weight with a single value of type float.
			Location is a class, 
				described by latitude with a single value of type double,
				described by longitude with a single value of type double,
				described by description with values of type string.
		'''.parse
		
		val resources = model.eAllContents.filter(SadlResource).toMap[concreteName]
		resources.get('Person').assertIs(OntConceptType.CLASS)
		resources.get('spouse').assertIs(OntConceptType.CLASS_PROPERTY)
		resources.get('friend').assertIs(OntConceptType.CLASS_PROPERTY)
		resources.get('age').assertIs(OntConceptType.DATATYPE_PROPERTY)
		resources.get('likes').assertIs(OntConceptType.DATATYPE_PROPERTY)
		
		resources.get('Birth').assertIs(OntConceptType.CLASS)
		resources.get('child').assertIs(OntConceptType.CLASS_PROPERTY)
		resources.get('mother').assertIs(OntConceptType.CLASS_PROPERTY)
		resources.get('location').assertIs(OntConceptType.CLASS_PROPERTY)
		resources.get('when').assertIs(OntConceptType.DATATYPE_PROPERTY)
		resources.get('weight').assertIs(OntConceptType.DATATYPE_PROPERTY)
		
		resources.get('Location').assertIs(OntConceptType.CLASS)
		resources.get('latitude').assertIs(OntConceptType.DATATYPE_PROPERTY)
		resources.get('longitude').assertIs(OntConceptType.DATATYPE_PROPERTY)
		resources.get('description').assertIs(OntConceptType.DATATYPE_PROPERTY)
	}
	
	@Test def void testGetOntConceptType_04() {
		val model = '''
			uri "http://sadl.org/TestSadlIde/AnonRestrictions" alias anonrest 
				version "$Revision: 1.3 $ Last modified on   $Date: 2015/06/30 21:27:33 $". 
			Person is a class described by owns with values of type Artifact.
			Artifact is a class.
			Manufacturer is a class.
			Apple is a Manufacturer.
			Dell is a Manufacturer.
			Computer is a type of Artifact described by manufacturer with values of type Manufacturer.
			Professor is a class described by teaches with values of type Student.
			Student is a type of Person.
			A Professor is an AppleProfessor only if teaches has at least one value of type
				{Student and (owns has at least one value of type {Computer and (manufacturer always has value Apple)})}.
			AppleLovingStudent is a type of Student, 
				described by owns with values of type {Computer and (manufacturer always has value Apple)}.
			A Computer is an AppleComputer only if manufacturer always has value Apple.		// necessary and sufficient conditions
			manufacturer of AppleComputer always has value Apple.							// hasValue restriction only
		'''.parse
		
		val resources = model.eAllContents.filter(SadlResource).toMap[concreteName]
		resources.get('Person').assertIs(OntConceptType.CLASS)
		resources.get('Artifact').assertIs(OntConceptType.CLASS)
		resources.get('Manufacturer').assertIs(OntConceptType.CLASS)
		resources.get('Apple').assertIs(OntConceptType.INSTANCE)
		resources.get('Dell').assertIs(OntConceptType.INSTANCE)
		resources.get('Computer').assertIs(OntConceptType.CLASS)
		resources.get('Professor').assertIs(OntConceptType.CLASS)
		resources.get('AppleProfessor').assertIs(OntConceptType.CLASS)
		resources.get('AppleLovingStudent').assertIs(OntConceptType.CLASS)
		resources.get('AppleComputer').assertIs(OntConceptType.CLASS)
	}
	
	@Test def void testGetOntConceptType_05() {
		val model = '''
			uri "http://sadl.org/model1" alias m1.
			annprop is a type of annotation.
		'''.parse
		model.eAllContents.filter(SadlResource).head.assertIs(OntConceptType.ANNOTATION_PROPERTY)
	}
	
	@Test def void testGetOntConceptType_06() {
		val model = '''
			uri "http://sadl.org/model1" alias m1.
			
			Airport_Ident is a type of string length 1-4 .
			Airport is a class, 
				described by ident with values of type Airport_Ident.
		'''.parse
		val airport = model.elements.get(1) as SadlClassOrPropertyDeclaration
		airport.describedBy.head.eContents.filter(SadlResource).head.assertIs(OntConceptType.DATATYPE_PROPERTY)
	}
	
	@Test def void testGetOntConceptType_07() {
		val model = '''
			uri "http://sadl.imp/annotation" alias ann.
			
			Rock (alias "rock") is a top-level class, 
				described by color (note "what color it is") with values of type string.
			
			color has ofInterestTo Everyone .	// uncommenting this causes color to be a declared instance
		'''.parse
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals(OntConceptType.DATATYPE_PROPERTY, name2resource.get('color').ontConceptType)
		assertEquals(OntConceptType.CLASS, name2resource.get('Rock').ontConceptType)
	}
	
	@Test def void testGetOntConceptType_08() {
		val model = '''
			uri "http://sadl.imp/relationship" alias rel.
			
			Person is a class.
			Pet is a class.
			
			relationship of Person to Pet is owns.
		'''.parse
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals(OntConceptType.CLASS, name2resource.get('Person').ontConceptType)
		assertEquals(OntConceptType.CLASS, name2resource.get('Pet').ontConceptType)
		assertEquals(OntConceptType.CLASS_PROPERTY, name2resource.get('owns').ontConceptType)
	}

	// https://github.com/crapo/sadlos2/issues/447
	@Test def void testGetOntConceptType_09() {
		val model = '''
			uri "http://glguy.net/sadl/alldifferentexample".
			C is a class.
			I is a C.
			J is a C.
			{I,J} are not the same.
		'''.parse
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]

		assertEquals(OntConceptType.INSTANCE, name2resource.get('I').ontConceptType)
		assertEquals(OntConceptType.INSTANCE, name2resource.get('J').ontConceptType)
		assertEquals(OntConceptType.CLASS, name2resource.get('C').ontConceptType)
	}

	@Test def void testGetOntConceptType_10() {
		val model = '''
			 uri "http://sadl.org/GrammarCoverage.sadl" alias GrammarCoverage.
			 
			 Person is a class.
			 
			 {Professor, Teacher, Student, Pupil} are types of Person.
			 relationship of {Professor or Teacher} to {Student or Pupil} is teaches.
		'''.parse
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]

		assertEquals(OntConceptType.CLASS_PROPERTY, name2resource.get('teaches').ontConceptType)
	}

	@Test def void testEscapedName() {
		val model = '''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen. 
					
			INPUT_CLASS is a class, must be one of {^A, B, C}.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals(OntConceptType.CLASS, name2resource.get('INPUT_CLASS').ontConceptType)
		assertEquals(OntConceptType.INSTANCE, name2resource.get('C').ontConceptType)
		assertEquals(OntConceptType.INSTANCE, name2resource.get('B').ontConceptType)
		assertEquals(OntConceptType.INSTANCE, name2resource.get('A').ontConceptType)
		
	}
	
	@Test def void testLocalVariable_01() {
		val model = '''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy, described by color with values of type string.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy.
			
			Rule AllThingysConnect: if x is a Thingy and y is a Thingy and x != y then x has connectedTo y .
			Rule AllThingysAreBlue: if x is a Thingy then color of x is "blue".
		'''.parse
		
		val resources = model.eAllContents.filter(SadlResource).toMap[concreteName]
		// TODO how do we check that the two rules have separate local variables?
		resources.get('x').assertIs(OntConceptType.VARIABLE)
		resources.get('y').assertIs(OntConceptType.VARIABLE)
	}
	
	   @Test
    def void testStackOverflow() {
       val model = '''
               uri "http://sadl.org/test.sadl" alias test.
               
               Foo is a type of Foo.
              //Foo is a class.
              //Foo2 is a type of Foo.
              //Foo3 is a type of Foo2.
              //Foo is a type of Foo2.
       '''.parse
              val resources = model.eAllContents.filter(SadlResource).toMap[concreteName]
              // TODO how do we check that the two rules have separate local variables?
              resources.get('Foo').assertIs(OntConceptType.CLASS)
    }
    
	@Test
    def void testIntList() {
       val model = '''
    		uri "http://sadl.org/test.sadl" alias test.
    		Grades is a type of int List.
		'''.parse
              val resources = model.eAllContents.filter(SadlResource).toMap[concreteName]
              // TODO how do we check that the two rules have separate local variables?
              resources.get('Grades').assertIs(OntConceptType.DATATYPE_LIST)
	}
	
	@Test
	def void testGetConcreteName() {
		val model = '''
			uri "http://sadl.org/Test/qualifiedname" alias qn. 
					
			Bar is a class, described by qn:foo with a single value of type string.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		assertTrue(name2resource.containsKey('Bar'))
		assertEquals(OntConceptType.CLASS, name2resource.get('Bar').ontConceptType)
		assertTrue(name2resource.containsKey('foo'))
		assertEquals(OntConceptType.DATATYPE_PROPERTY, name2resource.get('foo').ontConceptType)
	}
	
	@Test
	def void testGH594() {
		val model = '''
		 uri "http://sadl.org/GH594.sadl" alias GH594.
		 
		 prop1 is a property.
		 subprop1 is a type of prop1.
		 
		 prop2 is a property with values of type boolean.
		 subprop2 is a type of prop2.
		 
		 C1 is a class, described by subprop1.
		 C2 is a class, described by prop1.
 		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		assertTrue(name2resource.containsKey('prop1'))
		assertEquals(OntConceptType.RDF_PROPERTY, name2resource.get('prop1').ontConceptType)
		assertTrue(name2resource.containsKey('subprop1'))
		assertEquals(OntConceptType.RDF_PROPERTY, name2resource.get('subprop1').ontConceptType)
	}
	
	@Test
	def void testGH557() {
		val model = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Foo is a class described by bar with values of type Whim.
			 Whim is a class described by wham with values of type string.
			 MyFoo is a Foo with bar (a Whim MyWhim with wham "too bad").
			 Ask: select x where MyFoo is an x.
			 Ask: select c where MyFoo bar b and b is a c.
 		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		assertTrue(name2resource.containsKey('x'))
		assertEquals(OntConceptType.VARIABLE, name2resource.get('x').ontConceptType)
		assertTrue(name2resource.containsKey('b'))
		assertEquals(OntConceptType.VARIABLE, name2resource.get('b').ontConceptType)
		assertTrue(name2resource.containsKey('c'))
		assertEquals(OntConceptType.VARIABLE, name2resource.get('c').ontConceptType)
	}
	
	@Test
	def void testSameAs() {
		val model = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			Singer is a class.
			Instrumentalist is a class.
			Musician is the same as {Singer or Instrumentalist}. 		
			'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		assertTrue(name2resource.containsKey('Musician'))
		assertEquals(OntConceptType.CLASS, name2resource.get('Musician').ontConceptType)
	}

	@Test
	def void testSameAs2() {
		val model = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			Singer is a class.
			Instrumentalist is a class.
			Musician is a class.
			Musician is the same as {Singer or Instrumentalist}. 		
			'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		assertTrue(name2resource.containsKey('Musician'))
		assertEquals(OntConceptType.CLASS, name2resource.get('Musician').ontConceptType)
	}
	
	@Test
	def void testSameAs3() {
		val model = '''
		 uri "http://sadl.org/SameAsProperties.sadl" alias sap.
		 
		 Person is a class described by child with values of type Person,
		 	described by age with values of type int.
		 
		 offspring is the same as child.
		 
		 yearsSinceBirth is the same as age.
		  
		 A Person John has offspring (a Person Mary with yearsSinceBirth 23).
		 
		 rdfp1 is a property.
		 rdfp2 is the same as rdfp1.
		 
		 John has rdfp2 Mary.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		assertTrue(name2resource.containsKey('offspring'))
		assertEquals(OntConceptType.CLASS_PROPERTY, name2resource.get('offspring').ontConceptType)
		assertTrue(name2resource.containsKey('yearsSinceBirth'))
		assertEquals(OntConceptType.DATATYPE_PROPERTY, name2resource.get('yearsSinceBirth').ontConceptType)
		assertTrue(name2resource.containsKey('rdfp2'))
		assertEquals(OntConceptType.RDF_PROPERTY, name2resource.get('rdfp2').ontConceptType)
	}

	protected def void assertIs(SadlResource it, OntConceptType type) {
		assertNotNull(it)
		val typ = try {
		    ontConceptType
		  } catch (CircularDefinitionException e) {
		    e.definitionType
		  }
  		assertEquals(concreteName, type, typ)
	}
	
}