package com.ge.research.sadl.tests.model

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
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
			
			data type datatype1 is string length 1-4 .
			datatype2 is a type of string length 1-4 .
			
			SomeClass is a class, described by ident with values of type datatype1.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals("http://sadl.org/TestRequrements/StringLength#datatype1", name2resource.get('datatype1').conceptUri)
		assertEquals("http://sadl.org/TestRequrements/StringLength#datatype2", name2resource.get('datatype2').conceptUri)
		assertEquals("http://sadl.org/TestRequrements/StringLength#SomeClass", name2resource.get('SomeClass').conceptUri)
		
	}
	
	@Test def void testGetOntConceptType_01() {
		val model = '''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen. 
			
			data type datatype1 is string length 1-4 .
			datatype2 is a type of string length 1-4 .
			
			SomeClass is a class, described by ident with values of type datatype1.
		'''.parse
		
		val name2resource = model.eAllContents.filter(SadlResource).toMap[concreteName]
		
		assertEquals(OntConceptType.DATATYPE, name2resource.get('datatype1').ontConceptType)
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
	
	protected def void assertIs(SadlResource it, OntConceptType type) {
		assertNotNull(it)
		assertEquals(concreteName, type, ontConceptType)
	}
	
}