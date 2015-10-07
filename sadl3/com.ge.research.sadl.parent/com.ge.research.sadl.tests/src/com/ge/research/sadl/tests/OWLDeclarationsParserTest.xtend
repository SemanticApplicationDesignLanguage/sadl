package com.ge.research.sadl.tests

import org.junit.Test
import org.junit.Ignore

class OWLDeclarationsParserTest extends SADLParsingTest {
	
	@Test def void testDatatype() {
		'''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen version "$Revision: 1.1 $ Last modified on   $Date: 2015/02/02 22:11:13 $". 
			
			data type Airport_Ident is string length 1-4 .
			Airport_Ident is a type of string length 1-4 .
			
			Airport is a class, described by ident with values of type Airport_Ident.
		'''.assertNoErrors
	}
	
	@Test def void testDatatype_01() {
		'''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen version "$Revision: 1.1 $ Last modified on   $Date: 2015/02/02 22:11:13 $". 
			
			over12 is a type of int [12,].				// an int >= 12
			clothingsize is a type of {int or string}.	// either an int or a string
			enumeratedheight is a type of string {"short", "medium", "tall"}.	// enumeration of 3 possible string values
			SSN is a type of string "[0-9]{3}-[0-9]{2}-[0-9]{4}".
			year is a type of int length 4 .
		'''.assertNoErrors
	}
	
	@Ignore
	@Test def void testMultipleInstances() {
		'''
			{John, James, Susan} are instances of Actor.
		'''.assertNoErrors
	}
	
	@Test def void testClassDefinitionWithProperties() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			Person is a class, described by spouse with a single value of type Person,
				described by friend with values of type Person,
				described by age with a single value of type decimal,
				described by likes with values of type string.
			Birth is a class described by child with values of type Person,
				described by mother with a single value of type Person,
				described by location with a single value of type Location,
				described by ^when with a single value of type dateTime,
				described by weight with a single value of type float.
			Location is a class, described by latitude with a single value of type double,
				described by longitude with a single value of type double,
				described by description with values of type string.
		'''.assertNoErrors
	}
	
	@Test def void testClassDefinitions_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			SomeClass2 is a type of SomeClass.
			SomeClass3 is a type of {SomeClass, SomeClass2}.
			{^A,B,C} are top-level classes.
			{A1,B1,C1} are types of {SomeClass, ^A}.
		'''.assertNoErrors
	}
	
	@Test def void testClassDefinitions_withProperty_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class,
				described by p1 with a single value of type string,
				described by p2 has a List of values of type string,
				described by p3 with Lists of values of type string,
				described by p4 with values of type string.
				
		'''.assertNoErrors
	}
	
	@Test def void testClassDefinitions_withProperty_02() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class,
				described by p1 only with values of type string,
				described by p2 always has value 42,
				described by p3 with at least 42 values of type string,
				described by p4 with at most 42 values each of types {string, int},
				described by p4 with exactly one value.
				
		'''.assertNoErrors
	}
	
	@Test def void testEquivalenceAndComplemence() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a top-level class.
			SomeClass is the same as AnotherClass.
		'''.assertNoErrors
	}
	
	@Test def void testEquivalenceAndComplemence_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a top-level class.
			SomeClass is the same as not AnotherClass.
		'''.assertNoErrors
	}
	
	@Test def void testDisjointClasses_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a top-level class.
			SomeClass is the same as not AnotherClass.
		'''.assertNoErrors
	}
	
	@Test def void testPropertyDeclarations_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a top-level class.
			foo of SomeClass has a single value of type AnotherClass.
		'''.assertNoErrors
	}

}