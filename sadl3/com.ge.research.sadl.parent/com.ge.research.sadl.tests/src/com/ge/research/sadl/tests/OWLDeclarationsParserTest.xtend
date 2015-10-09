package com.ge.research.sadl.tests

import org.junit.Test
import org.junit.Ignore

class OWLDeclarationsParserTest extends SADLParsingTest {
	
	@Test def void testModelName_01() {
		'''
		uri "http://sadl.org/Tests/ModelName" alias ^class.
		'''.assertNoErrors
	}
	
	@Test def void testModelName_02() {
		'''
		uri "http://sadl.org/Tests/ModelName" alias mn version "1".		
		'''.assertNoErrors
	}
	
	@Test def void testModelName_03() {
		// this is fairly new and is probably inconsistent with annotations for classes, properties, and instances in that
		// the annotation is not right after the name but is after the prefix and version.
		'''
		uri "http://sadl.org/Tests/ModelName" alias mn version "1" (note "This is an rdfs:label") (alias "Model Name").		
		'''.assertNoErrors
	}
	
	@Test def void testModelName_04() {
		'''
		uri "http://sadl.org/Tests/ModelName" alias mn version "1" 
			(note "This is an rdfs:label", "this is another label")
			(alias "Model Name", "2nd alias").				
		'''.assertNoErrors
	}
	
	@Test def void testModelName_05() {
		'''
		uri "http://sadl.org/Tests/ModelName" alias mn version "1" 
			(note "This is an rdfs:label", "this is another label") (note "this is a 3rd label")
			(alias "Model Name", "2nd alias"), (alias "3rd alias").				
		'''.assertNoErrors
	}
	
	@Test def void testImport_01() {
		'''
		uri "http://sadl.org/Tests/Import" alias imp. 
		import "http://sadl.org.Tests/ModelName".		
		'''.assertNoErrors
	}
	
	@Test def void testImport_02() {
		'''
		uri "http://sadl.org/Tests/Import" alias imp. 
		import "http://sadl.org.Tests/ModelName" as mn2.		
		'''.assertNoErrors
	}

	
	
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
	
	@Test def void testClassDefinitions_01a() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			SomeClass2 is a type of SomeClass.
			SomeClass3 is a type of {SomeClass, SomeClass2}.		// It would probably be a good idea to remove comma-separated and 
																	//  require explict 'and' or 'or' in intersection or union unnamed
																	//  classes. The old grammar allowed ',' to be used for 'or'
																	//  This is a different case from a list, such as a list of
																	//  instances in the 'must be one of {int1, inst2, ...}'
			{^A,B,C} are top-level classes.
			{A1,B1,C1} are types of {SomeClass, ^A}.
		'''.assertNoErrors
	}
	
	@Test def void testClassDefinitions_02() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a class.
			SomeClass2 is a type of SomeClass.
			SomeClass3 is a type of {SomeClass and SomeClass2}.
			{^A,B,C} are classes.
			{A1,B1,C1} are types of {SomeClass or ^A}.
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
			
			SomeClass is a class.
			SomeOtherClass is a top-level class,
				described by p1 with a single value of type SomeClass,
				described by p2 has a List of values of type string,
				described by p3 with Lists of values of type int,
				described by p4 with values of type dateTime.
				
		'''.assertNoErrors
	}
	@Test def void testClassDefinitions_withPropertyAndConstraint_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a class.
			SomeOtherClass is a top-level class,
				described by p1 only with values of type SomeClass,
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
			uri "http://sadl.org/TestSadlIde/Compliments" alias compliments version "$Revision:$ Last modified on   $Date:$". 
			
			Mineral is a class.
			Vegetable is the same as not Mineral.  	// The LHS of the declaration does not need to be pre-defined
			
			NonOrganic is the same as Mineral.		// The LHS of the declaration does not need to be pre-defined
		'''.assertNoErrors
	}
	
	@Test def void testDisjointClasses_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a class.
			SomeClass and AnotherClass are disjoint.	// this is not an intersection class because it isn't in curley brackets
		'''.assertNoErrors
	}
	
	@Test def void testDisjointClasses_02() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a class.
			SomeClass and AnotherClass are disjoint.
			YetAnotherClass is a class.
			{SomeClass, AnotherClass, YetAnotherClass} are disjoint.		// this is just a list of classes, not union or intersection
		'''.assertNoErrors
	}
	
	@Test def void testPropertyDeclarations_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a top-level class.
			AnotherClass is a top-level class.
			fooProperty describes SomeClass with a single value of type AnotherClass.
			relationship of SomeClass to AnotherClass is fooProperty.
		'''.assertNoErrors
	}

	@Test def void testPropertyDeclarations_02() {
		'''
			uri "http://com.ge.research.propdecls".
			
			Person is a class.
			Professor is a type of Person.
			Student is a type of Person.
			relationship of Professor to Student is teaches.
		'''.assertNoErrors
	}

}