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
package com.ge.research.sadl.tests

import org.junit.Test
import org.junit.Ignore

class OWLDeclarationsParserTest extends SADLParsingTest {
	
/* These tests check for errors that should be found and reported */	
	@Test def void testModelName_Err() {
		var errs = newArrayList("not a valid URL")
		assertErrors('''
		uri "my/uri" alias m1.
		''', errs)
	}
		
	@Test def void testDRemovedDatatype() {
		var errs = newArrayList("missing EOF at 'data'")
		assertErrors('''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen version "$Revision: 1.1 $ Last modified on   $Date: 2015/02/02 22:11:13 $". 
			
			data type Airport_Ident is string length 1-4 .
			Airport_Ident is a type of string length 1-4 .
			
			Airport is a class, described by ident with values of type Airport_Ident.
		''', errs)
	}
	
/* These test check that no errors occur in valid constructs */	
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
	
	@Test def void testClassDefinitions_02b() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			Gender is a class, must be one of {Male, Female}.	// This creates a class equivalent to Gender, owl:oneOf Male, Female 
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
	
	@Test def void testConceptNamesWithDots() {
		'''
			uri "http://com.ge.research.sadl/morenames" alias morenames.
			Aircraft is a class.
			Aircraft2.1 is a type of Aircraft.
			MyAircraft.4 is a type of Aircraft2.1.
		'''.assertNoErrors
	}
	
	@Test def void testClassRestrictions_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			Person is a top-level class.		
			Gender is a class, must be one of {Male, Female, Unknown}.		
			gender describes Person has values of type Gender.		
			gender of Person can only be one of {Male, Female}.		// this creates an owl:AllValuesFrom restriction
				
		'''.assertNoErrors
	}
	
	@Test def void testClassRestrictions_02() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			Person is a top-level class.		
			Gender is a class, must be one of {Male, Female, Unknown}.		
			gender describes Person has values of type Gender.		
			gender of Person must be one of {Male, Female}.		// this creates both owl:AllValuesFrom and owl:SomeValuesFrom restrictions
				
		'''.assertNoErrors
	}
	
	@Test def void testClassRestrictions_03() {
		'''
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
		'''.assertNoErrors
	}
	
	@Ignore
	@Test def void testClassRestrictions_04() {
		'''
		uri "http://sadl.imp/TestThreeLevelDefaults".
		
		Thingy is a top-level class.
		Color is a top-level class, must be one of {Black, White, Green}.
		dp describes Thingy has values of type float.
		op describes Thingy has values of type Color. 
		ready describes Thingy has a single value of type boolean.
		
		dp of Thingy has level 0 default 2.3  . 
		ready of Thingy has level 1 default true.   
		op of Thingy has level 2 default White.
		'''.assertNoErrors
	}

	@Test def void testClassRestrictions_05() {		// AWC, 10/13/15: when placed in the editor, without any white space after the
													//  last period, an error is created which is hard to understand--any way EOF could
													//  act like newline or white space for end of file?
		'''
		uri "http://sadl.org/TestSadlIde/SadlTypedLists" alias typedLists version "$Revision:$ Last modified on   $Date:$". 
		
		Person is a class.
		Mother is a class, described by children with a List of values of type Person.
		
		Flight is a class, described by leg with a List of values of type FlightLeg.
		
			FlightLeg is a class, 
			described by departureTime with a single value of type dateTime,
			described by departureAirport with a single value of type Airport,
			described by arrivalTime with a single value of type dateTime,
			described by arrrivalAirport with a single value of type Airport.
		
			Airport is a class.
		
		FlightSchedule is a class,
			described by flight with Lists of values of type Flight.
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
	
	@Test def void testAllDifferentInstances_01() {
		'''
			uri "http://sadl.org/TestSadlIde/DifferentInstances" alias diffinstances version "$Revision:$ Last modified on   $Date:$". 
			
			Person is a class.
			William is a Person.
			Bill is a Person.
			William is not the same as Bill.
			George is a Person.
			{William, Bill, George} are not the same.		
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

	@Test def void testPropertyDeclarations_03() {
		'''
			uri "http://sadl.org/TestSadlIde/PropertyTypes" alias proptypes version "$Revision:$ Last modified on   $Date:$". 

			p1 is a property with values of type data.
			p1 has a single value.
			
			p2 is a property.
			p2 has a single subject.
			
			p3 is a property.
			p3 is symmetrical.
			
			p4 is a property.
			p4 is transitive.
			
			p3 is the inverse of p4.
		'''.assertNoErrors
	}	

	@Test def void testAnnotationPropertyDeclarations_01() {
		'''
			uri "http://sadl.imp/annotation" version "$Revision: 1.6 $ $Name:  $ $Date: 2015/06/30 21:27:33 $".
			
			// In this example, the museum has samples of Rock a class in the model,
			// but also has specimens assigned to various Holdings. An annotation
			// property is used to keep track of to which Holding a specific Rock belongs.
			// In addition, various properties of rocks are of interest to different
			// categories of museum Patrons, and the properties are annotated to reflect this.
			
			Rock is a top-level class, 
				described by color with values of type string,
				described by hardness with values of type Hardness.
			
			Hardness is a top-level class, must be one of {Hard, Soft}. 
			
			
			Holding is a top-level class, must be one of {SouthwestCollection, NortheastCollection}.
			Holding has label "this is a label".
			Holding has seeAlso "http://sadl.org/documentation". 
			
			PatronType is a top-level class, must be one of {Expert, Novice, Everyone}. 
			
			holding is a type of annotation.	// Note that an annotation cannot have a specified domain and range.
			ofInterestTo is a type of annotation.
			
			color has ofInterestTo Everyone .
			hardness has ofInterestTo Expert.
			
			age (alias "AGE") describes Rock has a single value of type int. 
			
			age has holding "European".
			Holding has holding "European".		

			ChipOfMammyGametBolder is a Rock, has holding NortheastCollection.
		'''.assertNoErrors
	}
	
	@Ignore("https://github.com/crapo/sadlos2/issues/14")
	@Test def void testInstanceDeclaration_01() {
		'''
			uri "http://com.ge.research.sadlGeorgeAndMartha".
			
			A Birth has child George, has mother Mary, 
				has location (a Location has latitude 38.186111, has longitude -76.930556, 
					has description "Pope's Creek Estate near present-day Colonial Beach, Virginia, USA"),
				has ^when "1732-02-22",
				has weight 9.45.
		'''.assertNoErrors
	}
	
	@Ignore("https://github.com/crapo/sadlos2/issues/14")
	@Test def void testInstanceDeclaration_02() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy, connectedTo T1, connectedTo T2.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceDeclaration_03() {			// AWC, 10/13/15: this should be an error. However, the 
																//	error message isn't very easy to understand....
		'''
			uri "http://sadl.imp/baduris".
			
			// Building this file (to create OWL model) should produce errors
			//  because localnames of concepts cannot start with a digit.
			
			SmallNumbers is a top-level class, must be one of {N1, N2, N3}.
		'''.assertNoErrors
	}

	@Test def void testInstanceDeclaration_04() {
		'''
			uri "http://sadl.imp/negnumber".
						
			Freezer is a class, described by expectedTemperature with values of type float.
			
			MyFreezer is a Freezer with expectedTemperature -20 .
			YourFreezer is a Freezer with expectedTemperature -20.5.
			SomeFreezer is a Freezer with expectedTemperature -3.2e2 .
		'''.assertNoErrors
	}
}