/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorTypeCheckingTest extends AbstractSADLModelProcessorTest {
	
	@Inject ValidationTestHelper validationTestHelper
	
	@Test
	def void testRuleVariableAsDomain() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 
			 Rule R1: if p1 is a Person and p2 is a Pet and p2 has owns p1 then p1 has caredFor true.
 		'''.sadl
		sadlModel.assertOnlyWarningsOrInfo
		val issues = validationTestHelper.validate(sadlModel)
		Assert.assertNotNull(issues)
		Assert.assertEquals(2, issues.size)
		Assert.assertEquals(issues.get(0).message, "Variable p2 is of type http://sadl.org/Test1.sadl#Pet which is not in domain of property http://sadl.org/Test1.sadl#owns")
		Assert.assertEquals(issues.get(1).message, "Variable p1 is of type http://sadl.org/Test1.sadl#Person which is not in domain of property http://sadl.org/Test1.sadl#caredFor")
	}
	
	@Test
	def void testUserDefinedEquation1() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime,
			 	described by age with values of type float.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 
			 External subtractDates(dateTime t1, dateTime t2, string u) returns float : "http://sadl.org/builtins/subtractDates".
			 
			 // to use the locally defined equation, it must be prefixed with the local model alias
			 Rule AgeRule: if p is a LivingThing then age of p is Test1:subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testSimpleRule() {
		val sadlModel = '''
			 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
			 
			 System is a class,
			    described by var1 with a single value of type int,
			    described by var2 with a single value of type int,
			    described by var3 with a single value of type int,
			    described by var4 with a single value of type int.
			     	
			 Rule example-1:
			 	if var2 of System is not var3 of System
			 	then var1 of System is  var2 of System + var3 of System.
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testSimpleRule2() {
		val sadlModel = '''
			 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
			 
			 System is a class,
			    described by var1 with a single value of type int,
			    described by var2 with a single value of type int,
			    described by var3 with a single value of type int,
			    described by var4 with a single value of type int.
			     	
			 Rule example-1:
			 	if var2 of System is not var3 of System and
			 	x is var2 of System + var3 of System
			 	then var1 of System is  x.
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testUserDefinedEquation2() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime,
			 	described by age with values of type float.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 
			 External subtractDates(dateTime t1, dateTime t2, string u) returns float : "http://sadl.org/builtins/subtractDates".
			 
			 Rule AgeRule: if p is a LivingThing then age of p is Test1:subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_15() {
		'''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
			Foo is a class.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			current_1:Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
//		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_16() {
		'''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
			Foo is a class.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl".
			import "http://sadl.org/I2.sadl".
			
			I1:Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
//		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_x() {

		val model_1 = '''
			uri "http://sadl.org/ExternalFooBar.sadl" alias efb.
			
			External efb:addOne(decimal X) returns decimal: "http://some/other/ns#addOne".
			
			Rule R1:
				if x = 1 and y = builtinfunctions:addOne(x) and z = efb:addOne(x)
				then print("builtinfunctions:addOne returned ",y," , efb:addOne returned ", z).
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
//		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}
	
	def void testIncompletelyDefinedEquation() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime,
			 	described by age with values of type float.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 		 
			 Rule AgeRule: if p is a LivingThing then age of p is subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertError("External equation declaration does not provide type information; can't type check.")
	}
}
