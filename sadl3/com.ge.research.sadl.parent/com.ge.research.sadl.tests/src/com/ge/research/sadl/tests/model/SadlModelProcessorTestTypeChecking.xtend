/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorTestTypeChecking extends AbstractProcessorTest {
	
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
	def void testUserDefinedEquation() {
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
			 
			 Rule AgeRule: if p is a LivingThing then age of p is subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testUndefinedEquation() {
		val sadlBuiltinModel = '''
			uri "http://sadl.org/builtinfunctions" alias builtinfunctions.
			
			// Please do not edit!! This model was auto-generated and may be replaced at any time.
			//   The content of this model is provided by the selected reasoner/translator pair.
			External sum(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#sum".
			
			External minus(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#minus".
			
			External product(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#product".
			
			External divide(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#divide".
			
			External mod(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#mod".
			
			External power(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#power".
			
			External ^and(boolean X, boolean X) returns boolean:
			"http://sadl.org/builtinfunctions#and".
			
			External ^or(boolean X, boolean X) returns boolean:
			"http://sadl.org/builtinfunctions#or".
			
			External ^not(boolean X) returns boolean:
			"http://sadl.org/builtinfunctions#not".
			
			External ^is(boolean X) returns boolean:
			"http://sadl.org/builtinfunctions#is".
			
			External ge(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#ge".
			
			External gt(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#gt".
			
			External le(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#le".
			
			External lt(decimal X, decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#lt".
			
			External sin(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#sin".
			
			External cos(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#cos".
			
			External tan(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#tan".
			
			External cotan(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#cotan".
			
			External sec(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#sec".
			
			External cosec(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#cosec".
			
			External arc_sin(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#arc_sin".
			
			External arc_cos(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#arc_cos".
			
			External arc_tan(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#arc_tan".
			
			External arc_cotan(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#arc_cotan".
			
			External arc_sec(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#arc_sec".
			
			External arc_cosec(decimal X) returns decimal:
			"http://sadl.org/builtinfunctions#arc_cosec".
		'''.sadl
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
		sadlBuiltinModel.assertNoErrors
		sadlModel.assertError("Function subtractDates is not defined so return type is unknown, cant do type checking")
	}
}
