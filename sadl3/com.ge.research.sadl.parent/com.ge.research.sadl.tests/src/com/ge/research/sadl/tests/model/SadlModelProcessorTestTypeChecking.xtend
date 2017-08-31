/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 * 
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLParsingTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.common.collect.Iterables
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorTestTypeChecking extends AbstractSADLParsingTest {

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
		val issues = validate(sadlModel)
		assertNotNull(issues)
		assertEquals(2, issues.size)
		assertEquals(issues.get(0).message,
			"Variable p2 is of type http://sadl.org/Test1.sadl#Pet which is not in domain of property http://sadl.org/Test1.sadl#owns")
		assertEquals(issues.get(1).message,
			"Variable p1 is of type http://sadl.org/Test1.sadl#Person which is not in domain of property http://sadl.org/Test1.sadl#caredFor")
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
			
			External Test1:subtractDates(dateTime t1, dateTime t2, string u) returns float : "http://sadl.org/builtins/subtractDates".
			
			Rule AgeRule: if p is a LivingThing then age of p is Test1:subtractDates(now(), dateOfBirth of p, "y").
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

	// https://github.com/crapo/sadlos2/issues/226#issuecomment-325383354
	@Test
	def void testLocalVsImportedNames_GH_226_05() {
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
			
			Baz is a type of Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 1, issues_1.size);
		assertTrue(issues_1.head.message, issues_1.head.message.startsWith('''Ambiguously imported name 'Foo'''));

		val model_2 = '''
			uri "http://sadl.org/Current_2.sadl" alias current_2.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Baz is a type of i1:Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_2 = validate(model_2);
		assertEquals(Iterables.toString(issues_2), 0, issues_2.size);

		val model_3 = '''
			uri "http://sadl.org/Current_3.sadl" alias current_3.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Baz is a type of i1:Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_3 = validate(model_3);
		assertEquals(Iterables.toString(issues_3), 0, issues_3.size);
	}

	@Test
	def void testLocalVsImportedNames_GH_226_06() {
		val i1 = '''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Baz is a type of Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
		assertFalse(model_1.sadlResourcesFrom.containsKey('Foo'));
		assertTrue(i1.sadlResourcesFrom.containsKey('Foo'));
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_09() {
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
			
			Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 1, issues_1.size);
		assertTrue(issues_1.head.message, issues_1.head.message.startsWith('''Ambiguously imported name 'Foo'''));

		val model_2 = '''
			uri "http://sadl.org/Current_2.sadl" alias current_2.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			current_2:Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_2 = validate(model_2);
		assertEquals(Iterables.toString(issues_2), 0, issues_2.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_10() {
		'''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 1, issues_1.size);
		assertTrue(issues_1.head.message, issues_1.head.message.startsWith('''Ambiguously imported name 'Foo'''));

		val model_2 = '''
			uri "http://sadl.org/Current_2.sadl" alias current_2.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			current_2:Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_2 = validate(model_2);
		assertEquals(Iterables.toString(issues_2), 0, issues_2.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_13() {
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
			
			Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 1, issues_1.size);
		assertTrue(issues_1.head.message, issues_1.head.message.startsWith('''Ambiguously imported name 'Foo'''));

		val model_2 = '''
			uri "http://sadl.org/Current_2.sadl" alias current_2.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Bar is a type of Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_2 = validate(model_2);
		assertEquals(Iterables.toString(issues_2), 1, issues_2.size);
		assertTrue(issues_2.head.message, issues_2.head.message.startsWith('''Ambiguously imported name 'Foo'''));
		
		val model_3 = '''
			uri "http://sadl.org/Current_3.sadl" alias current_3.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			current_3:Foo is a class.
			Bar is a type of current_3:Foo.
			Baz is a type of i1:Foo.
			Qux is a type of i2:Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_3 = validate(model_3);
		assertEquals(Iterables.toString(issues_3), 0, issues_3.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_14() {
		'''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 1, issues_1.size);
		assertTrue(issues_1.head.message, issues_1.head.message.startsWith('''Ambiguously imported name 'Foo'''));

		val model_2 = '''
			uri "http://sadl.org/Current_2.sadl" alias current_2.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			Bar is a type of Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_2 = validate(model_2);
		assertEquals(Iterables.toString(issues_2), 0, issues_2.size);
		
		val model_3 = '''
			uri "http://sadl.org/Current_3.sadl" alias current_3.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			current_3:Foo is a class.
			Bar is a type of current_3:Foo.
			Baz is a type of i1:Foo.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_3 = validate(model_3);
		assertEquals(Iterables.toString(issues_3), 0, issues_3.size);
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
		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}

}
