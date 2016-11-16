
/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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

class SADLParsingTest extends AbstractSADLParsingTest {

	@Test 
	def void testSublist_01() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching var1 is true and var2 is true and var3 is false).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_02() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching value >= 42).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_03() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching index >= 42).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_04() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching type is Person).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_05() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching OtherList does not contain value).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_06() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching index >= 42 and type is Person and OtherList does not contain value).
		'''.assertNoErrors
	}
	
	@Test 
	def void testListTypes_01() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			MyPets is a Pet List.
		'''.assertNoErrors
	}
	
	@Test 
	def void testListTypes_02() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			GennysPriorizedPets is the Pet List [Spot, Lassie].
		'''.assertNoErrors
	}
	
	@Test 
	def void testListTypes_03() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: MyPets is a Pet List.
		'''.assertNoErrors
	}

	@Test 
	def void testPropertyTypeOnly() {
		'''
			uri "http://com.ge.research.sadl/proptypeonly". 
			
			dtprop is a property with values of type data.
			objprop is a property with values of type class.
		'''.assertNoErrors
	}

	@Test 
	def void testSubjHasPropCompareExpr() {
		'''
			 uri "http://sadl.org/testrule.sadl" alias testrule.
			 
			 Person is a class described by age with values of type int.
			 
			 Rule R1: if x is a Person and x has age > 30 then print("hi").
			 
			 Rule R2: if x is a Person and age of x > 30 then print("hi").
 		'''.assertNoErrors
	}
	
	@Test
	def void testUnitsOnNumbers() {
		''' uri "http://sadl.org/OntologyWithUnittedQuantity.sadl" alias OntologyWithUnittedQuantity.
		 
		 	George has height 70 inches.
		'''.assertNoErrors
	}
	
	@Test
	def void testConstants() {
		'''
			 uri "http://sadl.org/OntologyWithoutUnittedQuantity.sadl" alias OntologyWithoutUnittedQuantity.
			 
			 Person is a class described by gender with a single value of type Gender,
			 	described by age with values of type decimal,
			 	described by height with values of type decimal,
			 	described by weight with values of type decimal.
			 Gender is a class, can only be one of {Male, Female}. 	
			 Obese is a type of Person.
			 	
			 George is a Person with age 23, with height 70, with weight 165.	
			 
			 Mary is a Person with weight PI.
			// Emily is a Person with weight e .		// why does this statement fail?
			 
			 Rule DumbRule:
			 	if p is a Person
			 	then weight of p is age of p * PI * e^3.
		'''.assertNoErrors
	}
	
	@Test
	def void testTestWithCommas() {
		'''
			uri "http://sadl.imp/abs".
			
			Thingy is a top-level class.
			intVal describes Thingy has values of type int.
			flVal describes Thingy has values of type float.
			dblVal describes Thingy has values of type double.
			
			Rule Rule_Name
				given
					x is a Thingy
			//	if
					
				then
					intVal of x is abs(-1) and
					flVal of x is abs(-1.0) and
					dblVal of x is abs(-1.0).
					
			MyThingy is a Thingy.
			
			Test: MyThingy has intVal 1, has flVal 1.0, has dblVal 1.0 .
		'''.assertNoErrors
	}
	
}
