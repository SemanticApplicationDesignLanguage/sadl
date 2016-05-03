
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

}
