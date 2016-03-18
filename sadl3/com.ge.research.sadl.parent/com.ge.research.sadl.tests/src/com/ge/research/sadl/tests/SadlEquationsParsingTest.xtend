package com.ge.research.sadl.tests

import org.junit.Test

class SadlEquationsParsingTest extends AbstractSADLParsingTest {
	
	@Test
	def void testSimpleEquation() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Equation myEquation(float x, float y) returns float -> x * y.
			Equation anotherEquation(float x) returns float -> myEquation(x, x).
		'''.assertNoErrors
	}
	
	@Test
	def void testQnameConflict() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Equation myEquation(float x, float y) returns f:b:c.
			Equation anotherEquation(float x) returns f: b:c.
		'''.assertNoErrors
	}
	
}