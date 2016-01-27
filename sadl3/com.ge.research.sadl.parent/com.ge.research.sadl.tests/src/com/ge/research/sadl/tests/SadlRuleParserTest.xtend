package com.ge.research.sadl.tests

import org.junit.Test
import org.junit.Ignore

class SadlRuleParserTest extends SADLParsingTest {
	
/* These tests check for errors that should be found and reported */	
	
/* These test check that no errors occur in valid constructs */	
	@Test def void testInstanceDeclaration_02() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy.
			
			Rule AllThingsConnect: if x is a Thingy and y is a Thingy and x != y then x has connectedTo y .
		'''.assertNoErrors
	
	}
}