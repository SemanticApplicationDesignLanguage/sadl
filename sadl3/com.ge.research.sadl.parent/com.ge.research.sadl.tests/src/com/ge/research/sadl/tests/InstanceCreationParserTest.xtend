package com.ge.research.sadl.tests

import org.junit.Test

class InstanceCreationParserTest extends SADLParsingTest {
	
	@Test def void testInstanceCreation_01(){
		'''
			«model»
			
			AirCraft is a top-level class.
			F16 is an AirCraft.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceCreation_02(){
		'''
			«model»
			
			AirCraft is a top-level class.
			An AirCraft F16.
		'''.assertNoErrors
	}
}