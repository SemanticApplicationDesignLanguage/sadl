package com.ge.research.sadl.tests

import org.junit.Test

class InstanceCreationParserTest extends SADLParsingTest {
	
	@Test def void testInstanceCreation_01(){
		'''
			AirCraft is a top-level class.
			F16 is an AirCraft.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceCreation_02(){
		'''
			AirCraft is a top-level class.
			An AirCraft F16.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceCreation_WithProperty_01(){
		'''
			AirCraft is a top-level class, described by name with a single value of type string.
			An AirCraft F16, with name 'F16'.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceCreation_WithNestedInstance_01(){
		'''
			Person is a top-level class, 
				described by bestFriend with a single value of type Person,
				described by name with a single value of type string.
			Fred is a Person,
				has name 'Fred',
				has bestFriend (Barny is a Person with name 'Barny').
		'''.assertNoErrors
	}
}