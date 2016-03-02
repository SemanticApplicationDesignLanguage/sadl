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

class InstanceCreationParserTest extends SADLParsingTest {
	
	@Test def void testInstanceCreation_01(){
		'''
			AirCraft is a top-level class.
			F16 is an AirCraft.
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testInstanceCreation_03(){
		'''
			^AirCraft is a top-level class.
			F16 is an AirCraft.
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testInstanceCreation_02(){
		'''
			AirCraft is a top-level class.
			An AirCraft F16.
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testInstanceCreation_WithProperty_01(){
		'''
			AirCraft is a top-level class, described by name with a single value of type string.
			An AirCraft F16, with name 'F16'.
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testInstanceCreation_WithNestedInstance_01(){
		'''
			Person is a top-level class, 
				described by bestFriend with a single value of type Person,
				described by name with a single value of type string.
			Fred is a Person,
				has name 'Fred',
				has bestFriend (Barny is a Person with name 'Barny').
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testInstanceCreation_WithAnonymousNestedInstance_01(){
		'''
			Person is a top-level class, 
				described by bestFriend with a single value of type Person,
				described by name with a single value of type string.
			Fred is a Person,
				has name 'Fred',
				has bestFriend (a Person with name 'Barny').
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testMisc(){
		'''
			Genius is a type of Person.
			iq describes Person has values of type string. 
			iq of Person must be one of {"low", "average", "high"}.
			
			a Person is a Genius only if iq always has value "high".
			
			George is a Person, has iq "high".
		'''.prependUri.assertNoErrors
	}
	
	@Test def void testMisc2(){
		'''
			uri "http://sadl.org/SadlLinking1/GenealogyBase" alias genbase version "$Revision:$ Last modified on   $Date:$". 
			
			Gender is a class, must be one of {Male, Female}.

			Person is a class, described by dateOfBirth with a single value of type dateTime,
				described by gender with a single value of type Gender,
				described by father with values of type {Person and (gender always has value Male)},
				described by mother with values of type {Person and (gender always has value Female)},
				described by spouse with a single value of type Person.

			child describes Person with values of type Person.	
			parent describes Person with values of type Person.
			child is the inverse of parent.

			A Person is a Woman only if gender always has value Female.
			A Person is a Man only if gender always has value Male.	

			father is a type of parent.
			mother is a type of parent.

			biologicalFather is a type of father.
			biologicalMother is a type of mother.

			wife is a type of spouse with values of type {Person and (gender always has value Female)}.
			husband is a type of spouse with values of type {Person and (gender always has value Male)}.
		'''.assertNoErrors
	}
}