/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.preferences.SadlPreferences
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Ignore
import org.junit.Test

/**
 * Mock test case that shows how to use customized preference values in JUnit Plug-in tests.
 * 
 * @author akos.kitta
 */
class SadlIgnoreUnittedQuantitiyTest extends AbstractSadlPlatformTest {

	@Test
	def void testIgnoreUnitsInSadl() {

		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
			uri "http://sadl.org/OntologyWithUnittedQuantity.sadl" alias OntologyWithUnittedQuantity.
			
			Person is a class described by gender with a single value of type Gender,
				described by age with values of type UnittedQuantity,
				described by height with values of type UnittedQuantity,
				described by heightPercentile with values of type UnittedQuantity,
				described by weight with values of type UnittedQuantity.
			Gender is a class, can only be one of {Male, Female}.
			Obese is a type of Person.
			
			George is a Person with age 23 years, with weight 165 lbs.
			George has height 70 inches, has heightPercentile 50 "%" .
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.empty);
			val ageProperty = jenaModel.getDatatypeProperty("http://sadl.org/OntologyWithUnittedQuantity.sadl#age")
			assertNotNull(ageProperty);
		]
	}

	@Test
	def void testIgnoreUnitsInSadl2() {

		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
			uri "http://sadl.org/OntologyWithUnittedQuantity.sadl" alias OntologyWithUnittedQuantity.
			
			Person is a class described by gender with a single value of type Gender,
				described by age with values of type UnittedQuantity,
				described by height with values of type UnittedQuantity,
				described by heightPercentile with values of type UnittedQuantity,
				described by weight with values of type UnittedQuantity.
			Gender is a class, can only be one of {Male, Female}.
			Obese is a type of Person.
			
			George is a Person with age 23 years, with weight 165 lbs.
			George has height 70 inches, has heightPercentile 50 "%" .
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.empty)
			val ageProperty = jenaModel.getObjectProperty("http://sadl.org/OntologyWithUnittedQuantity.sadl#age")
			assertNotNull(ageProperty);
		]
	}

}
