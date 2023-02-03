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

	@Test
	def void testIgnoreUnitsInSadl3() {

		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
			uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
			Shape is a class described by area with values of type UnittedQuantity.
			Rectangle is a class described  by height with values of type UnittedQuantity,
						 	described by width with values of type UnittedQuantity.
						 	
			MyRect is a Rectangle with width 4.0 ft, with height 2.2 ft.
						 	
			Rule R1: if x is a Rectangle then area of x is height of x * width of x.
						 
			Ask: select s, ar where s is a Rectangle and s has area ar.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.empty)
			for (rule : rules) {
				println(rule.toString)
			}
			assertTrue(processor.compareTranslations(rules.get(0).toString, 
				"Rule R1:  if rdf(x, rdf:type, impliedpropertiesinrule:Rectangle) and rdf(x, impliedpropertiesinrule:height, v0) and rdf(x, impliedpropertiesinrule:width, v1) and *(v0,v1,v2) then rdf(x, impliedpropertiesinrule:area, v2)."))
		]
	}

	@Test
	def void testIgnoreUnitsInSadl4() {

		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString));
//		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString))

		createFile('OntologyWithUnittedQuantity.sadl', '''
			uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
			Shape is a class described by area with values of type UnittedQuantity.
			Rectangle is a class described  by height with values of type UnittedQuantity,
						 	described by width with values of type UnittedQuantity.
						 	
			MyRect is a Rectangle with width 4.0 ft, with height 2.2 ft.
						 	
			Rule R1: if x is a Rectangle then area of x is height of x * width of x.
						 
			Ask: select s, ar where s is a Rectangle and s has area ar.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue.toString)
			}
			assertTrue(issues.empty)
			for (rule : rules) {
				println(rule.toString)
			}
			assertTrue(processor.compareTranslations(rules.get(0).toString, 
				"Rule R1:  if rdf(x, rdf:type, impliedpropertiesinrule:Rectangle) and rdf(x, impliedpropertiesinrule:height, v0) and rdf(x, impliedpropertiesinrule:width, v1) and *(v0,v1,v2) then rdf(x, impliedpropertiesinrule:area, v2)."))
		]
	}
	
	@Test
	def void testIgnoreUnitsInSadl5() {
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type int.
			 
			 George is a Person with age 25 years. 
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (!issues.empty) {
				for (issue : issues) {
					println(issue.toString)
				}
			}
			assertTrue(issues.empty)
		]
	}

	@Test
	def void testIgnoreUnitsInSadl6() {
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
				 uri "http://sadl.org/test2.sadl" alias test2.
				 
				 Person is a class described by age with values of type UnittedQuantity.
				 
				 George is a Person with age 25 years.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (!issues.empty) {
				for (issue : issues) {
					println(issue.toString)
				}
			}
			assertTrue(issues.empty)
		]
	}

	@Test
	def void testIgnoreUnitsInSadl7() {
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 TimeSpan is a type of UnittedQuantity.
			 
			 Person is a class described by age with values of type TimeSpan.  
			 
			 George is a Person with age 25 years.
 		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (!issues.empty) {
				for (issue : issues) {
					println(issue.toString)
				}
			}
			assertTrue(issues.empty)
		]
	}

}
