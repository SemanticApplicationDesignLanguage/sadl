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
package com.ge.research.sadl.ui.test.inference

import com.ge.research.sadl.model.gp.TestResult
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.reasoner.ConfigurationItem
import com.ge.research.sadl.reasoner.SadlCommandResult
import com.ge.research.sadl.ui.tests.AbstractSadlPlatformTest
import java.util.List
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

import static com.ge.research.sadl.ui.tests.GeneratedOutputFormat.*
import com.ge.research.sadl.reasoner.ResultSet
import org.junit.Ignore

/**
 * Test that demonstrate how to make assertions on the generated translator outputs, plus runs the inferencer too.
 * 
 * @author akos.kitta
 */
class ImpliedExtendedPropertiesTest extends AbstractSadlPlatformTest {

	@Test
	def void testImpliedPropertyInTest_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertyInTest.sadl" alias ImpliedPropertyInTest.
			 
			 Person is a class 
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Sue is a Person with age 23, with weight 125.
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Test: Sue is 23 .
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInRule_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule2.sadl" alias ipt.
			  
			 Person is a class 
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Adult is a type of Person.
			 
			 Sue is a Person with age 23, with weight 125.
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Rule R1: if x is a Person and x > 18 then x is an Adult.
			 
			 Test: Sue is an Adult .			 
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(x, rdf:type, ipt:Person) and rdf(x, ipt:age, v0) and >(v0,18) then rdf(x, rdf:type, ipt:Adult)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				if ((tr as TestResult).passed) 
					println("    passed\n")
				else {
					println("    failed\n")
				}
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInTest_02() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertyInTest.sadl" alias ipt.
			 
			 Person is a class 
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Sue is a Person with child (a Person Jane).
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Rule R1: if x is a Person and x has child y then x is 23.0.
			 
			 Test: Sue is 23 .
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(x, rdf:type, ipt:Person) and rdf(x, ipt:child, y) then rdf(x, ipt:age, 23.0)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
}
