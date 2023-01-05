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
import com.ge.research.sadl.reasoner.ResultSet
import com.ge.research.sadl.reasoner.SadlCommandResult
import com.ge.research.sadl.ui.tests.AbstractSadlPlatformTest
import java.util.List
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

/**
 * Test that demonstrate how to make assertions on the generated translator outputs, plus runs the inferencer too.
 * 
 * @author akos.kitta
 */
class UnittedQuantityTest extends AbstractSadlPlatformTest {

	@Test
	def void testUnittedQuantityInRule_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
			 
			 Shape is a class described by area with values of type UnittedQuantity.
			 
			 Rectangle is a class described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity.
			 	
			 MyRect is a Rectangle with width 4.0 ft, with height 2.2 ft.
			 	
			 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
			 
			 Ask: select s, ar where s is a Rectangle and s has area ar.
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"s\",\"ar\"
\"http://sadl.org/ImpliedPropertiesInRule.sadl#MyRect\",8.8 \"ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInRule_01b() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
			 
			 Shape is a class described by area with values of type UnittedQuantity.
			 
			 Rectangle is a class described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity.
			 	
			 MyRect is a Rectangle with width 4.0 ft, with height -2.2 ft.
			 	
			 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
			 
			 Ask: select s, ar where s is a Rectangle and s has area ar.
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"s\",\"ar\"
\"http://sadl.org/ImpliedPropertiesInRule.sadl#MyRect\",-8.8 \"ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	/*
	 * This test uses the internal UnittedQuantity handling of the product built-in to arrive a the correct answer
	 */
	@Test
	def void testUnittedQuantityInRule_01c() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
			 
			 Shape is a class described by area with values of type UnittedQuantity.
			 
			 Rectangle is a class described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity.
			 	
			 MyRect is a Rectangle with width 4.0 ft, with height 2.2 ft.
			 	
			 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
			 
			 Ask: select s, ar where s is a Rectangle and s has area ar.
			 Test: (select ar where MyRect has area ar) = 8.8 "ft*ft".
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				if (idx == 0) {
					assertTrue(tr instanceof ResultSet)
					assertEquals("\"s\",\"ar\"
\"http://sadl.org/ImpliedPropertiesInRule.sadl#MyRect\",8.8 \"ft*ft\"", (tr as ResultSet).toString.trim)
				}
				else if (idx == 1) {
					assertTrue(tr instanceof TestResult)
					assertTrue((tr as TestResult).passed)
				}
				idx++
			}
		];
	}
		
	@Test
	def void testUQInRule_product_grammar_2inputs() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/NamedUQinQuery.sadl" alias nameduqinquery.
				 
				 Shape is a class described by area with values of type UnittedQuantity.
				 
				 Rectangle is a type of Shape described by height with values of type UnittedQuantity, described by width with values of type UnittedQuantity.
				 
				 Rule AreaOfRect: if x is a Rectangle then area of x is height of x * width of x.
				 
				 StandardFootballField is a Rectangle with height (a UnittedQuantity StandardLength with ^value 120, with unit "yds"), with width 53.33 yds.
				 
				 Ask: select x, h, w, ar where x is a Rectangle and x has height h and x has width w and x has area ar.
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				(tr as ResultSet).setShowNamespaces(false)
				assertEquals("\"x\",\"h\",\"w\",\"ar\"
\"StandardFootballField\",StandardLength (120 \"yds\"),53.33 \"yds\",6399.599999999999 \"yds*yds\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Test
	def void testUQInRule_product_grammar_2inputsNoExpand() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/NamedUQinQuery.sadl" alias nameduqinquery.
				 
				 Shape is a class described by area with values of type UnittedQuantity.
				 
				 Rectangle is a type of Shape described by height with values of type UnittedQuantity, described by width with values of type UnittedQuantity.
				 
				 Rule AreaOfRect: if x is a Rectangle then area of x is height of x * width of x.
				 
				 StandardFootballField is a Rectangle with height (a UnittedQuantity StandardLength with ^value 120, with unit "yds"), with width 53.33 yds.
				 
				 Ask: select x, h, w, ar where x is a Rectangle and x has height h and x has width w and x has area ar.
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				(tr as ResultSet).setShowNamespaces(false)
				assertEquals("\"x\",\"h\",\"w\",\"ar\"
\"StandardFootballField\",StandardLength (120 \"yds\"),53.33 \"yds\",6399.599999999999 \"yds*yds\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Test
	def void testUQInRule_product_grammar3inputs() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule2.sadl" alias unittedquantityinrule2.
			 
			 Container is a class described by volume with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity,
			 	described by depth with values of type UnittedQuantity.
			 	
			 MyRP is a RectangleParallelopided with width 4.0 ft, with height 2.2 ft, with depth 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided then volume of x is height of x * width of x * depth of x.
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has volume v.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule2.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Test
	def void testUQInRule_product_grammar3inputsNoExpansion() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule2.sadl" alias unittedquantityinrule2.
			 
			 Container is a class described by volume with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity,
			 	described by depth with values of type UnittedQuantity.
			 	
			 MyRP is a RectangleParallelopided with width 4.0 ft, with height 2.2 ft, with depth 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided then volume of x is height of x * width of x * depth of x.
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has volume v.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule2.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Test
	def void testUQInRule_product_graph_pattern_explicit_product () {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule3.sadl" alias unittedquantityinrule3.
			 
			 Container is a class described by volume with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by dimension with values of type UnittedQuantity.
			 dimension of RectangleParallelopided has exactly 3 values.
			 	
			 MyRP is a RectangleParallelopided with dimension 4.0 ft, with dimension 2.2 ft, with dimension 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided then volume of x is product(x, dimension).
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has volume v.
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
			assertTrue(issues.filter[severity === Severity.ERROR].size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule3.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	/*
	 * This test uses a list of UnittedQuantities and so depends on the built-in product to handle processing of the 
	 * UnittedQuantity arguments.
	 */
	@Test
	def void testUQInRule_product_of_list() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule4.sadl" alias unittedquantityinrule4.
			 
			 Container is a class described by volume with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by dimension with values of type UnittedQuantity.
			 dimension of RectangleParallelopided has exactly 3 values.
			 	
			 MyRP is a RectangleParallelopided with dimension 4.0 ft, with dimension 2.2 ft, with dimension 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided and dimlst = list(x, dimension) then volume of x is product(dimlst).
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has volume v.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule4.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUQInRule_product_graph_pattern () {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule3.sadl" alias unittedquantityinrule3.
			 
			 Container is a class described by sumOfDimensions with values of type UnittedQuantity,
			 	described by volume with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by dimension with values of type UnittedQuantity.
			 dimension of RectangleParallelopided has exactly 3 values.
			 	
			 MyRP is a RectangleParallelopided with dimension 4.0 ft, with dimension 2.2 ft, with dimension 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided then volume of x is product(x, dimension).
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has volume v.
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
			assertTrue(issues.filter[severity === Severity.ERROR].size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule3.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUQInRule_product_uq_and_notuq() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/testUQTimesNumber.sadl" alias testuqtimesnumber.
			 
			 Shape is a class described by perimeter with values of type UnittedQuantity.
			 
			 Rectangle is a type of Shape described by height with values of type UnittedQuantity, described by width with values of type UnittedQuantity.
			 
			 Rule PerimeterOfRect: if x is a Rectangle then perimeter of x is (height of x + width of x) * 2. 
			 
			 MyRect is a Rectangle with height 2.5 ft, with width 10.0 ft.
			 
			 Ask: select x, y where x has perimeter y.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"x\",\"y\"
\"http://sadl.org/testUQTimesNumber.sadl#MyRect\",25.0 \"ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
		
	@Test
	def void testUQInRule_product_uq_and_notuqNotExpand() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/testUQTimesNumber.sadl" alias testuqtimesnumber.
			 
			 Shape is a class described by perimeter with values of type UnittedQuantity.
			 
			 Rectangle is a type of Shape described by height with values of type UnittedQuantity, described by width with values of type UnittedQuantity.
			 
			 Rule PerimeterOfRect: if x is a Rectangle then perimeter of x is (height of x + width of x) * 2. 
			 
			 MyRect is a Rectangle with height 2.5 ft, with width 10.0 ft.
			 
			 Ask: select x, y where x has perimeter y.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"x\",\"y\"
\"http://sadl.org/testUQTimesNumber.sadl#MyRect\",25.0 \"ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
		
	@Test
	def void testUQInRule_sum_graph_pattern () {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule3.sadl" alias unittedquantityinrule3.
			 
			 Container is a class described by sumOfDimensions with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by dimension with values of type UnittedQuantity.
			 dimension of RectangleParallelopided has exactly 3 values.
			 	
			 MyRP is a RectangleParallelopided with dimension 4.0 ft, with dimension 2.2 ft, with dimension 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided then sumOfDimensions of x is sum(x, dimension).
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has sumOfDimensions v.
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
			assertTrue(issues.filter[severity === Severity.ERROR].size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule3.sadl#MyRP\",7.300000000000001 \"ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUQInRule_sum_of_list() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule3.sadl" alias unittedquantityinrule3.
			 
			 Container is a class described by sumOfDimensions with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by dimension with values of type UnittedQuantity.
			 dimension of RectangleParallelopided has exactly 3 values.
			 	
			 MyRP is a RectangleParallelopided with dimension 4.0 ft, with dimension 2.2 ft, with dimension 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided and dimlst = list(x, dimension) then sumOfDimensions of x is sum(dimlst).
			 
			 Ask: select c, v where c is a RectangleParallelopided and c has sumOfDimensions v.
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
			assertTrue(issues.filter[severity === Severity.ERROR].size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule3.sadl#MyRP\",7.300000000000001 \"ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUQInRule_ge_simple() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 External unittedQuantity(decimal val, string unit) returns UnittedQuantity : "http://sadl.org#unittedQuantity".
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
«««			 Rule AdultRule: if p is a Person and p has age >= 18 years then p is an Adult.
			 Rule AdultRule2: if p is a Person and p has age uq and unit of uq is "years" and ^value of uq >= 18 then p is an Adult.
			 Expr: age >= 18 years.
			 Expr: p has age >= 18 years.
			 Test: Jane is an Adult.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUQInRule_ge_uq_literal() {
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 External unittedQuantity(decimal val, string unit) returns UnittedQuantity : "http://sadl.org#unittedQuantity".
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Rule AdultRule: if p is a Person and p has age >= 18 years then p is an Adult.
«««			 Rule AdultRule2: if p is a Person and p has age uq and unit of uq is "years" and ^value of uq >= 18 years then p is an Adult.
			 Expr: age >= 18 years.
			 Test: Jane is an Adult.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUQInRule_ge_ignoreuq() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString))
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 External unittedQuantity(decimal val, string unit) returns UnittedQuantity : "http://sadl.org#unittedQuantity".
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Rule AdultRule: if p is a Person and p has age >= 18 years then p is an Adult.
«««			 Rule AdultRule2: if p is a Person and p has age uq and unit of uq is "years" and ^value of uq >= 18 years then p is an Adult.
			 Expr: age >= 18 years.
			 Test: Jane is an Adult.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUQInRule_product_uq_and_notuq_notexpanded() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/testUQTimesNumber.sadl" alias testuqtimesnumber.
			 
			 Shape is a class described by perimeter with values of type UnittedQuantity.
			 
			 Rectangle is a type of Shape described by height with values of type UnittedQuantity, described by width with values of type UnittedQuantity.
			 
			 Rule PerimeterOfRect: if x is a Rectangle then perimeter of x is (height of x + width of x) * 2. 
			 
			 MyRect is a Rectangle with height 2.5 ft, with width 10.0 ft.
			 
			 Ask: select x, y where x has perimeter y.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"x\",\"y\"
\"http://sadl.org/testUQTimesNumber.sadl#MyRect\",25.0 \"ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Test
	def void testUQInRule_min_uq_expanded() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/GE2.sadl" alias ge2.
			 
			 Rectangle is a class described by height with values of type UnittedQuantity, 
			 	described by width with values of type UnittedQuantity,
			 	described by minDimension with values of type UnittedQuantity.
			 
			 Rule minDim: if x is a Rectangle then minDimension of x is min(height of x, width of x).
			 
			 TestRect is a Rectangle with height 5 inch, with width 4 inch.
			 
			 Test: minDimension of TestRect is 4 inch.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}

	@Test
	def void testUnittedQuantityInRule_Drew1() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl" alias mileslighttravelsperunitoftime.
				 
				 Trip is a class, described by timeTraveled with values of type UnittedQuantity,
				 	described by distanceTraveled with values of type UnittedQuantity,
				 	described by speed with values of type UnittedQuantity.
				 	
				 Rule DistanceTraveled: if tr is a Trip and tr has speed s and tr has timeTraveled t then distanceTraveled of tr is s * t. 
				 Trip1 is a Trip, with speed 299792458.0 "m/s", with timeTraveled 10000000000000.0 hrs.
				 Trip2 is a Trip, with speed 2.997924580E8 "m/s", with timeTraveled 1.0E13 hrs.
				 	
				 Ask: select tr, dt where tr is a Trip and tr has distanceTraveled dt.
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]
		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"tr\",\"dt\"
\"http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl#Trip2\",2.99792458E21 \"m/s*hrs\"
\"http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl#Trip1\",2.99792458E21 \"m/s*hrs\"
", (tr as ResultSet).toString)
			}
		];
	}

	@Test
	def void testUnittedQuantityInRule_Drew2() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl" alias mileslighttravelsperunitoftime.
				 
				 Trip is a class, described by timeTraveled with values of type UnittedQuantity,
				 	described by distanceTraveled with values of type UnittedQuantity,
				 	described by speed with values of type UnittedQuantity.
				 	
				 Rule DistanceTraveled3: 
				 	if tr is a Trip and 
				 	tr has speed s and 
				 	s has ^value sv and
				 	s has unit su and 
				 	su is "miles per second" and
				 	tr has timeTraveled t and
				 	t has ^value tv and
				 	t has unit tu and
				 	tu is "hrs" and
				 	dv = sv * tv * 60.0 * 60.0 and
				 	du = combineUnits("*", su, "sec")
				 	then thereExists(UnittedQuantity,^value, dv, unit, du, Plus, tr, distanceTraveled). 	
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
			assertEquals("Rule DistanceTraveled3:  if rdf(tr, rdf:type, mileslighttravelsperunitoftime:Trip) and rdf(tr, mileslighttravelsperunitoftime:speed, s) and rdf(s, sadlimplicitmodel:value, sv) and rdf(s, sadlimplicitmodel:unit, su) and is(su,\"miles per second\") and rdf(tr, mileslighttravelsperunitoftime:timeTraveled, t) and rdf(t, sadlimplicitmodel:value, tv) and rdf(t, sadlimplicitmodel:unit, tu) and is(tu,\"hrs\") and *(sv,tv,v0) and *(v0,60.0,v1) and *(v1,60.0,dv) and combineUnits(\"*\",su,\"sec\",du) then thereExists(sadlimplicitmodel:UnittedQuantity,sadlimplicitmodel:value,dv,sadlimplicitmodel:unit,du,sadlimplicitmodel:Plus,tr,mileslighttravelsperunitoftime:distanceTraveled).",
				rules.get(0).toString())
		]
	}

	@Test
	def void testUnittedQuantityInRule_Drew3() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl" alias mileslighttravelsperunitoftime.
			 
			 Trip is a class, described by timeTraveled with values of type UnittedQuantity,
			 	described by distanceTraveled with values of type UnittedQuantity,
			 	described by speed with values of type UnittedQuantity.
			 	
			// Rule DistanceTraveled: if tr is a Trip and tr has speed s and tr has timeTraveled t then distanceTraveled of tr is s * t. 
			// Rule DistanceTraveled2: if tr is a Trip and tr has speed s and tr has timeTraveled t then distanceTraveled of tr is product(s,t). 	
			 Rule DistanceTraveled3: 
			 	if tr is a Trip and 
			 	tr has speed s and 
			 	s has ^value sv and
			 	s has unit su and 
			 	su is "m/s" and
			 	tr has timeTraveled t and
			 	t has ^value tv and
			 	t has unit tu and
			 	tu is "hrs" and
			 	dv = sv * tv * 60 * 60 and
			 	du = combineUnits("*", su, "sec")
			 	then thereExists(UnittedQuantity,^value, dv, unit, du, Plus, tr, distanceTraveled). 	
			 	
			 Trip1 is a Trip, with speed 299792458.0 "m/s", with timeTraveled 10000000000000.0 hrs.
			 Trip2 is a Trip, with speed 2.997924580E8 "m/s", with timeTraveled 1.0E13 hrs.
			 	
			 Ask: select tr, dt where tr is a Trip and tr has distanceTraveled dt.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"tr\",\"dt\"
\"http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl#Trip2\",1.0792528488E25 \"m/s*sec\"
\"http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl#Trip1\",1.0792528488E25 \"m/s*sec\"
", (tr as ResultSet).toString)
			}
		];
	}

	@Test
	def void testUQInRule_min_uq_expanded_erroneous() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/GE2.sadl" alias ge2.
			 
			 Rectangle is a class described by height with values of type UnittedQuantity, 
			 	described by width with values of type UnittedQuantity,
			 	described by minDimension with values of type UnittedQuantity.
			 
			 Rule minDim: if x is a Rectangle then minDimension of x is min(height of x, 23).
			 
			 TestRect is a Rectangle with height 5 inch, with width 4 inch.
			 
			 Test: minDimension of TestRect is 4 inch.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]
	
	}

	@Test
	def void testUQInRule_min_uq_not_expanded() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/GE2.sadl" alias ge2.
			 
			 Rectangle is a class described by height with values of type UnittedQuantity, 
			 	described by width with values of type UnittedQuantity,
			 	described by minDimension with values of type UnittedQuantity.
			 
			 Rule minDim: if x is a Rectangle then minDimension of x is min(height of x, width of x).
			 
			 TestRect is a Rectangle with height 5 inch, with width 4 inch.
			 
			 Test: minDimension of TestRect is 4 inch.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}

	@Test
	def void testUnittedQuantityInQuery_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 

			 Ask: "select ?p where {?p <test:age> ?v0 . ?v0 <sadlimplicitmodel:unit> \"years\" . ?v0 <sadlimplicitmodel:value> ?v1 . FILTER(?v1 > 18)}".
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.class.canonicalName)
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"p\"
\"http://sadl.org/test.sadl#Jane\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInQuery_02() {
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.FALSE.toString))
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 "years".
			 
			 Ask: select p where p has age > 18 years.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			jenaModel.write(System.out)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.class.canonicalName)
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"p\"
\"http://sadl.org/test.sadl#Jane\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInQuery_03() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Ask: select p where p has age > 18 years.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			jenaModel.write(System.out)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.class.canonicalName)
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"p\"
\"http://sadl.org/test.sadl#Jane\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInQuery_04() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Ask: select p where 18 years < p has age.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			jenaModel.write(System.out)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.class.canonicalName)
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"p\"
\"http://sadl.org/test.sadl#Jane\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString))
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Test: age of Jane > 18 years.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_02() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Test: age of Jane > 18 years.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_03() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 External unittedQuantity(decimal val, string unit) returns UnittedQuantity : "http://sadl.org#unittedQuantity".
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 
			 Test: ^value of age of Jane > 18.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_04() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 External unittedQuantity(decimal val, string unit) returns UnittedQuantity : "http://sadl.org#unittedQuantity".
			 Person is a class described by age with values of type UnittedQuantity.
			 
			 Adult is a type of Person.
			  
			 Jane is a Person with age 23 years.
			 George is a Person with age 18 years.
			 
			 Test: age of Jane > age of George.
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_05() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestUQSubclass.sadl" alias testuqsubclass.
			 
			 Person is a class described by weight with values of type Weight,
			 	described by age with values of type TimeSpan.
			 
			 Weight is a type of UnittedQuantity.
			 TimeSpan is a type of UnittedQuantity.
			 
			 Adult is a type of Person.
			 
			 Rule AdultRule: if p is a Person and p has age >= 18 years then p is an Adult. 
			 
			 George is a Person with age 23 years, with weight 165 lbs.
			 
			 Jim is a Person with age 2 days, with weight 9.5 lbs.
			 
			 George is a Person with age (a TimeSpan with ^value 23, with unit "years"), 
			 	with weight (a Weight with ^value 165, with unit "lbs").
			 	
			 Jim is a Person with age (a TimeSpan with ^value 2, with unit "days"), 
			 	with weight (a Weight with ^value 9.5, with unit "lbs").
			 
			 Test: George is an Adult.
 	    ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_06() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 George is a Person with weight 156.9.
			 
			 Test: weight of George > 150 lbs.
 	    ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_07() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type UnittedQuantity, described by weight with values of type UnittedQuantity.

			 George is a Person with weight 156.9 lbs.
			 
			 Test: weight of George > 150 lbs.
 	    ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInTest_08() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type UnittedQuantity, described by weight with values of type UnittedQuantity.

			 George is a Person with weight 156.9 lbs.
			 
			 Test: George has weight w and w > 150 lbs.
 	    ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testExpandUnittedQuantityPreference_01() {

//		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.TRUE.toString));
//	this should be the default

		createFile('MissingInfo.sadl', '''
			 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias uqir.
			 
			 Shape is a class described by area with values of type UnittedQuantity.
			 
			 Rectangle is a class described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity.
			 				 	
			 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (rule : rules) {
				println(rule.toString)
			}
			assertEquals(0, issues.size)
			assertEquals("Rule R1:  if rdf(x, rdf:type, uqir:Rectangle) and rdf(x, uqir:height, v0) and rdf(v0, sadlimplicitmodel:value, v2) and rdf(v0, sadlimplicitmodel:unit, v4) and rdf(x, uqir:width, v1) and rdf(v1, sadlimplicitmodel:value, v3) and rdf(v1, sadlimplicitmodel:unit, v5) and *(v2,v3,v6) and combineUnits(\"*\",v4,v5,v7) then thereExists(UnittedQuantity,value,v6,unit,v7,Plus,x,uqir:area).", 
				rules.get(0).toString
			)
		]

	}
	
	@Test
	def void testExpandUnittedQuantityPreference_02() {

		updatePreferences(new PreferenceKey(SadlPreferences.EXPAND_UNITTEDQUANTITY_IN_TRANSLATION.id, Boolean.FALSE.toString));

		createFile('MissingInfo.sadl', '''
			 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias uqir.
			 
			 Shape is a class described by area with values of type UnittedQuantity.
			 
			 Rectangle is a class described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity.
			 				 	
			 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (rule : rules) {
				println(rule.toString)
			}
			assertEquals(0, issues.size)
			assertEquals("Rule R1:  if rdf(x, rdf:type, uqir:Rectangle) and rdf(x, uqir:height, v0) and rdf(x, uqir:width, v1) and *(v0,v1,v2) then rdf(x, uqir:area, v2).", 
				rules.get(0).toString
			)
		]

	}

	@Test
	def void testCombineUnits_03() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestCombineUnits.sadl" alias testcombineunits.
			 
			 Rectangle is a class, described by width with values of type float, 
			 	described by ^length with values of type float,
			 	described by area with values of type float,
			 	described by testStr with values of type string.
			 
			 Rule TestCombineUnits: if x is a Rectangle then testStr of x is combineUnits("*", "ft", combineUnits("*", "ft", "ft")). 		
			 
			 RectX is a Rectangle.
			 
			 Test: testStr of RectX is "ft*ft*ft".
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testCombineUnits_04() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestCombineUnits2.sadl" alias testcombineunits2.
			 
			 Rectangle is a class, described by width with values of type float, 
			 	described by ^length with values of type float,
			 	described by area with values of type UnittedQuantity.
			 
			  Rule AreaRule: 
			 	if x is a Rectangle
			 		and areaVar is width of x * ^length of x 
			 		and unitVar is combineUnits("*", "ft", "ft")
			 	then there exists a UnittedQuantity uq and uq has ^value areaVar and uq has unit unitVar plus x has area uq.
			 
			  RectX is a Rectangle with width 2.0, with ^length 3.0.
			 
«««			  Test: area of RectX is 6.0 "ft*ft".
			  Test: ^value of area of RectX is 6.0.
			  Test: unit of area of RectX is "ft*ft".
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testCombineUnits_05() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestCombineUnits2.sadl" alias testcombineunits2.
			 
			 Rectangle is a class, described by width with values of type float, 
			 	described by ^length with values of type float,
			 	described by area with values of type UnittedQuantity.
			 
			  Rule AreaRule: 
			 	if x is a Rectangle
			 		and areaVar is width of x * ^length of x 
			 		and unitVar is combineUnits("*", "ft", "ft")
			 	then there exists a UnittedQuantity uq and uq has ^value areaVar and uq has unit unitVar plus x has area uq.
			 
			  RectX is a Rectangle with width 2.0, with ^length 3.0.
			 
			  Test: area of RectX is 6.0 "ft*ft".
«««			  Test: ^value of area of RectX is 6.0.
«««			  Test: unit of area of RectX is "ft*ft".
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
	
	@Test
	def void testCombineUnits_06() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/TestCombineUnits3.sadl" alias testcombineunits3.
				 
				 Rectangle is a class, described by width with values of type UnittedQuantity, 
				 	described by ^length with values of type UnittedQuantity,
				 	described by area with values of type UnittedQuantity.
				 
				//  Rule AreaRule: 
				// 	if x is a Rectangle
				// 		and areaVar is width of x * ^length of x 
				// 		and unitVar is combineUnits("*", "ft", "ft")
				// 	then there exists a UnittedQuantity uq and uq has ^value areaVar and uq has unit unitVar plus x has area uq.
				 
				  Rule AreaRule: 
				 	if x is a Rectangle
				 		and wv is ^value of width of x
				 		and wu is unit of width of x
				 		and lv is ^value of ^length of x
				 		and lu is unit of ^length of x
				 		and av is wv * lv
				 		and au is combineUnits("*", wu, lu)
				 	then there exists a UnittedQuantity uq and uq has ^value av and uq has unit au plus x has area uq.
				
				  RectX is a Rectangle with width 2.0 ft, with ^length 3.0 ft.
				 
				  Test: area of RectX is 6.0 "ft*ft".
 			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
	
	@Test
	def void testUQInRule_Abs() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/absUQ.sadl" alias absuq.
			 
			 NumberAndAbsValue is a class, described by number with values of type UnittedQuantity, described by absValue with values of type UnittedQuantity.
			 
			 Rule R1: if x is a NumberAndAbsValue then absValue of x is abs(number of x).
			 
			 TestNum is a NumberAndAbsValue with number -23.5 dollars.
			 
			 Ask: select x where y is a NumberAndAbsValue and y has absValue x.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
			assertTrue(rules.size == 1)
			assertEquals("Rule R1:  if rdf(x, rdf:type, absuq:NumberAndAbsValue) and rdf(x, absuq:number, v0) and rdf(v0, sadlimplicitmodel:value, v1) and rdf(v0, sadlimplicitmodel:unit, v2) and abs(v1,v3) then thereExists(UnittedQuantity,value,v3,unit,v2,Plus,x,absuq:absValue).", rules.get(0).toString)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				var resultUQ = (tr as ResultSet).getResultAt(0,0)
				assertEquals("23.5 \"dollars\"", resultUQ.toString)
			}
		];
	}
		
	@Test
	def void testUQInRule_AbsEmbedded() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/absUqEmbedded.sadl" alias absuqembedded.
			 
			 NumberAndAbsValue is a class, described by number1 with values of type UnittedQuantity, 
			 	described by number2 with values of type UnittedQuantity,
			 	described by absValue with values of type UnittedQuantity.
			 
			 Rule R1: if x is a NumberAndAbsValue then absValue of x is abs(number1 of x + number2 of x).
			 
			 TestNum is a NumberAndAbsValue with number1 -23.5 dollars, with number2 10 dollars.
			 
			 Ask: select x where y is a NumberAndAbsValue and y has absValue x.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
			assertTrue(rules.size == 1)
			assertEquals("Rule R1:  if rdf(x, rdf:type, absuqembedded:NumberAndAbsValue) and rdf(x, absuqembedded:number1, v0) and rdf(v0, sadlimplicitmodel:value, v2) and rdf(v0, sadlimplicitmodel:unit, v4) and rdf(x, absuqembedded:number2, v1) and rdf(v1, sadlimplicitmodel:value, v3) and rdf(v1, sadlimplicitmodel:unit, v4) and +(v2,v3,v5) and abs(v5,v6) then thereExists(UnittedQuantity,value,v6,unit,v4,Plus,x,absuqembedded:absValue).", rules.get(0).toString)
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				var resultUQ = (tr as ResultSet).getResultAt(0,0)
				assertEquals("13.5 \"dollars\"", resultUQ.toString)
			}
		];
	}
		
}
