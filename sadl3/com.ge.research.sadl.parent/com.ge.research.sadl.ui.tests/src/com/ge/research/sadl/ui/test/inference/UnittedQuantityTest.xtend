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
import org.eclipse.xtext.diagnostics.Severity
import org.junit.Ignore

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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
		
	@Ignore
	@Test
	def void testUnittedQuantityInRule_02() {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule2.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Test
	def void testUnittedQuantityInRule_03() {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
	def void testUnittedQuantityInRule_04() {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule4.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	/*
	 * This test uses explicit 
	 */
	@Test
	def void testUnittedQuantityInRule_05() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/UnittedQuantityInRule5.sadl" alias unittedquantityinrule5.
			 
			 Container is a class described by volume with values of type UnittedQuantity.
			 
			 RectangleParallelopided is a type of Container 
			 	described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity,
			 	described by depth with values of type UnittedQuantity.
			 	
			 MyRP is a RectangleParallelopided with width 4.0 ft, with height 2.2 ft, with depth 1.1 ft.
			 	
			 Rule RP1: if x is a RectangleParallelopided then volume of x is product(height of x, width of x, depth of x).
			 
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"c\",\"v\"
\"http://sadl.org/UnittedQuantityInRule5.sadl#MyRP\",9.680000000000001 \"ft*ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInRule_06() {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInRule_07() {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testUnittedQuantityInRule_08() {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.class.canonicalName)
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"p\"
\"http://sadl.org/test.sadl#Jane\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
//	@Ignore		
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
//	@Ignore	
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
//	@Ignore	
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
//	@Ignore	
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
			assertEquals("Rule R1:  if rdf(x, rdf:type, uqir:Rectangle) and rdf(x, uqir:height, v0) and rdf(v0, sadlimplicitmodel:value, v2) and rdf(v0, sadlimplicitmodel:unit, v4) and rdf(x, uqir:width, v1) and rdf(v1, sadlimplicitmodel:value, v3) and rdf(v1, sadlimplicitmodel:unit, v5) and *(v2,v3,v6) and combineUnits(\"*\",v4,v5,v8) then thereExists(UnittedQuantity,value,v6,unit,v8,Plus,x,uqir:area).", 
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
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
				if (errs != null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr != null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
	
}
