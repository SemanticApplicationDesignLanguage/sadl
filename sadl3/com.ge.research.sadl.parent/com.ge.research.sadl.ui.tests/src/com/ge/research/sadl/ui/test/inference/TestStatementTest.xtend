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
class TestStatementTest extends AbstractSadlPlatformTest {

	@Test
	def void testTestStatement_00() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: George weight 156.9.
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
	def void testTestStatement_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: 23 < 156.9.
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
	def void testTestStatement_02() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: weight of George <= 156.9.
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
	def void testTestStatement_03() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: weight of George > age of Sue.
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
	def void testTestStatement_04() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: "select ?w where {<George> <weight> ?w}" > "select ?a where {<Sue> <age> ?a}".
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
	def void testTestStatement_05() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: age of George.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 1)
		]
	}		
		
	@Test 
	def void testTestStatement_06() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: age of Sue + weight of George.
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
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 1)
		]
	}		
		
	@Test 
	def void testTestStatement_07() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by age with values of type int, 
			 	described by weight with values of type float
			 	described by sober with values of type boolean.
			 
			 Sue is a Person with sober true.
			 
			 George is a Person with sober false.
			 
			 Test: sober of Sue is true.
			 Test: sober of George is true.
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
			val errors = issues.filter[severity === Severity.ERROR]
//			assertTrue(errors.size == 1)
		]
		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			var cntr = 0;
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
				if (cntr == 0) {
					assertTrue((tr as TestResult).passed)				
				}
				else {
					assertFalse((tr as TestResult).passed)
				}
				cntr++
			}
		];
	}		

	@Test 
	def void testTestStatement_08() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/test3.sadl" alias test3.
			 
			 Person is a class described by friend with values of type Person,
			 	described by sober with values of type boolean.
			 
			 Sue is a Person with sober true.
			 
			 George is a Person with friend Sue.
			 
			 Test: sober of friend of George is true.
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
			val errors = issues.filter[severity === Severity.ERROR]
//			assertTrue(errors.size == 1)
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
	def void testTestStatement_09() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestStatements.sadl" alias teststatements.
			  
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: (select w where George weight w) > (select ag where Sue age ag).
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
			val errors = issues.filter[severity === Severity.ERROR]
//			assertTrue(errors.size == 1)
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
	def void testTestStatement_10() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestStatements.sadl" alias teststatements.
			  
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: (select w where George weight w) > age of Sue.
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
			val errors = issues.filter[severity === Severity.ERROR]
//			assertTrue(errors.size == 1)
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
	def void testTestStatement_11() {
		// this test checks to make sure that the test is an expression that can be evaluated to true or false
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/TestStatements.sadl" alias teststatements.
			  
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: weight of George > (select ag where Sue age ag).
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
			val errors = issues.filter[severity === Severity.ERROR]
//			assertTrue(errors.size == 1)
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
