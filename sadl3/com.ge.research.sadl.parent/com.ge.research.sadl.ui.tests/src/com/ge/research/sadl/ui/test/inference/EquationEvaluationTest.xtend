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

/**
 * Test that demonstrate how to make assertions on the generated translator outputs, plus runs the inferencer too.
 * 
 * @author akos.kitta
 */
class EquationEvaluationTest extends AbstractSadlPlatformTest {

	@Test
	def void testSadlEquationInRule_01() {
		createFile('UseNumericConstants.sadl', '''
		uri "http://sadl.org/JavaExternal.sadl" alias javaexternal.
		
		External min(decimal n1, decimal n2) returns decimal : "java.lang.Math.min".
		
		Expr: min(2,3).
		
		Rule testRule: then print(min(2,3)).
		
		Ask: select x where x is an ExternalEquation.''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			var foundExpectedEvaluation = false
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
					if (issue.message.equals("Evaluates to: 2")) {
						foundExpectedEvaluation = true
					}
				}
			}
			assertTrue(foundExpectedEvaluation)
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule testRule:  if min(2,3,v0) then print(v0)."))
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_MINI_RULE")
		configItems.add(ci)
		assertInferencer('UseNumericConstants.sadl', null, configItems) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof ResultSet)
				while((tr as ResultSet).hasNext) {
					val row = (tr as ResultSet).next
					if(row.toString.trim.equals("[http://sadl.org/JavaExternal.sadl#min]")) {
						idx++
					}
				}
			}
			assertTrue(idx > 0)
		];
	}
		
	@Test
	def void testSadlEquationInRule_02() {
		createFile('UseNumericConstants.sadl', '''
		uri "http://sadl.org/StringFunctions.sadl" alias stringfunctions.
		
		External indexOf(string str, string match) returns int : "java.lang.String.indexOf".
		
		External substring(string str, int start) returns string : "java.lang.String.substring".
		
		Expr: indexOf("http://test.org#localname", "#").
		Expr: substring("http://test.org#localname", 16).
		
		URI is a class described by ^uri with values of type string, described by ln with values of type string.
		
		TestUri is a URI with ^uri "http://test.org#localname".
		
		Rule R1: if x is a URI and y is ^uri of x then ln of x is substring(y, indexOf(y, "#") + 1).
		
		Ask: select x, y where x is a URI and y is ln of x.''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			var evaluationsFound = 0
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
					if (issue.message.equals("Evaluates to: \"15\"")) {
						evaluationsFound++
					}
					else if (issue.message.equals("Evaluates to: \"localname\"")) {
						evaluationsFound++
					}
				}
			}
			assertEquals(2, evaluationsFound)
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(x, rdf:type, stringfunctions:URI) and rdf(x, stringfunctions:uri, y) and indexOf(y,\"#\",v0) and +(v0,1,v1) and substring(y,v1,v2) then rdf(x, stringfunctions:ln, v2)."))
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_MINI_RULE")
		configItems.add(ci)
		assertInferencer('UseNumericConstants.sadl', null, configItems) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"x\",\"y\"
\"http://sadl.org/StringFunctions.sadl#TestUri\",\"localname\"", tr.toString.trim)
				idx++
			}
		];
	}
	
	@Test
	def void testSadlEquationInRule_03() {
		createFile('UseNumericConstants.sadl', '''
			 uri "http://sadl.org/StringFormat.sadl" alias stringformat.
			 
			 External formatString(string fmt, ...) returns string : "java.lang.String.format".
			 
			 Expr: formatString("name is %s", "sonoo").
			 Expr: formatString("value is %f",32.33434). 
			 Expr: formatString("value is %32.12f",32.33434).  
			 Expr: formatString("value is %32.12f != %f", 32.33434, 23.456).
			 TestClass is a class described by formatedString with values of type string.

			 Rule R1: if x is a TestClass then formatedString of x is formatString("value is %32.12f != %f", 32.33434, 23.456).
			 TestInst is a TestClass.

			 Ask: select x, y where x is a TestClass and y is formatedString of x.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
	 			assertNotNull(jenaModel)
	 			var evaluationsFound = 0
	 			if (issues !== null) {
	 				for (issue : issues) {
	 					System.out.println(issue.message)
	 					if (issue.message.equals("Evaluates to: \"name is sonoo\"")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is 32.334340\"")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000\"")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000 != 23.456000\"")) {
	 						evaluationsFound++
	 					}
	 				}
	 			}
	 			assertEquals(4, evaluationsFound)
	 			if (rules !== null) {
	 				for (rule : rules) {
	 					System.out.println(rule.toString)
	 				}
	 			}
	 			assertTrue(rules.size == 1)
	 			assertTrue(
	 				processor.compareTranslations(rules.get(0).toString(),
	 					"Rule R1:  if rdf(x, rdf:type, stringformat:TestClass) and formatString(\"value is %32.12f != %f\",32.33434,23.456,v0) then rdf(x, stringformat:formatedString, v0)."))
	 		]
	 
	 		var List<ConfigurationItem> configItems = newArrayList
	 		val String[] catHier = newArrayOfSize(1)
	 		catHier.set(0, "Jena")
	 		val ci = new ConfigurationItem(catHier)
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM_MINI_RULE")
	 		configItems.add(ci)
	 		assertInferencer('UseNumericConstants.sadl', null, configItems) [
	 			var idx = 0
	 			for (scr : it) {
	 				println(scr.toString)
	 				assertTrue(scr instanceof SadlCommandResult)
	 				val tr = (scr as SadlCommandResult).results
	 				assertTrue(tr instanceof ResultSet)
	 				assertEquals("\"x\",\"y\"
\"http://sadl.org/StringFormat.sadl#TestInst\",\"value is                  32.334339141846 != 23.455999\"", tr.toString.trim)
	 				idx++
	 			}
	 		];
	}
	
	@Test
	def void testSadlEquationInRule_04() {
		createFile('UseNumericConstants.sadl', '''
			 uri "http://sadl.org/OtherTypes.sadl" alias othertypes.
			 
			 External booleanToString(boolean b) returns string : "java.lang.Boolean.toString".
			 External booleanToString2(boolean b) returns string : "java.lang.String.valueOf".
			 
			 Expr: booleanToString(false).
			 Expr: booleanToString2(true).
			  
			 Rule R1: then print("Rule output: ", booleanToString(false), ", ", booleanToString2(true)).
			 
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
	 			assertNotNull(jenaModel)
	 			var evaluationsFound = 0
	 			if (issues !== null) {
	 				for (issue : issues) {
	 					System.out.println(issue.message)
	 					if (issue.message.equals("Evaluates to: \"name is sonoo\"")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is 32.334340\"")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000\"")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000 != 23.456000\"")) {
	 						evaluationsFound++
	 					}
	 				}
	 			}
//	 			assertEquals(4, evaluationsFound)
	 			if (rules !== null) {
	 				for (rule : rules) {
	 					System.out.println(rule.toString)
	 				}
	 			}
	 			assertTrue(rules.size == 1)
//	 			assertTrue(
//	 				processor.compareTranslations(rules.get(0).toString(),
//	 					"Rule R1:  if rdf(x, rdf:type, stringformat:TestClass) and formatString(\"value is %32.12f != %f\",32.33434,23.456,v0) then rdf(x, stringformat:formatedString, v0)."))
	 		]
	 
	 		var List<ConfigurationItem> configItems = newArrayList
	 		val String[] catHier = newArrayOfSize(1)
	 		catHier.set(0, "Jena")
	 		val ci = new ConfigurationItem(catHier)
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM_MINI_RULE")
	 		configItems.add(ci)
	 		assertInferencer('UseNumericConstants.sadl', null, configItems) [
	 			var idx = 0
	 			for (scr : it) {
	 				println(scr.toString)
	 				assertTrue(scr instanceof SadlCommandResult)
	 				val tr = (scr as SadlCommandResult).results
	 				assertTrue(tr instanceof ResultSet)
//	 				assertEquals("\"x\",\"y\"
//\"http://sadl.org/StringFormat.sadl#TestInst\",\"value is                  32.334339141846 != 23.455999\"", tr.toString.trim)
	 				idx++
	 			}
	 		];		
	 }
}
