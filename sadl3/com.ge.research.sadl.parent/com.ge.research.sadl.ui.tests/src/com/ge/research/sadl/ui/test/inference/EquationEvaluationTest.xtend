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
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
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
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
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
		createFile('StringFunctions.sadl', '''
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
					if (issue.message.equals("Evaluates to: 15")) {
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
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer('StringFunctions.sadl', null, configItems) [
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
		createFile('StringFormat.sadl', '''
			 uri "http://sadl.org/StringFormat.sadl" alias stringformat.
			 
			 External formatString(string fmt, ...) returns string : "java.lang.String.format".
			 
			 Expr: formatString("name is %s", "sonoo").
			 Expr: formatString("value is %f",32.33434). 
			 Expr: formatString("value is %32.12f",32.33434).  
			 Expr: formatString("value is %32.12f != %f", 32.33434, 23.456).
			 Expr: formatString("value %32.12f is not null is %b", 32.33434, 23.456).
			 
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
	 					else if (issue.message.equals("Evaluates to: \"value is 32.334340\"") ||
	 						issue.message.equals("Evaluates to: \"value is 32.334339\"")	// this is what comes out of formatString for float 32.334340 input
	 					) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000\"") ||
	 						issue.message.equals("Evaluates to: \"value is                  32.334339141846\"")
	 					) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000 != 23.456000\"") ||
	 						issue.message.equals("Evaluates to: \"value is                  32.334339141846 != 23.455999\"")
	 					) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000 != 23.456000\"") ||
	 						issue.message.equals("Evaluates to: \"value                  32.334339141846 is not null is true\"")
	 					) {
	 						evaluationsFound++
	 					}
	 				}
	 			}
	 			assertEquals(5, evaluationsFound)
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
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM")
	 		configItems.add(ci)
	 		assertInferencer('StringFormat.sadl', null, configItems) [
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
		createFile('BooleanTypes.sadl', '''
			 uri "http://sadl.org/BooleanTypes.sadl" alias booleantypes.
			 
			 External booleanToString(boolean b) returns string : "java.lang.Boolean.toString".
			 External booleanToString2(boolean b) returns string : "java.lang.String.valueOf".
			 
			 Expr: booleanToString(false).
			 Expr: booleanToString2(true).
			  
			 ClassWithBooleanProps is a class, described by shouldBeTrue with values of type string,
			 	described by shouldBeFalse with values of type string.
			 	
			 MyCWBP is a ClassWithBooleanProps.
			 	 
			 Rule R1: if x is a ClassWithBooleanProps 
			 	then shouldBeFalse of x is booleanToString(false)
			 		and shouldBeTrue of x is booleanToString2(true).
			 
			 Ask: select x, p, v where x is a ClassWithBooleanProps and x has p v.
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
	 			assertNotNull(jenaModel)
	 			var evaluatesTrue = 0
	 			var evaluatesFalse = 0
	 			if (issues !== null) {
	 				for (issue : issues) {
	 					System.out.println(issue.message)
	 					if (issue.message.equals("Evaluates to: \"false\"")) {
	 						evaluatesFalse++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"true\"")) {
	 						evaluatesTrue++
	 					}
	 				}
	 			}
	 			assertEquals(1, evaluatesTrue)
	 			assertEquals(1, evaluatesFalse)
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
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM")
	 		configItems.add(ci)
	 		assertInferencer('BooleanTypes.sadl', null, configItems) [
	 			var idx = 0
	 			for (scr : it) {
	 				if (scr != null) {
		 				println(scr.toString)
		 				assertTrue(scr instanceof SadlCommandResult)
		 				val tr = (scr as SadlCommandResult).results
		 				assertTrue(tr instanceof ResultSet)
	//	 				assertEquals("\"x\",\"y\"
	//\"http://sadl.org/StringFormat.sadl#TestInst\",\"value is                  32.334339141846 != 23.455999\"", tr.toString.trim)
		 			}
		 			idx++
	 			}
	 		];		
	 }
	 
	@Test
	def void testSadlEquationInRule_05() {
		createFile('DoubleFunctions.sadl', '''
			 uri "http://sadl.org/DoubleFunctions.sadl" alias doublefunctions.
			 
			 External doubleToLongBits(double d) returns long : "java.lang.Double.doubleToLongBits".
			 External doubleCompare(double d1, double d2) returns int : "java.lang.Double.compareTo".
			 
			 Expr: doubleToLongBits(3.14159).
			 Expr: doubleCompare(3.14159, 3.141591).
			 
			 
			 ShowResults is a class described by dtlb with values of type long, described by dc with values of type int.
			 
			 Rule R1: if x is a ShowResults then dtlb of x is doubleToLongBits(3.14159).
			 Rule R2: if x is a ShowResults then dc of x is doubleCompare(3.14159, 3.141591).
			 
			 DoItNow is a ShowResults.
			 
			 Ask: select x, p, v where x is a ShowResults.
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
	 			assertNotNull(jenaModel)
	 			var evaluationsFound = 0
	 			if (issues !== null) {
	 				for (issue : issues) {
	 					System.out.println(issue.message)
	 					if (issue.message.equals("Evaluates to: 4614256650576692846")) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"-1\"")) {
	 						evaluationsFound++
	 					}
	 				}
	 			}
//	 			assertEquals(2, evaluationsFound)
	 			if (rules !== null) {
	 				for (rule : rules) {
	 					System.out.println(rule.toString)
	 				}
	 			}
	 			assertTrue(rules.size == 2)
//	 			assertTrue(
//	 				processor.compareTranslations(rules.get(0).toString(),
//	 					"Rule R1:  if rdf(x, rdf:type, stringformat:TestClass) and formatString(\"value is %32.12f != %f\",32.33434,23.456,v0) then rdf(x, stringformat:formatedString, v0)."))
	 		]
	 
	 		var List<ConfigurationItem> configItems = newArrayList
	 		val String[] catHier = newArrayOfSize(1)
	 		catHier.set(0, "Jena")
	 		val ci = new ConfigurationItem(catHier)
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM")
	 		configItems.add(ci)
	 		assertInferencer('DoubleFunctions.sadl', null, configItems) [
	 			var idx = 0
	 			for (scr : it) {
	 				if (scr != null) {
		 				println(scr.toString)
		 				assertTrue(scr instanceof SadlCommandResult)
		 				val tr = (scr as SadlCommandResult).results
		 				assertTrue(tr instanceof ResultSet)
	//	 				assertEquals("\"x\",\"y\"
	//\"http://sadl.org/StringFormat.sadl#TestInst\",\"value is                  32.334339141846 != 23.455999\"", tr.toString.trim)
		 			}
		 			idx++
	 			}
	 		];		
	 }
	 	
	@Test
	def void testSadlEquationInRule_06() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		createFile('BooleanTypes.sadl', '''
			 uri "http://sadl.org/BooleanTypes.sadl" alias booleantypes.
			 
			 External booleanToString(boolean b) returns string : "java.lang.Boolean.toString".
			 External booleanToString2(boolean b) returns string : "java.lang.String.valueOf".
			 
			 Expr: booleanToString(false).
			 Expr: booleanToString2(true).
			  
			 ClassWithBooleanProps is a class, described by shouldBeTrue with values of type boolean,
			 	described by shouldBeFalse with values of type boolean.
			 	
			 MyCWBP is a ClassWithBooleanProps.
			 	 
			 Rule R1: if x is a ClassWithBooleanProps 
			 	then shouldBeFalse of x is booleanToString(false)
			 		and shouldBeTrue of x is booleanToString2(true).
			 
			 Ask: select x, p, v where x is a ClassWithBooleanProps and x has p v.
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
	 			assertNotNull(jenaModel)
	 			var evaluatesTrue = 0
	 			var evaluatesFalse = 0
	 			var typeError1 = 0
	 			var typeError2 = 0
	 			if (issues !== null) {
	 				for (issue : issues) {
	 					System.out.println(issue.message)
	 					if (issue.message.equals("Evaluates to: \"false\"")) {
	 						evaluatesFalse++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"true\"")) {
	 						evaluatesTrue++
	 					}
	 					else if (issue.message.equals("booleantypes:shouldBeFalse, a datatype property with range  xsd:boolean, cannot be compared (is) with function booleantypes:booleanToString returning xsd:string.")) {
	 						typeError1++
	 					}
	 					else if (issue.message.equals("booleantypes:shouldBeTrue, a datatype property with range  xsd:boolean, cannot be compared (is) with function booleantypes:booleanToString2 returning xsd:string.")) {
	 						typeError2++
	 					}
	 				}
	 			}
	 			assertEquals(1, evaluatesTrue)
	 			assertEquals(1, evaluatesFalse)
	 			assertEquals(1, typeError1)
	 			assertEquals(1, typeError2)
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
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM")
	 		configItems.add(ci)
	 		assertInferencer('BooleanTypes.sadl', null, configItems) [
	 			var idx = 0
	 			for (scr : it) {
	 				if (scr != null) {
		 				println(scr.toString)
		 				assertTrue(scr instanceof SadlCommandResult)
		 				val tr = (scr as SadlCommandResult).results
		 				assertTrue(tr instanceof ResultSet)
	//	 				assertEquals("\"x\",\"y\"
	//\"http://sadl.org/StringFormat.sadl#TestInst\",\"value is                  32.334339141846 != 23.455999\"", tr.toString.trim)
		 			}
		 			idx++
	 			}
	 		];		
	 }
	 
	@Test // this test checks a static Java method with no arguments
	def void testSadlEquationInRule_07() {
		createFile('MathRandom.sadl', '''
			 uri "http://sadl.org/MathRandom.sadl" alias mathrandom.
			 
			 External random() returns float  : "java.lang.Math.random".
			 
			 ClassWithRandomValue is a class described by randomValue with values of type float.
			 
			 Rule R1: if x is a ClassWithRandomValue then randomValue of x is random().
			 
			 CWRV is a ClassWithRandomValue.
			 
			 Expr: random().
			 
			 Ask: select x, y where x is a ClassWithRandomValue and x has randomValue y.
			''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			var evaluationsFound = 0
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
					"Rule R1:  if rdf(x, rdf:type, mathrandom:ClassWithRandomValue) and random(v0) then rdf(x, mathrandom:randomValue, v0)."))
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer('MathRandom.sadl', null, configItems) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof ResultSet)
				assertTrue(tr.toString.trim.startsWith("\"x\",\"y\"
\"http://sadl.org/MathRandom.sadl#CWRV\",0."))
				idx++
			}
		];
	}
	
	@Test
	def void testSadlEquationInRule_08() {
		createFile('StringFormat.sadl', '''
			 uri "http://sadl.org/StringFormat.sadl" alias stringformat.
			 
			 External formatString(string fmt, ...) returns string : "java.lang.String.format".
			 
			 Expr: formatString("no arguments for VarArgs").
			 
			 TestClass is a class described by formatedString with values of type string.

			 Rule R1: if x is a TestClass then formatedString of x is formatString("no arguments for VarArgs").
			 TestInst is a TestClass.

			 Ask: select x, y where x is a TestClass and y is formatedString of x.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
	 			assertNotNull(jenaModel)
	 			var evaluationsFound = 0
	 			if (issues !== null) {
	 				for (issue : issues) {
	 					System.out.println(issue.message)
	 					if (issue.message.equals("Evaluates to: \"no arguments for VarArgs\"")) {
	 						evaluationsFound++
	 					}
	 				}
	 			}
	 			assertEquals(1, evaluationsFound)
	 			if (rules !== null) {
	 				for (rule : rules) {
	 					System.out.println(rule.toString)
	 				}
	 			}
	 			assertTrue(rules.size == 1)
	 			assertTrue(
	 				processor.compareTranslations(rules.get(0).toString(),
	 					"Rule R1:  if rdf(x, rdf:type, stringformat:TestClass) and formatString(\"no arguments for VarArgs\",v0) then rdf(x, stringformat:formatedString, v0)."))
	 		]
	 
	 		var List<ConfigurationItem> configItems = newArrayList
	 		val String[] catHier = newArrayOfSize(1)
	 		catHier.set(0, "Jena")
	 		val ci = new ConfigurationItem(catHier)
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM")
	 		configItems.add(ci)
	 		assertInferencer('StringFormat.sadl', null, configItems) [
	 			var idx = 0
	 			for (scr : it) {
	 				println(scr.toString)
	 				assertTrue(scr instanceof SadlCommandResult)
	 				val tr = (scr as SadlCommandResult).results
	 				assertTrue(tr instanceof ResultSet)
	 				assertEquals("\"x\",\"y\"
\"http://sadl.org/StringFormat.sadl#TestInst\",\"no arguments for VarArgs\"", tr.toString.trim)
	 				idx++
	 			}
	 		];
	}
	
	@Test
	def void testSadlEquationInRule_09() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		createFile('StringFormat.sadl', '''
			 uri "http://sadl.org/StringFormat.sadl" alias stringformat.
			 
			 External formatString(--) returns -- : "java.lang.String.format".
			 
			 Expr: formatString("name is %s", "sonoo").
			 Expr: formatString("value is %f",32.33434). 
			 Expr: formatString("value is %32.12f",32.33434).  
			 Expr: formatString("value is %32.12f != %f", 32.33434, 23.456).
			 Expr: formatString("value %32.12f is not null is %b", 32.33434, 23.456).
			 
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
	 					else if (issue.message.equals("Evaluates to: \"value is 32.334340\"") ||
	 						issue.message.equals("Evaluates to: \"value is 32.334339\"")	// this is what comes out of formatString for float 32.334340 input
	 					) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000\"") ||
	 						issue.message.equals("Evaluates to: \"value is                  32.334339141846\"")
	 					) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000 != 23.456000\"") ||
	 						issue.message.equals("Evaluates to: \"value is                  32.334339141846 != 23.455999\"")
	 					) {
	 						evaluationsFound++
	 					}
	 					else if (issue.message.equals("Evaluates to: \"value is                  32.334340000000 != 23.456000\"") ||
	 						issue.message.equals("Evaluates to: \"value                  32.334339141846 is not null is true\"")
	 					) {
	 						evaluationsFound++
	 					}
	 				}
	 			}
	 			assertEquals(5, evaluationsFound)
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
	 		ci.addNameValuePair("pModelSpec", "OWL_MEM")
	 		configItems.add(ci)
	 		assertInferencer('StringFormat.sadl', null, configItems) [
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
	
}
