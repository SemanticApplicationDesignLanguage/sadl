/*
 * © 2014-2017 General Electric Company – All Rights Reserved
 * 
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.model.gp.GraphPatternElement
import com.ge.research.sadl.processing.SadlConstants
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.apache.jena.ontology.AllValuesFromRestriction
import org.apache.jena.ontology.HasValueRestriction
import org.apache.jena.ontology.OntClass
import org.apache.jena.ontology.Restriction
import org.apache.jena.rdf.model.Literal
import org.apache.jena.rdf.model.Resource
import org.apache.jena.vocabulary.OWL
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

import static extension com.ge.research.sadl.tests.SadlTestAssertions.*
import java.util.List
import org.junit.Ignore

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class ExtendedIFTest extends AbstractSADLModelProcessorTest {

	@Test
	def void testUnits_01() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: 2 seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue)
			}
			val errors = issues.filter[severity === Severity.ERROR]
			errors.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults()
			assertEquals(forTest.size, 1)
			assertEquals("2 \"seconds\"", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_02() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: 2 "seconds".
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			val errors = issues.filter[severity === Severity.ERROR]
			errors.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults()
			assertEquals(forTest.size, 1)
			assertEquals("2 \"seconds\"", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_03() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: (2 + 3) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue.toString)
			}
			assertTrue(issues.size > 0)
			var found = false
			for (issue : issues) {
				if (issue.toString.startsWith("INFO:Evaluates to: 5 ")) {
					found = true;
				}
			}
			assertTrue(found)
			val forTest = processor.getIntermediateFormResults()
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity(+(2,3),\"seconds\")", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_05() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: PI "seconds".
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue.toString)
			}
			assertTrue(issues.size > 0)
			var found = false
			for (issue : issues) {
				if (issue.toString.startsWith("INFO:Unable to reduce 'unittedQuantity(PI,\"seconds\")'")) {
					found = true
				}
			}
			assertTrue(found)
			val forTest = processor.getIntermediateFormResults()
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity(PI,\"seconds\")", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_07() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: (PI + (1 + 2)) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue.toString)
			}
			assertTrue(issues.size > 0)
			var found = false
			for (issue : issues) {
				if (issue.toString.startsWith("WARNING:+(PI,+(1,2)) is too complex to be evaluated.")) {
					found = true
				}
			}
			assertTrue(found)
			val forTest = processor.getIntermediateFormResults()
			assertEquals(forTest.size, 1)
			assertEquals(forTest.get(0).toString(), "unittedQuantity(+(PI,+(1,2)),\"seconds\")")
		]
	}

	@Test
	def void testUnits_08() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: (PI) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue.toString)
			}
			issues.assertHasInfos(2)
			var found = false
			for (issue : issues) {
				if(issue.toString.startsWith("INFO:Unable to reduce 'unittedQuantity(PI,\"seconds\")'")) {
					found = true
				}
			}
			assertTrue(found)
			val forTest = processor.getIntermediateFormResults()
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity(PI,\"seconds\")", forTest.get(0).toString())
		]
	}

	@Ignore
	@Test
	def void testUnits_09() {
		val results = newArrayList(
/*a*/
//"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), is(v0,v1)))]",
  "[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(rdf(v0, value, v2), and(rdf(v0, unit, v3), and(rdf(v1, value, v4), and(rdf(v1, unit, v3), is(v2,v4)))))))]",
/*b*/
//"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), is(v0,v1)))]",
  "[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(rdf(v0, value, v2), and(rdf(v0, unit, v3), and(rdf(v1, value, v4), and(rdf(v1, unit, v3), is(v2,v4)))))))]",
/*c*/
//"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant5, model:cValue, v1), and(unittedQuantity(v1,\"seconds\",v2), is(v0,v2))))]",
  "[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant5, model:cValue, v1), and(rdf(v0, value, v3), and(rdf(v0, unit, \"seconds\"), is(v3,v1)))))]",
/*d*/
//"[and(rdf(model:System, model:past, v0), and(+(2 \"seconds\",3 \"seconds\",v1), is(v0,v1)))]",
  "[and(rdf(model:System, model:past, v0), and(rdf(v0, value, v1), and(+(2,3,v2), and(rdf(v0, unit, \"seconds\"), is(v1,v2)))))]",
/*e*/
//"[and(rdf(model:System, model:past, v0), and(+(2 \"seconds\",3 \"seconds\",v1), is(v0,v1)))]",
  "[and(rdf(model:System, model:past, v0), and(rdf(v0, value, v1), and(+(2,3,v2), and(rdf(v0, unit, \"seconds\"), is(v1,v2)))))]",
/*f*/
//"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(+(v1,3 \"seconds\",v2), is(v0,v2))))]",
  "[and(rdf(model:System, model:past, v0), and(rdf(v0, value, v1), and(rdf(v0, unit, \"seconds\") and(rdf(model:TimingConstant3, model:constantValue, v2), and(rdf(v2, value, v3),and(rdf(v2, unit, \"seconds\"), and(+(v3,3,v4), is(v1,v3))))]",
/*g*/
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(+(v1,3 \"seconds\",v2), is(v0,v2))))]",
/*h*/
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(+(v1,3 \"seconds\",v2), is(v0,v2))))]",
/*i*/
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant5, model:cValue, v1), and(+(v1,3,v2), and(unittedQuantity(v2,\"seconds\",v3), is(v0,v3)))))]",
/*j*/
"[rdf(model:System, model:past, 3 \"seconds\")]")
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.
			 	past of System has exactly 1 value.

			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 	constantValue of UnittedConstant has exactly 1 value. 
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 	cValue of SimpleConstant has exactly 1 value. 
			 
/*a*/			 Expr: past of System is (constantValue of TimingConstant3).
/*b*/			 Expr: past of System is constantValue of TimingConstant3.
/*c*/			 Expr: past of System is (cValue of TimingConstant5) seconds.
«««			 Expr: past of System is cValue of TimingConstant5 seconds.
/*d*/			 Expr: past of System is 2 seconds + 3 seconds.
/*e*/			 Expr: past of System is (2 seconds + 3 seconds).
/*f*/			 Expr: past of System is ((constantValue of TimingConstant3) + (3 seconds)).
/*g*/			 Expr: past of System is ((constantValue of TimingConstant3) + 3 seconds).
/*h*/			 Expr: past of System is constantValue of TimingConstant3 + 3 seconds.
«««			 Expr: past of System is (constantValue of TimingConstant3 + 3) seconds.
/*i*/			 Expr: past of System is (cValue of TimingConstant5 + 3) seconds. 
/*j*/			 Expr: past of System is 3 seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
//			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults()
			for (t:forTest) {
				println("\"" + t.toString + "\",")
			}
			var idx = 0
			for (t:forTest) {
				val t2 = processor.ifTranslator.cook(t)
				println(t2.toString)
				assertEquals(results.get(idx++), t2.toString)
			}
 		]
	}
	
	@Ignore
	@Test
	def void testUnits_09b() {
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.
			 	past of System has exactly 1 value. 

			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
			 Expr: past of a System is cValue of TimingConstant5 seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
			for (issue:issues) {
				println(issue.toString)
			}
 			issues.assertHasIssues(2);		// for now, satisfied with warning on units associated with subject; 
											// not sure how to separate this from expression that should be expandable
//			issues.assertHasIssues(1);
 			for (issue:issues) {
 				if (issue.severity.equals(Severity.ERROR)) {
 					assertEquals(issue.message,"past, an object property with range  http://sadl.org/sadlimplicitmodel#UnittedQuantity, cannot be compared (is) with cValue, a datatype property with range  xsd:decimal.")
 				}
 				if (issue.severity.equals(Severity.WARNING)) {
  					assertEquals(issue.message,"Units are associated with the subject of this expression; should the expression be in parentheses?")
 				}
 			}
		]
	}
 		
 	@Ignore
	@Test
	def void testUnits_09b2() {
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.
			 	past of System has exactly 1 value. 

			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
			 Expr: past of a System is (cValue of TimingConstant5) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
			for (issue:issues) {
				println(issue.toString)
			}
			val ifresults = processor.intermediateFormResults
			for (ifr : ifresults) {
				println(ifr.toString)
				val cifr = processor.ifTranslator.cook(ifr)
				println(cifr.toString)
				assertTrue(cifr instanceof List<?>)
				assertTrue((cifr as List<?>).size == 1)
				assertEquals(
				"and(rdf(v0, model:past, v1), and(rdf(v1, value, v3), and(rdf(v1, unit, \"seconds\"), and(rdf(model:TimingConstant5, model:cValue, v2), is(v3,v2)))))", 
					(cifr as List<?>).get(0).toString);
			}
 			issues.assertHasIssues(0);
		]
	}
 		
 	@Ignore
	@Test
	def void testUnits_09c() {
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.
			 	past of System has exactly 1 value.
			 									
			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
			 Expr: past of System is (constantValue of TimingConstant3 + 3) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
			for (issue : issues) {
				println(issue.toString)
			}
 			issues.assertHasIssues(1);
			assertEquals("constantValue, an object property with range  http://sadl.org/sadlimplicitmodel#UnittedQuantity, cannot operate (+) with xsd:int, an RDF datatype  xsd:int.", issues.head.message)
		]
	}
	
	@Ignore
	@Test
	def void testUnits_10() {
		val forTest = newArrayList(
"Rule R1:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, v4) and rdf(model:TimingConstant3, model:constantValue, v1) and rdf(v1, value, v3) and rdf(v1, unit, v4) and is(v2,v3) then rdf(model:System, model:approved, true).",
"Rule R1b:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, v4) and rdf(model:TimingConstant3, model:constantValue, v1) and rdf(v1, value, v3) and rdf(v1, unit, v4) and is(v2,v3) then rdf(model:System, model:approved, true).",
"Rule R1c:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, \"seconds\") and rdf(model:TimingConstant5, model:cValue, v1) and is(v2,v1) then rdf(model:System, model:approved, true).",
"Rule R1e:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, \"seconds\") and +(2,3,v1) and is(v2,v1) then rdf(model:System, model:approved, true).",
"Rule R1f:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, \"seconds\") and +(2,3,v1) and is(v2,v1) then rdf(model:System, model:approved, true).",
"Rule R5:  if rdf(model:System, model:inspection, model:Passed) then rdf(model:System, model:approved, true).",
"Rule R2:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, \"seconds\") and rdf(model:TimingConstant3, model:constantValue, v1) and rdf(v1, value, v3) and rdf(v1, unit, \"seconds\") and +(v3,3,v4) and is(v2,v4) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2b:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, \"seconds\") and rdf(model:TimingConstant3, model:constantValue, v1) and rdf(v1, value, v3) and rdf(v1, unit, \"seconds\") and +(v3,3,v4) and is(v2,v4) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2c:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v0) and rdf(v0, value, v2) and rdf(v0, unit, \"seconds\") and rdf(model:TimingConstant3, model:constantValue, v1) and rdf(v1, value, v3) and rdf(v1, unit, \"seconds\") and +(v3,3,v4) and is(v2,v4) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2e:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v0) and rdf(v0, value, v4) and rdf(v0, unit, \"seconds\") and rdf(model:TimingConstant5, model:cValue, v1) and +(v1,3,v2) and is(v4,v2) then rdf(model:System, model:inspection, model:Passed).",
"Rule R3:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, 3 \"seconds\") then rdf(model:System, model:inspection, model:Passed).",
"Rule R4:  if rdf(model:System, model:publicized, true) then rdf(model:System, model:inspection, model:Passed)."			
		)
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	approved of System has exactly 1 value. 
			 	inspection of System has exactly 1 value. 
			 	publicized of System has exactly 1 value. 
			 	
			 	past describes System with values of type UnittedQuantity.
			 	past of System has exactly 1 value. 
			
			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 constantValue of UnittedConstant has exactly 1 value. 
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 cValue of SimpleConstant has exactly 1 value. 
			 
			 Rule R1:
			 	if inspection of System is Passed and past of System is (constantValue of TimingConstant3)
			 	then approved of System is true.
			
			 Rule R1b:
			 	if inspection of System is Passed and past of System is constantValue of TimingConstant3
			 	then approved of System is true
			 	.
			
			Rule R1c:
			 	if inspection of System is Passed and past of System is (cValue of TimingConstant5) seconds
			 	then approved of System is true .
			
«««			Rule R1d:																									// has errors, tested separately below
«««			 	if inspection of System is Passed and past of System is cValue of TimingConstant5 seconds
«««			 	then approved of System is true .
			
			 Rule R1e:
			 	if inspection of System is Passed and past of System is 2 seconds + 3 seconds
			 	then approved of System is true.
			
			 Rule R1f:
			 	if inspection of System is Passed and past of System is (2 seconds + 3 seconds)
			 	then approved of System is true.
			
			 Rule R5:
			 	if inspection of System is Passed
			 	then approved of System is true .
			 
			 Rule R2:
			 	if publicized of System is true and past of System is ((constantValue of TimingConstant3) + (3 seconds))
			 	then inspection of System is Passed.
			
			 Rule R2b:
			 	if publicized of System is true and past of System is ((constantValue of TimingConstant3) + 3 seconds)
			 	then inspection of System is Passed.
			
			 Rule R2c:
			 	if publicized of System is true and past of System is constantValue of TimingConstant3 + 3 seconds
			 	then inspection of System is Passed.
			
«««			 Rule R2d:																										// has errors, tested separately below
«««			 	if publicized of System is true and past of System is (constantValue of TimingConstant3 + 3) seconds
«««			 	then inspection of System is Passed.
			
			 Rule R2e:
			 	if publicized of System is true and past of System is (cValue of TimingConstant5 + 3) seconds
			 	then inspection of System is Passed. 
			 	
			 Rule R3:
			 	if publicized of System is true and past of System is 3 seconds
			 	then inspection of System is Passed.
			
			 Rule R4:
			 	if publicized of System is true
			 	then inspection of System is Passed.	
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
			for (issue:issues) {
				println(issue.message)
			}
// 			issues.assertHasNoIssues;
  			assertTrue(rules.size == 12)
			for (rule:rules) {
				println("\"" + rule.toString + "\",")
			}
			var idx = 0
			for (t:forTest) {
				assertEquals(t.toString, rules.get(idx++).toString)
			}
 		]
	}
	
	@Test
	def void testUnits_11() {
		val grnd = newArrayList(
"and(rdf(v0 (a variable of type stblt:AircraftEngine), stblt:thrust, 25000 \"lb\"),and(rdf(v0 (a variable of type stblt:AircraftEngine), stblt:weight, 3500 \"lb\"),rdf(v0 (a variable of type stblt:AircraftEngine), stblt:sfc, 1.5)))"			
		)
		'''
			 uri "http://sadl.org/Suitability.sadl" alias stblt.
			 
			 Resource is a class.
			 Equipment is a type of Resource.
			 part describes Equipment with values of type Equipment.
			 {Aircraft, AircraftEngine} are types of Equipment.
			 part of Aircraft has at least 1 value of type AircraftEngine.
			 altitude describes Aircraft with values of type UnittedQuantity.
			 
			 thrust describes AircraftEngine with values of type UnittedQuantity.
			 weight describes Equipment with values of type UnittedQuantity.
			 speed describes Aircraft with values of type UnittedQuantity.
			 sfc describes AircraftEngine with values of type float.
			  
			 Mission is a class, described by requires with values of type Resource.
			 suitable describes Resource with values of type Mission.
			 
			// MissionX is a Mission, requires (an Aircraft with speed 1.0 mach, with altitude between 25000 ft and 260000 ft, with part
			// 	(an AircraftEngine with thrust at least 25000 lb, with weight 3500 lb, with at most sfc 1.5 )
			// ).
			 
			 MissionX is a Mission, requires (an Aircraft with speed 1.0 mach, with altitude 25000 ft, with part
			 	(an AircraftEngine with thrust 25000 lb, with weight 3500 lb, with sfc 1.5 )
			 ).
			
			 F100 is a type of AircraftEngine.
			 CF6 is a type of AircraftEngine.
		'''.sadl
		
		'''
			uri "http://sadl.org/testunits" alias tu.
			import "http://sadl.org/Suitability.sadl".
			Expr: an AircraftEngine with thrust 25000 lb, with weight 3500 lb, with sfc 1.5.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue)
			}
			assertTrue(issues.size == 1)
			val forTest = processor.getIntermediateFormResults()
			var idx = 0;
			for (tr : forTest) {
				if (tr instanceof GraphPatternElement) {
					assertEquals(grnd.get(idx++), (tr as GraphPatternElement).toDescriptiveString)
				}
				else {
					fail("intermediate form result should have been a GraphPatternElement")
				}
			}
		]
	}

	@Test
	def void testUnits_12() {
		val grnd = newArrayList(
"and(rdf(v0 (a variable of type stblt:Mission), stblt:requires, v1 (a variable of type stblt:Aircraft)),and(rdf(v1 (a variable of type stblt:Aircraft), stblt:speed, 1.0 \"mach\"),and(rdf(v1 (a variable of type stblt:Aircraft), stblt:altitude, 25000 \"ft\"),and(rdf(v1 (a variable of type stblt:Aircraft), stblt:part, v2 (a variable of type stblt:AircraftEngine)),and(rdf(v2 (a variable of type stblt:AircraftEngine), stblt:thrust, 25000 \"lb\"),and(rdf(v2 (a variable of type stblt:AircraftEngine), stblt:weight, 3500 \"lb\"),rdf(v2 (a variable of type stblt:AircraftEngine), stblt:sfc, 1.5)))))))"			
		)
		'''
			 uri "http://sadl.org/Suitability.sadl" alias stblt.
			 
			 Resource is a class.
			 Equipment is a type of Resource.
			 part describes Equipment with values of type Equipment.
			 {Aircraft, AircraftEngine} are types of Equipment.
			 part of Aircraft has at least 1 value of type AircraftEngine.
			 altitude describes Aircraft with values of type UnittedQuantity.
			 
			 thrust describes AircraftEngine with values of type UnittedQuantity.
			 weight describes Equipment with values of type UnittedQuantity.
			 speed describes Aircraft with values of type UnittedQuantity.
			 sfc describes AircraftEngine with values of type float.
			  
			 Mission is a class, described by requires with values of type Resource.
			 suitable describes Resource with values of type Mission.
			 
			// MissionX is a Mission, requires (an Aircraft with speed 1.0 mach, with altitude between 25000 ft and 260000 ft, with part
			// 	(an AircraftEngine with thrust at least 25000 lb, with weight 3500 lb, with at most sfc 1.5 )
			// ).
			 
			 MissionX is a Mission, requires (an Aircraft with speed 1.0 mach, with altitude 25000 ft, with part
			 	(an AircraftEngine with thrust 25000 lb, with weight 3500 lb, with sfc 1.5 )
			 ).
			
			 F100 is a type of AircraftEngine.
			 CF6 is a type of AircraftEngine.
		'''.sadl
		
		'''
			uri "http://sadl.org/testunits" alias tu.
			import "http://sadl.org/Suitability.sadl".
			Expr: a Mission, requires (an Aircraft with speed 1.0 mach, with altitude 25000 ft, with part
			 	(an AircraftEngine with thrust 25000 lb, with weight 3500 lb, with sfc 1.5 )).
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			for (issue : issues) {
				println(issue)
			}
			assertTrue(issues.size == 1)
			val forTest = processor.getIntermediateFormResults()
			var idx = 0;
			for (tr : forTest) {
				if (tr instanceof GraphPatternElement) {
					assertEquals(grnd.get(idx++), (tr as GraphPatternElement).toDescriptiveString)
//					println((tr as GraphPatternElement).toDescriptiveString)
				}
				else {
					fail("intermediate form result should have been a GraphPatternElement")
				}
			}
		]
	}

	@Test
	def void testListLength_01() {
		'''
			 uri  "http://sadl.org/Test.sadl" alias Test.
			 
			 Person is a class described by age with values of type int.
			 ChildrenList is a type of Person List length 1-*.
			 children describes Person with values of type ChildrenList.
			 children of Person has at most 1 value.
			 
			 PersonList is a type of Person List.
			 PersonListList is a type of PersonList List.
			 
			 foo describes Person with values of type PersonListList.
			 bar describes Person with values of type Person List length 1-4.
			 bar of Person only has values of type Person List.
			 bar of Person only has values of type Person List length 1-4.
			 bar of Person has at least one value of type Person List length 1-4.
			 bar of Person has at least 1 value of type Person List length 1-4.
			 bar of Person has at most 2 value of type Person List length 1-4.
			 
			 testval is an int List.
			 Rule R1: if x is a Person and
			 		x has bar y and 
			 		y is a Person List length 1-4
			 then 
			// 	print("Hurray!"). 
			 	x has age 50.
			
			Grades is a type of int List.
			
			Test3Grades is the Grades [95,86,67,99].
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
//			jenaModel.write(System.out, "RDF/XML-ABBREV")
//			val smitr = jenaModel.listSubModels
//			while (smitr.hasNext) {
//				smitr.next.write(System.out, "RDF/XML-ABBREV")
//			}
//			for (issue:issues) {
//				println(issue.message)
//			}
 			issues.assertHasNoIssues;
 			val barProp = jenaModel.getOntProperty("http://sadl.org/Test.sadl#bar")
 			val rng = barProp.range
 			assertTrue(rng.canAs(OntClass))
 			val sclitr = (rng.^as(OntClass)).listSuperClasses(true)
 			var lstTypeFound = false
 			var lstMinLenOK = false
 			var lstMaxLenOK = false
 			var lstFound = false
 			var lstRestFound = false
  			while (sclitr.hasNext) {
 				val sclss = sclitr.next
				if (sclss.isRestriction && sclss.^as(Restriction).allValuesFromRestriction) {
					val onprop = sclss.getPropertyValue(OWL.onProperty)
					if (onprop.isURIResource &&
						(onprop as Resource).URI.equals(SadlConstants.SADL_LIST_MODEL_FIRST_URI)) {
						if (sclss.^as(Restriction).^as(AllValuesFromRestriction).allValuesFrom.URI.equals(
							"http://sadl.org/Test.sadl#Person"))
							lstTypeFound = true
					}
				}	
 				else if (sclss.isRestriction && sclss.^as(Restriction).hasValueRestriction) {
					val onprop = sclss.getPropertyValue(OWL.onProperty)
					if (onprop.isURIResource) {
						if ((onprop as Resource).URI.equals(SadlConstants.SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI)) {
							if (sclss.^as(Restriction).^as(HasValueRestriction).hasValue.asLiteral.value == 1) {
								lstMinLenOK = true
							}
						} else if ((onprop as Resource).URI.equals(SadlConstants.SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI)) {
							if (sclss.^as(Restriction).^as(HasValueRestriction).hasValue.asLiteral.value == 4) {
								lstMaxLenOK = true
							}
						}
					}
				} 
 				else if (sclss.URIResource && sclss.URI.equals(SadlConstants.SADL_LIST_MODEL_LIST_URI)) {
					lstFound = true;
				}
			 }
 			val person = jenaModel.getOntClass("http://sadl.org/Test.sadl#Person")
 			val pitr = person.listSuperClasses(true)
 			while (pitr.hasNext) {
 				val sclss = pitr.next
 				if (sclss.isRestriction && sclss.^as(Restriction).onProperty.URI.equals("http://sadl.org/Test.sadl#bar") && sclss.^as(Restriction).allValuesFromRestriction) {
 					if (sclss.^as(Restriction).^as(AllValuesFromRestriction).allValuesFrom.equals(rng)) {
 						lstRestFound = true
 					}
 				}
 			}
  			assertTrue(lstTypeFound)
 			assertTrue(lstMinLenOK)
 			assertTrue(lstMaxLenOK)
 			assertTrue(lstFound)
 			assertTrue(lstRestFound)
//			for (rule:rules) {
//				println(rule.toString)
//			}
  			assertTrue(rules.size == 1)
 		]
	}
	
	@Test
	def void testListBuiltinElements() {
		'''
			 uri "http://org.sadl/ListExample" alias listexample version "$Revision: 1.2 $ Last modified on   $Date: 2015/11/10 18:40:29 $". 
			 
			 EVENT is a class.
			 
			 DELETE_EVENT is a type of EVENT,
			     described by deleted_item with a single value of type ITEM.
			 
			 PUSH_EVENT is a type of EVENT,
			     described by pushed_item with a single value of type ITEM.
			 
			 POP_EVENT is a type of EVENT.
			 
			 SELECT_EVENT is a type of EVENT.
			 
			 ORDER_EVENT is a type of EVENT.
			 
			 INSERT_EVENT is a type of EVENT,
			     described by existing_item with a single value of type ITEM,
			     described by inserted_item with a single value of type ITEM.
			 
			 ITEM is a class,
			     described by item_number with a single value of type int,
			     described by order_number with a single value of type int,
			     described by name with a single value of type string,
			     described by var1 with a single value of type boolean,
			     described by var2 with a single value of type boolean,
			     described by var3 with a single value of type boolean.
			 
			 BlueItem is a type of ITEM.
			 
			 System is a class,
			 //	described by error with a List of values of type string,
			     described by error with a single value of type string List,
			     described by list_of_items with values of type ITEM List,
			     described by restricted_list_of_items with values of type MarkerListType,
			     described by received with values of type EVENT.
			 
			 list_of_items of System only has values of type MarkerListType.
			 
			 Marker1 is an ITEM.
			 Marker2 is an ITEM.
			 Marker3 is an ITEM.
			 
			 List99 is an ITEM List.
			 
			 MarkerList is the ITEM List [Marker1, Marker2].
			 
			 MarkerListType is a type of ITEM List length 5.
			 MarkerListType is a type of ITEM List length 0-*.
			 
			 MarkerList2 is the MarkerListType [Marker1, Marker2, Marker3].
			 
			 Expr: element before Marker1 in MarkerList. 
			 Expr: element after Marker1 in MarkerList.
			 Expr: element 3 of MarkerList.
			 Expr: first element of MarkerList.
			 Expr: last element of MarkerList.
			 Expr: length of MarkerList.
			 Expr: count of Marker1 in MarkerList.
			 Expr: MarkerList contains Marker1.
			 Expr: MarkerList does not contain Marker1.
			 Expr: Marker1 is unique in MarkerList.
			 Expr: Marker1 is not unique in MarkerList. 
			 Expr: the sublist of MarkerList matching value is Marker1.
			 Expr: the sublist of MarkerList matching type is ITEM.
			 Expr: the sublist of MarkerList matching item_number > 3.
			 Expr: the sublist of MarkerList matching value is Marker1 and type is ITEM and item_number < 3.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			val grd = newArrayList(
"elementBefore(listexample:MarkerList,listexample:Marker1)",
"elementAfter(listexample:MarkerList,listexample:Marker1)",
"elementInList(listexample:MarkerList,3)",
"firstElement(listexample:MarkerList)",
"lastElement(listexample:MarkerList)",
"length(listexample:MarkerList)",
"count(listexample:MarkerList,listexample:Marker1)",
"contains(listexample:MarkerList,listexample:Marker1)",
"not(contains(listexample:MarkerList,listexample:Marker1))",
"unique(listexample:MarkerList,listexample:Marker1)",
"not(unique(listexample:MarkerList,listexample:Marker1))",
"sublist(listexample:MarkerList,is(value,listexample:Marker1))",
"sublist(listexample:MarkerList,is(type,listexample:ITEM))",
"sublist(listexample:MarkerList,>(listexample:item_number,3))",
"sublist(listexample:MarkerList,and(and(is(value,listexample:Marker1),is(type,listexample:ITEM)), <(listexample:item_number,3)))"				
			)
 			assertNotNull(jenaModel)
//			jenaModel.write(System.out, "RDF/XML-ABBREV")
//			val smitr = jenaModel.listSubModels
//			while (smitr.hasNext) {
//				smitr.next.write(System.out, "RDF/XML-ABBREV")
//			}
			for (issue:issues) {
				println(issue)
			}
 			issues.assertHasErrors(0);
			val ifrs = processor.getIntermediateFormResults();
			var idx = 0
 			for (ifr:ifrs) {
// 				println("\"" + ifr.toString + "\",")
 				assertTrue(processor.compareTranslations(ifr.toString, grd.get(idx++)))
 			}
 		]
	}
	
	@Test
	def void testPrecedence_01() {
		val forTest = newArrayList(
"is(rdf(Precedence:Joe, Precedence:age, null),rdf(rdf(Precedence:Jane, Precedence:friend, null), Precedence:age, null))",
"rdf(Precedence:Joe, Precedence:age, rdf(rdf(Precedence:Jane, Precedence:friend, null), Precedence:age, null))",
"+(2,*(3,4))",
"*(+(2,3),4)",
"+(-2,*(-3,-4))",
"-3.141592653589793",
"-3.141592653589793",
"+(-3.141592653589793,*(3,-2.718281828459045))",
"+(-3.141592653589793,*(3,-2.718281828459045))"
		)
		'''
			 uri "http://sadl.org/Precedence.sadl" alias Precedence.
			 
			 Person is a class,
			 	described by age with values of type int,
			 	described by friend with values of type Person.
			 	
			 Jane is a Person.
			 Joe is a Person.
			 
			 Expr: age of Joe is age of friend of Jane.	// IF is wrong for this one
			 Expr: Joe has age (age of friend of Jane).	// this is correct
			 
			 Expr: 2 + 3 * 4.
			 Expr: (2 + 3) * 4.
			 Expr: -2 + -3 * -4.
			 Expr: -PI.
			 Expr: -(PI).
			 Expr: -PI+3*-e.
			 Expr: -(PI)+3*(-e).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
//			assertTrue(results.size==7)
			for (result:results) {
				println("\"" + result.toString + "\",")
			}
			var idx = 0
			for (t:forTest) {
				assertEquals(t.toString, results.get(idx++).toString)
			}
		]
	}

	@Test
	def void testPrecedence_02() {
		val forTest = newArrayList(
"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]",
"[and(rdf(Precedence:Joe, Precedence:age, v1), and(rdf(Precedence:Jane, Precedence:friend, v0), rdf(v0, Precedence:age, v1)))]"
		)
		'''
			 uri "http://sadl.org/Precedence.sadl" alias Precedence.
			 
			 Person is a class,
			 	described by age with values of type int,
			 	described by friend with values of type Person.
			 	
			 Jane is a Person.
			 Joe is a Person.
			 
			 Expr: age of Joe is age of friend of Jane.	// This requires treating as rule conclusion to translate the same as following?
			 Expr: Joe has age (age of friend of Jane).	// this is correct
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
//			assertTrue(results.size==7)
			for (result:results) {
				println("\"" + result.toString + "\",")
			}
			var idx = 0
			for (t:forTest) {
				val t2 = processor.ifTranslator.cook(results.get(idx++), true)
				assertEquals(t2.toString, t.toString)
			}
		]
	}
	
	@Test
	def void testPrecedence_03() {
//		val forTest = newArrayList(
//"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]",
//"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]"
//		)
		'''
			  uri  "http://sadl.org/Test.sadl" alias Test.
			  
			  Person is a class described by age with values of type int, described by old with values of type boolean.
			  ChildrenList is a type of Person List length 1-*.
			  children describes Person with values of type ChildrenList.
			  children of Person has at most 1 value.
			  
			  fv describes Person with values of type float.
			  George is a Person with fv -PI, with fv -4.5.
			  
			  Rule R2:  if x is a Person and age of (element 1 of children of x) > 20
			  	then old of x is true. 
			   	
			  Rule R2b:  if x is a Person and age of element 1 of children of x > 20
			  	then old of x is true. 
			   	
			  Rule R3:  if x is a Person and age of last element of children of x > 20
			  	then old of x is true. 
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val G = jenaModel.getIndividual("http://sadl.org/Test.sadl#George")
			val pvitr = G.listPropertyValues(jenaModel.getProperty("http://sadl.org/Test.sadl#fv"))
			var f1 = false
			var f2 = false
			while (pvitr.hasNext) {
				val nv = pvitr.next
				if (nv instanceof Literal) {
					val v = (nv as Literal).value
					if (v instanceof Float) {
						val double d1 = (v as Float).doubleValue + 4.5
						if (d1 < 0.0001) f1 = true
						val double d2 = (v as Float).doubleValue + Math.PI
						if (d2 < 0.0001) f2 = true
					}
				}
			}
			assertTrue(f1 && f2)
//			val results = processor.getIntermediateFormResults()
//			if (issues !== null) {
//				for (issue:issues) {
//					println(issue.message)
//				}
//			}
//			assertTrue(results.size==7)
//			for (result:results) {
//				println(result.toString)
//			}
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(results.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testSubjHasProp_01() {
		val forTest = newArrayList(
"and(rdf(v0, SubjHasProp:prop1, v1), rdf(v0, SubjHasProp:prop2, SubjHasProp:InstOfClass3))"
		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   
			   Expr: a Class1 with prop1 a Class2 has prop2 InstOfClass3.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue)
				}
				val errors = issues.filter[severity === Severity.ERROR]
				assertTrue(errors.size == 1)
			}
			for (result:results) {
				println(result.toString)
			}
			assertTrue(results.size==1)
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}
	
	@Test
	def void testSubjHasProp_01P() {
		val forTest = newArrayList(
"and(rdf(v0, SubjHasProp:prop1, v1), rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3))"
		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   
			  // Rule R1: if a Class1 with prop1 a Class2 has prop2 InstOfClass3 then print("hi").
			   Expr: a Class1 with prop1 (a Class2 has prop2 InstOfClass3).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(results.size==1)
			for (result:results) {
				println(result.toString)
			}
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}
	
	@Test
	def void testSubjHasProp_02() {
		val forTest = newArrayList(
"and(rdf(v1, SubjHasProp:prop1, v2), and(rdf(v1, SubjHasProp:prop2, v0), rdf(v1, SubjHasProp:prop3, SubjHasProp:InstOfClass4)))"
		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   
			   Expr: a Class1 with prop1 a Class2 with prop2 a Class3 with prop3 InstOfClass4.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue)
				}
				val errors = issues.filter[severity === Severity.ERROR]
				assertTrue(errors.size==2)
			}
			for (result:results) {
				println(result.toString)
			}
			assertTrue(results.size==1)
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}
	
	@Test
	def void testSubjHasProp_02P() {
		val forTest = newArrayList(
"and(rdf(v0, SubjHasProp:prop1, v1), and(rdf(v1, SubjHasProp:prop2, v2), rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)))"
		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   
			   Expr: a Class1 with prop1 (a Class2 with prop2 (a Class3 with prop3 InstOfClass4)).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
				val errors = issues.filter[severity === Severity.ERROR]
				assertTrue(errors.size==0)
			}
			assertTrue(results.size==1)
			for (result:results) {
				println(result.toString)
			}
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}
	
	@Test
	def void testSubjHasProp_03() {
//		val forTest = newArrayList(
//"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]",
//"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]"
//		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   
			   Expr: a Class1 with prop1 (a Class2 with prop2 a Class3 with prop3 InstOfClass4, with prop2 InstOfClass3).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(results.size==1)
			for (result:results) {
				println(result.toString)
			}
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(results.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testSubjHasProp_04() {
		val forTest = newArrayList(
"and(rdf(v0, SubjHasProp:prop1, v1), and(rdf(v1, SubjHasProp:prop2, v2), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4), rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3))))"
		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   
			   Expr: a Class1 with prop1 (a Class2 with prop2 (a Class3 with prop3 InstOfClass4), with prop2 InstOfClass3).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(results.size==1)
			for (result:results) {
				println(result.toString)
			}
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}
	
	@Test
	def void testSubjHasProp_05() {
		val forTest = newArrayList(
"and(rdf(v0, SubjHasProp:prop1, v1), and(rdf(v1, SubjHasProp:prop2, v2), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4),and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3), rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3))))))"
		)
		'''
			   uri "http://sadl.org/SubjHasProp.sadl" alias SubjHasProp.
			   
			   Class1 is a class described by prop1 with values of type Class2.
			   
			   Class2 is a class described by prop2 with values of type Class3.
			   
			   Class3 is a class described by prop3 with values of type Class4.
			   
			   Class4 is a class described by prop4 with values of type decimal.
			   
			   InstOfClass3 is a Class3.
			   AnotherInstOfClass3 is a Class3.
			   InstOfClass4 is a Class4.
			   AnotherInstOfClass4 is a Class4.
			   
			   Expr: a Class1 with prop1 (a Class2 with prop2 (a Class3 with prop3 InstOfClass4, with prop3 AnotherInstOfClass4), with prop2 InstOfClass3, with prop2 AnotherInstOfClass3).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(results.size==1)
			for (result:results) {
				println(result.toString)
			}
			var idx = 0
			for (t:forTest) {
				assertTrue(processor.compareTranslations(results.get(idx++).toString, t.toString))
			}
		]
	}
	
	@Test
	def void testRuleNoArticles() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		'''
			    uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			    
			    UnittedQuantity has impliedProperty ^value.

			    Shape is a class described by area with values of type UnittedQuantity.
			    
			    Circle is a type of Shape described by radius with values of type UnittedQuantity.
			    
			    Rule AreaOfCircle: then area is PI*radius^2.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
//			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(rules.size==1)
			for (rule:rules) {
				println(rule.toString)
			}
			var idx = 0
//			for (t:forTest) {
//				assertEquals(rules.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testRuleDefiniteArticles() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		'''
			    uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			    
			    UnittedQuantity has impliedProperty ^value.
			    
			    Shape is a class described by area with values of type UnittedQuantity.
			    
			    Circle is a type of Shape described by radius with values of type UnittedQuantity.
			    
			    Rule AreaOfCircle2: then the area is PI* the radius^2.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(rules.size==1)
			for (rule:rules) {
				println(rule.toString)
			}
			var idx = 0
//			for (t:forTest) {
//				assertEquals(rules.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testRuleIndefiniteArticles() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		'''
			    uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			    
			    UnittedQuantity has impliedProperty ^value.
			    
			    Shape is a class described by area with values of type UnittedQuantity.
			    
			    Circle is a type of Shape described by radius with values of type UnittedQuantity.
			    
			    Rule AreaOfCircle: then area is PI*radius^2.
			    Rule AreaOfCircle2: then the area is PI* the radius^2.
			    Rule AreaOfCircle3: then an area is PI* a radius^2.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
//			assertTrue(rules.size==1)
			for (rule:rules) {
				println(rule.toString)
			}
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(results.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testRuleHLR340NoArticles() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		val sadlmodel1 ='''
		    uri "http://com.ge.research/sadl/BaseConcepts" alias baseconcepts version "$Revision: 1.1 $ Last modified on   $Date: 2015/01/27 22:39:45 $". 
		    
		    System is a top-level class.
		    Subsystem is a type of System. 
		    
		    Component is a type of Subsystem.
		    component describes System with values of type System.     
		    component is transitive.
		'''.sadl
		'''
			uri "http://sadl.org/LateralSteering/LateralSteering" alias latsteer version "$Revision: 1.1 $ Last modified on   $Date: 2015/01/27 22:39:45 $". 
			
			import "http://com.ge.research/sadl/BaseConcepts". 
			 
			// Subsystems related to HLRs
			Aircraft is a type of System.
			AircraftStatus is a class, must be one of {On_Ground, In_Air}.
			Air_Ground describes Aircraft with values of type AircraftStatus.
			
			{FMS (alias "Flight Management System"),
				GuidanceFunction,
				LateralSteeringFunction,
				FlightPlanManager,
				AutoFlightDirectorSystem, 
				NavigationFunction} are types of Subsystem.	
					
			// Structure of Subsystems	
			component of Aircraft has exactly 2 value of type FMS.
			component of FMS has exactly 1 values of type GuidanceFunction.
			component of FMS has exactly 1 value of type FlightPlanManager.
			component of FMS has exactly 1 value of type AutoFlightDirectorSystem.
			component of GuidanceFunction has exactly 1 value of type LateralSteeringFunction.
			
			SteeringCommand is a class.
			{RollCommand, PitchCommand, ThrustCommand} are types of SteeringCommand.
			
			Localizer_Capture_State_Type is a class, 
				must be one of {Uninitialized, Inactive, Straight_In, Intercept, Loc_Active, Missed_Capture, Canceled, Wait_For_Retrigger_State}.
				
			rollAngle describes RollCommand with values of type float.
			LNAV_Valid describes RollCommand with values of type boolean.
			SpeedCategories is a class, must be one of {Low, High} .
			mSpeed describes RollCommand with values of type SpeedCategories.
			
			cFlag1 describes RollCommand with values of type boolean.
			cFlag2 describes RollCommand with values of type boolean.
			
				
			generates describes LateralSteeringFunction with values of type RollCommand.	
			generates of LateralSteeringFunction has at least one value of type RollCommand.	
			
			Active_Plan_Exists describes FlightPlanManager with values of type boolean.
			System_State.Is_Valid describes NavigationFunction with values of type boolean.
			Course_Is_Capturable describes LateralSteeringFunction with values of type boolean.
			Ref_Path_Available describes LateralSteeringFunction with values of type boolean.
			LNAV_TO_Capable describes AutoFlightDirectorSystem with values of type boolean.
			localizer_Capture_State_Type describes LateralSteeringFunction with values of type Localizer_Capture_State_Type.
			
			Rule HRL340: if Air_Ground is In_Air and LNAV_TO_Capable is true then LNAV_Valid is true.
			Rule FQ340: if Air_Ground of Aircraft is In_Air and LNAV_TO_Capable of component of Aircraft is true 
				then LNAV_Valid of generates of component of component of Aircraft is true.
			Rule FQ340b: 
			  if x is an Aircraft and Air_Ground of x is In_Air and 
			    v0 is a FMS and v0 is component of x and
			    v1 is a AutoFlightDirectorSystem and v1 is component of v0 and v1 has LNAV_TO_Capable true and 
			    v2 is a GuidanceFunction and v2 is component of x and 
				v3 is a LateralSteeringFunction and v3 is component of v2 and 
				v4 is a RollCommand and v3 generates v4
			  then v4 has LNAV_Valid true. 
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(rules.size==3)
			for (rule:rules) {
				println(rule.toDescriptiveString)
			}
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(rules.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testRuleHLR340DefiniteArticles() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		'''
			    uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			    
			    UnittedQuantity has impliedProperty ^value.
			    
			    Shape is a class described by area with values of type UnittedQuantity.
			    
			    Circle is a type of Shape described by radius with values of type UnittedQuantity.
			    
			    Rule AreaOfCircle2: then the area is PI* the radius^2.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(rules.size==1)
			for (rule:rules) {
				println(rule.toString)
			}
			var idx = 0
//			for (t:forTest) {
//				assertEquals(rules.get(idx++).toString, t.toString)
//			}
		]
	}
	@Test
	def void testRuleHLR340IndefiniteArticles() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		'''
			    uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			    
			    UnittedQuantity has impliedProperty ^value.
			    
			    Shape is a class described by area with values of type UnittedQuantity.
			    
			    Circle is a type of Shape described by radius with values of type UnittedQuantity.
			    
			    Rule AreaOfCircle: then area is PI*radius^2.
			    Rule AreaOfCircle2: then the area is PI* the radius^2.
			    Rule AreaOfCircle3: then an area is PI* a radius^2.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
//			assertTrue(rules.size==1)
			for (rule:rules) {
				println(rule.toString)
			}
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(results.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testRuleFlexibleArgumentNumber() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1)), and(rdf(v1, SubjHasProp:prop2, v2)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4)), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3)), (rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3)))))))))))]"
		)
		'''
			    uri "http://sadl.org/StrConcatTest.sadl" alias sct.
			    
			    Shape is a class described by area with values of type UnittedQuantity.
			    		    
			    Rule StrConcatTest: if x is a Shape and str = strConcat("hello ", "world", ".", " Glad", "to ", "be ", "here.") then print(str).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
 			issues.assertHasIssues(2);	// one for each function, strConcat and print.
 			var param1issueFound = false;
 			for (issue : issues) {
 				if (issue.message.equals("Function 'strConcat' has an unknown (--) parameter type, cannot do argument type checking.")) {
 					param1issueFound = true;
 				}
 			}
			assertTrue(param1issueFound) 
			for (rule:rules) {
				println(rule.toString)
			}
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(results.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testRuleUnboundVariableBug() {
		'''
			 uri "http://sadl.org/BottomUp.sadl" alias bottomup.
			 
			 Duration is a class, described by p with values of type UnittedQuantity.
			 
			 Rule R1: if x is a Duration then p of x is 2 seconds + 3 seconds.
			 D1 is a Duration.

			 Ask: select d, v where d has p v.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			if (issues !== null) {
				for (issue:issues) {
					println(issue)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
 			errors.assertHasNoIssues
			for (rule:rules) {
				println(rule.toString)
			}
			assertTrue(rules.size == 1)
			assertEquals("Rule R1:  if rdf(x, rdf:type, bottomup:Duration) and +(2,3,v0) then thereExists(UnittedQuantity,value,v0,unit,\"seconds\",Plus,x,bottomup:p).", rules.get(0).toString)
		]
	}
	
	@Test
	def void testGH_858() {
		'''
			 uri "http://sadl.org/OtherTypesBug.sadl" alias othertypesbug.
			 
			 Rule R1: then print("Multiple built-in args: ", 1 + 2, 3 + 4).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			for (rule:rules) {
				println(rule.toString)
			}
			assertTrue(rules.size == 1)
			assertEquals("Rule R1:  if +(1,2,v0) and +(3,4,v1) then print(\"Multiple built-in args: \",v0,v1).", rules.get(0).toString)
		]
	}
	
	@Test
	def void testGH_874() {
		'''
		uri "http://sadl.org/propChains.sadl" alias propchains.
		
		 Part is a class
		          described by partID with values of type string
		          described by processing with values of type Process.
		
		 Process is a class
		          described by temperature with values of type Temperature.
		
		 Temperature is a type of UnittedQuantity.
		
		 p1 is a Part
		          partID "123"
		          processing (a Process temperature (a Temperature ^value 100 unit "C")).
		  
		  
		 // what is the temperature of processing of p1?
		 Ask: select x where x is the temperature of processing of p1.
		
		// what is the temperature of processing of (a Part partID "123")?
		 Ask: select x where x is the temperature of processing of (a Part with partID "123").
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			var cnt = 0
			for (cmd:cmds) {
				println(cmd.toString)
				if (cmd.toString.equals("select x where rdf(propchains:p1, propchains:processing, v0) . rdf(v0, propchains:temperature, x)")) cnt++
				if (cmd.toString.equals("select x where rdf(v0, propchains:partID, \"123\") . rdf(v0, propchains:processing, v1) . rdf(v1, propchains:temperature, x)")) cnt++
			}
			assertTrue(cnt == 2)
		]
	}

	@Test
	def void testGH_874b() {
		'''
		uri "http://sadl.org/propChains.sadl" alias propchains.
		
		 Part is a class
		          described by partID with values of type string
		          described by processing with values of type Process.
		
		 Process is a class
		          described by temperature with values of type Temperature.
		
		 Temperature is a type of UnittedQuantity.
		
		 p1 is a Part
		          partID "123"
		          processing (a Process temperature (a Temperature ^value 100 unit "C")).
		  
		  
		// what is the unit of a temperature of a processing of (a Part partID "123")?
		 Ask: select x where x is the unit of a temperature of a processing of (a Part with partID "123").
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.empty)
			for (cmd:cmds) {
				println(cmd.toString)
			}
			assertTrue(cmds.size == 1)
			assertEquals("select x where rdf(v0, propchains:partID, \"123\") . rdf(v0, propchains:processing, v1) . rdf(v1, propchains:temperature, v2) . rdf(v2, sadlimplicitmodel:unit, x)",
				cmds.get(0).toString)
		]
	}

	@Test
	def void testGH_874c() {
		'''
		uri "http://sadl.org/propChains.sadl" alias propchains.
		
		 Part is a class
		          described by partID with values of type string
		          described by processing with values of type Process.
		
		 Process is a class
		          described by temperature with values of type Temperature.
		
		 Temperature is a type of UnittedQuantity.
		
		 p1 is a Part
		          partID "123"
		          processing (a Process temperature (a Temperature ^value 100 unit "C")).
		  
		  
		// what is the unit of a temperature of a processing of (a Part partID "123")?
		 Ask: the temperature of a processing of (a Part with partID "123").
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.empty)
			for (cmd:cmds) {
				println(cmd.toString)
			}
			assertTrue(cmds.size == 1)
			assertEquals("select v0 v1 v2 where rdf(v0, propchains:partID, \"123\") . rdf(v0, propchains:processing, v1) . rdf(v1, propchains:temperature, v2)",
				cmds.get(0).toString
			)
			
		]
	}

	@Test
	def void testGH_874d() {
		'''
		uri "http://sadl.org/propChains.sadl" alias propchains.
		
		 Part is a class
		          described by partID with values of type string
		          described by processing with values of type Process.
		
		 Process is a class
		          described by temperature with values of type Temperature.
		
		 Temperature is a type of UnittedQuantity.
		
		 p1 is a Part
		          partID "123"
		          processing (a Process temperature (a Temperature ^value 100 unit "C")).
		  	  
		 Ask: partID.
		 
		 Ask: processing.
		 
		 Ask: temperature of processing.
		 
		 Ask: unit of temperature of processing of a Part.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.empty)
			for (cmd:cmds) {
				println(cmd.toString)
			}
			assertTrue(cmds.size == 4)
			assertEquals("select v0 v1 where rdf(v0, propchains:partID, v1)", cmds.get(0).toString)
			assertEquals("select v0 v1 where rdf(v0, propchains:processing, v1)", cmds.get(1).toString)
			assertEquals("select v0 v1 v2 where rdf(v0, propchains:processing, v1) . rdf(v1, propchains:temperature, v2)", cmds.get(2).toString)
			assertEquals("select v0 v1 v2 v3 where rdf(v0, propchains:processing, v1) . rdf(v1, propchains:temperature, v2) . rdf(v2, sadlimplicitmodel:unit, v3)", cmds.get(3).toString)
		]
	}

	@Test
	def void testGH_882() {
		val forTest = "Rule Test:  if rdf(inst, rdf:type, test:Class1) and rdf(inst, test:xyz, xyz1) and rdf(xyz1, rdf:type, abc) and rdf(abc, rdfs:subClassOf, test:XYZ) and rdf(abc, test:new1, v0) and rdf(abc, test:new1, z1) and rdf(z1, test:ccc1Val, someName1) and rdf(obj1, rdf:type, test:Object) and rdf(obj1, test:objProp, obj2) and rdf(obj2, rdf:type, test:Object2) and rdf(obj2, test:someProp3, inst) and rdf(obj2, someName1, v1) and rdf(v1, sadlimplicitmodel:value, z2) and rdf(xyz1, test:new3, v2) and >=(z2,v2) and rdf(obj3, rdf:type, test:Object2) and rdf(obj3, someName1, v3) and rdf(v3, sadlimplicitmodel:value, z3) and >=(z3,v2) then print(\"in Test *********************************************************************************************\")."

		val rdfmodel ='''
			uri "http://www.w3.org/2000/01/rdf-schema" alias rdfs.
			
			^type is a property.
			
			rdfs:domain is a property.
			
			rdfs:range is a property.
			
			rdfs:subClassOf is a property.
			
			rdfs:subPropertyOf is a property.
			
			rdfs:subTypeOf is a property.
			
			rdfs:onProperty is a property.
			
			rdfs:allValuesFrom is a property.
			
			rdfs:onClass is a property.		
			'''.sadl
		'''
			uri "http://kdl.ge.com/Test.sadl" alias test.
			
			import "http://www.w3.org/2000/01/rdf-schema".
			
			Object is a class
			
			   described by objProp with values of type Object2.
			Object2 is a class
			
			   described by someProp1 with values of type UnittedQuantity
			
			   described by someProp2 with values of type UnittedQuantity
			
			   described by someProp3 with values of type Class1.
			Class1 is a class,
			
			   described by xyz with values of type XYZ.
			CCC1 is a class,
			
			   described by ccc1Val  with values of type class.
			XYZ is a class
			
			  described by new1 with a single value of type CCC1
			
			   described by new2 with a single value of type CCC1
			
			   described by new3 with a single value of type float.
			ABC is a type of XYZ.
			
			new1 of ABC is (a CCC1 with ccc1Val someProp1).
			
			new2 of ABC is (a CCC1 with ccc1Val someProp2).
			
			Rule Test
			
			if inst is a Class1
			
			and xyz of inst is xyz1
			
			and xyz1 is a abc
			
			and abc rdfs:subClassOf XYZ
			
			and new1 of abc is a CCC1
			
			and new1 of abc is z1
			
			and z1 has ccc1Val someName1
			
			and obj1 is a Object
			
			and obj2 is objProp of obj1
			
			and obj2 is a Object2
			
			and inst is someProp3 of obj2
			
			and ^value of someName1 of obj2 is z2
			
			and z2 >= new3 of xyz1
			
			and obj3 is a Object2
			
			and ^value of someName1 of obj3 is z3
			
			and z3 >= new3 of xyz1
			
			then print("in Test *********************************************************************************************").
			
			mx is a Class1
			
			has xyz (a ABC with new3 0.0).
			
			p1 is a Object
			
			with objProp (a Object2
			
			                 with someProp3 mx
			
			                 with someProp1 (a UnittedQuantity with ^value 3.2 with unit "xxx1")
			
			                 with someProp2 (a UnittedQuantity with ^value 8.3 with unit "xxx2")
			
			                 ).
			Ask: "select * where {?x ?y ?z} limit 2".
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(rules.size==1)
			for (rule:rules) {
				println(rule.toString)
			}
			assertTrue(processor.compareTranslations(rules.get(0).toString, forTest))
//			var idx = 0
//			for (t:forTest) {
//				assertEquals(rules.get(idx++).toString, t.toString)
//			}
		]
	}
	
	@Test
	def void testTestStatement_01() {
		'''
			 uri "http://sadl.org/TestStatements.sadl" alias teststatements.
			  
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: "select ?w where {<George> <weight> ?w}" > "select ?a where {<Sue> <age> ?a}".
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertNotNull(cmds)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			issues.assertHasNoIssues;
			assertEquals(cmds.size, 1)
			assertEquals("\"select ?w where {<George> <weight> ?w}\" > \"select ?a where {<Sue> <age> ?a}\"", cmds.get(0).toString())
		]
	}

	@Test
	def void testTestStatement_02() {
		'''
			 uri "http://sadl.org/TestStatements.sadl" alias teststatements.
			  
			 Person is a class described by age with values of type int, described by weight with values of type float.
			 
			 Sue is a Person with age 23.
			 
			 George is a Person with weight 156.9.
			 
			 Test: (select w where George weight w) > (select ag where Sue age ag).
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertNotNull(cmds)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.empty)
		]
	}

}
