/*
 * © 2014-2017 General Electric Company – All Rights Reserved
 * 
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

import static extension com.ge.research.sadl.tests.SadlTestAssertions.*
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.ontology.Restriction
import com.ge.research.sadl.processing.SadlConstants
import com.hp.hpl.jena.ontology.AllValuesFromRestriction
import com.hp.hpl.jena.ontology.HasValueRestriction
import com.hp.hpl.jena.util.iterator.ExtendedIterator
import com.hp.hpl.jena.vocabulary.OWL
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.rdf.model.Literal

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
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
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
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
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
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity((+(2,3)),\"seconds\")", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_04() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: (2 + 3) "seconds".
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity((+(2,3)),\"seconds\")", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_05() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: PI "seconds".
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity(PI,\"seconds\")", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_06() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: PI seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
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
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
			assertEquals(forTest.size, 1)
			assertEquals(forTest.get(0).toString(), "unittedQuantity((+(PI,(+(1,2)))),\"seconds\")")
		]
	}

	@Test
	def void testUnits_08() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: (PI) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(true, false)
			assertEquals(forTest.size, 1)
			assertEquals("unittedQuantity(PI,\"seconds\")", forTest.get(0).toString())
		]
	}

	@Test
	def void testUnits_09() {
		val results = newArrayList(
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), is(v0,v1)))]",
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), is(v0,v1)))]",
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant5, model:cValue, v1), and(unittedQuantity(v1,\"seconds\",v2), is(v0,v2))))]",
"[and(rdf(model:System, model:past, v0), and(+(2 \"seconds\",3 \"seconds\",v1), is(v0,v1)))]",
"[and(rdf(model:System, model:past, v0), and(+(2 \"seconds\",3 \"seconds\",v1), is(v0,v1)))]",
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(+(v1,3 \"seconds\",v2), is(v0,v2))))]",
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(+(v1,3 \"seconds\",v2), is(v0,v2))))]",
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), and(+(v1,3 \"seconds\",v2), is(v0,v2))))]",
"[and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant5, model:cValue, v1), and(+(v1,3,v2), and(unittedQuantity(v2,\"seconds\",v3), is(v0,v3)))))]",
"[and(rdf(model:System, model:past, v0), is(v0,3 \"seconds\"))]")
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.

			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
			 Expr: past of System is (constantValue of TimingConstant3).
			 Expr: past of System is constantValue of TimingConstant3.
			 Expr: past of System is (cValue of TimingConstant5) seconds.
«««			 Expr: past of System is cValue of TimingConstant5 seconds.
			 Expr: past of System is 2 seconds + 3 seconds.
			 Expr: past of System is (2 seconds + 3 seconds).
			 Expr: past of System is ((constantValue of TimingConstant3) + (3 seconds)).
			 Expr: past of System is ((constantValue of TimingConstant3) + 3 seconds).
			 Expr: past of System is constantValue of TimingConstant3 + 3 seconds.
«««			 Expr: past of System is (constantValue of TimingConstant3 + 3) seconds.
			 Expr: past of System is (cValue of TimingConstant5 + 3) seconds. 
			 Expr: past of System is 3 seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
			issues.assertHasNoIssues;
//			if (issues !== null) {
//				for (issue : issues) {
//					println(issue.message)
//				}
//			}
			val forTest = processor.getIntermediateFormResults(false, false)
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++), t.toString)
			}
 		]
	}
	
	@Test
	def void testUnits_09b() {
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.

			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
			 Expr: past of System is cValue of TimingConstant5 seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
 			issues.assertHasIssues(2);
 			for (issue:issues) {
 				if (issue.severity.equals(Severity.ERROR)) {
 					assertEquals(issue.message,"past, an object property with range  UnittedQuantity, cannot be compared (is) with cValue, a datatype property with range  decimal.")
 				}
 				if (issue.severity.equals(Severity.WARNING)) {
  					assertEquals(issue.message,"Units are associated with the subject of this expression; should the expression be in parentheses?")
 				}
 			}
		]
	}
 		
	@Test
	def void testUnits_09c() {
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.

			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
			 Expr: past of System is (constantValue of TimingConstant3 + 3) seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out)
 			issues.assertHasIssues(1);
			assertEquals(issues.head.message,"constantValue, an object property with range  UnittedQuantity, cannot operate (+) with int, an RDF datatype  int.")
		]
	}
	
	@Test
	def void testUnits_10() {
		val forTest = newArrayList(
"Rule R1:  if and(rdf(model:System, model:inspection, model:Passed), and(rdf(model:System, model:past, v0), and(rdf(model:TimingConstant3, model:constantValue, v1), is(v0,v1)))) then rdf(model:System, model:approved, true).",
"Rule R1b:  if and(rdf(model:System, model:inspection, model:Passed), and(rdf(model:System, model:past, v2), and(rdf(model:TimingConstant3, model:constantValue, v3), is(v2,v3)))) then rdf(model:System, model:approved, true).",
"Rule R1c:  if and(rdf(model:System, model:inspection, model:Passed), and(rdf(model:System, model:past, v4), and(rdf(model:TimingConstant5, model:cValue, v5), and(unittedQuantity(v5,\"seconds\",v6), is(v4,v6))))) then rdf(model:System, model:approved, true).",
"Rule R1e:  if and(rdf(model:System, model:inspection, model:Passed), and(rdf(model:System, model:past, v7), and(+(2 \"seconds\",3 \"seconds\",v8), is(v7,v8)))) then rdf(model:System, model:approved, true).",
"Rule R1f:  if and(rdf(model:System, model:inspection, model:Passed), and(rdf(model:System, model:past, v9), and(+(2 \"seconds\",3 \"seconds\",v10), is(v9,v10)))) then rdf(model:System, model:approved, true).",
"Rule R5:  if rdf(model:System, model:inspection, model:Passed) then rdf(model:System, model:approved, true).",
"Rule R2:  if and(rdf(model:System, model:publicized, true), and(rdf(model:System, model:past, v11), and(rdf(model:TimingConstant3, model:constantValue, v12), and(+(v12,3 \"seconds\",v13), is(v11,v13))))) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2b:  if and(rdf(model:System, model:publicized, true), and(rdf(model:System, model:past, v14), and(rdf(model:TimingConstant3, model:constantValue, v15), and(+(v15,3 \"seconds\",v16), is(v14,v16))))) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2c:  if and(rdf(model:System, model:publicized, true), and(rdf(model:System, model:past, v17), and(rdf(model:TimingConstant3, model:constantValue, v18), and(+(v18,3 \"seconds\",v19), is(v17,v19))))) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2e:  if and(rdf(model:System, model:publicized, true), and(rdf(model:System, model:past, v20), and(rdf(model:TimingConstant5, model:cValue, v21), and(+(v21,3,v22), and(unittedQuantity(v22,\"seconds\",v23), is(v20,v23)))))) then rdf(model:System, model:inspection, model:Passed).",
"Rule R3:  if and(rdf(model:System, model:publicized, true), rdf(model:System, model:past, 3 \"seconds\")) then rdf(model:System, model:inspection, model:Passed).",
"Rule R4:  if rdf(model:System, model:publicized, true) then rdf(model:System, model:inspection, model:Passed)."			
		)
		'''
			 uri "http://sadl.org/model.sadl" alias model (alias "This isn't the model prefix, this is an rdfs:label on the ontology") 
			 	(note "Sorry about the two usages of alias--it's there because of lack of foresight long ago.").
			 
			 System is a class described by approved with values of type boolean,
			 	described by inspection with values of type Result,
			 	described by publicized with values of type boolean.
			 	
			 	past describes System with values of type UnittedQuantity.
			
			 Result is a class, can only be one of {Passed, Failed}.
			 
			 UnittedConstant is a class described by constantValue with values of type UnittedQuantity.
			 
			 TimingConstant3 is a UnittedConstant with constantValue (a UnittedQuantity with ^value 5, with unit "seconds").
			 
			 SimpleConstant is a class described by cValue with values of type decimal.
			 TimingConstant5 is a SimpleConstant with cValue 5.
			 
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
 			issues.assertHasNoIssues;
//			for (issue:issues) {
//				println(issue.message)
//			}
  			assertTrue(rules.size == 12)
//			for (rule:rules) {
//				println(rule.toString)
//			}
			var idx = 0
			for (t:forTest) {
				assertEquals(rules.get(idx++).toString, t.toString)
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
	def void testPrecedence_01() {
		val forTest = newArrayList(
"is((rdf(Precedence:Joe, Precedence:age, null)),(rdf((rdf(Precedence:Jane, Precedence:friend, null)), Precedence:age, null)))",
"rdf(Precedence:Joe, Precedence:age, (rdf((rdf(Precedence:Jane, Precedence:friend, null)), Precedence:age, null)))",
"+(2,(*(3,4)))",		
"*((+(2,3)),4)",
"+(-2,(*(-3,-4)))",
"-(PI)",
"-(PI)",
"+((-(PI)),(*(3,(-(e)))))",
"+((-(PI)),(*(3,(-(e)))))"
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
			val results = processor.getIntermediateFormResults(true, false)
//			assertTrue(results.size==7)
//			for (result:results) {
//				println(result.toString)
//			}
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}

	@Test
	def void testPrecedence_02() {
		val forTest = newArrayList(
"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]",
"[and(rdf(Precedence:Jane, Precedence:friend, v0), and(rdf(v0, Precedence:age, v1), rdf(Precedence:Joe, Precedence:age, v1)))]"
		)
		'''
			 uri "http://sadl.org/Precedence.sadl" alias Precedence.
			 
			 Person is a class,
			 	described by age with values of type int,
			 	described by friend with values of type Person.
			 	
			 Jane is a Person.
			 Joe is a Person.
			 
			 Expr: age of Joe is age of friend of Jane.	// This requires treating as rule conclusion to translate correctly
			 Expr: Joe has age (age of friend of Jane).	// this is correct
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults(false, true)
//			assertTrue(results.size==7)
//			for (result:results) {
//				println(result.toString)
//			}
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
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
			val results = processor.getIntermediateFormResults(false, true)
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
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
	
}
