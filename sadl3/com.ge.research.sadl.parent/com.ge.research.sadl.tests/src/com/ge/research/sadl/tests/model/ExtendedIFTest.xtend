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
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			issues.assertHasNoIssues;
			val forTest = processor.getIntermediateFormResults(false, false)
			for (t:forTest) {
				println("\"" + t.toString + "\",")
			}
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
			 	past of System has exactly 1 value. 

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
 					assertEquals(issue.message,"past, an object property with range  http://sadl.org/sadlimplicitmodel#UnittedQuantity, cannot be compared (is) with cValue, a datatype property with range  xsd:decimal.")
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
 			issues.assertHasIssues(1);
			assertEquals(issues.head.message,"constantValue, an object property with range  http://sadl.org/sadlimplicitmodel#UnittedQuantity, cannot operate (+) with int, an RDF datatype  xsd:int.")
		]
	}
	
	@Test
	def void testUnits_10() {
		val forTest = newArrayList(
"Rule R1:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v0) and rdf(model:TimingConstant3, model:constantValue, v1) and is(v0,v1) then rdf(model:System, model:approved, true).",
"Rule R1b:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v2) and rdf(model:TimingConstant3, model:constantValue, v3) and is(v2,v3) then rdf(model:System, model:approved, true).",
"Rule R1c:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v4) and rdf(model:TimingConstant5, model:cValue, v5) and unittedQuantity(v5,\"seconds\",v6) and is(v4,v6) then rdf(model:System, model:approved, true).",
"Rule R1e:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v7) and +(2 \"seconds\",3 \"seconds\",v8) and is(v7,v8) then rdf(model:System, model:approved, true).",
"Rule R1f:  if rdf(model:System, model:inspection, model:Passed) and rdf(model:System, model:past, v9) and +(2 \"seconds\",3 \"seconds\",v10) and is(v9,v10) then rdf(model:System, model:approved, true).",
"Rule R5:  if rdf(model:System, model:inspection, model:Passed) then rdf(model:System, model:approved, true).",
"Rule R2:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v11) and rdf(model:TimingConstant3, model:constantValue, v12) and +(v12,3 \"seconds\",v13) and is(v11,v13) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2b:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v14) and rdf(model:TimingConstant3, model:constantValue, v15) and +(v15,3 \"seconds\",v16) and is(v14,v16) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2c:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v17) and rdf(model:TimingConstant3, model:constantValue, v18) and +(v18,3 \"seconds\",v19) and is(v17,v19) then rdf(model:System, model:inspection, model:Passed).",
"Rule R2e:  if rdf(model:System, model:publicized, true) and rdf(model:System, model:past, v20) and rdf(model:TimingConstant5, model:cValue, v21) and +(v21,3,v22) and unittedQuantity(v22,\"seconds\",v23) and is(v20,v23) then rdf(model:System, model:inspection, model:Passed).",
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
 			issues.assertHasNoIssues;
//			for (issue:issues) {
//				println(issue.message)
//			}
  			assertTrue(rules.size == 12)
			for (rule:rules) {
				println("\"" + rule.toString + "\",")
			}
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
"not((contains(listexample:MarkerList,listexample:Marker1)))",
"unique(listexample:MarkerList,listexample:Marker1)",
"not((unique(listexample:MarkerList,listexample:Marker1)))",
"sublist(listexample:MarkerList,(is(value,listexample:Marker1)))",
"sublist(listexample:MarkerList,(is(type,listexample:ITEM)))",
"sublist(listexample:MarkerList,(>(listexample:item_number,3)))",
"sublist(listexample:MarkerList,(and((and((is(value,listexample:Marker1)), (is(type,listexample:ITEM)))), (<(listexample:item_number,3)))))"				
			)
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
			val ifrs = processor.getIntermediateFormResults(true, false);
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
			for (result:results) {
				println("\"" + result.toString + "\",")
			}
			var idx = 0
			for (t:forTest) {
				assertEquals(results.get(idx++).toString, t.toString)
			}
		]
	}

	@Test
	def void testPrecedence_02() {
		val forTest = newArrayList(
"[and(rdf(Precedence:Joe, Precedence:age, v1), and(rdf(Precedence:Jane, Precedence:friend, v0), rdf(v0, Precedence:age, v1)))]",
"[and(rdf(Precedence:Joe, Precedence:age, v1), and(rdf(Precedence:Jane, Precedence:friend, v0), rdf(v0, Precedence:age, v1)))]"
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
			for (result:results) {
				println("\"" + result.toString + "\",")
			}
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
	
	@Test
	def void testSubjHasProp_01() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1), rdf(v0, SubjHasProp:prop2, SubjHasProp:InstOfClass3))]"
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
			val results = processor.getIntermediateFormResults(false, true)
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
				assertTrue(issues.size == 1)
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
	def void testSubjHasProp_01P() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1), rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3))]"
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
			val results = processor.getIntermediateFormResults(false, true)
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
"[and(rdf(v1, SubjHasProp:prop1, v2), and(rdf(v1, SubjHasProp:prop2, v0), rdf(v1, SubjHasProp:prop3, SubjHasProp:InstOfClass4)))]"
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
			val results = processor.getIntermediateFormResults(false, true)
			if (issues !== null) {
				for (issue:issues) {
					println(issue.severity + ": " + issue.message)
				}
				assertTrue(issues.size==2)
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
	def void testSubjHasProp_02P() {
		val forTest = newArrayList(
"[and(rdf(v0, SubjHasProp:prop1, v1), and(rdf(v1, SubjHasProp:prop2, v2), rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4)))]"
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
			val results = processor.getIntermediateFormResults(false, true)
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
				assertTrue(issues.size==0)
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
			val results = processor.getIntermediateFormResults(false, true)
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
"[and(rdf(v0, SubjHasProp:prop1, v1), and(rdf(v1, SubjHasProp:prop2, v2), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4), rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3))))]"
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
			val results = processor.getIntermediateFormResults(false, true)
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
"[and(rdf(v0, SubjHasProp:prop1, v1), and(rdf(v1, SubjHasProp:prop2, v2), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:InstOfClass4), and(rdf(v2, SubjHasProp:prop3, SubjHasProp:AnotherInstOfClass4), and(rdf(v1, SubjHasProp:prop2, SubjHasProp:InstOfClass3), rdf(v1, SubjHasProp:prop2, SubjHasProp:AnotherInstOfClass3))))))]"
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
			val results = processor.getIntermediateFormResults(false, true)
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
	
}
