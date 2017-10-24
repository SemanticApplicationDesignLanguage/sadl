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
import com.google.common.collect.Iterables
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.eclipse.xtext.diagnostics.Severity

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class ExtendedIFTest extends AbstractSADLModelProcessorTest {

	@Test
	def void testCRule_01() {
		'''
			uri "http://sadl.org/rulevars.sadl" alias rulevars.
						
			Person is a class.
			teaches describes Person with values of type Person.
			knows describes Person with values of type Person.
			A relationship of Person to Person is acquaintance. 
			
			Rule R1 if x is a Person and x has teaches y then x has knows y.
			 
			Rule R2 if x is a Person and x has teaches y then x has acquaintance y. 
			
			Rule R3 if x is a Person and x teaches y then x knows y.
			
			//Rule R4: if a Person knows a second Person then the second Person knows the first Person.
			Rule R4b: if a Person has knows a second Person then the second Person has knows the first Person.
			
			Rule R5: if x is a Person and knows of x is y then knows of y is x.
			Rule R5b: if x is a Person and x knows y then y knows x.
			
			Rule R6: if x is a Person and y teaches x then x knows y.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			if (issues !== null) {
//				for (issue : issues) {
//					print(issue.message)
//				}
//			}
//			if (rules !== null) {
//				for (rule : rules) {
//					print(rule.toString + "\n")
//				}
//			}
			assertEquals(0, issues.size)
			assertEquals(7, rules.size);
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if and(rdf(x, rdf:type, rulevars:Person), rdf(x, rulevars:teaches, y)) then rdf(x, rulevars:knows, y)."))
			assertTrue(
				processor.compareTranslations(rules.get(1).toString(),
					"Rule R2:  if and(rdf(x, rdf:type, rulevars:Person), rdf(x, rulevars:teaches, y)) then rdf(x, rulevars:acquaintance, y)."))
			assertTrue(
				processor.compareTranslations(rules.get(2).toString(),
					"Rule R3:  if and(rdf(x, rdf:type, rulevars:Person), rdf(x, rulevars:teaches, y)) then rdf(x, rulevars:knows, y)."))
			assertTrue(
				processor.compareTranslations(rules.get(3).toString(),
					"Rule R4b:  if and(rdf(v0, rdf:type, rulevars:Person), and(rdf(v0, rulevars:knows, v1), and(rdf(v1, rdf:type, rulevars:Person), !=(v0,v1)))) then rdf(v1, rulevars:knows, v0)."))
			assertTrue(
				processor.compareTranslations(rules.get(4).toString(),
					"Rule R5:  if and(rdf(x, rdf:type, rulevars:Person), rdf(x, rulevars:knows, y)) then rdf(y, rulevars:knows, x)."))
			assertTrue(
				processor.compareTranslations(rules.get(5).toString(),
					"Rule R5b:  if and(rdf(x, rdf:type, rulevars:Person), rdf(x, rulevars:knows, y)) then rdf(y, rulevars:knows, x)."))
			assertTrue(
				processor.compareTranslations(rules.get(6).toString(),
					"Rule R6:  if and(rdf(x, rdf:type, rulevars:Person), rdf(y, rulevars:teaches, x)) then rdf(x, rulevars:knows, y)."))
		]
	}

	@Test
	def void testCRule_02() {
		'''
			uri "http://sadl.org/rulevars.sadl" alias rulevars.
						
			Person is a class.
			teaches describes Person with values of type Person.
			knows describes Person with values of type Person.
			A relationship of Person to Person is acquaintance. 
			
			Rule R1 if a Person has teaches another Person then the Person has knows the other Person.
			 
			«««			Rule R2 if a Person teaches a second Person then the first Person has acquaintance the second Person. 
			
			//Rule R3: if a Person knows a second Person then the second Person knows the first Person.
			Rule R3b: if a Person has knows a second Person then the second Person has knows the first Person.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			if (issues !== null) {
//				for (issue : issues) {
//					print(issue.message)
//				}
//			}
//			if (rules !== null) {
//				for (rule : rules) {
//					print(rule.toString + "\n")
//				}
//			}
			assertTrue(issues.size == 0)
			assertTrue(rules.size == 2)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if and(rdf(v0, rdf:type, rulevars:Person), and(rdf(v0, rulevars:teaches, v1), and(rdf(v1, rdf:type, rulevars:Person), !=(v0,v1)))) then rdf(v0, rulevars:knows, v1)."))
			assertTrue(
				processor.compareTranslations(rules.get(1).toString(),
					"Rule R3b:  if and(rdf(v0, rdf:type, rulevars:Person), and(rdf(v0, rulevars:knows, v1), and(rdf(v1, rdf:type, rulevars:Person), !=(v0,v1)))) then rdf(v1, rulevars:knows, v0)."))
		]
	}

	@Test
	def void testCRule_03() {
		'''
			uri "http://sadl.org/model3.sadl" alias model3.
			 
			AdjacencyType is a class, must be one of {TANGENT, CONVEX}.
			
			AbstractSADLnx is a top-level class.
					
			AbstractEdge (alias "Edge") is a type of AbstractSADLnx,
				described by edgeAdjacencyType with a single value of type AdjacencyType.
				
			Intersection is a type of AbstractEdge.
			
			AbstractFace is a type of AbstractSADLnx,
				described by edge with values of type AbstractEdge, 
				described by adjacentFace with values of type AbstractFace.
				
			{Cylindrical, Blending} are types of AbstractFace.
			
			isFloorFace describes Cylindrical with a single value of type boolean. 
			concave describes Cylindrical with a single value of type boolean. 
			
			AbstractFeature is a type of AbstractSADLnx,
				described by featureFace with values of type AbstractFace,
				described by otherFace with values of type AbstractFace.
			
			PadFillet is a type of AbstractFeature,
				described by bottomFace with a single value of type AbstractFace,
				described by bottomEdge with a single value of type AbstractEdge.
				
			facesShareEndPoint describes AbstractFace with values of type AbstractFace.
			
			Rule findPadFillet1:
			if a Blending has edge a first Intersection with edgeAdjacencyType TANGENT and
				the Blending has edge a second Intersection with edgeAdjacencyType TANGENT and
				the second Intersection != the first Intersection and
				a first AbstractFace has edge the second Intersection and
				concave of the first AbstractFace is false and	
				isFloorFace of the first AbstractFace is false and	
				the first AbstractFace is a Cylindrical and 
				a second AbstractFace has edge the first Intersection and
				the first Blending != the second AbstractFace and
				the Blending has adjacentFace a third AbstractFace and
				the Blending has facesShareEndPoint the third AbstractFace and	
				not the first AbstractFace has facesShareEndPoint the second AbstractFace and	
				the second AbstractFace has edge an AbstractEdge with edgeAdjacencyType CONVEX and
				the AbstractEdge != the first Intersection
			then
				there exists a PadFillet with featureFace the Blending and
				the PadFillet has otherFace the second AbstractFace and
				the PadFillet has bottomFace the first AbstractFace and
				the PadFillet has bottomEdge the second Intersection and
				the PadFillet has otherFace "Pad Fillet".
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			if (issues !== null) {
//				for (issue : issues) {
//					print(issue.message)
//				}
//			}
//			if (rules !== null) {
//				for (rule : rules) {
//					print(rule.toString)
//				}
//			}
			assertTrue(issues.size == 0)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule findPadFillet1:  if and(rdf(v0, rdf:type, model3:Blending), and(rdf(v0, model3:edge, v1), and(rdf(v1, rdf:type, model3:Intersection), and(rdf(v1, model3:edgeAdjacencyType, model3:TANGENT), and(rdf(v0, model3:edge, v2), and(rdf(v2, rdf:type, model3:Intersection), and(rdf(v2, model3:edgeAdjacencyType, model3:TANGENT), and(!=(v2,v1), and(rdf(v3, rdf:type, model3:AbstractFace), and(rdf(v3, model3:edge, v2), and(rdf(v3, model3:concave, false), and(rdf(v3, model3:isFloorFace, false), and(rdf(v3, rdf:type, model3:Cylindrical), and(rdf(v4, rdf:type, model3:AbstractFace), and(rdf(v4, model3:edge, v1), and(!=(v3,v4), and(!=(v0,v4), and(rdf(v0, model3:adjacentFace, v5), and(rdf(v5, rdf:type, model3:AbstractFace), and(!=(v4,v5), and(!=(v3,v5), and(rdf(v0, model3:facesShareEndPoint, v5), and(Not(rdf(v3, model3:facesShareEndPoint, v4)), and(rdf(v4, model3:edge, v6), and(rdf(v6, rdf:type, model3:AbstractEdge), and(rdf(v6, model3:edgeAdjacencyType, model3:CONVEX), !=(v6,v1))))))))))))))))))))))))))) then and(there exists(v7), and(rdf(v7, rdf:type, model3:PadFillet), and(rdf(v7, model3:featureFace, v0), and(rdf(v7, model3:otherFace, v4), and(rdf(v7, model3:bottomFace, v3), and(rdf(v7, model3:bottomEdge, v2), rdf(v7, model3:otherFace, \"Pad Fillet\")))))))."))
		]
	}

	@Test
	def void testCRule_04() {
		'''
			uri "http://sadl.org/genealogy.sadl" alias genealogy.
			
			Person is a class described by gender with values of type Gender,
				described by child with values of type Person.
				
			Gender is a class, must be one of {Male, Female}.
			
			{Man, Woman, Parent, Mother, Father, Aunt, Uncle, Grandparent, Grandfather, Grandmother} are types of Person.
			
			sibling describes Person with values of type Person.
			sibling is symmetrical .
			aunt describes Person with values of type Person.
			uncle describes Person with values of type Person.
			grandchild describes Person with values of type Person.
			son is a type of child.
			daughter is a type of child.
			grandson is a type of grandchild.
			granddaughter is a type of grandchild.
			cousin describes Person with values of type Person.
			cousin is symmetrical .
			
			A Person is a Parent only if child has at least 1 value.
			A Person is a Man only if gender always has value Male.
			A Person is a Woman only if gender always has value Female.
			A Person is a Mother only if child has at least 1 value and gender always has value Female.
			A Person is a Father only if child has at least 1 value and gender always has value Male.
			A Grandparent is a Grandmother only if gender always has value Female.
			A Grandparent is a Grandfather only if gender always has value Male.
			
			Rule DaughterRule:
				if a Person has child a second Person and the second Person has gender Female
				then the Person has daughter the second Person.
				
			Rule SonRule:
				if a Person has child another Person and the other Person has gender Male
				then the Person has son the other Person.
					
			Rule SiblingRule: 
				if a Person has child a second Person and the Person has child a third Person
				then the second Person has sibling the third Person.
				
			Rule GrandparentRule:
				if a Person has child a second Person and the second Person has child a third Person
				then the Person is a Grandparent and the Person has grandchild the third Person.
				
			Rule GranddaughterRule:
				if a Person has grandchild another Person 
					and the other Person has gender Female
				then the Person has granddaughter the other Person. 	
				
			Rule GrandsonRule:
				if a Person has grandchild another Person 
					and the other Person has gender Male
				then the Person has grandson the other Person. 	
				
			Rule AuntRule:
				if a Person has sibling a second Person 
					and the second Person has gender Female
					and the Person has child a third Person
				then the second Person is an Aunt
					 and the third Person has aunt the second Person.
					 
			Rule UncleRule:
				if a Person has sibling a second Person 
					and the second Person has gender Male
					and the Person has child a third Person
				then the second Person is an Uncle
					 and the third Person has uncle the second Person.
					 
			Rule CousinRule:
				if a Person has sibling a second Person
					and the Person has child a third Person
					and the second Person has child a fourth Person
				then the third Person has cousin the fourth Person.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
//			if (issues !== null) {
//				for (issue : issues) {
//					print(issue.message + "\n")
//				}
//			}
//			if (rules !== null) {
//				for (rule : rules) {
//					print(rule.toString + "\n")
//				}
//			}
			Assert.assertNotNull(issues)
			assertTrue(issues.size == 0)
			assertTrue(rules.size == 9)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule DaughterRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:child, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), rdf(v1, genealogy:gender, genealogy:Female))))) then rdf(v0, genealogy:daughter, v1)."))
			assertTrue(
				processor.compareTranslations(rules.get(1).toString(),
					"Rule SonRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:child, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), rdf(v1, genealogy:gender, genealogy:Male))))) then rdf(v0, genealogy:son, v1)."))
			assertTrue(
				processor.compareTranslations(rules.get(2).toString(),
					"Rule SiblingRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:child, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), and(rdf(v0, genealogy:child, v2), and(rdf(v2, rdf:type, genealogy:Person), and(!=(v1,v2), !=(v0,v2)))))))) then rdf(v1, genealogy:sibling, v2)."))
			assertTrue(
				processor.compareTranslations(rules.get(3).toString(),
					"Rule GrandparentRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:child, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), and(rdf(v1, genealogy:child, v2), and(rdf(v2, rdf:type, genealogy:Person), and(!=(v1,v2), !=(v0,v2)))))))) then and(rdf(v0, rdf:type, genealogy:Grandparent), rdf(v0, genealogy:grandchild, v2))."))
			assertTrue(
				processor.compareTranslations(rules.get(4).toString(),
					"Rule GranddaughterRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:grandchild, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), rdf(v1, genealogy:gender, genealogy:Female))))) then rdf(v0, genealogy:granddaughter, v1)."))
			assertTrue(
				processor.compareTranslations(rules.get(5).toString(),
					"Rule GrandsonRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:grandchild, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), rdf(v1, genealogy:gender, genealogy:Male))))) then rdf(v0, genealogy:grandson, v1)."))
			assertTrue(
				processor.compareTranslations(rules.get(6).toString(),
					"Rule AuntRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:sibling, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), and(rdf(v1, genealogy:gender, genealogy:Female), and(rdf(v0, genealogy:child, v2), and(rdf(v2, rdf:type, genealogy:Person), and(!=(v1,v2), !=(v0,v2))))))))) then and(rdf(v1, rdf:type, genealogy:Aunt), rdf(v2, genealogy:aunt, v1))."))
			assertTrue(
				processor.compareTranslations(rules.get(7).toString(),
					"Rule UncleRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:sibling, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), and(rdf(v1, genealogy:gender, genealogy:Male), and(rdf(v0, genealogy:child, v2), and(rdf(v2, rdf:type, genealogy:Person), and(!=(v1,v2), !=(v0,v2))))))))) then and(rdf(v1, rdf:type, genealogy:Uncle), rdf(v2, genealogy:uncle, v1))."))
			assertTrue(
				processor.compareTranslations(rules.get(8).toString(),
					"Rule CousinRule:  if and(rdf(v0, rdf:type, genealogy:Person), and(rdf(v0, genealogy:sibling, v1), and(rdf(v1, rdf:type, genealogy:Person), and(!=(v0,v1), and(rdf(v0, genealogy:child, v2), and(rdf(v2, rdf:type, genealogy:Person), and(!=(v1,v2), and(!=(v0,v2), and(rdf(v1, genealogy:child, v3), and(rdf(v3, rdf:type, genealogy:Person), and(!=(v2,v3), and(!=(v1,v3), !=(v0,v3))))))))))))) then rdf(v2, genealogy:cousin, v3)."))
		]

	}

	@Test
	def void testUnits_01() {
		'''
			uri "http://sadl.org/testunits" alias tu.
			Expr: 2 seconds.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
		val sadlModel = '''
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
			assertEquals('''Expected no issues. Got: «Iterables.toString(issues)»''', 0, issues.size);
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
		val sadlModel = '''
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
 			assertEquals(issues.size, 2)
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
		val sadlModel = '''
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
 			assertEquals(issues.size, 1)
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
		val sadlModel = '''
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
 			assertTrue(issues.size == 0)
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
}
