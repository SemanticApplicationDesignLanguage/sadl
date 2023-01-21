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
package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.preferences.SadlPreferences
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Ignore
import org.junit.Test
import org.eclipse.xtext.diagnostics.Severity

class SadlModelArticleUITest extends AbstractSadlPlatformTest {

	@Test
	def void testArticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.FALSE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of Circle is Y.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			assertEquals(0, issues.size)
		]

	}

	@Test
	def void testArticles_02() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.FALSE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of Circle is Y.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			assertEquals(0, issues.size);
		]

	}

//	@Ignore
	@Test
	def void testArticles_03() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of a Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of the Circle is Y.
 		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					print(rule.toString + "\n")
				}
			}
//			assertTrue(issues.size == 0)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(v0, rdf:type, TestArticles:Circle) and rdf(v0, TestArticles:radius, X) and >(X,0) and ^(X,2,v1) and *(v1,PI,Y) then rdf(v0, TestArticles:area, Y)."))
		]

	}
	
	@Test
	def void testArticles_04() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0
			then
				area of Circle is X^2*PI.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.get(0).toString.contains(""))
			if (rules !== null) {
				for (rule : rules) {
					print(rule.toString + "\n")
				}
			}
		]

	}
	
	@Test
	def void testArticles_05() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of a Circle and
				X > 0
			then
				area of the Circle is X^2*PI.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					print(rule.toString + "\n")
				}
			}
//			assertTrue(issues.size == 0)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(v0, rdf:type, TestArticles:Circle) and rdf(v0, TestArticles:radius, X) and >(X,0) and ^(X,2,v1) and *(v1,PI,v2) then rdf(v0, TestArticles:area, v2)."))
		]

	}
	
	@Test
	def void testCRule_01() {
		val grd = newArrayList(
"Rule R1:  if rdf(x, rdf:type, rulevars:Person) and rdf(x, rulevars:teaches, y) then rdf(x, rulevars:knows, y).",
"Rule R2:  if rdf(x, rdf:type, rulevars:Person) and rdf(x, rulevars:teaches, y) then rdf(x, rulevars:acquaintance, y).",
"Rule R3:  if rdf(x, rdf:type, rulevars:Person) and rdf(x, rulevars:teaches, y) then rdf(x, rulevars:knows, y).",
"Rule R4:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:knows, v1) and !=(v0,v1) then rdf(v1, rulevars:knows, v0).",
"Rule R4b:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:knows, v1) and !=(v0,v1) then rdf(v1, rulevars:knows, v0).",
"Rule R5:  if rdf(x, rdf:type, rulevars:Person) and rdf(x, rulevars:knows, y) then rdf(y, rulevars:knows, x).",
"Rule R5b:  if rdf(x, rdf:type, rulevars:Person) and rdf(x, rulevars:knows, y) then rdf(y, rulevars:knows, x).",
"Rule R6:  if rdf(x, rdf:type, rulevars:Person) and rdf(y, rulevars:teaches, x) then rdf(x, rulevars:knows, y).")

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/rulevars.sadl" alias rulevars.
						
			Person is a class.
			teaches describes Person with values of type Person.
			knows describes Person with values of type Person.
			A relationship of Person to Person is acquaintance. 
			
			Rule R1 if x is a Person and x has teaches y then x has knows y.
			 
			Rule R2 if x is a Person and x has teaches y then x has acquaintance y. 
			
			Rule R3 if x is a Person and x teaches y then x knows y.
			
			Rule R4: if a Person knows a second Person then the second Person knows the first Person.
			Rule R4b: if a Person has knows a second Person then the second Person has knows the first Person.
			
			Rule R5: if x is a Person and knows of x is y then knows of y is x.
			Rule R5b: if x is a Person and x knows y then y knows x.
			
			Rule R6: if x is a Person and y teaches x then x knows y.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			if (rules !== null) {
	 			for (rule:rules) {
	 				println("\"" + rule.toString + "\",")
	 			}
			}
			issues.assertHasNoIssues;
			assertEquals(8, rules.size);
 			var grdidx = 0
 			for (rule:rules) {
 				assertTrue(processor.compareTranslations(rule.toString(), grd.get(grdidx++)))
 			}
 		]
	}

	@Test
	def void testCRule_02() {
		val grd = newArrayList(
"Rule R1:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:teaches, v1) and !=(v0,v1) then rdf(v0, rulevars:knows, v1).",
"Rule R2:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:teaches, v1) and !=(v0,v1) then rdf(v0, rulevars:acquaintance, v1).",
"Rule R3:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:knows, v1) and !=(v0,v1) then rdf(v1, rulevars:knows, v0).",
"Rule R3b:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:knows, v1) and !=(v0,v1) then rdf(v1, rulevars:knows, v0).")
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		
		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/rulevars.sadl" alias rulevars.
						
			Person is a class.
			teaches describes Person with values of type Person.
			knows describes Person with values of type Person.
			A relationship of Person to Person is acquaintance. 
			
			Rule R1 if a Person has teaches another Person then the Person has knows the other Person.
			 
			Rule R2 if a Person teaches a second Person then the first Person has acquaintance the second Person. 
			
			Rule R3: if a Person knows a second Person then the second Person knows the first Person.
			Rule R3b: if a Person has knows a second Person then the second Person has knows the first Person.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					print(issue.message)
				}
			}
			if (rules !== null) {
	 			for (rule:rules) {
	 				println("\"" + rule.toString + "\",")
	 			}
			}
			issues.assertHasNoIssues;
			assertTrue(rules.size == 4)
 			var grdidx = 0
 			for (rule:rules) {
 				assertTrue(processor.compareTranslations(rule.toString(), grd.get(grdidx++)))
 			}
		]
	}

	@Test
	def void testCRule_03() {
		val grd = newArrayList(
//"Rule findPadFillet1:  if rdf(v0, rdf:type, model3:Blending) and rdf(v1, rdf:type, model3:Intersection) and rdf(v0, model3:edge, v1) and rdf(v1, model3:edgeAdjacencyType, model3:TANGENT) and rdf(v0, model3:edge, v2) and rdf(v2, rdf:type, model3:Intersection) and rdf(v2, model3:edgeAdjacencyType, model3:TANGENT) and !=(v2,v1) and rdf(v3, rdf:type, model3:AbstractFace) and rdf(v3, model3:edge, v2) and rdf(v3, model3:concave, false) and rdf(v3, model3:isFloorFace, false) and rdf(v3, rdf:type, model3:Cylindrical) and rdf(v4, rdf:type, model3:AbstractFace) and rdf(v4, model3:edge, v1) and !=(v3,v4) and !=(v0,v4) and rdf(v0, model3:adjacentFace, v5) and rdf(v5, rdf:type, model3:AbstractFace) and !=(v4,v5) and !=(v3,v5) and rdf(v0, model3:facesShareEndPoint, v5) and not(rdf(v3, model3:facesShareEndPoint, v4)) and rdf(v4, model3:edge, v6) and rdf(v6, rdf:type, model3:AbstractEdge) and rdf(v6, model3:edgeAdjacencyType, model3:CONVEX) and !=(v6,v1) then thereExists(v7) and rdf(v7, rdf:type, model3:PadFillet) and rdf(v7, model3:featureFace, v0) and rdf(v7, model3:otherFace, v4) and rdf(v7, model3:bottomFace, v3) and rdf(v7, model3:bottomEdge, v2) and rdf(v7, model3:otherFace, \"Pad Fillet\").",
"Rule findPadFillet1:  if rdf(v0, rdf:type, model3:Blending) and rdf(v1, rdf:type, model3:Intersection) and rdf(v0, model3:edge, v1) and rdf(v1, model3:edgeAdjacencyType, model3:TANGENT) and rdf(v2, rdf:type, model3:Intersection) and rdf(v0, model3:edge, v2) and rdf(v2, model3:edgeAdjacencyType, model3:TANGENT) and !=(v2,v1) and rdf(v3, model3:edge, v2) and rdf(v3, model3:concave, false) and rdf(v3, model3:isFloorFace, false) and rdf(v3, rdf:type, model3:Cylindrical) and rdf(v4, rdf:type, model3:AbstractFace) and rdf(v4, model3:edge, v1) and !=(v3,v4) and !=(v0,v4) and rdf(v5, rdf:type, model3:AbstractFace) and rdf(v0, model3:adjacentFace, v5) and !=(v4,v5) and !=(v3,v5) and rdf(v0, model3:facesShareEndPoint, v5) and not(rdf(v3, model3:facesShareEndPoint, v4)) and rdf(v6, rdf:type, model3:AbstractEdge) and rdf(v4, model3:edge, v6) and rdf(v6, model3:edgeAdjacencyType, model3:CONVEX) and !=(v6,v1) then thereExists(model3:PadFillet,model3:featureFace,v0,model3:otherFace,v4,model3:bottomFace,v3,model3:bottomEdge,v2,model3:otherFace,\"Pad Fillet\")."
)

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
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
			if a Blending has edge (a first Intersection with edgeAdjacencyType TANGENT) and
				the Blending has edge (a second Intersection with edgeAdjacencyType TANGENT) and
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
				the second AbstractFace has edge (an AbstractEdge with edgeAdjacencyType CONVEX) and
				the AbstractEdge != the first Intersection
			then
				there exists a PadFillet with featureFace the Blending and
				the PadFillet has otherFace the second AbstractFace and
				the PadFillet has bottomFace the first AbstractFace and
				the PadFillet has bottomEdge the second Intersection and
				the PadFillet has otherFace "Pad Fillet".
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			if (rules !== null) {
	 			for (rule:rules) {
	 				println("\"" + rule.toString + "\",")
	 			}
			}
			assertTrue(issues.size >= 1)		// should be 1 but there is a duplicate awc 11/27/15
			assertTrue(rules.size == 1)
 			var grdidx = 0
 			for (rule:rules) {
 				assertTrue(processor.compareTranslations(rule.toString(), grd.get(grdidx++)))
 			}
		]
	}

	@Test
	def void testCRule_04() {
		val grd = newArrayList(
"Rule DaughterRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v1) and !=(v0,v1) and rdf(v1, genealogy:gender, genealogy:Female) then rdf(v0, genealogy:daughter, v1).",
"Rule SonRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v1) and !=(v0,v1) and rdf(v1, genealogy:gender, genealogy:Male) then rdf(v0, genealogy:son, v1).",
"Rule SiblingRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v1) and !=(v0,v1) and rdf(v2, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v2) and !=(v1,v2) and !=(v0,v2) then rdf(v1, genealogy:sibling, v2).",
"Rule GrandparentRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v1) and !=(v0,v1) and rdf(v2, rdf:type, genealogy:Person) and rdf(v1, genealogy:child, v2) and !=(v1,v2) and !=(v0,v2) then rdf(v0, rdf:type, genealogy:Grandparent) and rdf(v0, genealogy:grandchild, v2).",
"Rule GranddaughterRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:grandchild, v1) and !=(v0,v1) and rdf(v1, genealogy:gender, genealogy:Female) then rdf(v0, genealogy:granddaughter, v1).",
"Rule GrandsonRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:grandchild, v1) and !=(v0,v1) and rdf(v1, genealogy:gender, genealogy:Male) then rdf(v0, genealogy:grandson, v1).",
"Rule AuntRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:sibling, v1) and !=(v0,v1) and rdf(v1, genealogy:gender, genealogy:Female) and rdf(v2, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v2) and !=(v1,v2) and !=(v0,v2) then rdf(v1, rdf:type, genealogy:Aunt) and rdf(v2, genealogy:aunt, v1).",
"Rule UncleRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:sibling, v1) and !=(v0,v1) and rdf(v1, genealogy:gender, genealogy:Male) and rdf(v2, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v2) and !=(v1,v2) and !=(v0,v2) then rdf(v1, rdf:type, genealogy:Uncle) and rdf(v2, genealogy:uncle, v1).",
"Rule CousinRule:  if rdf(v0, rdf:type, genealogy:Person) and rdf(v1, rdf:type, genealogy:Person) and rdf(v0, genealogy:sibling, v1) and !=(v0,v1) and rdf(v2, rdf:type, genealogy:Person) and rdf(v0, genealogy:child, v2) and !=(v1,v2) and !=(v0,v2) and rdf(v3, rdf:type, genealogy:Person) and rdf(v1, genealogy:child, v3) and !=(v2,v3) and !=(v1,v3) and !=(v0,v3) then rdf(v2, genealogy:cousin, v3).")

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
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
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					print(issue.message + "\n")
				}
			}
			if (rules !== null) {
	 			for (rule:rules) {
	 				println("\"" + rule.toString + "\",")
	 			}
			}
			issues.assertHasNoIssues;
			assertTrue(rules.size == 9)
 			var grdidx = 0
 			for (rule:rules) {
 				assertTrue(processor.compareTranslations(rule.toString(), grd.get(grdidx++)))
 			}
		]

	}

	@Test
	def void testCRule_05() {
		val grd = newArrayList(
"Rule R1:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:teaches, v1) and !=(v0,v1) then rdf(v0, rulevars:knows, v1).",
"Rule R2:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:teaches, v1) and !=(v0,v1) then rdf(v0, rulevars:acquaintance, v1).",
"Rule R3:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:knows, v1) and !=(v0,v1) then rdf(v1, rulevars:knows, v0).",
"Rule R3b:  if rdf(v0, rdf:type, rulevars:Person) and rdf(v1, rdf:type, rulevars:Person) and rdf(v0, rulevars:knows, v1) and !=(v0,v1) then rdf(v1, rulevars:knows, v0).")
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		
		createFile('UseArticles.sadl', '''
			    uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			    
			    UnittedQuantity has impliedProperty ^value.
			    
			    Shape is a class described by area with values of type UnittedQuantity.
			    
			    Circle is a type of Shape described by radius with values of type UnittedQuantity.
			    
			    Rule AreaOfCircle2: then the area is PI* the radius^2.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			if (rules !== null) {
	 			for (rule:rules) {
	 				println("\"" + rule.toString + "\",")
	 			}
				for ( rule : rules) {
					val rc = processor.ifTranslator.cook(rule);
					println(rc.toString);
				}
			}
			assertEquals(0, issues.filter[severity === Severity.ERROR].size)
			assertEquals(0, issues.filter[severity === Severity.WARNING].size)
			assertEquals(3, issues.filter[severity === Severity.INFO].size)
//			assertTrue(rules.size == 4)
// 			var grdidx = 0
// 			for (rule:rules) {
// 				assertTrue(processor.compareTranslations(rule.toString(), grd.get(grdidx++)))
// 			}
		]
	}

	@Test
	def void testVariables_01() {
		val grd = newArrayList(
"Rule R1:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:teaches, y) then rdf(x, ht:acquaintance, y).",
"Rule R2:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:teaches, y) then rdf(x, ht:knows, y).",
"Rule R3:  if rdf(v0, rdf:type, ht:Person) and rdf(v1, rdf:type, ht:Person) and rdf(v0, ht:knows, v1) and !=(v0,v1) then rdf(v1, ht:knows, v0).",
"Rule R4:  if rdf(v0, rdf:type, ht:Person) and rdf(v1, rdf:type, ht:Person) and rdf(v0, ht:knows, v1) and !=(v0,v1) then rdf(v1, ht:knows, v0).",
"Rule R5:  if rdf(x, ht:knows, y) then rdf(y, ht:knows, x).",
"Rule R6:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:knows, y) then rdf(y, ht:knows, x).",
"Rule R7:  if rdf(x, rdf:type, ht:Parent) then thereExists(ht:Person,x,ht:teaches).")

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/HasTests.sadl" alias ht.
			 
			 Person is a class.
			 age describes Person with values of type int.		// a "passive" property--a characteristic that a Person has; also a DatatypeProperty
			 child describes Person with values of type Person.	// another "passive" property, this time an ObjectProperty
			 A Person is a Parent only if child has at least 1 value.
			 
			 teaches describes Person with values of type Person. // an "action" property--something that a Person does
			 
			 // Instance declarations
			 George is a Person.
			 John is a Person, has age 23, has teaches George.	// this is currently valid SADL grammar but not good English
			 Julia is a Person, teaches George.	// this does not work but is desired
			 
			 Sue is a Person. 
			 Sue teaches George.	// this works currently, which is good
			 
			 Lana is a Parent.
			 
			 
			 knows describes Person with values of type Person.		// an "active" property
			 The relationship of Person to Person is acquaintance.	// a "passive" property
			 
			 Rule R1 if x is a Person and x has teaches y then x has acquaintance y.	// this works
			 
			 Rule R2 if x is a Person and x teaches y then x knows y.	// this doesn't but is desired

			 Rule R3: if a Person knows a second Person then the second Person knows the first Person.
			 Rule R4: if a Person has knows a second Person then the second Person has knows the first Person.
			 Rule R5: if x knows y then y knows x.
			 
			 Rule R6: if x is a Person and knows of x is y then knows of y is x.
			 
			 Rule R7: if x is a Parent then there exists a Person and x has teaches the Person.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
 			if (issues.size > 0) {
 				for (issue:issues) {
 					print(issue.toString)
 				}
 			}
 			assertTrue(issues.size == 0)
 			assertTrue(rules.size == 7)
 			for (rule:rules) {
 				println("\"" + rule.toString + "\",")
 			}
 			var grdidx = 0
 			for (rule:rules) {
 				assertTrue(processor.compareTranslations(rule.toString(), grd.get(grdidx++)))
 			}
 		]
	}
	
	@Test
	def void test904a_witharticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/generalcase.sadl" alias generalcase.
			 
			 FinalClass is a class.
			 pn describes FinalClass with values of type Rn.
			 Rn is a class.
			 pm describes Rn with values of type Rm.
			 Rm is a class.
			 
			 pl describes Rm with values of type Rpl.
			 Rpl is a class.
			 p1 describes Rpl with values of type Rp1.
			 Rp1 is a class.
			 
			 qk describes Rm with values of type Rqk.
			 Rqk is a class.
			 q1 describes Rqk with values of type Rq1.
			 Rq1 is a class.
			 
			 FC1 is a FinalClass.
			 
			 Ask: p1 of pl and q1 of qk of pm of pn of FC1.
			 Ask: p1 of pl of pm of pn of FC1 and q1 of qk of pm of pn of FC1.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			assertEquals(0, issues.size)
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertEquals(2, commands.size)
			assertEquals(commands.get(0).toString, commands.get(1).toString)
			assertEquals(commands.get(0).toString, "select v0 v1 v2 v3 v4 v5 where and(rdf(generalcase:FC1, generalcase:pn, v0), and(rdf(v0, generalcase:pm, v1), and(rdf(v1, generalcase:pl, v2), and(rdf(v2, generalcase:p1, v3), and(rdf(v1, generalcase:qk, v4), rdf(v4, generalcase:q1, v5))))))")
		]

	}

	@Test
	def void test904b_witharticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Part is a class described by processing with values of type Processing.
			 Processing is a class described by temperature with values of type float,
			 	described by volume with values of type float.
			 	
			 part1 is a Part. 	
			 	
			 Ask: temperature and volume of the processing of part1.
			 Ask: temperature of the processing of part1 and volume of the processing of part1.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			assertEquals(0, issues.size)
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertEquals(2, commands.size)
			assertEquals(commands.get(0).toString, commands.get(1).toString)
			assertEquals(commands.get(0).toString, "select v0 v1 v2 where and(rdf(test:part1, test:processing, v0), and(rdf(v0, test:temperature, v1), rdf(v0, test:volume, v2)))")
		]

	}

	@Test
	def void test904d_witharticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part. 	
			
			Ask: temperature and processNum of the processing of part1.
			Ask: temperature of the processing of part1 and processNum of the processing of part1.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertEquals(2, commands.size)
			assertEquals(commands.get(0).toString, commands.get(1).toString)
			assertEquals(commands.get(0).toString, "select v0 v1 v2 where and(rdf(test:part1, test:processing, v0), and(rdf(v0, test:temperature, v1), rdf(v0, test:processNum, v2)))")
		]

	}

	@Test
	def void test904e_witharticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process,
				described by partID with values of type string.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part with partID "123". 	
			
			Ask: temperature of the processing of (a Part with partID "123").
			Ask: temperature and processNum of the processing of (a Part with partID "123").
			Ask: temperature of the processing of (a Part with partID "123") and processNum of the processing of the Part.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertEquals(3, commands.size)
			assertEquals(commands.get(0).toString, "select v0 v1 v2 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v1), rdf(v1, test:temperature, v2)))")
			assertEquals(commands.get(1).toString, "select v0 v1 v2 v3 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v1), and(rdf(v1, test:temperature, v2), rdf(v1, test:processNum, v3))))")
			assertEquals(commands.get(2).toString, "select v0 v1 v2 v3 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v1), and(rdf(v1, test:temperature, v2), rdf(v1, test:processNum, v3))))")
		]

	}

	@Test
	def void test904e_witharticles_02() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process,
				described by partID with values of type string.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part with partID "123". 	
			
			Ask: temperature of the processing of (a Part with partID "123") and processNum of the processing of (a Part with partID "123").
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertEquals(2, issues.size)
			assertEquals("There is already an implicit variable with ordinality 1. Please use 'a second' to create another implicit variable or 'the first' to refer to the existing implicit variable.", issues.get(1).message)
			assertEquals(1, commands.size)
			assertEquals(commands.get(0).toString, "select v0 v2 v3 v4 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v2), and(rdf(v2, test:temperature, v3), rdf(v2, test:processNum, v4))))")
		]

	}

	@Test
	def void test904e_withoutarticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.FALSE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process,
				described by partID with values of type string.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part with partID "123". 	
			
			Ask: temperature of the processing of (a Part with partID "123").
			Ask: temperature and processNum of the processing of (a Part with partID "123").
			Ask: temperature of the processing of (a Part with partID "123") and processNum of the processing of (a Part with partID "123").
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertEquals(3, commands.size)
			assertEquals(commands.get(0).toString, "select v0 v1 v2 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v1), rdf(v1, test:temperature, v2)))")
			assertEquals(commands.get(1).toString, "select v0 v1 v2 v3 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v1), and(rdf(v1, test:temperature, v2), rdf(v1, test:processNum, v3))))")
			assertEquals(commands.get(2).toString, "select v0 v2 v3 v1 v4 v5 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v2), and(rdf(v2, test:temperature, v3), and(rdf(v1, test:partID, \"123\"), and(rdf(v1, test:processing, v4), rdf(v4, test:processNum, v5))))))")
		]

	}

	@Test
	def void test904e_withoutarticles_02() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.FALSE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process,
				described by partID with values of type string.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part with partID "123". 	
			
			Ask: temperature of the processing of (a Part with partID "123") and processNum of the processing of the Part.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (cmd : commands) {
				println(cmd.toString)
			}
			assertTrue(issues.size == 2)
			assertEquals("The use of articles is not enabled in preferences but the content is only valid when enabled.", issues.get(1).message)
			assertEquals(1, commands.size)
			assertEquals(commands.get(0).toString, "select v0 v2 v3 v1 v4 v5 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v2), and(rdf(v2, test:temperature, v3), and(rdf(v1, test:processing, v4), rdf(v4, test:processNum, v5)))))"
			)
		]

	}

}
