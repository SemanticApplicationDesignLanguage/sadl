package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.InjectWith
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.junit.runner.RunWith
import org.junit.Assert
import static org.junit.Assert.*
import org.junit.Test

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelArticleTest extends AbstractSADLModelProcessorTest {
	
	
	@Test
	def void testArticles_01() {
		'''
			 uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			         	if X is the radius of MyCircle and
				X > 0 and
				Y is X^2*PI
			then
				the area of MyCircle is Y.
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
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if and(rdf(TestArticles:MyCircle, TestArticles:radius, X), and(>(X,0), and(^(X,2,v0), *(v0,PI,Y)))) then rdf(TestArticles:MyCircle, TestArticles:area, Y)."))
		]

	}
	
	@Test
	def void testArticles_02() {
		'''
			 uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			         	if X is radius of the MyCircle and
				X > 0 and
				Y is X^2*PI
			then
				area of the MyCircle is Y.
  		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
					"Rule R1:  if and(rdf(TestArticles:MyCircle, TestArticles:radius, X), and(>(X,0), and(^(X,2,v0), *(v0,PI,Y)))) then rdf(TestArticles:MyCircle, TestArticles:area, Y)."))
		]

	}
	
	@Test
	def void testArticles_03() {
		'''
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
  		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
					"Rule R1:  if and(rdf(v0, rdf:type, TestArticles:Circle), and(rdf(v0, TestArticles:radius, X), and(>(X,0), and(^(X,2,v0), *(v0,PI,Y))))) then rdf(v0, TestArticles:area, Y)."))
		]

	}
	
	@Test
	def void testArticles_04() {
		'''
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
  		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
					"Rule R1:  if and(rdf(TestArticles:Circle, TestArticles:radius, X), and(>(X,0), and(^(X,2,v0), *(v0,PI,Y)))) then rdf(TestArticles:Circle, TestArticles:area, Y)."))
		]

	}
	
}