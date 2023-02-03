package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelArticleTest extends AbstractSADLModelProcessorTest {
	
//	@Ignore
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
					"Rule R1:  if rdf(TestArticles:MyCircle, TestArticles:radius, X) and >(X,0) and ^(X,2,v0) and *(v0,PI,Y) then rdf(TestArticles:MyCircle, TestArticles:area, Y)."))
		]

	}
	
//	@Ignore
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
					"Rule R1:  if rdf(TestArticles:MyCircle, TestArticles:radius, X) and >(X,0) and ^(X,2,v0) and *(v0,PI,Y) then rdf(TestArticles:MyCircle, TestArticles:area, Y)."))
		]

	}
	
	@Ignore
	@Test
	def void testMissingClass() {
		val sadlModel = '''
			  uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			  
			  Shape is a class described by area with values of type UnittedQuantity.
			  
			  Circle is a type of Shape described by radius with values of type UnittedQuantity.
			  
			  Rule AreaOfCircle: then area is PI*radius^2.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
// 			jenaModel.write(System.out, "RDF/XML-ABBREV")
 			if (issues.size > 0) {
 				for (issue:issues) {
 					println(issue.toString)
 				}
 			}
// 			assertTrue(issues.size == 0)
 			assertTrue(rules.size == 1)
 			for(rule:rules) {
 				println(rule.toString)
 			}
 			assertTrue(processor.compareTranslations(rules.get(0).toString(),"Rule R1:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:teaches, y) then rdf(x, ht:acquaintance, y)."))
  		]
	}
	
}