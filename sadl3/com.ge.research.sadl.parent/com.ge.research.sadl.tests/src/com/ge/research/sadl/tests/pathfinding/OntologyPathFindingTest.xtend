package com.ge.research.sadl.tests.pathfinding;

import com.ge.research.sadl.jena.IntermediateFormTranslator
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import org.junit.Ignore

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
public class OntologyPathFindingTest extends AbstractSADLModelProcessorTest {

	@Ignore ("This requires path finding to pass")
	@Test 
	def void test_01() {
		val forTest = newArrayList(

		)
 		val grd = "{}"
		'''
			  uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			  Shape is a class described by area with values of type float.
			  
			  Circle is a type of Shape described by radius with values of type float.
			  
			  Rule R1: if c is a Circle then area is PI*radius^2.
			  
  		'''.assertValidatesTo([ jenaModel, rules, cmds, issues, rprocessor |
			println("Issues:")
			for (issue:issues) {
				println(issue)
			}
//			assertTrue(issues.size == 0)
			assertTrue(issues.filter[severity === Severity.ERROR].nullOrEmpty)
//			assertTrue(reqlst.size == 1)
//			val req = reqlst.get(0)
			(rprocessor as JenaBasedSadlModelProcessor).setExpandMissingPatternsInValidation(true)
			val ift = rprocessor.getIfTranslator as IntermediateFormTranslator
			var forTestIdx = 0
			for (r : rules) {
				println("Raw Rule:")
				println(r.toDescriptiveString)
//				assertTrue(rprocessor.compareTranslations(r.toDescriptiveString, forTest.get(forTestIdx++)))
				assertTrue(ift.addMissingTriplePatterns(jenaModel, r))
				println("Half-baked Rule:")
				println(r.toDescriptiveString)
//				assertTrue(rprocessor.compareTranslations(r.toDescriptiveString, forTest.get(forTestIdx++)))
				if (r.ruleName.equals("R1")) {
					val thens = r.getThens
//					val whens = r.getWhens
//					assertTrue(thens.get(0) instanceof BuiltinElement && (thens.get(0) as BuiltinElement).funcName.equals("shallBe"))
//					val assignPr = (thens.get(0) as BuiltinElement).arguments.get(0)
//					assertTrue(assignPr instanceof ProxyNode)
//					assertTrue((assignPr as ProxyNode).proxyFor instanceof BuiltinElement)
//					val arg0 = ((assignPr as ProxyNode).proxyFor as BuiltinElement).arguments.get(0)
//					assertTrue(arg0 instanceof NamedNode)
//					val replacement = (arg0 as NamedNode).missingTripleReplacement
//					assertTrue(replacement instanceof ProxyNode)
//					assertTrue((replacement as ProxyNode).proxyFor instanceof TripleElement)
				}
				val cr = ift.cook(r);
				println("Fully-baked Rule:")
				println(cr.toDescriptiveString)
//				assertTrue(rprocessor.compareTranslations(cr.toDescriptiveString, forTest.get(forTestIdx++)))
			}
    ])
	}

	@Test 
	def void test_02() {
		val forTest = newArrayList(

		)
 		val grd = "{}"
		'''
			 uri "http://sadl.org/TestGraphPatternBuiltins.sadl" alias tgpb.
			 
			 Student is a class described by test with values of type Test.
			 Test is a class described by score with values of type int.
			 averageScore describes Student with a single value of type float.
			 minScore describes Student with a single value of type int.
			 maxScore describes Student with a single value of type int.
			 prodOfScores describes Student with a single value of type long.
			 sumOfScores describes Student with a single value of type int.
			 
			 Rule avgRule: if s is a Student then averageScore of s is average(s, test, t, t, score).
			 Rule minRule: if s is a Student then minScore of s is min(s, test, t, t, score).
			 Rule maxRule: if s is a Student then maxScore of s is max(s, test, t, t, score).
			 Rule prodRule: if s is a Student then prodOfScores of s is product(s, test, t, t, score).
			 Rule sumRule: if s is a Student then sumOfScores of s is sum(s, test, t, t, score).
  		'''.assertValidatesTo([ jenaModel, rules, cmds, issues, rprocessor |
			println("Issues:")
			for (issue:issues) {
				println(issue.message)
			}
//			assertTrue(issues.size == 0)
//			assertTrue(issues.filter[severity === Severity.ERROR].nullOrEmpty)
//			assertTrue(reqlst.size == 1)
//			val req = reqlst.get(0)
			(rprocessor as JenaBasedSadlModelProcessor).setExpandMissingPatternsInValidation(true)
			val ift = rprocessor.getIfTranslator as IntermediateFormTranslator
			var forTestIdx = 0
			for (r : rules) {
				println("Raw Rule:")
				println(r.toDescriptiveString)
//				assertTrue(rprocessor.compareTranslations(r.toDescriptiveString, forTest.get(forTestIdx++)))
				assertTrue(ift.addMissingTriplePatterns(jenaModel, r))
				println("Half-baked Rule:")
				println(r.toDescriptiveString)
//				assertTrue(rprocessor.compareTranslations(r.toDescriptiveString, forTest.get(forTestIdx++)))
				if (r.ruleName.equals("R1")) {
					val thens = r.getThens
//					val whens = r.getWhens
//					assertTrue(thens.get(0) instanceof BuiltinElement && (thens.get(0) as BuiltinElement).funcName.equals("shallBe"))
//					val assignPr = (thens.get(0) as BuiltinElement).arguments.get(0)
//					assertTrue(assignPr instanceof ProxyNode)
//					assertTrue((assignPr as ProxyNode).proxyFor instanceof BuiltinElement)
//					val arg0 = ((assignPr as ProxyNode).proxyFor as BuiltinElement).arguments.get(0)
//					assertTrue(arg0 instanceof NamedNode)
//					val replacement = (arg0 as NamedNode).missingTripleReplacement
//					assertTrue(replacement instanceof ProxyNode)
//					assertTrue((replacement as ProxyNode).proxyFor instanceof TripleElement)
				}
				val cr = ift.cook(r);
				println("Fully-baked Rule:")
				println(cr.toDescriptiveString)
//				assertTrue(rprocessor.compareTranslations(cr.toDescriptiveString, forTest.get(forTestIdx++)))
			}
    ])
	}
}
