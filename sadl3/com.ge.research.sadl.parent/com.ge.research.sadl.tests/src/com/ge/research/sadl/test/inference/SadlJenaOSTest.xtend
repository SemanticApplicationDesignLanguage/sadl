package com.ge.research.sadl.test.inference

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.InjectWith
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.junit.runner.RunWith
import org.junit.Assert
import static org.junit.Assert.*
import org.junit.Test
import com.ge.research.sadl.reasoner.ConfigurationManager
import org.junit.Ignore

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlJenaOSTest extends AbstractSADLModelProcessorTest {
	
	
	@Test
	def void testThereExists_01() {
		'''
			 uri "http://sadl.org/SadlJenaOSTest.sadl" alias sjost.
			
			Person is a class described by child with values of type Person.
			A Person is a Parent only if child has at least 1 value.
			
			Rule UnnamedChild: 
			if X is a Parent
			then there exists a Person and X has child the Person.
			'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
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
			assertTrue(issues.size == 0)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule UnnamedChild:  if rdf(X, rdf:type, sjost:Parent) then thereExists(v0) and rdf(X, sjost:child, v1)."))
		]

	}
	
//	@Ignore
	@Test
	def void testGetTranslatorInstance() {
		val cm = new ConfigurationManager
		val tr = cm.translator
		print(tr)
	}
	
}
