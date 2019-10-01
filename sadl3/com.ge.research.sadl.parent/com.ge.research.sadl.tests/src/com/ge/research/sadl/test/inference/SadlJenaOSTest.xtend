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
	
//	@Ignore
	@Test
	def void testGetTranslatorInstance() {
		val cm = new ConfigurationManager
		val tr = cm.translator
		print(tr)
	}
		
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
	
	@Test
	def void testNoValue_01() {
		'''
		 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
		 
		 System is a class,
		    described by var1 with a single value of type int,
		    described by var2 with a single value of type int,
		    described by var3 with a single value of type int,
		    described by var4 with a single value of type int.
		     	
		 Rule example-1:
		 	if var2 of a System is var3 of the System
		 	then var1 of a System is  var2 of the System + var3 of the System.
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
					"Ruleexample-1: if rdf(v0,rulevars2:var2,v5) and rdf(v1,rulevars2:var3,v6) and is(v5,v6) and
rdf(v3,rulevars2:var2,v7) and rdf(v4,rulevars2:var3,v8) and +(v7,v8,v9) then rdf(v2,rulevars2:var1,v9)."))
		]
	}
	
	@Test
	def void testNoValue_02() {
		'''
		 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
		 
		 System is a class,
		    described by var1 with a single value of type int,
		    described by var2 with a single value of type int,
		    described by var3 with a single value of type int,
		    described by var4 with a single value of type int.
		     	
		 Rule example-1:
		 	if var2 of a System is not var3 of the System
		 	then var1 of a System is  var2 of the System + var3 of the System.
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
					"Rule example-1:  if rdf(v0, rulevars2:var2, v5) and not(rdf(v1, rulevars2:var3, v5)) and 
rdf(v3, rulevars2:var2, v7) and rdf(v4, rulevars2:var3, v8) and +(v7,v8,v9) then rdf(v2, rulevars2:var1, v9)."))
		]
	}
	
	@Test
	def void testNoValue_03() {
		'''
		 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
		 
		 Status is a class, must be one of {Success, Failed}.
		 System is a class,
		    described by var1 with a single value of type Status,
		    described by var2 with a single value of type Status,
		    described by var3 with a single value of type Status,
		    described by var4 with a single value of type Status.
		     	
		 Rule example-3:
		 	if var2 of a System is not Success
		 	then var1 of a System is Failed and var3 of the System is Failed.
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
					"Rule example-3:  if not(rdf(v0, rulevars2:var2, rulevars2:Success)) 
then rdf(v1, rulevars2:var1, rulevars2:Failed) and rdf(v2, rulevars2:var3, rulevars2:Failed).
"))
		]
	}
	
}
