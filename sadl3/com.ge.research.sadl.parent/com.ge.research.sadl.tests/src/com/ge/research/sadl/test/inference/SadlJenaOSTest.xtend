package com.ge.research.sadl.test.inference

import com.ge.research.sadl.reasoner.ConfigurationManager
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

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
			comment describes Person with values of type string.
			A Person is a Parent only if child has at least 1 value.
			
			Rule UnnamedChild: 
			if X is a Parent
			then there exists a Person y and X has child y plus y has comment "hello" and y has comment "world".
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
					"Rule UnnamedChild:  if rdf(X, rdf:type, sjost:Parent) then thereExists(sjost:Person,X,sjost:child,Plus,sjost:comment,\"hello\",sjost:comment,\"world\")."))
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
		 	then var1 of the System is  var2 of the System + var3 of the System.
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
			assertTrue(issues.size == 5)		// content requires articles to be enabled, variable not assigned in premises
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
		 	then var1 of the System is  var2 of the System + var3 of the System.
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
			assertTrue(issues.size == 5)	// content requires articles to be enabled, variable not assigned in premises
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
		 	then var1 of the System is Failed and var3 of the System is Failed.
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
			assertTrue(issues.size == 4)	// content requires articles to be enabled, 2 var not bound in premises
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule example-3:  if not(rdf(v0, rulevars2:var2, rulevars2:Success)) 
then rdf(v1, rulevars2:var1, rulevars2:Failed) and rdf(v2, rulevars2:var3, rulevars2:Failed).
"))
		]
	}
	
	@Test
	def void testSingleQuoteInPrintArgument() {
		'''
			 uri "http://sadl.org/SadlJenaOSTest.sadl" alias sjost.
			
			Person is a class described by child with values of type Person.
			comment describes Person with values of type string.
			A Person is a Parent only if child has at least 1 value.
			
			Rule UnnamedChild: 
			if X is a Parent
			then print("Now's the time!").
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
//			assertTrue(issues.size == 1)
//			assertTrue(issues.get(0).message.equals("Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information"))
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule UnnamedChild:  if rdf(X, rdf:type, sjost:Parent) then print(\"Now's the time!\")."))
		]
	}
}
