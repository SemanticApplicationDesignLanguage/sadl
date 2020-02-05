package com.ge.research.sadl.ui.test.inference

import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.ui.tests.AbstractSadlPlatformTest
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

class SadlJenaOSTest extends AbstractSadlPlatformTest {
	
	@Test
	def void testThereExists_01() {
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		
		createFile('UseArticles.sadl', '''
			 uri "http://sadl.org/SadlJenaOSTest.sadl" alias sjost.
			
			Person is a class described by child with values of type Person.
			A Person is a Parent only if child has at least 1 value.
			
			Rule UnnamedChild: 
			if a Person is a Parent
			then there exists a second Person and the Person has child the second Person.
			''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					print(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					print(rule.toString + "\n")
				}
			}
			assertTrue(issues.size == 0)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule UnnamedChild:  if rdf(v0, rdf:type, sjost:Parent) then thereExists(v1) and rdf(v1, rdf:type, sjost:Person) and rdf(v0, sjost:child, v1)."))
		]

	}
	
	@Test
	def void testNoValue_01() {
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		
		createFile('UseArticles.sadl', '''
		 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
		 
		 System is a class,
		    described by var1 with a single value of type int,
		    described by var2 with a single value of type int,
		    described by var3 with a single value of type int,
		    described by var4 with a single value of type int.
		     	
		 Rule example-1:
		 	if var2 of a System is var3 of the System
		 	then var1 of the System is  var2 of the System + var3 of the System.
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
					"Rule example-1:  if rdf(v0, rdf:type, rulevars2:System) and rdf(v0, rulevars2:var2, v1) and 
rdf(v0, rulevars2:var3, v2) and is(v1,v2) and +(v1,v2,v3) then rdf(v0, rulevars2:var1, v3)."))
		]
	}
	
	@Test
	def void testNoValue_02() {
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
		 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
		 
		 System is a class,
		    described by var1 with a single value of type int,
		    described by var2 with a single value of type int,
		    described by var3 with a single value of type int,
		    described by var4 with a single value of type int.
		     	
		 Rule example-1:
		 	if var2 of a System is not var3 of the System
		 	then var1 of the System is  var2 of the System + var3 of the System.
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
					"Rule example-1:  if rdf(v0, rdf:type, rulevars2:System) and rdf(v0, rulevars2:var2, v1) and rdf(v0, rulevars2:var3, v2) and not(rdf(v0, rulevars2:var3, v1)) and +(v1,v2,v3) then rdf(v0, rulevars2:var1, v3)."))
		]
	}
	
	@Test
	def void testNoValue_03() {
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
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
		''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
					"Rule example-3: if rdf(v0, rdf:type, rulevars2:System) and not(rdf(v0, rulevars2:var2, rulevars2:Success)) 
then rdf(v0,rulevars2:var1,rulevars2:Failed) and rdf(v0,rulevars2:var3,rulevars2:Failed)."))
		]
	}
}
