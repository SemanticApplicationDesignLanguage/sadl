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
					"Rule UnnamedChild:  if and(rdf(TestArticles:MyCircle, TestArticles:radius, X), and(>(X,0), and(^(X,2,v0), *(v0,PI,Y)))) then rdf(TestArticles:MyCircle, TestArticles:area, Y)."))
		]

	}
	
}
