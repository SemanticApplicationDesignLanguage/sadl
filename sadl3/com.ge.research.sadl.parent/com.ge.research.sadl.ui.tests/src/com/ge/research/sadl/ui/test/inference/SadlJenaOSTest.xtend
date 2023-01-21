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
					"Rule UnnamedChild:  if rdf(v0, rdf:type, sjost:Parent) then thereExists(sjost:Person,v0,sjost:child)."))
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

	@Test
	def void testGH_874c() {
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		
		createFile('UseArticles.sadl', '''
		uri "http://sadl.org/propChains.sadl" alias propchains.
		
		 Part is a class
		          described by partID with values of type string
		          described by processing with values of type Process.
		
		 Process is a class
		          described by temperature with values of type Temperature.
		
		 Temperature is a type of UnittedQuantity.
		
		 p1 is a Part
		          partID "123"
		          processing (a Process temperature (a Temperature ^value 100 unit "C")).
		  
		  
		// what are the unit and value of the temperature of processing of (a Part partID "123")?
		 Ask: select u,v  where u is unit of temperature of processing of (a Part with partID "123") 
		 	and v is ^value of temperature of processing of the Part.
		''').resource.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.isEmpty)
			for (cmd:cmds) {
				println(cmd.toString)
			}
			assertTrue(cmds.size == 1)
			assertTrue(processor.compareTranslations(cmds.get(0).toString, "select u v where rdf(v0, propchains:partID, \"123\") . rdf(v0, propchains:processing, v1) . rdf(v1, propchains:temperature, v2) . rdf(v2, sadlimplicitmodel:unit, u) . rdf(v2, sadlimplicitmodel:value, v)"))
		]
	}
	
	@Test
	def void testGH_874d() {
		
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		
		createFile('UseArticles.sadl', '''
		uri "http://sadl.org/chainqueries.sadl" alias chainqueries.
		
		 {C1, C2, C3, C4} are classes.
		 partID describes C1 with values of type string.
		 p1 describes C1 with values of type C2.
		 p2 describes C2 with values of type C3.
		 p3 describes C3 with values of type C4.
		 temperature describes C4 with values of type Temperature.
		 
		 Temperature is a type of UnittedQuantity.
		 	
		 i1 is a C1
		          partID "123"
		          p1 (a C2 with p2 (a C3 with p3 (a C4 with temperature (a Temperature ^value 100 unit "C")))).
		  
		// what are the unit and value of the temperature of processing of (a Part partID "123")?
		 Ask: select u,v  where u is unit of temperature of p3 of p2 of p1 of (a C1 with partID "123") 
		 	and v is ^value of temperature of p3 of p2 of p1 of the C1.
		''').resource.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			val results = processor.getIntermediateFormResults()
			if (issues !== null) {
				for (issue:issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.isEmpty)
			for (cmd:cmds) {
				println(cmd.toString)
			}
			assertTrue(cmds.size == 1)
			assertTrue(processor.compareTranslations(cmds.get(0).toString, "select u v where rdf(v0, chainqueries:partID, \"123\") . rdf(v0, chainqueries:p1, v1) . rdf(v1, chainqueries:p2, v2) . rdf(v2, chainqueries:p3, v3) . rdf(v3, chainqueries:temperature, v4) . rdf(v4, sadlimplicitmodel:unit, u) . rdf(v4, sadlimplicitmodel:value, v)"))
		]
	}
	
}
