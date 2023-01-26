package com.ge.research.sadl.jena.translator;

import com.ge.research.sadl.model.gp.Query
import com.ge.research.sadl.reasoner.ConfigurationManager
import com.ge.research.sadl.reasoner.TranslationException
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.apache.jena.ontology.OntModelSpec
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.vocabulary.XSD
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith
import org.pojava.datetime.Duration

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
public class TestJenaTranslator extends AbstractSADLModelProcessorTest {

	@Test
	def void testGetTranslatorInstance() {
		val cm = new ConfigurationManager
		val tr = cm.translator
		print(tr)
		assertNotNull(tr)
	}
		
	@Test
	def void testThereExists_01() {
		'''
			 uri "http://sadl.org/SadlJenaOSTest.sadl" alias sjost.
			
			Person is a class described by child with values of type Person.
			A Person is a Parent only if child has at least 1 value.
			
			Rule UnnamedChild: 
			if X is a Parent
			then there exists a Person p and X has child p.
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
					"Rule UnnamedChild:  if rdf(X, rdf:type, sjost:Parent) then thereExists(sjost:Person,X,sjost:child)."))
		]

	}
	
	@Test
	def void testThereExists_02() {
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
			assertTrue(issues.size == 2)
			assertTrue(issues.get(0).toString.contains("does not reference the matching instance") ||
				issues.get(1).toString.contains("does not reference the matching instance"))
			assertTrue(issues.size == 2)
			assertTrue(issues.get(0).toString.contains("The use of articles is not enabled in preferences but the content is only valid when enabled") ||
				issues.get(1).toString.contains("The use of articles is not enabled in preferences but the content is only valid when enabled"))			
		]

	}
	
	@Test
	def void testNoValue_01() {
		'''
		 uri "http://sadl.org/ruleps2.sadl" alias ruleps2.
		 
		 System is a class,
		    described by p1 with a single value of type int,
		    described by p2 with a single value of type int,
		    described by p3 with a single value of type int,
		    described by p4 with a single value of type int.
		     	
		 Rule example-1:
		 	if x is a System and p2 of x is p3 of x
		 	then p1 of x is  p2 of x + p3 of x.
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
			val trans = getTranslator(processor)
			if (rules != null) {
				for (rule : rules) {
					val rl = trans.translateRule(jenaModel, "http://sadl.org/ruleps2.sadl", rule)
					print(rl)
					assertTrue(processor.compareTranslations(rl, 
					"[example-1: (?x rdf:type http://sadl.org/ruleps2.sadl#System), (?x http://sadl.org/ruleps2.sadl#p2 ?v0), (?x http://sadl.org/ruleps2.sadl#p3 ?v1), equal(?v0, ?v1), sum(?v0, ?v1, ?v2) -> (?x http://sadl.org/ruleps2.sadl#p1 ?v2)]"))
				}
			}
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule example-1:  if rdf(x, rdf:type, ruleps2:System) and rdf(x, ruleps2:p2, v0) and rdf(x, ruleps2:p3, v1) and equal(v0,v1) and sum(v0,v1,v2) then rdf(x, ruleps2:p1, v2)."))
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
			assertTrue(issues.size == 4)		// construct requires articles to be enabled (3 errors), v2 (mistranslation) not defined
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
			assertTrue(issues.size == 3)	// content requires articles to be enabled, 2 concl var not bound
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule example-3:  if not(rdf(v0, rulevars2:var2, rulevars2:Success)) 
then rdf(v1, rulevars2:var1, rulevars2:Failed) and rdf(v2, rulevars2:var3, rulevars2:Failed).
"))
		]
	}
	
	@Test
	def void testPOJavaDuration() {
		val durStr = "23 minutes"	
		val pojDur = new Duration(durStr)
		val stdStr = pojDur.toString
		println(stdStr)
		
		val durStr2 = "23 min"	
		val pojDur2 = new Duration(durStr2)
		val stdStr2 = pojDur2.toString
		println(stdStr2)

		val durStr3 = "PT23M"	
		val pojDur3 = new Duration(durStr3)
		val stdStr3 = pojDur3.toString
		println(stdStr3)
		
		val durUri = XSD.duration.URI
		val m = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM)
		val tl = m.createTypedLiteral(stdStr, durUri)
		println(tl.toString)
		val tl2 = m.createTypedLiteral(stdStr2, durUri)
		println(tl2.toString)
		val tl3 = m.createTypedLiteral(stdStr3, durUri)
		println(tl3.toString)
		assertEquals(tl.toString, "23m^^http://www.w3.org/2001/XMLSchema#duration")
		assertEquals(tl2.toString, "23m^^http://www.w3.org/2001/XMLSchema#duration")
		assertEquals(tl3.toString, "23m^^http://www.w3.org/2001/XMLSchema#duration")
	}
	
	@Test
	def void testPOJavaDuration_02() {
		val durUri = XSD.duration.URI
		val m = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM)
		
		val durStr = "23 minutes 15 seconds"		
		val lit1 = SadlUtils.getLiteralMatchingDataPropertyRange(m, durUri, durStr);
		println(lit1.lexicalForm)
		
		val durStr2 = "1 week 3 days 2 hours 23 min 15 sec"	
		val lit2 = SadlUtils.getLiteralMatchingDataPropertyRange(m, durUri, durStr2);
		println(lit2.lexicalForm)
		
		assertEquals("PT23M15S^^http://www.w3.org/2001/XMLSchema#duration", lit1.toString)
		assertEquals("PT10D2H23M15S^^http://www.w3.org/2001/XMLSchema#duration", lit2.toString)
		
		try {
			val durStr3 = "1 year 3 months"	
			val lit3 = SadlUtils.getLiteralMatchingDataPropertyRange(m, durUri, durStr3);
			println(lit3.lexicalForm)
			fail("Should have generated error with years and months")
		}
		catch (TranslationException e) {
			println("Exception: " + e.getMessage())
		}
	}
	
	@Test
	def void testQuery01() {
		'''
		 uri "http://sadl.org/Issue442.sadl" alias Issue442.
		 
		 Person is a class described by age with values of type int.
		 
		 George is a Person.
		 
		 Ask: select p where p is a Person and age of p is not known.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
					if (cmd instanceof Query) {
						val query = getTranslator(processor).translateQuery(jenaModel, "http://sadl.org/Issue442.sadl", cmd as Query)
						println(query)
						val expected = "select ?p where {?p <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/Issue442.sadl#Person> . OPTIONAL {?p <http://sadl.org/Issue442.sadl#age> ?v1} . FILTER (!bound(?v1))}"
						assertEquals(expected.trim, query.toString.trim)
					}
				}
			}
		]
	}

	@Test
	def void testIsNotA_01() {
		'''
		 uri "http://sadl.org/gh633b.sadl" alias gh633b.
		 
		 B is a class.
		 C is a class.
		 D is a class.
		 
		 Rule r:
		 if x is a B and x is not a C
		 then x is a D.
		 
		 Ask: select x where x is not a C.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(rules !== null)
			assertTrue(rules.size() == 1)
			println(rules.get(0))
			val rexp = "Rule r:  if rdf(x, rdf:type, gh633b:B) and not(rdf(x, rdf:type, gh633b:C)) then rdf(x, rdf:type, gh633b:D)."
			assertTrue(processor.compareTranslations(rules.get(0).toString(), rexp))
			assertTrue(cmds !== null)
			
			for (cmd : cmds) {
				println(cmd.toString)
				if (cmd instanceof Query) {
					val query = getTranslator(processor).translateQuery(jenaModel, "http://sadl.org/gh633b.sadl", cmd as Query)
					println(query)
					val expected = "select ?x where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?v1 . FILTER (!EXISTS { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/gh633b.sadl#C> })}"
					assertTrue(processor.compareTranslations(expected.trim, query.toString.trim))
				}
			}
		]
	}
	
	@Test
	def void testIsNotA_02() {
		'''
		 uri "http://sadl.org/gh633b.sadl" alias gh633b.
		 
		 B is a class.
		 C is a class.
		 D is a class.
		 
		 Rule r:
		 if x is a B and x is not a C
		 then x is a D.
		 
		 Ask: select x where x is a B and x is not a C.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(cmds !== null)
			
			for (cmd : cmds) {
				println(cmd.toString)
				if (cmd instanceof Query) {
					val query = getTranslator(processor).translateQuery(jenaModel, "http://sadl.org/gh633b.sadl", cmd as Query)
					println(query)
					val expected = "select ?x where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/gh633b.sadl#B> . FILTER (!EXISTS { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/gh633b.sadl#C> })}"
					assertTrue(processor.compareTranslations(expected.trim, query.toString.trim))
				}
			}
		]
	}
	
	@Test
	def void testIsNotA_03() {
		'''
		 uri "http://sadl.org/gh633b.sadl" alias gh633b.
		 
		 B is a class.
		 C is a class.
		 D is a class.
		 
		 Rule r:
		 if x is a B and x is not a C
		 then x is a D.
		 
		 Ask: select x where x is not a C and x is a B.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(cmds !== null)
			
			for (cmd : cmds) {
				println(cmd.toString)
				if (cmd instanceof Query) {
					val query = getTranslator(processor).translateQuery(jenaModel, "http://sadl.org/gh633b.sadl", cmd as Query)
					println(query)
					val expected = "select ?x where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/gh633b.sadl#B> . FILTER (!EXISTS { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://sadl.org/gh633b.sadl#C> })}"
					assertTrue(processor.compareTranslations(expected.trim, query.toString.trim))
				}
			}
		]
	}
	
	@Test
	def void testEvaluateSadlEquation_01() {
		 val sadlModel1 = '''
			 uri "http://sadl.org/JavaExternal.sadl" alias javaexternal.
			 
			 External min(decimal n1, decimal n2) returns decimal : "java.lang.Math.min".
			 
			 Expr: min(2,3).
			 
			 Rule testRule: then print(min(2,3)).
 		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
 			assertNotNull(rules)
 			assertTrue(rules.size == 1)
 			val rule = getTranslator(processor).translateRule(jenaModel, "http://sadl.org/JavaExternal.sadl", rules.get(0))
 			println(rule.toString)
  			assertEquals("[testRule: evaluateSadlEquation('http://sadl.org/JavaExternal.sadl#min'^^http://www.w3.org/2001/XMLSchema#string, 2, 3, ?v0) -> print(?v0)]", rule.toString)
 		]
		
	}
	
	@Test
	def void testEvaluateSadlEquation_02() {
		// this tests the processing of an ellipsis without a type, meaning any number of arguments of any type
		 val sadlModel1 = '''
			 uri "http://sadl.org/StringFormat.sadl" alias stringformat.
			 
			 External formatString(string fmt, ...) returns string : "java.lang.String.format".
			 
			 Expr: formatString("name is %s", "sonoo").
			 Expr: formatString("value is %f",32.33434). 
			 Expr: formatString("value is %32.12f",32.33434).  
			 Expr: formatString("value is %32.12f != %f", 32.33434, 23.456).
			 
			 TestClass is a class described by formatedString with values of type string.
			 
			 Rule R1: if x is a TestClass then formatedString of x is formatString("value is %32.12f != %f", 32.33434, 23.456).
 		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
 			assertNotNull(rules)
 			assertTrue(rules.size == 1)
 			val rule = getTranslator(processor).translateRule(jenaModel, "http://sadl.org/JavaExternal.sadl", rules.get(0))
 			println(rule.toString)
// 			assertEquals(rule.toString, "[testRule: evaluateSadlEquation('http://sadl.org/JavaExternal.sadl#min'^^http://www.w3.org/2001/XMLSchema#string, 2, 3, ?v0) -> print(?v0)]")
 		]
		
	}

	@Test
	def void testEvaluateSadlEquation_03() {
		// this tests the processing of a typed ellipsis, meaning any number of arguments of the specified type
		 val sadlModel1 = '''
			 uri "http://sadl.org/StringFormat.sadl" alias stringformat.
			 
			 External formatString(string fmt, float ... vals) returns string : "java.lang.String.format".
			 
			 Expr: formatString("name is %s", "sonoo").
			 Expr: formatString("value is %f",32.33434). 
			 Expr: formatString("value is %32.12f",32.33434).  
			 Expr: formatString("value is %32.12f != %f", 32.33434, 23.456).
			 
			 TestClass is a class described by formatedString with values of type string.
			 
			 Rule R1: if x is a TestClass then formatedString of x is formatString("value is %32.12f != %f", 32.33434, 23.456).
 		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
 			var errorFound = false;
 			for (issue : issues) {
 				println(issue.toString)
 				if (issue.toString.startsWith("ERROR:java.lang.NumberFormatException: For input string: \"sonoo\"")) {
 					errorFound = true;
 				}
 			}
 			assertTrue(errorFound)
 			assertNotNull(rules)
 			assertTrue(rules.size == 1)
 			val rule = getTranslator(processor).translateRule(jenaModel, "http://sadl.org/JavaExternal.sadl", rules.get(0))
 			println(rule.toString)
 			assertEquals(rule.toString, "[R1: (?x rdf:type http://sadl.org/StringFormat.sadl#TestClass), evaluateSadlEquation('http://sadl.org/StringFormat.sadl#formatString'^^http://www.w3.org/2001/XMLSchema#string, 'value is %32.12f != %f'^^http://www.w3.org/2001/XMLSchema#string, 32.33434, 23.456, ?v0) -> (?x http://sadl.org/StringFormat.sadl#formatedString ?v0)]")
 		]
		
	}
}
