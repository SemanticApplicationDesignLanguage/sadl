package com.ge.research.sadl.ui.test.inference;

import com.ge.research.sadl.ui.tests.AbstractSadlPlatformTest
import org.junit.Test

import static com.ge.research.sadl.ui.tests.GeneratedOutputFormat.*
import java.nio.file.Paths
import com.ge.research.sadl.jena.UtilsForJena
import com.ge.research.sadl.reasoner.utils.SadlUtils
import java.io.File
import com.ge.research.sadl.reasoner.SadlCommandResult
import com.ge.research.sadl.reasoner.ResultSet
import com.ge.research.sadl.preferences.SadlPreferences
import org.eclipse.xtext.preferences.PreferenceKey
import org.eclipse.xtext.diagnostics.Severity
import java.util.List
import com.ge.research.sadl.reasoner.ConfigurationItem
import org.junit.Ignore

class TestSwiProlog extends AbstractSwiPrologTest {

static val LIKES = '''
		 uri "http://sadl.org/test.sadl" alias test.
		 
		 Person is a class described by likes with values of type Food.
		 Food is a class.
		 {indian, mild, chinese, italian} are types of Food.
		 
		 Sam is a Person.
		 {dahl,  curry, tandoori, kurma} are instances of indian.
		 {chop_suey, chow_mein, sweet_and_sour} are instances of chinese.
		 {pizza, spagetti} are instances of italian.
		 
		 {dahl, tandoori, kurma} are instances of mild.
		 
		 chips is a Food.
		 
		 Rule R1: if x is a mild and x is a indian then Sam likes x.
		 Rule R2: if x is a chinese then Sam likes x.
		 Rule R3: if x is an italian then Sam likes x.
		 Rule R4: if x is chips then Sam likes x. 
		 
		 Ask: select x where x is an indian and x is a mild.
		 Ask: select x where Sam likes x.
	'''
	
	@Test
	def void testLikes() {
		if (!canRunSwiProlog) {
			return
		}
		val sfname = 'Likes.sadl'
		createFile(sfname, LIKES)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Likes.sadl', OWL) [
			println(it)
		]
		val content = getPrologFileContent("Likes.pl")
		println(content)
		assertEquals(":- rdf_load('SadlBaseModel.owl').
:- rdf_load('SadlImplicitModel.owl').
:- consult('SadlImplicitModel.pl').
:- rdf_load('SadlBuiltinFunctions.owl').
:- consult('SadlBuiltinFunctions.pl').
holds('http://sadl.org/test.sadl#likes', 'http://sadl.org/test.sadl#Sam', PVx) :- holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/test.sadl#mild'), holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/test.sadl#indian').
holds('http://sadl.org/test.sadl#likes', 'http://sadl.org/test.sadl#Sam', PVx) :- holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/test.sadl#chinese').
holds('http://sadl.org/test.sadl#likes', 'http://sadl.org/test.sadl#Sam', PVx) :- holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/test.sadl#italian').
holds('http://sadl.org/test.sadl#likes', 'http://sadl.org/test.sadl#Sam', 'http://sadl.org/test.sadl#chips').
".trim(),content.trim())
//		assertGeneratedOutputFor('Likes.sadl', PL) [
//			println(it)
//		]
		assertInferencer(sfname, null, null) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof ResultSet)
				if (idx == 0) {
					assertEquals("SADL Command Result:
  select PVx where holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/test.sadl#indian'), holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/test.sadl#mild')
  \"PVx\"
  \"http://sadl.org/test.sadl#tandoori\"
  \"http://sadl.org/test.sadl#dahl\"
  \"http://sadl.org/test.sadl#kurma\"
".trim, scr.toString.trim)
				}
				if (idx == 1) {
					assertEquals("SADL Command Result:
  select PVx where holds('http://sadl.org/test.sadl#likes', 'http://sadl.org/test.sadl#Sam', PVx)
  \"PVx\"
  \"http://sadl.org/test.sadl#tandoori\"
  \"http://sadl.org/test.sadl#dahl\"
  \"http://sadl.org/test.sadl#kurma\"
  \"http://sadl.org/test.sadl#sweet_and_sour\"
  \"http://sadl.org/test.sadl#chow_mein\"
  \"http://sadl.org/test.sadl#chop_suey\"
  \"http://sadl.org/test.sadl#spagetti\"
  \"http://sadl.org/test.sadl#pizza\"
  \"http://sadl.org/test.sadl#chips\"
".trim, scr.toString.trim)
				}
				idx++
			}
			assertTrue(idx > 0)
		];

	}

static val SHAPES = '''
uri "http://sadl.org/Shapes/Shapes" alias shape. 

Shape is a class described by area with values of type float.
'''

static val RECTANGLE = '''
uri "http://sadl.org/Shapes/Rectangle" alias rect version "$Revision:$ Last modified on   $Date:$". 

import "http://sadl.org/Shapes/Shapes". 

Rectangle is a type of Shape,
	described by height with values of type float,
	described by width with values of type float.
'''

static val RULE1 = '''
uri "http://sadl.org/Shapes/Rule" alias rule version "$Revision:$ Last modified on   $Date:$". 

import "http://sadl.org/Shapes/Rectangle". 

Rule AreaOfRect: if x is a Rectangle then area of x is height of x * width of x .

«««Rule AreaOfRect2: then area of a Rectangle = height of the Rectangle * width of the Rectangle.
'''

static val RULE2 = '''
uri "http://sadl.org/Shapes/Rule" alias rule version "$Revision:$ Last modified on   $Date:$". 

import "http://sadl.org/Shapes/Rectangle". 

«««Rule AreaOfRect: if x is a Rectangle then area of x is height of x * width of x .

Rule AreaOfRect2: then area of a Rectangle = height of the Rectangle * width of the Rectangle.
'''

static val TEST = '''
uri "http://sadl.org/Shapes/Test" alias test version "$Revision:$ Last modified on   $Date:$".

import "http://sadl.org/Shapes/Rule". 

MyRect is a Rectangle with height 2.5, with width 5.5.

Test: area of MyRect is 13.75 .
 
Ask: select ar where MyRect has area ar.
'''

	@Test
	def void testShapes1() {
		if (!canRunSwiProlog) {
			return
		}
		val sfname = 'Shapes.sadl'
		createFile(sfname, SHAPES)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Shapes.sadl', OWL) [
			println(it)
		]
		val rectfname = 'Rectangle.sadl'
		createFile(rectfname, RECTANGLE)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Rectangle.sadl', OWL) [
			println(it)
		]
		val r1fname = 'Rule1.sadl'
		createFile(r1fname, RULE1)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Rule1.sadl', OWL) [
			println(it)
		]
		val content = getPrologFileContent("Rule1.pl")
		println(content)
		assertEquals(":- rdf_load('SadlBaseModel.owl').
:- rdf_load('SadlImplicitModel.owl').
:- consult('SadlImplicitModel.pl').
:- rdf_load('SadlBuiltinFunctions.owl').
:- consult('SadlBuiltinFunctions.pl').
:- rdf_load('Rectangle.owl').
:- consult('Rectangle.pl').
holds('http://sadl.org/Shapes/Shapes#area', PVx, PVv2) :- holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVx, 'http://sadl.org/Shapes/Rectangle#Rectangle'), holds('http://sadl.org/Shapes/Rectangle#height', PVx, literal(type('http://www.w3.org/2001/XMLSchema#float',PV0))), atom_number(PV0,PVv0), holds('http://sadl.org/Shapes/Rectangle#width', PVx, literal(type('http://www.w3.org/2001/XMLSchema#float',PV1))), atom_number(PV1,PVv1), PVv2 is PVv0 * PVv1.
".trim(),content.trim())
		val testfname = 'Test.sadl'
		createFile(testfname, TEST)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Test.sadl', OWL) [
			println(it)
		]

		assertInferencer(testfname, null, null) [
			var idx = 0
			for (scr : it) {
				if (scr !== null) {
					println(scr.toString)
					if (scr instanceof SadlCommandResult) {
						if (idx == 0) {
							assertEquals("SADL Command Result:
  rdf(test:MyRect, shape:area, 13.75)".trim, scr.toString.trim)
						}
						if (idx == 1) {
							assertEquals("SADL Command Result:
  select PVar where holds('http://sadl.org/Shapes/Shapes#area', 'http://sadl.org/Shapes/Test#MyRect', PVar)
  \"PVar\"
  \"13.75\"".trim, scr.toString.trim)
						}
						idx++
					}
				}
			}
		]
	}
	
	@Test
	def void testShapes2() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		val sfname = 'Shapes.sadl'
		createFile(sfname, SHAPES)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Shapes.sadl', OWL) [
			println(it)
		]
		val rectfname = 'Rectangle.sadl'
		createFile(rectfname, RECTANGLE)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Rectangle.sadl', OWL) [
			println(it)
		]
		val r1fname = 'Rule2.sadl'
		createFile(r1fname, RULE2)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Rule2.sadl', OWL) [
			println(it)
		]
		val content = getPrologFileContent("Rule2.pl")
		println(content)
		assertEquals(":- rdf_load('SadlBaseModel.owl').
:- rdf_load('SadlImplicitModel.owl').
:- consult('SadlImplicitModel.pl').
:- rdf_load('SadlBuiltinFunctions.owl').
:- consult('SadlBuiltinFunctions.pl').
:- rdf_load('Rectangle.owl').
:- consult('Rectangle.pl').
holds('http://sadl.org/Shapes/Shapes#area', PVv0, PVv3) :- holds('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', PVv0, 'http://sadl.org/Shapes/Rectangle#Rectangle'), holds('http://sadl.org/Shapes/Rectangle#height', PVv0, literal(type('http://www.w3.org/2001/XMLSchema#float',PV0))), atom_number(PV0,PVv1), holds('http://sadl.org/Shapes/Rectangle#width', PVv0, literal(type('http://www.w3.org/2001/XMLSchema#float',PV1))), atom_number(PV1,PVv2), PVv3 is PVv1 * PVv2.
".trim(),content.trim())
		val testfname = 'Test.sadl'
		createFile(testfname, TEST)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('Test.sadl', OWL) [
			println(it)
		]

		assertInferencer(testfname, null, null) [
			var idx = 0
			for (scr : it) {
				if (scr !== null) {
					println(scr.toString)
					if (scr instanceof SadlCommandResult) {
						if (idx == 0) {
							assertEquals("SADL Command Result:
  rdf(test:MyRect, shape:area, 13.75)".trim, scr.toString.trim)
						}
						if (idx == 1) {
							assertEquals("SADL Command Result:
  select PVar where holds('http://sadl.org/Shapes/Shapes#area', 'http://sadl.org/Shapes/Test#MyRect', PVar)
  \"PVar\"
  \"13.75\"".trim, scr.toString.trim)
						}
						idx++
					}
				}
			}
		]
	}
	
	@Ignore
	@Test
	def void testUnittedQuantityInRule_01() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
			 
			 Shape is a class described by area with values of type UnittedQuantity.
			 
			 Rectangle is a class described  by height with values of type UnittedQuantity,
			 	described by width with values of type UnittedQuantity.
			 	
			 MyRect is a Rectangle with width 4.0 ft, with height 2.2 ft.
			 	
			 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
			 
			 Ask: select s, ar where s is a Rectangle and s has area ar.
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
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			val errors = issues.filter[severity === Severity.ERROR]
			assertTrue(errors.size == 0)
		]
		assertGeneratedOutputFor('JavaExternal.sadl', OWL) [
			println(it)
		]
		val content = getPrologFileContent("JavaExternal.pl")
		println(content)

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM_RDFS")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val errs = (scr as SadlCommandResult).errors
				if (errs !== null) {
					for (err : errs) {
						println(err.toString)
					}
				}
				val tr = (scr as SadlCommandResult).results
				if (tr !== null) {
					println(tr.toString)
				}
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"s\",\"ar\"
\"http://sadl.org/ImpliedPropertiesInRule.sadl#MyRect\",8.8 \"ft*ft\"", (tr as ResultSet).toString.trim)
			}
		];
	}
		
}
