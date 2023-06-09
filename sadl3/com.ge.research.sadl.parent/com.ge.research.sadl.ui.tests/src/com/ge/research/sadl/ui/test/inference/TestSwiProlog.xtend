package com.ge.research.sadl.ui.test.inference;

import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.reasoner.ResultSet
import com.ge.research.sadl.reasoner.SadlCommandResult
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

import static com.ge.research.sadl.ui.tests.GeneratedOutputFormat.*
import com.ge.research.sadl.model.gp.TestResult
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
	
static val UQTest1 = '''
 uri "http://sadl.org/ImpliedPropertiesInRule.sadl" alias impliedpropertiesinrule.
 
 Shape is a class described by area with values of type UnittedQuantity.
 
 Rectangle is a class described  by height with values of type UnittedQuantity,
 	described by width with values of type UnittedQuantity.
 	
 MyRect is a Rectangle with width 4.0 ft, with height 2.2 ft.
 	
 Rule R1: if x is a Rectangle then area of x is height of x * width of x.
 
 Ask: select s, ar where s is a Rectangle and s has area ar.
'''

	@Test
	def void testUQ1() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'UQTest1.sadl'
		createFile(sfname, UQTest1)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('UQTest1.sadl', OWL) [
			println(it)
		]
		val content = getPrologFileContent("UQTest1.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
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
				println((tr as ResultSet).toString)
//				if (idx == 0) {
//					assertEquals("".trim, scr.toString.trim)
//				}
//				if (idx == 1) {
//					assertEquals("".trim, scr.toString.trim)
//				}
				idx++
			}
			assertTrue(idx > 0)
		];

	}

static val DirectCreateUnittedQuantityReference = '''
 uri "http://sadl.org/test2.sadl"  alias test2.
 
 Timespan is a class described by ^duration with values of type UnittedQuantity.
 
 
 Rule R1: if x is a Timespan then ^duration of x is createUnittedQuantity(25, "miles").
 
 Rule R2: if x is a Timespan and dv is 25 and du is "miles" and uq is createUnittedQuantity(25, "miles") then ^duration of x is uq.
 
 TS25miles is a Timespan.
 
 Ask: select t where t is a Timespan.
 
 Ask: select uq where uq is a UnittedQuantity.
 
 Ask: select t, p, v where t is a Timespan and t has p v.
 '''

	@Ignore
	@Test
	def void testDirectCreateUnittedQuantityReference() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'DirectCreateUnittedQuantityReference.sadl'
		createFile(sfname, DirectCreateUnittedQuantityReference)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor('DirectCreateUnittedQuantityReference.sadl', OWL) [
			println(it)
		]
		val content = getPrologFileContent("DirectCreateUnittedQuantityReference.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
//		assertGeneratedOutputFor('Likes.sadl', PL) [
//			println(it)
//		]
		assertInferencer(sfname, null, null) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
//				assertTrue(tr instanceof ResultSet)
				if (tr instanceof ResultSet) {
					println((tr as ResultSet).toString)
				}
				else {
					println(scr.toString)
				}
				if (idx == 0) {
					assertNotNull(tr)
					assertEquals("\"PVt\"
\"http://sadl.org/test2.sadl#TS25miles\"".trim, tr.toString.trim)
				}
				if (idx == 1) {
					assertNotNull(tr)
					assertEquals("\"PVt\",\"PVp\",\"PVv\"
\"http://sadl.org/test2.sadl#TS25miles\",\"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\",\"http://sadl.org/test2.sadl#Timespan\"
\"http://sadl.org/test2.sadl#TS25miles\",\"http://sadl.org/test2.sadl#duration\",25.0 \"miles\"
\"http://sadl.org/test2.sadl#TS25miles\",\"http://sadl.org/test2.sadl#duration\",25.0 \"miles\"".trim, tr.toString.trim)
				}
				idx++
			}
			assertTrue(idx > 0)
		];

	}

static val LIGHT_TRAVEL = '''
 uri "http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl" alias mileslighttravelsperunitoftime.
 
 Trip is a class, described by timeTraveled with values of type UnittedQuantity,
 	described by distanceTraveled with values of type UnittedQuantity,
 	described by speed with values of type UnittedQuantity.
 	
 Rule DistanceTraveled3: 
 	if tr is a Trip and 
 	tr has speed s and 
 	s has ^value sv and
 	s has unit su and 
 	su is "m/s" and
 	tr has timeTraveled t and
 	t has ^value tv and
 	t has unit tu and
 	tu is "hrs" and
 	dv = sv * tv * 60.0 * 60.0 and
 	du = combineUnits("*", su, tu) and
 	uq = createUnittedQuantity(dv, du)
 	then tr has distanceTraveled uq. 
 	
 Trip1 is a Trip, with speed 299792458.0 "m/s", with timeTraveled 10000000000000.0 hrs.
 Trip2 is a Trip, with speed 2.997924580E8 "m/s", with timeTraveled 1.0E13 hrs.
 	
 Ask: select tr, dt where tr is a Trip and tr has distanceTraveled dt.
'''

	@Test
	def void testUQLightTravel() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'MilesLightTravelsPerUnitOfTime.sadl'
		createFile(sfname, LIGHT_TRAVEL)
		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor(sfname, OWL) [
			println(it)
		]
		val content = getPrologFileContent("MilesLightTravelsPerUnitOfTime.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
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
				println((tr as ResultSet).toString)
				assertEquals("\"PVtr\",\"PVdt\"
\"http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl#Trip1\",1.0792528E25 \"m/s*hrs\"
\"http://sadl.org/MilesLightTravelsPerUnitOfTime.sadl#Trip2\",1.0792528E25 \"m/s*hrs\"".trim, tr.toString.trim)
				idx++
			}
			assertTrue(idx > 0)
		];

	}
	
	static val TEST2 = '''
 uri "http://sadl.org/test2.sadl"  alias test2.
 
 Timespan is a class described by ^duration with values of type UnittedQuantity.
 
 
// Rule R1: if x is a Timespan then ^duration of x is createUnittedQuantity(25, "miles").
 
 Rule R2: if x is a Timespan and dv is 25 and du is "miles" and uq is createUnittedQuantity(25, "miles") then ^duration of x is uq.
 
 TS25miles is a Timespan.
 
 Ask: select t, d where t is a Timespan and t has ^duration d.
	'''

	@Test
	def void testTest2a() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'test2a.sadl'
		createFile(sfname, TEST2)
//		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor(sfname, OWL) [
			println(it)
		]
		val content = getPrologFileContent("test2a.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
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
				println((tr as ResultSet).toString)
//				if (idx == 0) {
//					assertEquals("".trim, scr.toString.trim)
//				}
//				if (idx == 1) {
//					assertEquals("".trim, scr.toString.trim)
//				}
				idx++
			}
			assertTrue(idx > 0)
		];

	}
	
	static val TEST2b = '''
 uri "http://sadl.org/test2.sadl"  alias test2.
 
 Timespan is a class described by ^duration with values of type UnittedQuantity.
 
 Rule R2: if x is a Timespan and dv is 25 and du is "miles" and uq is createUnittedQuantity(dv, du) then ^duration of x is uq.
 
 TS25miles is a Timespan.
 
 Ask: select t, d where t is a Timespan and t has ^duration d.
	'''

	@Test
	def void testTest2() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'test2b.sadl'
		createFile(sfname, TEST2b)
//		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor(sfname, OWL) [
			println(it)
		]
		val content = getPrologFileContent("test2b.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
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
				println((tr as ResultSet).toString)
//				if (idx == 0) {
//					assertEquals("".trim, scr.toString.trim)
//				}
//				if (idx == 1) {
//					assertEquals("".trim, scr.toString.trim)
//				}
				idx++
			}
			assertTrue(idx > 0)
		];

	}
	
	static val TEST2c = '''
 uri "http://sadl.org/test2.sadl"  alias test2.
 
 Timespan is a class described by ^duration with values of type UnittedQuantity.
 
 Rule R1: if x is a Timespan then ^duration of x is createUnittedQuantity(25, "miles").
 
 TS25miles is a Timespan.
 
 Ask: select t, d where t is a Timespan and t has ^duration d.
	'''

	@Test
	def void testTest2c() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'test2c.sadl'
		createFile(sfname, TEST2c)
//		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor(sfname, OWL) [
			println(it)
		]
		val content = getPrologFileContent("test2c.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
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
				println((tr as ResultSet).toString)
//				if (idx == 0) {
//					assertEquals("".trim, scr.toString.trim)
//				}
//				if (idx == 1) {
//					assertEquals("".trim, scr.toString.trim)
//				}
				idx++
			}
			assertTrue(idx > 0)
		];

	}
	
	static val TestCombineUnits='''
 uri "http://sadl.org/TestCombineUnits.sadl" alias testcombineunits.
 
 Thing is a class described by unit1 with values of type string, 
 	described by unit2 with values of type string,
 	described by combined with values of type string.
 	
 Rule CU: if x is a Thing and x has unit1 u1 and x has unit2 u2 and cu is combineUnits('*', u1, u2) 
 	then x has combined cu.
 	
 MyThing is a Thing with unit1 "foot", with unit2 "lbs". 	
 	
 Test: MyThing has combined "foot*lbs".
	'''
	
	@Test
	def void testCombineUnits() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'TestCombineUnits.sadl'
		createFile(sfname, TestCombineUnits)
//		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor(sfname, OWL) [
			println(it)
		]
		val content = getPrologFileContent("TestCombineUnits.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
//		assertGeneratedOutputFor('Likes.sadl', PL) [
//			println(it)
//		]
		assertInferencer(sfname, null, null) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
				idx++
			}
			assertTrue(idx > 0)
		];

	}
	
	static val TestCreateUnittedQuantity='''
  uri "http://sadl.org/TestCreateUnittedQuantity.sadl" alias testcreateunittedquantity.
  
  Thing is a class described by tunit with values of type string, 
  	described by tvalue with values of type decimal,
  	described by tuq with values of type UnittedQuantity.
  	
  Rule CU: if x is a Thing and x has tunit u and x has tvalue v and uq is createUnittedQuantity(v, u) 
  	then x has tuq uq.
  	
  MyThing is a Thing with tvalue 25, with tunit "ft". 	
  	
  Test: MyThing has tuq 25 "ft".
	'''
	
	@Test
	def void testCreateUnittedQuantity() {
		if (!canRunSwiProlog) {
			return
		}
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'TestCombineUnits.sadl'
		createFile(sfname, TestCreateUnittedQuantity)
//		assertNoErrorsInWorkspace;
		assertGeneratedOutputFor(sfname, OWL) [
			println(it)
		]
		val content = getPrologFileContent("TestCombineUnits.pl")
		println(content)
//		assertEquals("".trim(),content.trim())
//		assertGeneratedOutputFor('Likes.sadl', PL) [
//			println(it)
//		]
		assertInferencer(sfname, null, null) [
			var idx = 0
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
				idx++
			}
			assertTrue(idx > 0)
		];

	}
}
