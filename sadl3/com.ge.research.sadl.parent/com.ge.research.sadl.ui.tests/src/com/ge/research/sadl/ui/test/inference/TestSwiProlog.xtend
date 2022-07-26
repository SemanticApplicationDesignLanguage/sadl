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
		val projectPath = Paths.get(project.locationURI);
		val modelFolderPath = projectPath.resolve(UtilsForJena.OWL_MODELS_FOLDER_NAME);
		val su = new SadlUtils
		val fn = su.fileUrlToFileName(modelFolderPath.toFile.absolutePath) + "/Likes.pl"
		val content = su.fileToString(new File(fn))
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

}
