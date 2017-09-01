/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.scoping.TestScopeProvider
import com.ge.research.sadl.tests.AbstractSADLParsingTest
import com.google.common.base.Stopwatch
import com.google.inject.Inject
import java.util.concurrent.TimeUnit
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
class SadlModelProcessorTestAmbiguousNames extends AbstractSADLParsingTest {
	
	@Inject ValidationTestHelper validationTestHelper
	
	@Test
	def void testPerformance() {
		val models = 10
		val classesPerModel = 10
		for (i : 0..<models) {
			'''
				uri "http://sadl.org/Names«i».sadl" alias Names«i».
				
				«FOR z : 0..<i»
					import "http://sadl.org/Names«z».sadl".
				«ENDFOR»
				
				«FOR j : 0..<classesPerModel»
					RClass«i»_«j» is a class.
					Class«i»_«j» is a class described by oprop1 with values of type RClass«i»_«j»,
					 	described by iprop1 with values of type int,
					 	described by fprop1 with values of type float,
					 	described by sprop1 with values of type string.
				«ENDFOR»
			'''.sadl
		}
		
		val sadlModel3 = '''
			 uri "http://sadl.org/Instances.sadl".
			 
			 «FOR z : 0..<models»
			 	import "http://sadl.org/Names«z».sadl".
			 «ENDFOR»
			 
			 «FOR i : 0..<models»
			 	«FOR j : 0..<classesPerModel»
			 		R«i»_«j» is a RClass«i»_«j».
			 		C«i»_«j» is a Class«i»_«j» with oprop1 R1, with iprop1 3, with fprop1 4.5, with sprop1 "hello world".
				 «ENDFOR»
			 «ENDFOR»
			 

		'''.sadl
		val sw = Stopwatch.createStarted
		EcoreUtil2.resolveAll(sadlModel3.resourceSet)
		println(sw.elapsed(TimeUnit.MILLISECONDS))
	}
	
	@Test
	def void testAmbiguousDetection() {
		val models = 2
		val classesPerModel = 1
		for (i : 0..<models) {
			val s = '''
				uri "http://sadl.org/Names«i».sadl" alias Names«i».
				
				«FOR z : 0..<i»
					import "http://sadl.org/Names«z».sadl".
				«ENDFOR»
				
				«FOR j : 0..<classesPerModel»
					RClass«i»_«j» is a class.
				«ENDFOR»
			''';
			TestScopeProvider.registerResource(s.sadl, true)
		}
		
		val sadlModel3 = '''
			 uri "http://sadl.org/Instances.sadl".
			 
			 «FOR z : 0..<models»
			 	import "http://sadl.org/Names«z».sadl".
			 «ENDFOR»
			 
			 «FOR i : 0..<models»
			 	«FOR j : 0..<classesPerModel»
			 		R«i»_«j» is a RClass«i»_«j».
				 «ENDFOR»
			 «ENDFOR»
			 

		'''.sadl;
		TestScopeProvider.registerResource(sadlModel3, true)
		val issues3 = validationTestHelper.validate(sadlModel3)
		assertTrue(issues3.nullOrEmpty)
	}

	@Test
	def void testRequirement_Model() {
		val sadlModel1 = '''
			 uri "http://sadl.org/Names1.sadl" alias Names1.
			 
			 RClass1 is a class.
			 Class1 is a class described by oprop1 with values of type RClass1.
		'''.sadl
		val sadlModel2 = '''
			 uri "http://sadl.org/Names2.sadl" alias Names2.
			 
			 RClass1 is a class.
			 Class1 is a class described by oprop1 with values of type RClass1.
		'''.sadl
		val sadlModel3 = '''
			 uri "http://sadl.org/Names3.sadl" alias Names3.
			 
			 import "http://sadl.org/Names1.sadl".
			 import "http://sadl.org/Names2.sadl".
			 
			 R1 is a RClass1.
			 C1 is a Class1 with oprop1 R1.
		'''.sadl
		val sadlModel4 = '''	
			 uri "http://sadl.org/Names.sreq" alias Names.
			 
			 import "http://sadl.org/Names3.sadl".
		'''.sadl
		sadlModel1.assertNoErrors
		sadlModel2.assertNoErrors
		TestScopeProvider.registerResource(sadlModel3, true)
		val issues3 = validationTestHelper.validate(sadlModel3)
		assertNotNull(issues3)
		assertTrue(issues3.size() > 0)
		for (issue: issues3) {
			System.err.println(issue.toString)
		}
		sadlModel4.assertNoErrors
	}
	
	@Test
	def void testRequirement_ModelMinimal() {
		val sadlModel1 = '''
			 uri "http://sadl.org/Names1.sadl" alias Names1.
			 
			 RClass1 is a class.
			 Class1 is a class described by oprop1 with values of type RClass1,
			 	described by iprop1 with values of type int,
			 	described by fprop1 with values of type float,
			 	described by sprop1 with values of type string.
			 	
			 RClass2 is a class.
			 Class2 is a class described by oprop2 with values of type RClass2,
			 	described by iprop2 with values of type int,
			 	described by fprop2 with values of type float,
			 	described by sprop2 with values of type string.
			 	
		'''.sadl
		val sadlModel2 = '''
			 uri "http://sadl.org/Names2.sadl" alias Names2.
			 
			 RClass1 is a class.
			 Class1 is a class described by oprop1 with values of type RClass1,
			 	described by iprop1 with values of type int,
			 	described by fprop1 with values of type float,
			 	described by sprop1 with values of type string.
			 	
			 RClass2 is a class.
			 Class2 is a class described by oprop2 with values of type RClass2,
			 	described by iprop2 with values of type int,
			 	described by fprop2 with values of type float,
			 	described by sprop2 with values of type string.
			 	
		'''.sadl
		val sadlModel3 = '''
			 uri "http://sadl.org/Names3.sadl" alias Names3.
			 
			 import "http://sadl.org/Names1.sadl".
			 import "http://sadl.org/Names2.sadl".
			 
			 R1 is a RClass1.
			 C1 is a Class1 with oprop1 R1, with iprop1 3, with fprop1 4.5, with sprop1 "hello world".
			  
			 R2 is a RClass2.
			 C2 is a Class2 with oprop2 R2, with iprop2 3, with fprop2 4.5, with sprop2 "hello world".
			  
		'''.sadl
		val sadlModel4 = '''	
			 uri "http://sadl.org/Names4.sreq" alias Names4.
			 
			 import "http://sadl.org/Names3.sadl".
			 
			 R1 is a RClass1.
			 C1 is a Class1 with oprop1 R1, with iprop1 3, with fprop1 4.5, with sprop1 "hello world".
			  
			 R2 is a RClass2.
			 C2 is a Class2 with oprop2 R2, with iprop2 3, with fprop2 4.5, with sprop2 "hello world".
			 
			 Ask: select p, v where R1 has p v.
		'''.sadl
		sadlModel1.assertNoErrors
		sadlModel2.assertNoErrors
		sadlModel3.assertNoErrors
		sadlModel4.assertNoErrors
		val issues3 = validationTestHelper.validate(sadlModel3)
		assertNotNull(issues3)
		for (issue: issues3) {
			System.err.println(issue.toString)
		}
	}
	
}
