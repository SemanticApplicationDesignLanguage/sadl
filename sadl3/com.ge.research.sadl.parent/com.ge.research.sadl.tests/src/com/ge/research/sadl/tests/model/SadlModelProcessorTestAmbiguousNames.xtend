/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.google.common.base.Stopwatch
import com.google.inject.Inject
import java.util.concurrent.TimeUnit
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
class SadlModelProcessorTestAmbiguousNames extends AbstractProcessorTest {
	
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
		val issues3 = validationTestHelper.validate(sadlModel3)
		assertNotNull(issues3)
		assertTrue(issues3.size() > 0)
		for (issue: issues3) {
			System.err.println(issue.toString)
		}
	}

	@Test
	def void testRequirement_Model() {
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
			 	
			 RClass3 is a class.
			 Class3 is a class described by oprop3 with values of type RClass3,
			 	described by iprop3 with values of type int,
			 	described by fprop3 with values of type float,
			 	described by sprop3 with values of type string.
			 	
			 RClass4 is a class.
			 Class4 is a class described by oprop4 with values of type RClass4,
			 	described by iprop4 with values of type int,
			 	described by fprop4 with values of type float,
			 	described by sprop4 with values of type string.
			 	
			 RClass5 is a class.
			 Class5 is a class described by oprop5 with values of type RClass5,
			 	described by iprop5 with values of type int,
			 	described by fprop5 with values of type float,
			 	described by sprop5 with values of type string.
			 	
			 RClass6 is a class.
			 Class6 is a class described by oprop6 with values of type RClass6,
			 	described by iprop6 with values of type int,
			 	described by fprop6 with values of type float,
			 	described by sprop6 with values of type string.
			 	
			 RClass7 is a class.
			 Class7 is a class described by oprop7 with values of type RClass7,
			 	described by iprop7 with values of type int,
			 	described by fprop7 with values of type float,
			 	described by sprop7 with values of type string.
			 	
			 RClass8 is a class.
			 Class8 is a class described by oprop8 with values of type RClass8,
			 	described by iprop8 with values of type int,
			 	described by fprop8 with values of type float,
			 	described by sprop8 with values of type string.
			 	
			 RClass9 is a class.
			 Class9 is a class described by oprop9 with values of type RClass9,
			 	described by iprop9 with values of type int,
			 	described by fprop9 with values of type float,
			 	described by sprop9 with values of type string.
			 	
			 RClass10 is a class.
			 Class10 is a class described by oprop10 with values of type RClass10,
			 	described by iprop10 with values of type int,
			 	described by fprop10 with values of type float,
			 	described by sprop10 with values of type string.
			 	
			 RClass11 is a class.
			 Class11 is a class described by oprop11 with values of type RClass11,
			 	described by iprop11 with values of type int,
			 	described by fprop11 with values of type float,
			 	described by sprop11 with values of type string.
			 	
			 RClass12 is a class.
			 Class12 is a class described by oprop12 with values of type RClass12,
			 	described by iprop12 with values of type int,
			 	described by fprop12 with values of type float,
			 	described by sprop12 with values of type string.
			 	
			 RClass13 is a class.
			 Class13 is a class described by oprop13 with values of type RClass13,
			 	described by iprop13 with values of type int,
			 	described by fprop13 with values of type float,
			 	described by sprop13 with values of type string.
			 	
			 RClass14 is a class.
			 Class14 is a class described by oprop14 with values of type RClass14,
			 	described by iprop14 with values of type int,
			 	described by fprop14 with values of type float,
			 	described by sprop14 with values of type string.
			 	
			 RClass15 is a class.
			 Class15 is a class described by oprop15 with values of type RClass15,
			 	described by iprop15 with values of type int,
			 	described by fprop15 with values of type float,
			 	described by sprop15 with values of type string.
			 	
			 RClass16 is a class.
			 Class16 is a class described by oprop16 with values of type RClass16,
			 	described by iprop16 with values of type int,
			 	described by fprop16 with values of type float,
			 	described by sprop16 with values of type string.
			 	
			 RClass17 is a class.
			 Class17 is a class described by oprop17 with values of type RClass17,
			 	described by iprop17 with values of type int,
			 	described by fprop17 with values of type float,
			 	described by sprop17 with values of type string.
			 	
			 RClass18 is a class.
			 Class18 is a class described by oprop18 with values of type RClass18,
			 	described by iprop18 with values of type int,
			 	described by fprop18 with values of type float,
			 	described by sprop18 with values of type string.
			 	
			 RClass19 is a class.
			 Class19 is a class described by oprop19 with values of type RClass19,
			 	described by iprop19 with values of type int,
			 	described by fprop19 with values of type float,
			 	described by sprop19 with values of type string.
			 	
			 RClass20 is a class.
			 Class20 is a class described by oprop20 with values of type RClass20,
			 	described by iprop20 with values of type int,
			 	described by fprop20 with values of type float,
			 	described by sprop20 with values of type string.
			 	
			 RClass21 is a class.
			 Class21 is a class described by oprop21 with values of type RClass21,
			 	described by iprop21 with values of type int,
			 	described by fprop21 with values of type float,
			 	described by sprop21 with values of type string.
			 	
			 RClass22 is a class.
			 Class22 is a class described by oprop22 with values of type RClass22,
			 	described by iprop22 with values of type int,
			 	described by fprop22 with values of type float,
			 	described by sprop22 with values of type string.
			 	
			 RClass23 is a class.
			 Class23 is a class described by oprop23 with values of type RClass23,
			 	described by iprop23 with values of type int,
			 	described by fprop23 with values of type float,
			 	described by sprop23 with values of type string.
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
			 	
			 RClass3 is a class.
			 Class3 is a class described by oprop3 with values of type RClass3,
			 	described by iprop3 with values of type int,
			 	described by fprop3 with values of type float,
			 	described by sprop3 with values of type string.
			 	
			 RClass4 is a class.
			 Class4 is a class described by oprop4 with values of type RClass4,
			 	described by iprop4 with values of type int,
			 	described by fprop4 with values of type float,
			 	described by sprop4 with values of type string.
			 	
			 RClass5 is a class.
			 Class5 is a class described by oprop5 with values of type RClass5,
			 	described by iprop5 with values of type int,
			 	described by fprop5 with values of type float,
			 	described by sprop5 with values of type string.
			 	
			 RClass6 is a class.
			 Class6 is a class described by oprop6 with values of type RClass6,
			 	described by iprop6 with values of type int,
			 	described by fprop6 with values of type float,
			 	described by sprop6 with values of type string.
			 	
			 RClass7 is a class.
			 Class7 is a class described by oprop7 with values of type RClass7,
			 	described by iprop7 with values of type int,
			 	described by fprop7 with values of type float,
			 	described by sprop7 with values of type string.
			 	
			 RClass8 is a class.
			 Class8 is a class described by oprop8 with values of type RClass8,
			 	described by iprop8 with values of type int,
			 	described by fprop8 with values of type float,
			 	described by sprop8 with values of type string.
			 	
			 RClass9 is a class.
			 Class9 is a class described by oprop9 with values of type RClass9,
			 	described by iprop9 with values of type int,
			 	described by fprop9 with values of type float,
			 	described by sprop9 with values of type string.
			 	
			 RClass10 is a class.
			 Class10 is a class described by oprop10 with values of type RClass10,
			 	described by iprop10 with values of type int,
			 	described by fprop10 with values of type float,
			 	described by sprop10 with values of type string.
			 	
			 RClass11 is a class.
			 Class11 is a class described by oprop11 with values of type RClass11,
			 	described by iprop11 with values of type int,
			 	described by fprop11 with values of type float,
			 	described by sprop11 with values of type string.
			 	
			 RClass12 is a class.
			 Class12 is a class described by oprop12 with values of type RClass12,
			 	described by iprop12 with values of type int,
			 	described by fprop12 with values of type float,
			 	described by sprop12 with values of type string.
			 	
			 RClass13 is a class.
			 Class13 is a class described by oprop13 with values of type RClass13,
			 	described by iprop13 with values of type int,
			 	described by fprop13 with values of type float,
			 	described by sprop13 with values of type string.
			 	
			 RClass14 is a class.
			 Class14 is a class described by oprop14 with values of type RClass14,
			 	described by iprop14 with values of type int,
			 	described by fprop14 with values of type float,
			 	described by sprop14 with values of type string.
			 	
			 RClass15 is a class.
			 Class15 is a class described by oprop15 with values of type RClass15,
			 	described by iprop15 with values of type int,
			 	described by fprop15 with values of type float,
			 	described by sprop15 with values of type string.
			 	
			 RClass16 is a class.
			 Class16 is a class described by oprop16 with values of type RClass16,
			 	described by iprop16 with values of type int,
			 	described by fprop16 with values of type float,
			 	described by sprop16 with values of type string.
			 	
			 RClass17 is a class.
			 Class17 is a class described by oprop17 with values of type RClass17,
			 	described by iprop17 with values of type int,
			 	described by fprop17 with values of type float,
			 	described by sprop17 with values of type string.
			 	
			 RClass18 is a class.
			 Class18 is a class described by oprop18 with values of type RClass18,
			 	described by iprop18 with values of type int,
			 	described by fprop18 with values of type float,
			 	described by sprop18 with values of type string.
			 	
			 RClass19 is a class.
			 Class19 is a class described by oprop19 with values of type RClass19,
			 	described by iprop19 with values of type int,
			 	described by fprop19 with values of type float,
			 	described by sprop19 with values of type string.
			 	
			 RClass20 is a class.
			 Class20 is a class described by oprop20 with values of type RClass20,
			 	described by iprop20 with values of type int,
			 	described by fprop20 with values of type float,
			 	described by sprop20 with values of type string.
			 	
			 RClass21 is a class.
			 Class21 is a class described by oprop21 with values of type RClass21,
			 	described by iprop21 with values of type int,
			 	described by fprop21 with values of type float,
			 	described by sprop21 with values of type string.
			 	
			 RClass22 is a class.
			 Class22 is a class described by oprop22 with values of type RClass22,
			 	described by iprop22 with values of type int,
			 	described by fprop22 with values of type float,
			 	described by sprop22 with values of type string.
			 	
			 RClass23 is a class.
			 Class23 is a class described by oprop23 with values of type RClass23,
			 	described by iprop23 with values of type int,
			 	described by fprop23 with values of type float,
			 	described by sprop23 with values of type string.		
		'''.sadl
		val sadlModel3 = '''
			 uri "http://sadl.org/Names3.sadl" alias Names3.
			 
			 import "http://sadl.org/Names1.sadl".
			 import "http://sadl.org/Names2.sadl".
			 
			 R1 is a RClass1.
			 C1 is a Class1 with oprop1 R1, with iprop1 3, with fprop1 4.5, with sprop1 "hello world".
			  
			 R2 is a RClass2.
			 C2 is a Class2 with oprop2 R2, with iprop2 3, with fprop2 4.5, with sprop2 "hello world".
			  
			 R3 is a RClass3.
			 C3 is a Class3 with oprop3 R3, with iprop3 3, with fprop3 4.5, with sprop3 "hello world".
			  
			 R4 is a RClass4.
			 C4 is a Class4 with oprop4 R4, with iprop4 3, with fprop4 4.5, with sprop4 "hello world".
			  
			 R5 is a RClass5.
			 C5 is a Class5 with oprop5 R5, with iprop5 3, with fprop5 4.5, with sprop5 "hello world".
			  
			 R6 is a RClass6.
			 C6 is a Class6 with oprop6 R6, with iprop6 3, with fprop6 4.5, with sprop6 "hello world".
			  
			 R7 is a RClass7.
			 C7 is a Class7 with oprop7 R7, with iprop7 3, with fprop7 4.5, with sprop7 "hello world".
			  
			 R8 is a RClass8.
			 C8 is a Class8 with oprop8 R8, with iprop8 3, with fprop8 4.5, with sprop8 "hello world".
			  
			 R9 is a RClass9.
			 C9 is a Class9 with oprop9 R9, with iprop9 3, with fprop9 4.5, with sprop9 "hello world".
			  
			 R10 is a RClass10.
			 C10 is a Class10 with oprop10 R10, with iprop10 3, with fprop10 4.5, with sprop10 "hello world".
			  
			 R11 is a RClass11.
			 C11 is a Class11 with oprop11 R11, with iprop11 3, with fprop11 4.5, with sprop11 "hello world".
			  
			 R12 is a RClass12.
			 C12 is a Class12 with oprop12 R12, with iprop12 3, with fprop12 4.5, with sprop12 "hello world".
			  
			 R13 is a RClass13.
			 C13 is a Class13 with oprop13 R13, with iprop13 3, with fprop13 4.5, with sprop13 "hello world".
			  
			 R14 is a RClass14.
			 C14 is a Class14 with oprop14 R14, with iprop14 3, with fprop14 4.5, with sprop14 "hello world".
			  
			 R15 is a RClass15.
			 C15 is a Class15 with oprop15 R15, with iprop15 3, with fprop15 4.5, with sprop15 "hello world".
			  
			 R16 is a RClass16.
			 C16 is a Class16 with oprop16 R16, with iprop16 3, with fprop16 4.5, with sprop16 "hello world".
			  
			 R17 is a RClass17.
			 C17 is a Class17 with oprop17 R17, with iprop17 3, with fprop17 4.5, with sprop17 "hello world".
			  
			 R18 is a RClass18.
			 C18 is a Class18 with oprop18 R18, with iprop18 3, with fprop18 4.5, with sprop18 "hello world".
			  
			 R19 is a RClass19.
			 C19 is a Class19 with oprop19 R19, with iprop19 3, with fprop19 4.5, with sprop19 "hello world".
			  
			 R20 is a RClass20.
			 C20 is a Class20 with oprop20 R20, with iprop20 3, with fprop20 4.5, with sprop20 "hello world".
			  
			 R21 is a RClass21.
			 C21 is a Class21 with oprop21 R21, with iprop21 3, with fprop21 4.5, with sprop21 "hello world".
			  
			 R22 is a RClass22.
			 C22 is a Class22 with oprop22 R22, with iprop22 3, with fprop22 4.5, with sprop22 "hello world".
			  
			 R23 is a RClass23.
			 C23 is a Class23 with oprop23 R23, with iprop23 3, with fprop23 4.5, with sprop23 "hello world".
		'''.sadl
		val sadlModel4 = '''	
			 uri "http://sadl.org/Names.sreq" alias Names.
			 
			 import "http://sadl.org/Names3.sadl".
		'''.sadl
		sadlModel1.assertNoErrors
		sadlModel2.assertNoErrors
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
//		assertTrue(issues3.size() > 0)
		for (issue: issues3) {
			System.err.println(issue.toString)
		}
////		sadlModel4.assertNoErrors
//		val issues4 = validationTestHelper.validate(sadlModel4)
//		assertNotNull(issues4)
////		assertTrue(issues4.size() > 0)
//		for (issue: issues4) {
//			System.err.println(issue.toString)
//		}
	}
	
}
