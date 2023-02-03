/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.google.inject.Inject
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import static org.junit.Assert.*

@RunWith(XtextRunner)
//@InjectWith(RequirementsInjectorProvider)
class SadlModelProcessorCircularImportsTest extends AbstractSADLModelProcessorTest {
	
	@Inject ValidationTestHelper validationTestHelper
	
	@Test
	def void testRequirement_Model() {
		val sadlModel1 = '''
			 uri "http://sadl.org/model1.sadl" alias model1.
			 
			 import "http://sadl.org/model2.sadl".
 		'''.sadl
		val sadlModel2 = '''
			 uri "http://sadl.org/model2.sadl" alias model2.
			 
			 import "http://sadl.org/model1.sadl".
 		'''.sadl
		val issues1 = validationTestHelper.validate(sadlModel1)
		val issues2 = validationTestHelper.validate(sadlModel2)
		if (issues1 !== null) {
			for (issue: issues1) {
				System.err.println(issue.toString)
			}
		}
		if (issues2 !== null) {
			for (issue: issues2) {
				System.err.println(issue.toString)
			}
		}
		assertTrue(issues1 !== null && issues1.size > 0)
		assertTrue(issues2 !== null && issues2.size > 0)
		assertTrue(issues1.get(0).toString.contains("Dependency cycle was detected"))
		assertTrue(issues2.get(0).toString.contains("Dependency cycle was detected"))
		
	}
	
}
