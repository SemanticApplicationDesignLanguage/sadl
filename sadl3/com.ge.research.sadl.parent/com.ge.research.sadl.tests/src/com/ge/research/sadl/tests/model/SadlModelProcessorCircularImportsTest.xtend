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
//		sadlModel1.assertNoErrors
//		sadlModel2.assertNoErrors
		val issues1 = validationTestHelper.validate(sadlModel1)
		val issues2 = validationTestHelper.validate(sadlModel2)
//		val sprocessor1 = getReqProcessor(sadlModel1)
//		val List<Issue> issues1= newArrayList
//		sprocessor1.onValidate(sadlModel1, new ValidationAcceptor([issues1 += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(sadlModel1)))
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
	}
	
}
