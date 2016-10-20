/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.OntModel
import java.util.ArrayList
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.Issue
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.eclipse.xtext.validation.CheckMode
import java.util.Map

@RunWith(XtextRunner)
//@InjectWith(RequirementsInjectorProvider)
class SadlModelProcessorTestCircularImports extends AbstractProcessorTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> sadlProcessorProvider
	@Inject IPreferenceValuesProvider preferenceProvider
	
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
		if (issues1 != null) {
			for (issue: issues1) {
				System.err.println(issue.toString)
			}
		}
		if (issues2 != null) {
			for (issue: issues2) {
				System.err.println(issue.toString)
			}
		}
	}
	
}
