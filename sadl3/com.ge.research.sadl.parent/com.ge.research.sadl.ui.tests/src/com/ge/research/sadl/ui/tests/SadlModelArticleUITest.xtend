/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptorImpl
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.OntModel
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.preferences.PreferenceKey
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.junit.Test

class SadlModelArticleUITest extends AbstractSadlPlatformTest {
	
	@Inject
	Provider<JenaBasedSadlModelProcessor> processorProvider;
	
	@Test
	def void testArticles_01() {


		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of Circle is Y.
		''').resource.assertValidatesTo [ jenaModel, issues |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.size > 1)
		]

	}
	
	@Test
	def void testArticles_02() {


		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.FALSE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of Circle is Y.
		''').resource.assertValidatesTo [ jenaModel, issues |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(issues.size == 0)
		]

	}

	
	protected def Resource assertValidatesTo(Resource resource, (OntModel, List<Issue>)=>void assertions) {
		val issues = newArrayList;
		issues.addAll(validate(resource));
		val processor = processorProvider.get
		val acceptor = new ValidationAcceptorImpl([issues += it]);
		val preferenceValues = preferenceValuesProvider.getPreferenceValues(resource);
		val context = new ProcessorContext(cancelIndicator, preferenceValues);
		processor.onValidate(resource, acceptor, CheckMode.FAST_ONLY, context);
		assertions.apply(processor.theJenaModel, issues);
		return resource;
	}

}
