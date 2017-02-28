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
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptor
import com.google.common.collect.Iterables
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.OntModel
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.junit.Test

import static org.eclipse.xtext.util.CancelIndicator.NullImpl
import org.eclipse.xtext.preferences.PreferenceKey
import com.ge.research.sadl.preferences.SadlPreferences

/**
 * Mock test case that shows how to use customized preference values in pure JUnit tests.
 * 
 * @author akos.kitta
 */
class SadlIgnoreUnittedQuantitiyTest extends AbstractSadlPlatformTest {

	@Inject
	Provider<JenaBasedSadlModelProcessor> processorProvider;

	@Test
	def void testIgnoreUnitsInSadl() {

		updatePreferences(new PreferenceKey(SadlPreferences.IGNORE_UNITTEDQUANTITIES.id, Boolean.TRUE.toString));

		createFile('OntologyWithUnittedQuantity.sadl', '''
			uri "http://sadl.org/OntologyWithUnittedQuantity.sadl" alias OntologyWithUnittedQuantity.
			
			Person is a class described by gender with a single value of type Gender,
				described by age with values of type UnittedQuantity,
				described by height with values of type UnittedQuantity,
				described by heightPercentile with values of type UnittedQuantity,
				described by weight with values of type UnittedQuantity.
			Gender is a class, can only be one of {Male, Female}.
			Obese is a type of Person.
			
			George is a Person with age 23 years, with weight 165 lbs.
			George has height 70 inches, has heightPercentile 50 "%" .
		''').resource.assertValidatesTo [ jenaModel, issues |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			val ageProperty = jenaModel.getDatatypeProperty("http://sadl.org/OntologyWithUnittedQuantity.sadl#age")
			assertNotNull(ageProperty);
		]
	}

	protected def Resource assertValidatesTo(Resource resource, (OntModel, List<Issue>)=>void assertions) {
		val issues = newArrayList;
		issues.addAll(validator.validate(resource, CheckMode.FAST_ONLY, NullImpl));
		assertTrue('''Expected no validation errors. Got the followings: «Iterables.toString(issues)».''',
			issues.empty);
		val processor = processorProvider.get
		val acceptor = new ValidationAcceptor([issues += it]);
		val preferenceValues = preferenceValuesProvider.getPreferenceValues(resource);
		val context = new ProcessorContext(NullImpl, preferenceValues);
		processor.onValidate(resource, acceptor, CheckMode.FAST_ONLY, context);
		assertions.apply(processor.theJenaModel, issues);
		return resource;
	}

}
