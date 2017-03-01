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
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.utils.SadlQualifiedNameToStringService
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.query.QueryExecutionFactory
import java.util.ArrayList
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.preferences.MapBasedPreferenceValues
import org.eclipse.xtext.preferences.PreferenceValuesByLanguage
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test

import static com.ge.research.sadl.preferences.SadlPreferences.IGNORE_UNITTEDQUANTITIES
import static org.junit.Assert.*
import org.junit.Ignore

/**
 * Mock test case that shows how to use customized preference values in pure JUnit tests.
 * 
 * @author akos.kitta
 */
class SadlIgnoreUnittedQuantitiyTest extends AbstractProcessorTest { 

	static val LANGUAGE_ID = 'com.ge.research.sadl.SADL';

	@Accessors(PROTECTED_GETTER)
	@Inject SadlQualifiedNameToStringService qualifiedNameToStringService;
	
	@Accessors(PROTECTED_GETTER)
	@Inject XtextResourceSet currentResourceSet

	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> processorProvider
	@Inject IPreferenceValuesProvider preferenceValuesProvider;

	@Before
	def void setPreferences() {
		val sadlPreferences = SadlPreferences.preferences.toMap([id], [defaultValue]);
		val preferenceValues = new MapBasedPreferenceValues(sadlPreferences);

		// Modify the default preferences for tests.
		preferenceValues.put(IGNORE_UNITTEDQUANTITIES, Boolean.TRUE.toString);

		val referenceValuesByLanguage = new PreferenceValuesByLanguage;
		referenceValuesByLanguage.put(LANGUAGE_ID, preferenceValues);
		referenceValuesByLanguage.attachToEmfObject(currentResourceSet);
	}

	@After
	def void removePreferences() {
		PreferenceValuesByLanguage.removeFromEmfObject(currentResourceSet);
	}

	@Ignore
	@Test
	def void testIgnoreUnitsInSadl() {
		val sadlModel = '''
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
		'''.assertValidatesTo [jenaModel, issues |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			val ageprop = jenaModel.getDatatypeProperty("http://sadl.org/OntologyWithUnittedQuantity.sadl#age")
			assertNotNull(ageprop);
		]
		val preferenceValues = preferenceValuesProvider.getPreferenceValues(sadlModel);

		val expected = Boolean.TRUE.toString;
		val actual = preferenceValues.getPreference(IGNORE_UNITTEDQUANTITIES)

		assertEquals(
			'''Expected «expected» as the 'IGNORE_UNITTEDQUANTITIES' value. Was: «actual».''', expected, actual);
		sadlModel.assertOnlyWarningsOrInfo
		val issues = validationTestHelper.validate(sadlModel)
		Assert.assertNotNull(issues)
	}

	protected def boolean queryResultContains(OntModel m, String q, String r) {
		var qe = QueryExecutionFactory.create(q, m)
		var results =  qe.execSelect()
		var vars = results.resultVars
    	var resultsList = new ArrayList<String>()
    	while (results.hasNext()) {
    		var result = results.next()
    		var sb = new StringBuffer();
    		var cntr = 0
    		for (var c = 0; c < vars.size(); c++) {
    			if (cntr++ > 0) {
    				sb.append(" ")
    			}
    			sb.append(result.get(vars.get(c)))
    		}
    		resultsList.add(sb.toString())
    	}
    	if (resultsList.contains(r)) {
    		return true
    	}
    	System.out.println("Query result does not contain '" + r + "':")
    	var itr = resultsList.iterator()
    	if (itr.hasNext()) {
    		while (itr.hasNext()) {
    			System.out.println("   " + itr.next().toString())
    		}
    	}
    	else {
    		System.out.println("    Query returned no results");
    	}
    	return false
	}
	
	protected def Resource assertValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code)
		validationTestHelper.assertNoErrors(model)
		val processor = processorProvider.get
		val List<Issue> issues= newArrayList
		processor.onValidate(model.eResource, new ValidationAcceptor([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceValuesProvider.getPreferenceValues(model.eResource)))
		assertions.apply(processor.theJenaModel, issues)
		return model.eResource
	}

	protected def Resource assertValidatesTo(ResourceSet resourceSet, CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code, resourceSet);
		val xtextIssues = validationTestHelper.validate(model);
		val processor = processorProvider.get
		val List<Issue> issues= new ArrayList(xtextIssues);
		processor.onValidate(model.eResource, new ValidationAcceptor([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceValuesProvider.getPreferenceValues(model.eResource)))
		assertions.apply(processor.theJenaModel, issues)
		return model.eResource
	}
}
