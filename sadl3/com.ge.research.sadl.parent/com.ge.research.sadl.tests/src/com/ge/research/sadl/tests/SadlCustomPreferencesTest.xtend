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
package com.ge.research.sadl.tests

import com.ge.research.sadl.preferences.SadlPreferences
import com.google.inject.Inject
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.preferences.MapBasedPreferenceValues
import org.eclipse.xtext.preferences.PreferenceValuesByLanguage
import org.junit.After
import org.junit.Before
import org.junit.Test

import static com.ge.research.sadl.preferences.SadlPreferences.IGNORE_UNITTEDQUANTITIES
import static org.junit.Assert.*

/**
 * Mock test case that shows how to use customized preference values in pure JUnit tests.
 * 
 * @author akos.kitta
 */
class SadlCustomPreferencesTest extends AbstractLinkingTest {

	static val LANGUAGE_ID = 'com.ge.research.sadl.SADL';

	@Inject
	IPreferenceValuesProvider preferenceValuesProvider;

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

	@Test
	def void check_Ignore_UnittedQuantities_AreOverridden() {
		val resource = '''
			uri "http://sadl.org/Foo.sadl".
		'''.sadl;
		val preferenceValues = preferenceValuesProvider.getPreferenceValues(resource);

		val expected = Boolean.TRUE.toString;
		val actual = preferenceValues.getPreference(IGNORE_UNITTEDQUANTITIES)

		assertEquals(
			'''Expected «expected» as the 'IGNORE_UNITTEDQUANTITIES' value. Was: «actual».''', expected, actual);
	}

}
