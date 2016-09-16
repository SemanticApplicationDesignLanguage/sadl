package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.preferences.SadlPreferences
import com.google.inject.Injector
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test

class SadlPreferencesTest extends Assert {

    static Injector injector = new SADLUiInjectorProvider().injector

    @Inject IPreferenceStoreAccess preferenceStoreAccess
    @Inject IPreferenceValuesProvider preferenceValuesProvider

    @Before def void setUp() throws Exception {
        injector.injectMembers(this);
    }

    @Test def void testEclipsePreferenceStore() {
        val key = SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id
        var store = preferenceStoreAccess.getWritablePreferenceStore()
        assertEquals(false, store.getBoolean(key))

        store.setValue(key, SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.defaultValue)
        assertEquals(false, store.getBoolean(key))

        store.setValue(key, true)
        assertEquals(true, store.getBoolean(key))
    }

    @Test def void testEclipsePreferencesProvider() {
        val id = SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id
        assertEquals("false", store.getPreference(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES))

        preferenceStoreAccess.getWritablePreferenceStore().setValue(id,
            SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.defaultValue)
        assertEquals("false", store.getPreference(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES))

        preferenceStoreAccess.getWritablePreferenceStore().setValue(id, true)
        assertEquals("true", store.getPreference(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES))
    }

    def store() {
        preferenceValuesProvider.getPreferenceValues(new XtextResource(URI.createFileURI("foo.sadl")))
    }

    @After def void tearDown() throws Exception {
        preferenceStoreAccess.getWritablePreferenceStore().setToDefault(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id)
    }

}
