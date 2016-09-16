package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.preferences.SadlPreferences
import com.google.inject.Injector
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import javax.inject.Inject

class SadlPreferencesTest extends Assert {
    static Injector injector = new SADLUiInjectorProvider().injector
    @Inject IPreferenceStoreAccess preferenceStoreAccess

    @Before def void setUp() throws Exception {
        injector.injectMembers(this);
    }

    @Test def void testDefault() {
        val key = SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id
        var store = preferenceStoreAccess.getWritablePreferenceStore()
        assertEquals(false, store.getBoolean(key))

        store.setValue(key, SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.defaultValue)
        assertEquals(false, store.getBoolean(key))

        store.setValue(key, true)
        assertEquals(true, store.getBoolean(key))
    }

}
