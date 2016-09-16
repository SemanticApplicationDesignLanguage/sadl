package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.ui.internal.SadlActivator
import com.google.inject.Binder
import com.google.inject.Guice
import com.google.inject.name.Names
import org.eclipse.xtext.Constants
import org.eclipse.xtext.service.AbstractGenericModule
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test

class SadlPreferencesTest extends Assert {
    static final String LANGUAGE_ID = SadlActivator.COM_GE_RESEARCH_SADL_SADL
    IPreferenceStoreAccess preferenceStoreAccess

//    override void initialize(IPreferenceStoreAccess access) {
//        access.getWritablePreferenceStore().setDefault("someBoolean", true)
//    }
    @Before def void setUp() throws Exception {
        preferenceStoreAccess = Guice.createInjector(new AbstractGenericModule() {
            def void configureModule(Binder binder) {
                binder.bind(String).annotatedWith(Names.named(Constants.LANGUAGE_NAME)).toInstance(LANGUAGE_ID)
//                binder.bind(IPreferenceStoreInitializer).toInstance(PreferenceStoreAccessTest.this)
            }
        }).getInstance(IPreferenceStoreAccess)
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

    @After def void tearDown() throws Exception {
        preferenceStoreAccess.getWritablePreferenceStore().setToDefault(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id)
    }

}
