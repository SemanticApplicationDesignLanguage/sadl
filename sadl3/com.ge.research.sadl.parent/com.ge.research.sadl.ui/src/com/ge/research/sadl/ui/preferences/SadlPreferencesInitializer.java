/**
 * 
 */
package com.ge.research.sadl.ui.preferences;

import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import com.ge.research.sadl.preferences.SadlPreferences;

/**
 * @author dhuebner
 *
 */
public class SadlPreferencesInitializer implements IPreferenceStoreInitializer {

	/* (non-Javadoc)
	 * @see org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer#initialize(org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess)
	 */
	@SuppressWarnings("restriction")
	@Override
	public void initialize(IPreferenceStoreAccess access) {
		access.getWritablePreferenceStore().setDefault(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.getId(), false);
	}

}
