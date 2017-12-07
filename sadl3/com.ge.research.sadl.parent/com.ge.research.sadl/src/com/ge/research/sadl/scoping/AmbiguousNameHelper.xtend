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
package com.ge.research.sadl.scoping

import com.google.inject.Inject
import com.google.inject.Singleton
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.resource.XtextResource

import static com.ge.research.sadl.preferences.SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES

import static extension com.google.common.base.Strings.nullToEmpty

/**
 * Helper for the ambiguous name detection.
 * 
 * @author akos.kitta
 */
@Singleton
class AmbiguousNameHelper {
	
	static val AMBIGUOUS_NAME_CACHE_KEY = CHECK_FOR_AMBIGUOUS_NAMES.id;
	
	@Inject extension IPreferenceValuesProvider;
	
	/**
	 * Checks whether the ambiguous name detection is enabled for the given Xtext resource.
	 * If not explicitly stated: {@code false}, then it assumes, {@code true}.
	 */
	def dispatch boolean isAmbiguousNameCheckEnabled(XtextResource it) {
		return cache.get(AMBIGUOUS_NAME_CACHE_KEY, it) [
			val enabled = getPreferenceValues(it).getPreference(CHECK_FOR_AMBIGUOUS_NAMES).nullToEmpty;
			return Boolean.FALSE.toString.toLowerCase !== enabled.toLowerCase;
		];
	}

	def dispatch boolean isAmbiguousNameCheckEnabled(Void it) {
		return false;
	}

	def dispatch boolean isAmbiguousNameCheckEnabled(Resource it) {
		return false;
	}
	
}