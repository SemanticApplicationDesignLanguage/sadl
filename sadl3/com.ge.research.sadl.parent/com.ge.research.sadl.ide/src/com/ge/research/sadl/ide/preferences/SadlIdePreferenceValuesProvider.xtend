/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.preferences

import com.ge.research.sadl.utils.JSONHelper
import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.inject.Inject
import com.google.inject.Singleton
import java.net.URI
import java.nio.file.Paths
import java.util.HashMap
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.preferences.MapBasedPreferenceValues

/**
 * Generic IDE preferences value provider.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlIdePreferenceValuesProvider implements IPreferenceValuesProvider {

	@Inject
	SadlProjectHelper projectHelper;

	@Inject
	DefaultPreferenceValuesProvider delegate;

	@Inject
	JSONHelper jsonHelper;

	override getPreferenceValues(Resource context) {
		val defaultPreferences = delegate.getPreferenceValues(context);
		// TODO: isn't it too expensive? Let's cache it, but then we have to send a push from client to server when the settings.json changes and invalidate the cache.
		// We could use the resource cache. Let's see the performance first.
		if (context !== null && context.URI.file) {
			val root = projectHelper.getRoot(new URI(context.URI.toString()));
			if (root !== null) {
				val json = Paths.get(root).resolve('settings.json').toFile;
				if (json.exists && json.canRead) {
					val raw = jsonHelper.asMap(json.toPath);
					val filtered = new HashMap;
					raw.forEach [ key, value |
						if (keyPredicate.apply(key) && valuePredicate.apply(value)) {
							filtered.put(keyMapper.apply(key as String), '''«value»''');
						}
					];
					return new MapBasedPreferenceValues(defaultPreferences, filtered);
				}
			}
		}
		return defaultPreferences;
	}

	protected def (Object)=>boolean getKeyPredicate() {
		return [
			it instanceof String && (it as String).startsWith('sadl.')
		];
	}

	protected def (Object)=>boolean getValuePredicate() {
		return [
			it instanceof String || it instanceof Boolean || it instanceof Number;
		];
	}

	protected def (String)=>String getKeyMapper() {
		return [
			replaceFirst('sadl.', '')
		];
	}

}
