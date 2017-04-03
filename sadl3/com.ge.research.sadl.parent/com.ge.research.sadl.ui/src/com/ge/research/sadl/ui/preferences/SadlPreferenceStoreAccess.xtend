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
package com.ge.research.sadl.ui.preferences

import com.ge.research.sadl.ui.internal.SadlActivator
import com.google.inject.Inject
import com.google.inject.Injector
import com.google.inject.Key
import com.google.inject.Singleton
import com.google.inject.name.Names
import java.util.Collection
import org.eclipse.xtext.Constants
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer
import org.eclipse.xtext.ui.editor.preferences.PreferenceStoreAccessImpl
import org.slf4j.LoggerFactory

/**
 * Preference store access to share a singleton instance between multiple DSLs.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlPreferenceStoreAccess extends PreferenceStoreAccessImpl {
	
	static val LOGGER = LoggerFactory.getLogger(SadlPreferenceStoreAccess);
	
	val Collection<String> initializedLanguages;
	
	@Inject
	Injector injector;
	
	new() {
		this.initializedLanguages = newHashSet();
		LOGGER.info('''Creating preference store access: «this».''');
	}
	
	@Override
	override toString() {
		return '''Shared SADL preference store access [«System.identityHashCode(this)»]''';
	}
	
	@Override
	override setLanguageNameAsQualifier(String languageName) {
		// Noop. This is to be able to share the preference store access between multiple languages.
	}
	
	@Override
	override protected getQualifier() {
		return SadlActivator.COM_GE_RESEARCH_SADL_SADL;
	}
	
	@Override
	override protected lazyInitialize() {
		// Do the initialization per language.
		initializeForLanguage(injector);
	}
	
	@Inject
	package def void initializeForLanguage(Injector anInjector) {
		// Do the initialization per language.
		val languageId = anInjector.getInstance(Key.get(String, Names.named(Constants.LANGUAGE_NAME)));
		if (languageId !== null && !initializedLanguages.contains(languageId)) {
			synchronized (this) {
				if (!initializedLanguages.contains(languageId)) {
					// First register it as initialized, otherwise we run into an endless loop.
					initializedLanguages.add(languageId);
					anInjector.initializer.initialize(this);
				}
			}
		}
	}
	
	private def getInitializer(Injector injector) {
		return injector.getInstance(IPreferenceStoreInitializer.CompositeImpl);
	}
	
}