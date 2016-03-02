/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.processing

import com.ge.research.sadl.processing.SadlImportProcessorProvider
import com.ge.research.sadl.processing.ISadlImportProcessor
import com.google.inject.Provider
import org.eclipse.core.runtime.RegistryFactory
import com.google.inject.Inject
import com.google.inject.Injector

class ExtensionPointBasedSadlImportProcessorProvider extends SadlImportProcessorProvider {
	
	static val EXTENSION_ID = 'com.ge.research.sadl.ui.sadl_import_processor'

	@Inject Injector injector;
	
	override getAllProviders() {
		val registry = RegistryFactory.getRegistry();
		val configElements = registry.getConfigurationElementsFor(EXTENSION_ID).toList
		return configElements.map [ configElement |
			new Provider<ISadlImportProcessor>() {
				override get() {
					val result = configElement.createExecutableExtension("class") as ISadlImportProcessor
					injector.injectMembers(result)
					return result
				}
			};
		];
	}
	
}