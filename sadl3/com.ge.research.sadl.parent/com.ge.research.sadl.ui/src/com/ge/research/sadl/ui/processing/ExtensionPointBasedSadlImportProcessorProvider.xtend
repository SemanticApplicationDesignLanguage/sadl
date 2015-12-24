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