package com.ge.research.sadl.ui.processing

import com.ge.research.sadl.processing.ISadlModelProcessor
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.google.inject.Provider
import org.eclipse.core.runtime.RegistryFactory
import com.google.inject.Inject
import com.google.inject.Injector

class ExtensionPointBasedSadlModelProcessorProvider extends SadlModelProcessorProvider {
	
	static val EXTENSION_ID = 'com.ge.research.sadl.ui.sadl_model_processor'

	@Inject Injector injector;
	
	override getAllProviders() {
		val registry = RegistryFactory.getRegistry();
		val configElements = registry.getConfigurationElementsFor(EXTENSION_ID).toList
		return configElements.map [ configElement |
			new Provider<ISadlModelProcessor>() {
				override get() {
					val result = configElement.createExecutableExtension("class") as ISadlModelProcessor
					injector.injectMembers(result)
					return result
				}
			};
		];
	}
	
}