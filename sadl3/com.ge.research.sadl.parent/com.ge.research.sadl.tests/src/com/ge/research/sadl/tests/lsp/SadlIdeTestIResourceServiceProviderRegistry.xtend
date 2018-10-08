package com.ge.research.sadl.tests.lsp

import com.google.inject.Provider
import org.eclipse.xtext.resource.FileExtensionProvider
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.resource.impl.ResourceServiceProviderRegistryImpl
import com.google.inject.Singleton

@Singleton
class SadlIdeTestIResourceServiceProviderRegistry implements Provider<IResourceServiceProvider.Registry> {

	val IResourceServiceProvider.Registry registry = loadRegistry

	private def IResourceServiceProvider.Registry loadRegistry() {
		val registry = new ResourceServiceProviderRegistryImpl()
		val injector = new SadlIdeTestSetup().createInjectorAndDoEMFRegistration();
		val resourceServiceProvider = injector.getInstance(IResourceServiceProvider)
		val extensionProvider = injector.getInstance(FileExtensionProvider)
		for (ext : extensionProvider.fileExtensions) {
			if (registry.extensionToFactoryMap.containsKey(ext)) {
				if (extensionProvider.primaryFileExtension == ext) {
					registry.extensionToFactoryMap.put(ext, resourceServiceProvider)
				}
			} else {
				registry.extensionToFactoryMap.put(ext, resourceServiceProvider)
			}
		}
		return registry;
	}

	override get() {
		return registry
	}

}