package com.ge.research.sadl.ui.external

import com.ge.research.sadl.external.ExternalEmfResourceGenerator
import com.ge.research.sadl.external.ExternalEmfResourceServiceProvider
import com.google.inject.Inject
import com.google.inject.Injector
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IStorage
import org.eclipse.emf.common.util.URI
import org.eclipse.xtend.lib.annotations.Delegate
import org.eclipse.xtext.generator.GeneratorDelegate
import org.eclipse.xtext.ui.resource.IResourceUIServiceProvider

class ExternalEmfUIResourceServiceProvider extends ExternalEmfResourceServiceProvider implements IResourceUIServiceProvider {
	
	@Delegate @Inject IResourceUIServiceProvider delegate

	@Inject
	Injector injector;
	
	override canHandle(URI uri, IStorage storage) {
		super.canHandle(uri) && !storage.isReadOnly && (storage instanceof IResource) && !(storage as IResource).isDerived
	}
	
	override <T> get(Class<T> clazz) {
		if (clazz == GeneratorDelegate) {
			return injector.getInstance(ExternalEmfResourceGenerator) as T;
		}
		return delegate.get(clazz);
	}
		
}