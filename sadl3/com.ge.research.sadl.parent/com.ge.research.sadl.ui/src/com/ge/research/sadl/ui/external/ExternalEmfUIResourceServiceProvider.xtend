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
		if (super.canHandle(uri) && storage instanceof IResource) {
			// SadlBaseModel.owl should be treated as an external resource although it is read-only (by default) and derived.
			// TODO: this and the predicate must go into a IDE independent place so we can reuse it in the LS
			if (uri.platform && uri.segmentCount === 4 && uri.toString.endsWith('OwlModels/SadlBaseModel.owl')) {
				return true;
			}
			
			return !storage.isReadOnly && !(storage as IResource).isDerived;
		}
		return false;
	}
	
	override <T> get(Class<T> clazz) {
		if (clazz == GeneratorDelegate) {
			return injector.getInstance(ExternalEmfResourceGenerator) as T;
		}
		return delegate.get(clazz);
	}

}