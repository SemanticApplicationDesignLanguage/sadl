package com.ge.research.sadl.ui.external

import com.ge.research.sadl.external.ExternalEmfResourceGenerator
import com.ge.research.sadl.external.ExternalEmfResourceServiceProvider
import com.ge.research.sadl.utils.SadlBaseModelHelper
import com.google.inject.Inject
import com.google.inject.Injector
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IStorage
import org.eclipse.emf.common.util.URI
import org.eclipse.xtend.lib.annotations.Delegate
import org.eclipse.xtext.generator.GeneratorDelegate
import org.eclipse.xtext.ui.resource.IResourceUIServiceProvider

class ExternalEmfUIResourceServiceProvider extends ExternalEmfResourceServiceProvider implements IResourceUIServiceProvider {

	@Inject
	Injector injector;

	@Delegate
	@Inject
	IResourceUIServiceProvider delegate

	@Inject
	SadlBaseModelHelper baseModelHelper;

	override canHandle(URI uri, IStorage storage) {
		if (super.canHandle(uri)) {
			if (baseModelHelper.is(uri)) {
				return true;
			}
			return !storage.isReadOnly && (storage instanceof IResource) && !(storage as IResource).isDerived;
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
