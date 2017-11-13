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
package com.ge.research.sadl.processing

import com.google.common.base.Optional
import com.google.inject.Inject
import com.google.inject.Injector
import com.google.inject.Singleton
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.util.OnChangeEvictingCache

@Singleton
class SadlModelProcessorProvider extends AbstractSadlProcessorProvider<IModelProcessor, Resource> implements IModelProcessorProvider {

	static val MODEL_PROCESSOR_CACHE_KEY = 'modelprocessor';
	static val EXTENSION_ID = 'com.ge.research.sadl.sadl_model_processor';

	@Inject
	OnChangeEvictingCache cache

	@Inject
	new(Injector injector) {
		super(IModelProcessor, injector);
	}

	
	override IModelProcessor getProcessor(Resource resource) {
		return cache.get(MODEL_PROCESSOR_CACHE_KEY, resource) [
			doCreateProcessor(resource);
		];
	}

	
	override protected getExtensionPointId() {
		return Optional.of(EXTENSION_ID);
	}

	/**
	 * Creates a model processor instance for the given resource argument.
	 * <p>
	 * By default it returns with a composite model processor instance wrapping
	 * all available model processor instances. Clients may override to add
	 * specific behavior.
	 */
	protected def IModelProcessor doCreateProcessor(Resource resource) {
		val processor = allProcessors.findFirst[isSupported(resource)];
		return if (processor === null) IModelProcessor.NOOP else processor;
	}

}
