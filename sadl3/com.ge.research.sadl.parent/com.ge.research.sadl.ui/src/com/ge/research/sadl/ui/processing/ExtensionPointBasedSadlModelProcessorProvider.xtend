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

import com.ge.research.sadl.processing.IModelProcessor
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.google.inject.Inject
import com.google.inject.Injector
import com.google.inject.Singleton

/**
 * Eclipse extension-point based {@code SADL} model processor provider. Discovers 
 * and collects the implementations via extension-points.
 */
@Singleton
class ExtensionPointBasedSadlModelProcessorProvider extends SadlModelProcessorProvider {

	val ModelProcessorDelegate delegate;

	@Inject
	new(Injector injector, ModelProcessorDelegate delegate) {
		super(injector);
		this.delegate = delegate;
	}
	
	@Override
	override getAllProcessors() {
		return delegate.allProcessors;
	}

	/**
	 * Model processor provider delegate that does the discovery via extension-points. 
	 * 
	 * @author akos.kitta
	 */
	@Singleton
	private static final class ModelProcessorDelegate extends ExtensionPointBasedSadlProcessorProviderDelegate<IModelProcessor> {

		static val EXTENSION_ID = 'com.ge.research.sadl.ui.sadl_model_processor';

		@Inject
		protected new(Injector injector) {
			super(IModelProcessor, injector)
		}

		@Override
		override protected getExtensionPointId() {
			EXTENSION_ID;
		}

	}

}
