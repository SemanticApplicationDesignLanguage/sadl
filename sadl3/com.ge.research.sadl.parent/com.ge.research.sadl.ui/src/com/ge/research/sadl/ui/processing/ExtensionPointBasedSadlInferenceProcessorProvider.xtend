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

import com.ge.research.sadl.processing.ISadlInferenceProcessor
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider
import com.google.inject.Inject
import com.google.inject.Injector

/**
 * Eclipse extension-point based {@code SADL} inference provider. Discovers and collects the 
 * implementations via extension-points.
 */
class ExtensionPointBasedSadlInferenceProcessorProvider extends SadlInferenceProcessorProvider {

	@Inject
	InferenceProcessorDelegate delegate;

	@Override
	override getAllProcessors() {
		return delegate.allProviders;
	}

	/**
	 * Inference processor provider delegate that does the discovery via extension-points. 
	 * Caches all the provided inference processors.
	 * 
	 * @author akos.kitta
	 */
	private static final class InferenceProcessorDelegate extends AbstractExtensionPointBasedSadlProcessorProviderDelegate<ISadlInferenceProcessor> {

		static val EXTENSION_ID = 'com.ge.research.sadl.ui.sadl_inference_processor';

		@Inject
		protected new(Injector injector) {
			super(ISadlInferenceProcessor, injector)
		}

		@Override
		override protected getExtensionPointId() {
			EXTENSION_ID;
		}

	}

}
