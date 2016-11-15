/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests

import com.ge.research.sadl.SADLRuntimeModule
import com.ge.research.sadl.processing.CompositeModelProcessor
import com.ge.research.sadl.processing.IModelProcessor
import com.ge.research.sadl.processing.IModelProcessorProvider
import org.eclipse.emf.ecore.resource.Resource

/**
 * Class for disabling the all the {@link IModelProcessor model processor}
 * instances when running the test with this injector provider.
 * 
 * @author akos.kitta
 */
class SADLNoopModelProcessorsInjectorProvider extends SADLInjectorProvider {

	/**
	 * Shared NOOP model processor provider instance.
	 */
	static val NOOP_PROCESSOR_PROVIDER = new IModelProcessorProvider() {

		@Override
		override getProcessor(Resource resource) {
			return new CompositeModelProcessor(emptyList());
		}

	};

	@Override
	override protected createRuntimeModule() {
		return new NoopModelProcessorsRuntimeModule();
	}

	/**
	 * Customized {@code SADL} runtime module that does not have any use any
	 * model processor instances.
	 */
	protected static class NoopModelProcessorsRuntimeModule extends SADLRuntimeModule {

		/**
		 * A composite model processor is used with zero delegates.
		 */
		def <Class extends IModelProcessorProvider> bindIModelProcessorProvider() {
			return NOOP_PROCESSOR_PROVIDER;
		}

	}

}
