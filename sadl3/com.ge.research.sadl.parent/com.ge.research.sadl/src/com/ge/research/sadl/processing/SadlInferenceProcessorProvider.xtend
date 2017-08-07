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
import com.google.common.collect.ImmutableList
import com.google.inject.Inject
import com.google.inject.Injector
import org.eclipse.emf.ecore.resource.Resource
import java.util.Map

/**
 * Provides {@code SADL} inferences.
 */
class SadlInferenceProcessorProvider extends AbstractSadlProcessorProvider<ISadlInferenceProcessor, Resource> {

	static val EXTENSION_ID = 'com.ge.research.sadl.sadl_inference_processor';

	static val ISadlInferenceProcessor NOOP_INFERENCER = new ISadlInferenceProcessor() {
		
		override runInference(Resource resource, String owlModelPath, String modelFolderPath, Map<String, String> prefMap) throws SadlInferenceException {
			return newArrayOfSize(0);
		}
		
		override runNamedQuery(Resource resource, String queryName) throws SadlInferenceException {
			return newArrayOfSize(0);
		}		
	}

	@Inject
	new(Injector injector) {
		super(ISadlInferenceProcessor, injector);
	}

	
	override getProcessor(Resource resource) {
		return doCreateProcessor(resource);
	}

	
	override protected getExtensionPointId() {
		return Optional.of(EXTENSION_ID);
	}

	protected def doCreateProcessor(Resource resource) {
		val processors = ImmutableList.copyOf(allProcessors);
		return processors.head ?: NOOP_INFERENCER;
	}

}
