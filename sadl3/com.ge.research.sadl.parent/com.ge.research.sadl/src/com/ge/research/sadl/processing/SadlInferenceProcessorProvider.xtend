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

import com.google.common.collect.ImmutableList
import org.eclipse.emf.ecore.resource.Resource

import static java.util.Collections.singletonList

/**
 * Provides {@code SADL} inferences in a headless case.
 */
class SadlInferenceProcessorProvider extends AbstractSadlProcessorProvider<ISadlInferenceProcessor> {

	static val ISadlInferenceProcessor NOOP_INFERENCER = [
		val result = newArrayOfSize(3);
		result.set(2, singletonList('No registered SADL inferencer were found.'));
		return result;
	];

	new() {
		super(ISadlInferenceProcessor)
	}

	@Override
	override ISadlInferenceProcessor getProcessor(Resource resource) {
		return doCreateProcessor(resource)
	}

	protected def doCreateProcessor(Resource resource) {
		val processors = ImmutableList.copyOf(allProcessors);
		return processors.head ?: NOOP_INFERENCER;
	}

}
