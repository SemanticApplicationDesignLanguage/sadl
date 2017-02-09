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

import com.google.inject.Provider
import java.util.Set
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.util.internal.EmfAdaptable
import java.util.Map

class SadlInferenceProcessorProvider {
	
	@EmfAdaptable @Data static class InternalAdapter {
		protected ISadlInferenceProcessor processor
	}
	
	public static val Set<Provider<ISadlInferenceProcessor>> Registry = newHashSet
	
	def ISadlInferenceProcessor getProcessor(Resource resource) {
		val result = doCreateProcessor(resource)
		return result
	}
	
	protected def doCreateProcessor(Resource resource) {
		val processors = getAllProviders.map[get];
		return new ISadlInferenceProcessor() {
			override runInference(Resource resource, String owlModelPath, String modelFolderPath, Map<String, String> prefMap) throws SadlInferenceException {
				val pitr = processors.iterator
				while (pitr.hasNext()) {
					val pr = pitr.next
					return pr.runInference(resource, owlModelPath, modelFolderPath, prefMap)
				}
			}
			
		}
	}
	
	protected def Iterable<? extends Provider<? extends ISadlInferenceProcessor>> getAllProviders() {
		Registry
	}
	
}