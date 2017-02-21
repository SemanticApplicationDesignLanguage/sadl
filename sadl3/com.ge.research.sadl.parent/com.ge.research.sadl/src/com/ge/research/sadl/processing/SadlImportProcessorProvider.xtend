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
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.util.internal.EmfAdaptable

/**
 * SADL import processor for both the headless and the Eclipse-based use-cases.
 */
class SadlImportProcessorProvider extends AbstractSadlProcessorProvider<ISadlImportProcessor, ResourceSet> {

	static val EXTENSION_ID = 'com.ge.research.sadl.sadl_import_processor';

	@EmfAdaptable @Data static class InternalAdapter {
		private ISadlImportProcessor processor
	}

	@Inject
	new(Injector injector) {
		super(ISadlImportProcessor, injector)
	}

	@Override
	override ISadlImportProcessor getProcessor(ResourceSet resourceSet) {
		val adapter = InternalAdapter.findInEmfObject(resourceSet)
		if (adapter !== null) {
			return adapter.processor
		}
		val result = doCreateProcessor(resourceSet)
		new InternalAdapter(result).attachToEmfObject(resourceSet)
		return result
	}

	@Override
	override protected getExtensionPointId() {
		return Optional.of(EXTENSION_ID);
	}

	protected def doCreateProcessor(ResourceSet set) {

		// XXX akitta: it does not make much sense. It uses the first and ignores the others.
		return new ISadlImportProcessor() {

			override onImport(Resource resource, String targetProjectOwlModelsFolder) {
				val itr = allProcessors.iterator;
				while (itr.hasNext()) {
					val processor = itr.next;
					return processor.onImport(resource, targetProjectOwlModelsFolder);
				}
			}

			override onImport(String owlContent) {
				val itr = allProcessors.iterator;
				while (itr.hasNext()) {
					val processor = itr.next;
					return processor.onImport(owlContent);
				}
			}

		};
	}

}
