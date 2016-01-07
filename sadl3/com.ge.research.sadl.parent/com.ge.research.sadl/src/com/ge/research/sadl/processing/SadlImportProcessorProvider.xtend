package com.ge.research.sadl.processing

import com.google.inject.Provider
import java.util.Set
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.util.internal.EmfAdaptable

class SadlImportProcessorProvider {
	
	@EmfAdaptable @Data static class InternalAdapter {
		protected ISadlImportProcessor processor
	}
	
	public static val Set<Provider<ISadlImportProcessor>> Registry = newHashSet
	
	def ISadlImportProcessor getProcessor(ResourceSet resourceSet) {
		val adapter = InternalAdapter.findInEmfObject(resourceSet)
		if (adapter !== null) {
			return adapter.processor
		}
		val result = doCreateProcessor(resourceSet)
		new InternalAdapter(result).attachToEmfObject(resourceSet)
		return result
	}
	
	protected def doCreateProcessor(ResourceSet set) {
		val processors = getAllProviders.map[get];
		return new ISadlImportProcessor() {
			override onImport(Resource resource, String targetProjectOwlModelsFolder) {
				val pitr = processors.iterator
				while (pitr.hasNext()) {
					val pr = pitr.next
					return pr.onImport(resource, targetProjectOwlModelsFolder)
				}
			}
			
			override onImport(String owlContent) {
				val pitr = processors.iterator
				while (pitr.hasNext()) {
					val pr = pitr.next
					return pr.onImport(owlContent)
				}
			}
			
		}
	}
	
	protected def Iterable<? extends Provider<? extends ISadlImportProcessor>> getAllProviders() {
		Registry
	}
	
}