package com.ge.research.sadl.processing

import com.google.inject.Provider
import java.util.Set
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.util.internal.EmfAdaptable
import org.eclipse.xtext.validation.Issue

class SadlModelProcessorProvider {
	
	@EmfAdaptable @Data static class InternalAdapter {
		protected ISadlModelProcessor processor
	}
	
	public static val Set<Provider<ISadlModelProcessor>> Registry = newHashSet
	
	def ISadlModelProcessor getProcessor(ResourceSet resourceSet) {
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
		return new ISadlModelProcessor() {
			override onValidate(Resource resource, IAcceptor<Issue> issueAcceptor, CancelIndicator cancelIndicator) {
				processors.forEach[onValidate(resource, issueAcceptor, cancelIndicator)]
			}
			override onGenerate(Resource resource, IFileSystemAccess2 fsa, CancelIndicator cancelIndicator) {
				processors.forEach[onGenerate(resource, fsa, cancelIndicator)]
			}
		}
	}
	
	protected def Iterable<? extends Provider<? extends ISadlModelProcessor>> getAllProviders() {
		Registry
	}
	
}