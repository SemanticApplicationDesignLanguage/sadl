package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.CompositeModelProcessor
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.ge.research.sadl.tests.AbstractSADLParsingTest
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import com.google.inject.Provider
import com.ge.research.sadl.processing.IModelProcessor

abstract class AbstractProcessorTest extends AbstractSADLParsingTest {
	
	@Inject IModelProcessorProvider processorProvider
	
	@Inject def void registerProcessors(SadlModelProcessorProvider processorProvider, JenaBasedSadlModelProcessor instance) {
		val Provider<IModelProcessor> provider = [instance];
		processorProvider.registerProcessor(provider);
	}
	
	def JenaBasedSadlModelProcessor getReqProcessor(Resource resource) {
		return (processorProvider.getProcessor(resource) as CompositeModelProcessor).processors.filter(JenaBasedSadlModelProcessor).head
	}
}
