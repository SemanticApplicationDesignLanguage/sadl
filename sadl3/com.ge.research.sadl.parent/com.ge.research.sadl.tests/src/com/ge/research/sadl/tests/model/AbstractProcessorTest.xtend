package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.CompositeModelProcessor
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.ge.research.sadl.tests.AbstractSADLParsingTest
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.Resource

abstract class AbstractProcessorTest extends AbstractSADLParsingTest {
	
	@Inject IModelProcessorProvider processorProvider
	
	@Inject def void registerProcessors(SadlModelProcessorProvider processorProvider, JenaBasedSadlModelProcessor instance) {
		processorProvider.registerModelProcessor(instance);
	}
	
	def JenaBasedSadlModelProcessor getReqProcessor(Resource resource) {
		return (processorProvider.getProcessor(resource) as CompositeModelProcessor).processors.filter(JenaBasedSadlModelProcessor).head
	}
}
