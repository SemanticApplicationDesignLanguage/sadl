package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.google.inject.Inject
import com.google.inject.Provider
import org.junit.After
import org.eclipse.emf.ecore.resource.Resource
import com.ge.research.sadl.processing.SadlModelProcessorProvider.CompositeModelProcessor
import com.ge.research.sadl.tests.AbstractSADLParsingTest

class AbstractProcessorTest extends AbstractSADLParsingTest {
	
	@Inject IModelProcessorProvider processorProvider
	
	@After
	def void tearDown() {
		SadlModelProcessorProvider.SADL_PROCESSOR_Registry.clear
	}
	
	@Inject def void registerProcessors(Provider<JenaBasedSadlModelProcessor> sadlProcessorProvider) {
		SadlModelProcessorProvider.SADL_PROCESSOR_Registry.add([sadlProcessorProvider.get])
	}
	
	def JenaBasedSadlModelProcessor getReqProcessor(Resource resource) {
		return (processorProvider.getProcessor(resource) as CompositeModelProcessor).processors.filter(JenaBasedSadlModelProcessor).head
	}
}
