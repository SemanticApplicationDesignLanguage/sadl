package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.OntModel
import java.util.List
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.Issue
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelManagerProviderTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> processorProvider
	
	@Test def void myFirstTestCase() {
		'''
			uri "my/uri".
			Foo is a class.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
//			assertEquals("my/uri", modelManager.theJenaModel.getModelBaseURI())
			assertNotNull(jenaModel)
			assertTrue(issues.empty)
		]
	}
	
	@Test def void importTestCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			import "http://sadl.org/model2".
			Foo is a class.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
//			assertEquals("http://sadl.org/model1", jenaModel.getModelBaseURI())
//			assertEquals("m1", jenaModel.getModelAlias());
//			assertTrue(jenaModel.getOrderedImports().size() == 1);
//			assertEquals("http://sadl.org/model2", jenaModel.getOrderedImports.get(0).getUri());
		]
	}
	
	protected def void assertValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code)
		validationTestHelper.assertNoErrors(model)
		val processor = processorProvider.get
		val List<Issue> issues= newArrayList
		processor.onValidate(model.eResource, [issues += it], CancelIndicator.NullImpl)
		assertions.apply(processor.theJenaModel, issues)
	}
}