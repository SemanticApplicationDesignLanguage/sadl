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
	
// TODO Is this the right place/method to check that errors are generated?	
//	@Test def void invalidUriTestCase() {
//		'''
//			uri "http://sadl.org/model1" alias m1.
//			Foo is a class.
//		'''.assertInValidatesTo [ jenaModel, issues |
//			// expectations go here
////			assertEquals("my/uri", modelManager.theJenaModel.getModelBaseURI())
//			assertNotNull(jenaModel)
//			assertTrue(issues.size == 1)
//			assertTrue(issues.get(0).toString().contains(""))
//		]
//	}
	@Test def void modelNameCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			assertTrue(jenaModel.listOntologies().hasNext())
			assertTrue(jenaModel.listOntologies().next().URI.toString().equals("http://sadl.org/model1"))
		]
	}
	
	@Test def void mySimpleClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Foo is a class.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("Foo")) {
					found = true;
				}
			}	
			assertTrue(found);
		]
	}
	
	@Test def void mySimplePropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			prop is a property.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listOntProperties().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("prop")) {
					found = true;
				}
			}	
			assertTrue(found);
		]
	}
	
	@Test def void mySimpleInstanceDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Foo is a class.
			MyFoo is a Foo.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listIndividuals().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("MyFoo")) {
					found = true;
				}
			}	
			assertTrue(found);
		]
	}
	
	@Test def void myAnnotationPropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			annprop is a type of annotation.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listAnnotationProperties().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("annprop")) {
					found = true;
				}
			}	
			assertTrue(found);
		]
	}
	
	@Test def void importTestCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			import "http://sadl.org/model2".
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			assertTrue(jenaModel.listOntologies().hasNext())
			var imports = jenaModel.listOntologies().next().listImports
			assertTrue(imports.next.URI.equals("http://sadl.org/model2"))
			assertFalse(imports.hasNext())
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

	protected def void assertInValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code);
		validationTestHelper.assertError(model, null, code.toString, null );
		val processor = processorProvider.get
		val List<Issue> issues= newArrayList
		processor.onValidate(model.eResource, [issues += it], CancelIndicator.NullImpl)
		assertions.apply(processor.theJenaModel, issues)
	}
}