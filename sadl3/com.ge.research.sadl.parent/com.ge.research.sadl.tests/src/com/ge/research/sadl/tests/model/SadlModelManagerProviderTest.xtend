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
import com.hp.hpl.jena.ontology.UnionClass
import java.util.ArrayList

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelManagerProviderTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> processorProvider
	
// TODO Is this the right place/method to check that errors are generated?	
	@Test def void invalidUriTestCase() {
		'''
			uri "my/uri" alias m1.
			Foo is a class.
		'''.assertInValidatesTo [ jenaModel, issues |
			// expectations go here
//			assertEquals("my/uri", modelManager.theJenaModel.getModelBaseURI())
			assertNotNull(jenaModel)
			assertEquals(1, issues.size())
			assertTrue(issues.toString(), issues.get(0).toString().contains("'my/uri' is not a valid URL"))
		]
	}
	
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
	
	@Test def void myClassWithPropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Shape is a class described by area with values of type int.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var prop = jenaModel.getOntProperty("http://sadl.org/model1#area")
			var itr = prop.listDomain().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("Shape")) {
					found = true;
				}
			}	
			if (!found) {
				jenaModel.write(System.out, "N3")
			}
			assertTrue(found);
			itr = prop.listRange().toIterable().iterator
			found = false;
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("int")) {
					found = true;
				}
			}	
			if (!found) {
				jenaModel.write(System.out, "N3")
			}
		]
	}
	
	@Test def void mySubClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Food is a class.
			Pizza is a type of Food.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var cls = jenaModel.getOntClass("http://sadl.org/model1#Pizza")
			var itr = cls.listSuperClasses(true).toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("Food")) {
					found = true;
				}
			}
			if (!found) {
				jenaModel.write(System.out, "N3");
			}	
			assertTrue(found);
		]
	}
	
	@Test def void myUnionSuperClassClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Food is a class.
			Drink is a class.
			Refreshment is a type of {Food or Drink}.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt != null && nxt.isURIResource) {
					if (nxt.localName.equals("Refreshment")) {
						found = true;
						var sciter = nxt.listSuperClasses(true);
						while (sciter.hasNext()) {
							var sc = sciter.next();
							var uc = sc.asUnionClass();
							var operands = uc.listOperands();
							while (operands.hasNext) {
								var opcls = operands.next();
								assertTrue(opcls.localName.equals("Food") || opcls.localName.equals("Drink"));	
							}
						}
					}
				}
			}	
			if (!found) {
				jenaModel.write(System.out, "N3");
			}
			assertTrue(found);
		]
	}
	
	@Test def void myIntersectionSuperClassClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Liquid is a class.
			Consumable is a class.
			PotableLiquid is a type of {Liquid and Consumable}.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt != null && nxt.isURIResource) {
					if (nxt.localName.equals("PotableLiquid")) {
						found = true;
						var sciter = nxt.listSuperClasses(true);
						while (sciter.hasNext()) {
							var sc = sciter.next();
							var uc = sc.asIntersectionClass();
							var operands = uc.listOperands();
							while (operands.hasNext) {
								var opcls = operands.next();
								assertTrue(opcls.localName.equals("Liquid") || opcls.localName.equals("Consumable"));	
							}
						}
					}
				}
			}	
			if (!found) {
				jenaModel.write(System.out, "N3");
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
			var itr = jenaModel.listObjectProperties().toIterable().iterator
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
	
	@Test def void mySubPropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			prop1 is a property.
			prop2 is a type of prop1.
		'''.assertValidatesTo [ jenaModel, issues |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var prop = jenaModel.getOntProperty("http://sadl.org/model1#prop2")
			var itr = prop.listSuperProperties(true).toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("prop1")) {
					found = true;
				}
			}
			if (!found) {
				jenaModel.write(System.out, "N3")
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
		val xtextIssues = validationTestHelper.validate(model);
		val processor = processorProvider.get
		val List<Issue> issues= new ArrayList(xtextIssues);
		processor.onValidate(model.eResource, [issues += it], CancelIndicator.NullImpl)
		assertions.apply(processor.theJenaModel, issues)
	}
}