/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptorImpl
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.AbstractSADLParsingTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.AllValuesFromRestriction
import com.hp.hpl.jena.ontology.CardinalityRestriction
import com.hp.hpl.jena.ontology.HasValueRestriction
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.vocabulary.OWL
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorTestBasics extends AbstractSADLParsingTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> processorProvider
	@Inject IPreferenceValuesProvider preferenceProvider
	
	@Test
	def void testInstanceDeclaration1() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 Person is a class described by age with values of type decimal.
			 Dog is a class described by owner with values of type Person.
			 A Dog Lassie.
			 The Dog Spot.
			 Lassie has owner (a Person Sam with age 32).
			 Spot has owner (Sonya is a Person age 32).
			 
 		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			assertNotNull(jenaModel.getIndividual("http://sadl.org/Test1.sadl#Lassie"))
 			assertNotNull(jenaModel.getIndividual("http://sadl.org/Test1.sadl#Spot"))
 			assertNotNull(jenaModel.getIndividual("http://sadl.org/Test1.sadl#Sam"))
 			assertNotNull(jenaModel.getIndividual("http://sadl.org/Test1.sadl#Sonya"))
			val stmtitr = jenaModel.listStatements(jenaModel.getIndividual("http://sadl.org/Test1.sadl#Lassie"), 
				jenaModel.getProperty("http://sadl.org/Test1.sadl#owner"),
				jenaModel.getIndividual("http://sadl.org/Test1.sadl#Sam"))
			assertTrue(stmtitr.hasNext)
			val stmtitr2 = jenaModel.listStatements(jenaModel.getIndividual("http://sadl.org/Test1.sadl#Sam"), 
				jenaModel.getProperty("http://sadl.org/Test1.sadl#age"), null as RDFNode)
			assertTrue(stmtitr2.hasNext)
 		]
	}
	
	@Test
	def void testPropertySingleValue() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with a single value of type float.
 		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof CardinalityRestriction) {
 				assertTrue((sprc as CardinalityRestriction).onProperty.URI.equals("http://sadl.org/test.sadl#age"))
 				assertTrue((sprc as CardinalityRestriction).cardinality == 1)
 			}
 		]
	}

	@Test
	def void testPropertySingleValue2() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 age describes Person with a single value of type float.
 		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof CardinalityRestriction) {
 				assertTrue((sprc as CardinalityRestriction).onProperty.URI.equals("http://sadl.org/test.sadl#age"))
 				assertTrue((sprc as CardinalityRestriction).cardinality == 1)
 			}
 		]
	}

	@Test
	def void testPropertyAlwaysHasValueTrue() {
		val sadlModel = '''
			 uri "http://sadl.org/MTC1" alias Name version "$Revision:$ Last modified on   $Date:$". 
			 
			 SYSTEM is a class,
			 	described by input1 with a single value of type int,
			 	described by output1 with a single value of type boolean,
			 	described by output2 with a single value of type boolean,
			 	described by output3 with values of type int,
			 	described by output4 with values of type boolean,
			 	described by output5 with values of type int.
			 output1 of SYSTEM always has value true.
 		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/MTC1#SYSTEM")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof HasValueRestriction) {
 				assertTrue((sprc as HasValueRestriction).onProperty.URI.equals("http://sadl.org/MTC1#output1"))
 				assertTrue((sprc as HasValueRestriction).hasValue.asLiteral.value.equals("true"))
 			}
 		]
	}
	
	@Test
	def void testNamedStructureAnnotationsRule() {
		val implicitModel = '''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			
			impliedProperty is a type of annotation.
			expandedProperty is a type of annotation.
			UnittedQuantity is a class,
				described by ^value with values of type decimal,
				described by unit with values of type string.
			^Rule is a class.
			NamedQuery is a class.
		'''.assertValidatesTo[p1, p2|]
		
		val sadlModel = '''
			uri "http://sadl.org/Shapes.sadl" alias Shapes.
			 
			Shape is a class described by area with values of type float.
			 
			comment is a type of annotation.
			 
			Circle is a type of Shape, described by radius with values of type float.
			 
			MyCircle is a Circle with radius 3.
			 
			Rule AreaOfCircle:
			 	if c is a Circle
			 	then area of c is radius of c ^ 2 * PI.
			 	
			AreaOfCircle has comment "ho".
			 	
			Ask: area. 	
			
«««			Ask: x is a ^Rule.
		'''.assertValidatesTo[jenaModel, issues |
				assertNotNull(jenaModel)
				jenaModel.write(System.out)
				assertTrue(issues.size == 0)
			]
	}

	@Test
	def void testTypedList1() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 IntegerList is a type of int List.
 		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#IntegerList")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			var listSubclass = false
 			do {
 				val sprc = itr.next as OntClass
 				if ((sprc as OntClass).URIResource && (sprc as OntClass).URI.equals("http://sadl.org/sadllistmodel#List")) {
 					listSubclass = true
 				}
 				if (sprc.canAs(AllValuesFromRestriction)) {
 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
 					val obj = opitr.nextStatement.object
 					if ((obj as com.hp.hpl.jena.rdf.model.Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
 						val v = vitr.nextStatement.object
 						assertTrue((v as com.hp.hpl.jena.rdf.model.Resource).URI.equals("http://www.w3.org/2001/XMLSchema#int"));
  					}
 				}
 			} while (itr.hasNext)
 			assertTrue(listSubclass)
 					
 		]
	}

	protected def Resource assertValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code)
		validationTestHelper.assertNoErrors(model)
		val processor = processorProvider.get
		val List<Issue> issues= newArrayList
		processor.onValidate(model.eResource, new ValidationAcceptorImpl([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
		assertions.apply(processor.theJenaModel, issues)
		return model.eResource
	}

}
