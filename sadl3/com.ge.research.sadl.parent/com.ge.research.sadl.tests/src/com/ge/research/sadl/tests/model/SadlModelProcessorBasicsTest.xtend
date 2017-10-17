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
import com.hp.hpl.jena.ontology.Restriction
import com.hp.hpl.jena.ontology.OntResource
import com.hp.hpl.jena.vocabulary.RDFS
import org.junit.Ignore

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorBasicsTest extends AbstractSADLParsingTest {
	
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

	@Test
	def void testLiteralOutOfRangeInt() {
		var errs = newArrayList("Value is not in range of property",
			"Unable to convert value '-2147483649 (-2147483649)' to type 'http://www.w3.org/2001/XMLSchema#int'(For input string: \"-2147483649\")",
			"Value is not in range of property",
			"Unable to convert value '2147483648 (2147483648)' to type 'http://www.w3.org/2001/XMLSchema#int'(For input string: \"2147483648\")",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property",
			"Value is not in range of property"
		)
		val sadlModel = '''
			 uri "http://sadl.org/LiteralOutOfRange.sadl" alias LiteralOutOfRange.
			 
			 Foo is a class described by bar with values of type int.
			 MyFoo1 is a Foo with bar -2147483649 .
			 MyFoo2 is a Foo with bar 2147483648 .
			 
			 Rule R1 if f is a Foo and bar of f > 2147483648 then print("big").
			 Rule R2 if f is a Foo then bar of f is -2147483649 .
			 
			 Rule R3 if f is a Foo and bar of f >= 2147483648 then print("big").
			 Rule R4 if f is a Foo then bar of f is -2147483649 .
			 
			 Rule R5 if f is a Foo and bar of f < -2147483649 then print("big").
			 Rule R6 if f is a Foo then bar of f is -2147483649 .
			 
			 Rule R7 if f is a Foo and bar of f <= -2147483649 then print("big").
			 Rule R8 if f is a Foo then bar of f is -2147483649 .
			  
			 Rule R9 if f is a Foo and bar of f > 2147483647 then print("big").
			 Rule R10 if f is a Foo then bar of f is -2147483648 .
			 
			 Rule R11 if f is a Foo and bar of f >= 2147483647 then print("big").
			 Rule R12 if f is a Foo then bar of f is -2147483648 .
			 
			 Rule R13 if f is a Foo and bar of f < -2147483648 then print("big").
			 Rule R14 if f is a Foo then bar of f is -2147483648 .
			 
			 Rule R15 if f is a Foo and bar of f <= -2147483648 then print("big").
			 Rule R16 if f is a Foo then bar of f is -2147483648 .
 		'''.sadl
 		val issues = validationTestHelper.validate(sadlModel)
		assertNotNull(issues)
		assertEquals(14, issues.size)
		var errIdx = 0
		var mismatches = 0
		for (issue:issues) {
			val err = errs.get(errIdx++)
			if (!issue.toString.contains(err)) {
				System.out.println(issue.toString + " != " + err)
				mismatches++
			}
		}
		assertEquals(mismatches, 0)
 	}
 	
 	@Test
 	def void testUseOfClassAsProperty() {
 		var errs = newArrayList("'THING' is not a property",
 			"'THING' is not a property"
 		)
 		val sadlModel = '''
 			 uri "http://sadl.org/ErrorFromCraig.sadl" alias ErrorFromCraig.
 			 
 			SYSTEM is a class.	// this must have been imported?
 			
 			THING is a class.
 			THING describes SYSTEM.
 			THING has exactly 2 values.
 			input describes THING with values of type INPUT.
 			INPUT is a class.
 			stuff describes INPUT with values of type integer.
 		'''.sadl
 		val issues = validationTestHelper.validate(sadlModel)
 		assertNotNull(issues)
 		assertEquals(2, issues.size)
		var errIdx = 0
		var mismatches = 0
		for (issue:issues) {
			val err = errs.get(errIdx++)
			if (!issue.toString.contains(err)) {
				System.out.println(issue.toString + " != " + err)
				mismatches++
			}
		}
		assertEquals(mismatches, 0)
 	}

	@Test
	def void testLiteralOutOfRangeLong() {
		var errs = newArrayList("Error converting to a number",
			"Unable to convert value '-9223372036854775809' to type 'http://www.w3.org/2001/XMLSchema#long'(For input string: \"-9223372036854775809\")",
			"Error converting to a number",
			"Unable to convert value '9223372036854775808' to type 'http://www.w3.org/2001/XMLSchema#long'(For input string: \"9223372036854775808\")",
			"Value is not in range of property",
			"Error converting to a number",
			"Value is not in range of property"
		)
		val sadlModel = '''
			 uri "http://sadl.org/LiteralOutOfRangeLong.sadl" alias LiteralOutOfRangeLong.
			 
			 Foo is a class described by bar with values of type long.
			 MyFoo1 is a Foo with bar -9223372036854775809 .
			 MyFoo2 is a Foo with bar 9223372036854775808 .
			 
			 Rule R1 if f is a Foo and bar of f >= 9223372036854775807 then print("bigger than long").
			 Rule R2 if f is a Foo then bar of f is -9223372036854775808 .   
			
			 Rule R3 if f is a Foo and bar of f > 9223372036854775807 then print("bigger than long").
			 Rule R4 if f is a Foo then bar of f is -9223372036854775809 .
			  
			 Rule R5 if f is a Foo and bar of f = 9223372036854775807 then print("bigger than long").
			 Rule R6 if f is a Foo then bar of f is -9223372036854775808 .   
			  
			 Rule R7 if f is a Foo and bar of f <= 9223372036854775807 then print("bigger than long").
			 Rule R8 if f is a Foo then bar of f is -9223372036854775808 .   
			
			 Rule R9 if f is a Foo and bar of f < 9223372036854775807 then print("bigger than long").
			 Rule R10 if f is a Foo then bar of f is -9223372036854775808 .
 		'''.sadl
		val issues = validationTestHelper.validate(sadlModel)
		assertNotNull(issues)
		assertEquals(7, issues.size)
		var errIdx = 0
		var mismatches = 0
		for (issue:issues) {
			val err = errs.get(errIdx++)
			if (!issue.toString.contains(err)) {
				System.out.println(issue.toString + " != " + err)
				mismatches++
			}
		}
		assertEquals(mismatches, 0)
	}
	
	@Test
	def void testInstanceInConditionBeforeDeclaration() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias m.
			 
			  SYSTEM is a class.
			      property_1 describes SYSTEM with values of type CLASS.
			      property_1 of SYSTEM only has values of type CLASS.
			      property_1 of SYSTEM has exactly 1 values.
			      property_1 of SYSTEM always has value Instance_1. 
			      
			  CLASS is a class, must be one of {Instance_1, Instance_2}.

			  SYSTEM2 is a class.
			     property_1b describes SYSTEM2 with values of type CLASS2.
			     property_1b of SYSTEM2 only has values of type CLASS2.
			     property_1b of SYSTEM2 has exactly 1 values.
			     property_1b of SYSTEM2 always has value Instance_1b. 
			      
			 CLASS2 is a class, can only be one of {Instance_1b, Instance_2b}.
			 
			 Bar is a class.
			 OtherClass is a class described by foo with values of type Bar.
			 foo of OtherClass always has value GoldBar.
			 
			 GoldBar is a Bar.
		'''.assertValidatesTo[jenaModel, issues |
				assertNotNull(jenaModel)
//				jenaModel.write(System.out)
				assertTrue(issues.size == 0)
			]
	}
	
	@Test
	def void testInstanceInConditionBeforeDeclaration2() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias m.
			 
			  SYSTEM is a class.
			      property_1 describes SYSTEM with values of type CLASS2.
			      property_1 of SYSTEM only has values of type CLASS2.
			      property_1 of SYSTEM has exactly 1 values.
			      
			  CLASS2 is a class.

			  SYSTEM2 is a class.
			     property_1b describes SYSTEM2 with values of type CLASS3.
			     property_1b of SYSTEM2 only has values of type CLASS3.
			     property_1b of SYSTEM2 has exactly 1 values.
			      
			 CLASS3 is a type of CLASS2.
		'''.assertValidatesTo[jenaModel, issues |
				assertNotNull(jenaModel)
//				jenaModel.write(System.out)
				assertTrue(issues.size == 0)
			]
	}
	
	@Test
	def void testInstanceInConditionBeforeDeclaration3() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias m.
			 
			  SYSTEM is a class.
			      property_1 describes SYSTEM with values of type CLASS2.
			      property_1 of SYSTEM has at least one value of type CLASS2.
			      property_1 of SYSTEM has exactly 1 values.
			      
			  CLASS2 is a class.

			  SYSTEM2 is a class.
			     property_1b describes SYSTEM2 with values of type CLASS3.
			     property_1b of SYSTEM2 has at least one value of type CLASS3.
			     property_1b of SYSTEM2 has exactly 1 values.
			      
			 CLASS3 is a type of CLASS2.
		'''.assertValidatesTo[jenaModel, issues |
				assertNotNull(jenaModel)
//				jenaModel.write(System.out)
				assertTrue(issues.size == 0)
			]
	}
	
	@Test
	def void testParametersPassed() {
		var errs = newArrayList("Number of arguments does not match function declaration",
			"string, an RDF datatype  string, cannot operate (passed argument) with decimal, an RDF datatype  decimal.",
			"string, an RDF datatype  string, cannot operate (passed argument) with decimal, an RDF datatype  decimal.",
			"Number of arguments does not match function declaration "
		)
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias m.
			 
			 Foo is a class described by intProp with values of type int,
			 	described by longProp with values of type long,
			 	described by decimalProp with values of type decimal,
			 	described by otherDecimalProp with values of type decimal.
			 	
			 Equation eqAdd23(decimal x) returns decimal: x + 23 .
			 	
			 Rule R1: if f is a Foo then otherDecimalProp is max(PI, decimalProp of f).
			 Rule R1b: if f is a Foo then otherDecimalProp is max(PI, decimalProp of f, e). 
			 Rule R1c: if f is a Foo then otherDecimalProp is max(PI). 
			 Rule R2: if f is a Foo then otherDecimalProp is max(PI, "23").
			 Rule R3: if f is a Foo then otherDecimalProp is eqAdd23(PI).
			 Rule R4: if f is a Foo then otherDecimalProp is eqAdd23("14").
			 Rule R5: if f is a Foo then otherDecimalProp is eqAdd23(PI, "14").
		'''.sadl
		val issues = validationTestHelper.validate(sadlModel)
		assertNotNull(issues)
		assertEquals(4, issues.size)
		var errIdx = 0
		var mismatches = 0
		for (issue:issues) {
			val err = errs.get(errIdx++)
			if (!issue.toString.contains(err)) {
				System.out.println(issue.toString + " != " + err)
				mismatches++
			}
		}
		assertEquals(mismatches, 0)
	}
	
	@Test
	def void testAllValuesFromUnnamedTypedList() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias model.
			 Whimsy is a class.			 
			 Foo is a class described by bar with values of type Whimsy List.	 
			 bar of Foo only has values of type Whimsy List.
		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
// 			jenaModel.write(System.out, "N-TRIPLE")
 			assertTrue(issues.size == 0)
 			assertNotNull(jenaModel.getOntClass("http://sadl.org/model.sadl#Foo"))
 			val fooclass = jenaModel.getOntClass("http://sadl.org/model.sadl#Foo")
 			val eitr = fooclass.listSuperClasses
 			var found = false
 			while (eitr.hasNext) {
 				val sprcls =eitr.next
 				if (sprcls instanceof OntClass) {
 					val onprop = (sprcls as OntClass).asRestriction.onProperty
 					if (onprop.localName.equals("bar")) {
 						val rng = onprop.range
 						if ((sprcls as OntClass).asRestriction.allValuesFromRestriction) {
 							val avfcls = (sprcls as OntClass).asRestriction.asAllValuesFromRestriction.allValuesFrom
 							assertTrue(avfcls.anon)
 							assertTrue(avfcls instanceof OntResource)
 							assertTrue(avfcls.equals(rng)		// this is the test to see that only one class of type "Whimsy List" is created
 							)
 							val nitr = (avfcls as OntResource).listPropertyValues(RDFS.subClassOf)
 							while (nitr.hasNext) {
 								val nd = nitr.next
 								if (nd.URIResource && nd.asResource.localName.equals("List")) {
 									found = true;
 								}
 							}
 						}
 					}
 				}
 			}
 			assertTrue(found)
 		]
	}

	@Ignore ("This can't pass until the newer grammar is being used so that unnamed typed lists can have length restrictions")
	@Test
	def void testAllValuesFromUnnamedTypedList_02() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias model.
			 Whimsy is a class.			 
			 Foo is a class described by bar with values of type Whimsy List length 1-4.	 
			 bar of Foo only has values of type Whimsy List length 1-4.
		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
// 			jenaModel.write(System.out, "N-TRIPLE")
 			assertTrue(issues.size == 0)
 			assertNotNull(jenaModel.getOntClass("http://sadl.org/model.sadl#Foo"))
 			val fooclass = jenaModel.getOntClass("http://sadl.org/model.sadl#Foo")
 			val eitr = fooclass.listSuperClasses
 			var found = false
 			while (eitr.hasNext) {
 				val sprcls =eitr.next
 				if (sprcls instanceof OntClass) {
 					val onprop = (sprcls as OntClass).asRestriction.onProperty
 					if (onprop.localName.equals("bar")) {
 						if ((sprcls as OntClass).asRestriction.allValuesFromRestriction) {
 							val avfcls = (sprcls as OntClass).asRestriction.asAllValuesFromRestriction.allValuesFrom
 							assertTrue(avfcls.anon)
 							assertTrue(avfcls instanceof OntResource)
 							val nitr = (avfcls as OntResource).listPropertyValues(RDFS.subClassOf)
 							while (nitr.hasNext) {
 								val nd = nitr.next
 								if (nd.URIResource && nd.asResource.localName.equals("List")) {
 									found = true;
 								}
 							}
 						}
 					}
 				}
 			}
 			assertTrue(found)
 		]
	}

	@Ignore ("This can't pass until the newer grammar is being used so that unnamed typed lists can have length restrictions")
	@Test
	def void testAllValuesFromUnnamedTypedList_03() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias model.
			 Whimsy is a class.			 
			 Foo is a class described by bar with values of type Whimsy List length 1-4.	 
			 bar of Foo only has values of type Whimsy List length 1-*.
		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
// 			jenaModel.write(System.out, "N-TRIPLE")
 			assertTrue(issues.size == 0)
 			assertNotNull(jenaModel.getOntClass("http://sadl.org/model.sadl#Foo"))
 			val fooclass = jenaModel.getOntClass("http://sadl.org/model.sadl#Foo")
 			val eitr = fooclass.listSuperClasses
 			var found = false
 			while (eitr.hasNext) {
 				val sprcls =eitr.next
 				if (sprcls instanceof OntClass) {
 					val onprop = (sprcls as OntClass).asRestriction.onProperty
 					if (onprop.localName.equals("bar")) {
 						if ((sprcls as OntClass).asRestriction.allValuesFromRestriction) {
 							val avfcls = (sprcls as OntClass).asRestriction.asAllValuesFromRestriction.allValuesFrom
 							assertTrue(avfcls.anon)
 							assertTrue(avfcls instanceof OntResource)
 							val nitr = (avfcls as OntResource).listPropertyValues(RDFS.subClassOf)
 							while (nitr.hasNext) {
 								val nd = nitr.next
 								if (nd.URIResource && nd.asResource.localName.equals("List")) {
 									found = true;
 								}
 							}
 						}
 					}
 				}
 			}
 			assertTrue(found)
 		]
	}

	@Test
	def void testAllValuesFromUnnamedTypedList_04() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias model.
			 Whimsy is a class.	
			 Mopsy is a class.		 
			 Foo is a class described by bar with values of type Whimsy List,
			 				described by rab with values of type Mopsy List,
			 				described by status with values of type string.	 
			 bar of Foo only has values of type Whimsy List.
			 rab of Foo only has values of type Mopsy List.
			 
			 Rule R1: if x is a Foo and bar of x = rab of x then status of x is "this should not ever work".
			 
		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
// 			jenaModel.write(System.out, "N-TRIPLE")
 			assertTrue(issues.size == 1)
 			issues.get(0).message.equals("bar, an object property with range  a List of values of type Whimsy, cannot be compared (=) with rab, an object property with range  a List of values of type Mopsy.")
 		]
	}

	@Test
	def void testAllValuesFromUnnamedTypedList_05() {
		val sadlModel = '''
			 uri "http://sadl.org/model.sadl" alias model.
			 Whimsy is a class.	
			 WhimsyList is a type of Whimsy List.
			 Mopsy is a class.		 
			 MopsyList is a type of Mopsy List.
			 Foo is a class described by bar with values of type WhimsyList,
			 				described by rab with values of type MopsyList,
			 				described by status with values of type string.	 
			 bar of Foo only has values of type WhimsyList.
			 rab of Foo only has values of type MopsyList.
			 
			 Rule R1: if x is a Foo and bar of x = rab of x then status of x is "this should not ever work".
			 
		'''.assertValidatesTo [ jenaModel, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
// 			jenaModel.write(System.out, "N-TRIPLE")
 			assertTrue(issues.size == 1)
 			issues.get(0).message.equals("bar, an object property with range  a List of values of type WhimsyList, cannot be compared (=) with rab, an object property with range  a List of values of type MopsyList.")
 		]
	}

	protected def Resource assertValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code)
//		validationTestHelper.assertNoErrors(model)
		val processor = processorProvider.get
		val List<Issue> issues= newArrayList
		processor.onValidate(model.eResource, new ValidationAcceptorImpl([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
		assertions.apply(processor.theJenaModel, issues)
		return model.eResource
	}

}
