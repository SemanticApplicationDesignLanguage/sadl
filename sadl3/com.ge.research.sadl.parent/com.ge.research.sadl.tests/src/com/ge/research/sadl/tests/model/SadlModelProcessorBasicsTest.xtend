/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.apache.jena.ontology.AllValuesFromRestriction
import org.apache.jena.ontology.CardinalityRestriction
import org.apache.jena.ontology.HasValueRestriction
import org.apache.jena.ontology.OntClass
import org.apache.jena.rdf.model.RDFNode
import org.apache.jena.rdf.model.Resource
import org.apache.jena.vocabulary.OWL
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.apache.jena.ontology.OntResource

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorBasicsTest extends AbstractSADLModelProcessorTest {
		
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
			 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
	def void testSubpropertyWithRange () {
		val sadlModel = '''
			 uri "http://sadl.org/Issue471.sadl" alias Issue471.
			 
			 Person is a class described by sibling with values of type Person.
			 
			 {Man, Woman} are types of Person.
			 
			 sister is a type of sibling with values of type Woman.			 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val sisProp = jenaModel.getOntProperty("http://sadl.org/Issue471.sadl#sister")
 			assertNotNull(sisProp)
 			val rng = sisProp.range
 			assertNotNull(rng)
 			assertTrue(rng.URI.equals("http://sadl.org/Issue471.sadl#Woman"))
  		]
	}
	
	@Test
	def void testInstanceNotTheSame1() {
		val sadlModel = '''
			  uri "http://sadl.org/GH466.sadl" alias GH466.
			  
			  Entity is a class.
			 
			 CodeFile is an Entity.
			 
			 {CodeFile, SomeModel} are not the same.
			 
			 SomeModel is an Entity. 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)		// just checking for errors, owl:AllDifferent doesn't query well
 		]
	}
	
	@Test
	def void testInstanceNotTheSame2() {
		val sadlModel = '''
			  uri "http://sadl.org/GH466.sadl" alias GH466.
			  
			  Entity is a class.
			 
			 CodeFile is an Entity.
			 
			 CodeFile is not the same as SomeModel.
			 
			 SomeModel is an Entity. 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)		// just checking for errors, owl:AllDifferent doesn't query well
 		]
	}

	@Test
	def void testPropertySingleValue() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by age with a single value of type float.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
//		val implicitModel = '''
//			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
//			
//			impliedProperty is a type of annotation.
//			expandedProperty is a type of annotation.
//			UnittedQuantity is a class,
//				described by ^value with values of type decimal,
//				described by unit with values of type string.
//			^Rule is a class.
//			NamedQuery is a class.
//		'''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |]
//		
		val sadlModel = '''
			uri "http://sadl.org/Shapes.sadl" alias Shapes.
			 
			Shape is a class described by area with values of type float.
			 
			comment is a type of annotation.
			 
			Circle is a type of Shape, described by radius with values of type float.
			 
			MyCircle is a Circle with radius 3.
			 
			Rule AreaOfCircle (comment "ho, ho"):
			 	if c is a Circle
			 	then area of c is radius of c ^ 2 * PI.
			 	
«««			AreaOfCircle has comment "ho".
			 	
			Ask: area. 	
			
«««			Ask: x is a ^Rule.
		'''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
				assertNotNull(jenaModel)
				val rinst = jenaModel.getIndividual("http://sadl.org/Shapes.sadl#AreaOfCircle")
				val annp = jenaModel.getAnnotationProperty("http://sadl.org/Shapes.sadl#comment")
				val annv = rinst.getPropertyValue(annp);
				assertTrue(annv.asLiteral.value == "ho, ho")
				assertTrue(issues.size == 0)
			]
	}

	@Test
	def void testQueries() {
		val sadlModel = '''
			 uri "http://sadl.org/QueryWithEmbeddedDecl.sadl" alias QueryWithEmbeddedDecl.
			 			 
			 Foo is a class described by bar with values of type Whim.
			 Whim is a class described by wham with values of type string.
			 
			 W1 is a Whim with wham "hi".
			 F1 is a Foo with bar W1.
			 
			 Ask: select i where i is a Foo.
			 Ask: select i where i is a Foo with bar (a Whim with wham "hi").
			 Ask: select i where i is a Foo and i has bar x and x is a Whim and x has wham "hi".
			 Ask: i is a Foo with bar (a Whim with wham "hi").
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			for (issue:issues) {
 				println(issue.toString)
 			}
  			assertTrue(issues.size == 0)
  			for (cmd:cmds) {
  				println(cmd.toString)
  			}
		]
 	}
 	
	@Test
	def void testModelContents() {
		val sadlModel = '''
		 uri "http://sadl.org/gh557.sadl" alias gh557.
		 
		 Person is a class.
		 Man is a type of Person.
		 George is a Man.
		 
		 Ask: select x where George is an x.
		 '''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			for (issue:issues) {
 				println(issue.toString)
 			}
  		]
		val cnts = sadlModel.allContents
		while (cnts.hasNext) {
			val el = cnts.next
			println(el.class.toString)
		}
	}
	
 	@Test
 	def void testAmbiguousNamesInAparqlQuery() {
 		val sadlModel1 = '''
 		 uri "http://sadl.org/model1.sadl" alias model1.
 		 
 		 Rock is a class described by weight with values of type float.
 		 
 		 MyRock is a Rock with weight 24.5.
 		'''.sadl
 		val sadlModel2 = '''
		  uri "http://sadl.org/model2.sadl" alias model2.
		  
		  Oil is a class described by weight with values of type int.
  		'''.sadl
 		val sadlModel3 = '''
 		 uri "http://sadl.org/test.sadl" alias tst.
 		 
 		 import "http://sadl.org/model1.sadl".
 		 import "http://sadl.org/model2.sadl".
 		 
 		// Ask: select x, y where x has weight y.
 		 
 		 Ask: "select ?x ?y where {?x <weight> ?y}".
 		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
 			assertNotNull(issues)
 			assertTrue(issues.size == 1)
 			val issue0 = issues.get(0).toString
 			assertTrue(issue0.startsWith("ERROR:weight is ambiguous; see 'http://sadl.org/model2.sadl#weight' and 'http://sadl.org/model1.sadl#weight'"))
 		]
 	}
 	
 	@Test
 	def void testCommentNotOnDefinition() {
 		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (note "all good things") is a class.
			age is a property with values of type data.
			age (note "how old") describes AllThingsGood with values of type float.
			AllThingsGood (note "comment").
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.empty)
			val rsrc = jenaModel.getOntResource("http://sadl.org/a.sadl#AllThingsGood")
			val cmnts = rsrc.listComments(null)
			assertNotNull(cmnts)
//			assertTrue(cmnts.size == 2)
			var cntr = 0
			while (cmnts.hasNext) {
				val cmnt = cmnts.next;
				val cmntstr = cmnt.toString
				if (cmntstr.equals("all good things@en")) {
					cntr++
				}
				if (cmntstr.equals("comment@en")) {
					cntr++
				}
			}
			assertTrue(cntr == 2)
			
		]
 	}
 	
 	@Test
 	def void testCommentNotInNamespace() {
 				val sadlModel1 = '''
			 uri "http://sadl.org/model1.sadl" alias model1.
			 
			Rock is a class.
 		'''.sadl
		val sadlModel2 = '''
			 uri "http://sadl.org/model2.sadl" alias model2.
			 
			 import "http://sadl.org/model1.sadl".
			 
			 Rock (note "not here").
 		'''.sadl
 		val issues1 = _validationTestHelper.validate(sadlModel1)
		val issues2 = _validationTestHelper.validate(sadlModel2)
 		assertTrue(issues1.empty)
 		assertFalse(issues2.empty)
 		assertTrue(issues2.toString.contains("Annotations are only allowed in the model where a concept is defined"))
 	
 	}
	
	@Test
	def void testTypedList1() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 IntegerList is a type of int List.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
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
 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
 						val v = vitr.nextStatement.object
 						assertTrue((v as Resource).URI.equals("http://www.w3.org/2001/XMLSchema#int"));
  					}
 				}
 			} while (itr.hasNext)
 			assertTrue(listSubclass)
 					
 		]
	}

	@Test
	def void testTypedList2() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Test is a class described by grades with values of type int List length 2-4.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntProperty("http://sadl.org/test.sadl#grades")
 			val itr = pcls.listRange()
 			assertTrue(itr.hasNext)
 			do {
 				val rng = itr.next as OntResource
 				if (rng.canAs(OntClass)) {
	  				val itr2 = (rng.^as(OntClass)).listSuperClasses(true)
	 				assertTrue(itr2.hasNext)
		 			var listSubclass = false
		 			do {
		 				val sprc = itr2.next as OntClass
		 				if ((sprc as OntClass).URIResource && (sprc as OntClass).URI.equals("http://sadl.org/sadllistmodel#List")) {
		 					listSubclass = true
		 				}
		 				if (sprc.canAs(AllValuesFromRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
		 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
		 						val v = vitr.nextStatement.object
		 						assertTrue((v as Resource).URI.equals("http://www.w3.org/2001/XMLSchema#int"));
		  					}
		 				}
		 			} while (itr2.hasNext)
		 			assertTrue(listSubclass)
		 		}
 			} while (itr.hasNext)
  					
 		]
	}

	@Test
	def void testTypedList3() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Test is a class.
			 foo describes Test with values of type Test List length 2-4.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntProperty("http://sadl.org/test.sadl#foo")
 			val itr = pcls.listRange()
 			assertTrue(itr.hasNext)
 			do {
 				val rng = itr.next as OntResource
 				if (rng.canAs(OntClass)) {
	  				val itr2 = (rng.^as(OntClass)).listSuperClasses(true)
	 				assertTrue(itr2.hasNext)
		 			var listSubclass = false
		 			var hasLengthRest = false
		 			do {
		 				val sprc = itr2.next as OntClass
		 				if ((sprc as OntClass).URIResource && (sprc as OntClass).URI.equals("http://sadl.org/sadllistmodel#List")) {
		 					listSubclass = true
		 				}
		 				if (sprc.canAs(AllValuesFromRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
		 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
		 						val v = vitr.nextStatement.object
		 						assertTrue((v as Resource).URI.equals("http://sadl.org/test.sadl#Test"));
		  					}
		 				}
		 				else if (sprc.canAs(HasValueRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#maxLengthRestriction")) {
		 						hasLengthRest = true;
		 					}
		 				}
		 			} while (itr2.hasNext)
		 			assertTrue(listSubclass)
		 			assertTrue(hasLengthRest)
		 		}
 			} while (itr.hasNext)
  					
 		]
	}

	@Test
	def void testTypedList4() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Test is a class.
			 grades describes Test with values of type int List length 2-4.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntProperty("http://sadl.org/test.sadl#grades")
 			val itr = pcls.listRange()
 			assertTrue(itr.hasNext)
 			do {
 				val rng = itr.next as OntResource
 				if (rng.canAs(OntClass)) {
	  				val itr2 = (rng.^as(OntClass)).listSuperClasses(true)
	 				assertTrue(itr2.hasNext)
		 			var listSubclass = false
		 			var hasLengthRest = false
		 			do {
		 				val sprc = itr2.next as OntClass
		 				if ((sprc as OntClass).URIResource && (sprc as OntClass).URI.equals("http://sadl.org/sadllistmodel#List")) {
		 					listSubclass = true
		 				}
		 				if (sprc.canAs(AllValuesFromRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
		 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
		 						val v = vitr.nextStatement.object
		 						assertTrue((v as Resource).URI.equals("http://www.w3.org/2001/XMLSchema#int"));
		  					}
		 				}
		 				else if (sprc.canAs(HasValueRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#maxLengthRestriction")) {
		 						hasLengthRest = true;
		 					}
		 				}
		 			} while (itr2.hasNext)
		 			assertTrue(listSubclass)
		 			assertTrue(hasLengthRest)
		 		}
 			} while (itr.hasNext)
  					
 		]
	}

	@Test
	def void testTypedList5() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Test is a class described by foo with values of type Test List length 2-4.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntProperty("http://sadl.org/test.sadl#foo")
 			val itr = pcls.listRange()
 			assertTrue(itr.hasNext)
 			do {
 				val rng = itr.next as OntResource
 				if (rng.canAs(OntClass)) {
	  				val itr2 = (rng.^as(OntClass)).listSuperClasses(true)
	 				assertTrue(itr2.hasNext)
		 			var listSubclass = false
		 			var hasLengthRest = false
		 			do {
		 				val sprc = itr2.next as OntClass
		 				if ((sprc as OntClass).URIResource && (sprc as OntClass).URI.equals("http://sadl.org/sadllistmodel#List")) {
		 					listSubclass = true
		 				}
		 				if (sprc.canAs(AllValuesFromRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
		 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
		 						val v = vitr.nextStatement.object
		 						assertTrue((v as Resource).URI.equals("http://sadl.org/test.sadl#Test"));
		  					}
		 				}
		 				else if (sprc.canAs(HasValueRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#maxLengthRestriction")) {
		 						hasLengthRest = true;
		 					}
		 				}
		 			} while (itr2.hasNext)
		 			assertTrue(listSubclass)
		 			assertTrue(hasLengthRest)
		 		}
 			} while (itr.hasNext)
  					
 		]
	}

	@Test
	def void testTypedList6() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Test is a class described by grades with values of type int List length 2-4.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntProperty("http://sadl.org/test.sadl#grades")
 			val itr = pcls.listRange()
 			assertTrue(itr.hasNext)
 			do {
 				val rng = itr.next as OntResource
 				if (rng.canAs(OntClass)) {
	  				val itr2 = (rng.^as(OntClass)).listSuperClasses(true)
	 				assertTrue(itr2.hasNext)
		 			var listSubclass = false
		 			var hasLengthRest = false
		 			do {
		 				val sprc = itr2.next as OntClass
		 				if ((sprc as OntClass).URIResource && (sprc as OntClass).URI.equals("http://sadl.org/sadllistmodel#List")) {
		 					listSubclass = true
		 				}
		 				if (sprc.canAs(AllValuesFromRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#first")) {
		 						val vitr = jenaModel.listStatements(sprc, OWL.allValuesFrom, null as RDFNode)
		 						val v = vitr.nextStatement.object
		 						assertTrue((v as Resource).URI.equals("http://www.w3.org/2001/XMLSchema#int"));
		  					}
		 				}
		 				else if (sprc.canAs(HasValueRestriction)) {
		 					val opitr = jenaModel.listStatements(sprc, OWL.onProperty, null as RDFNode)
		 					val obj = opitr.nextStatement.object
		 					if ((obj as Resource).URI.equals("http://sadl.org/sadllistmodel#maxLengthRestriction")) {
		 						hasLengthRest = true;
		 					}
		 				}
		 			} while (itr2.hasNext)
		 			assertTrue(listSubclass)
		 			assertTrue(hasLengthRest)
		 		}
 			} while (itr.hasNext)
  					
 		]
	}

	@Test
	def void testNoDomainObjectProperty() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 PhysicalThing is a class.
			 weight with values of type UnittedQuantity.
			 
			 MyPetRock is a PhysicalThing with weight 10 lbs.
			 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
	 		for (issue:issues) {
				println(issue.toString)
			}
			assertNotNull(issues)
 			assertTrue(issues.size == 1)
 			assertTrue(issues.get(0).toString.contains("Can't check domain of property 'weight'"))
 		]
	}
	
	@Test
	def void testNoDomainAnnotationProperty() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 PhysicalThing is a class.
			 moreInfo is a type of annotation.
			 
			 MyPetRock is a PhysicalThing with moreInfo "more info".
			 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
	 		for (issue:issues) {
				println(issue.toString)
			}
			assertNotNull(issues)
 			assertTrue(issues.size == 0)
 		]
	}
	
	@Test
	def void testFormsOfSPV() {
		val model = '''
			uri "http://sadl.org/testformsofspv" alias tspv.
			Person is a class described by spouse with values of type Person.
			{George, Martha} are instances of Person.
			 George has spouse Martha.
			 
			{Jane, Jack} are instances of Person.
			 Jane spouse Jack.
			 
			{Bill, Bonnie} are instances of Person.
			 The spouse of Bill is Bonnie.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			val ns = "http://sadl.org/testformsofspv#"
			assertTrue(
				jenaModel.listStatements(jenaModel.getIndividual(ns + "George"),
					jenaModel.getOntProperty(ns + "spouse"), jenaModel.getIndividual(ns + "Martha")).next() !== null)
			assertTrue(
				jenaModel.listStatements(jenaModel.getIndividual(ns + "Jane"), jenaModel.getOntProperty(ns + "spouse"),
					jenaModel.getIndividual(ns + "Jack")).next() !== null)
			assertTrue(
				jenaModel.listStatements(jenaModel.getIndividual(ns + "Bill"), jenaModel.getOntProperty(ns + "spouse"),
					jenaModel.getIndividual(ns + "Bonnie")).next() !== null)
		]
	}
	
	
	@Test
	def void testSameAsDefinition() {
		'''
		uri "http://sadl.org/testsameasdefn" alias tsd.
		Man is a class.
		Parent is a class.
		Father is the same as {Man and Parent}.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			val ns = "http://sadl.org/testsameasdefn#"
			assertNotNull(
				jenaModel.getOntClass(ns + "Father"))
		]
	}

}
