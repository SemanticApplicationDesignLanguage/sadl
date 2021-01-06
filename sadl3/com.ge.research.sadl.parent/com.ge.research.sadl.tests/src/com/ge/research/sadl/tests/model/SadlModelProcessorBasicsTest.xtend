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
import org.apache.jena.ontology.Restriction
import org.apache.jena.vocabulary.OWL2
import org.apache.jena.vocabulary.RDF
import org.apache.jena.rdf.model.RDFList
import org.eclipse.xtext.diagnostics.Severity
import org.apache.jena.vocabulary.XSD
import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.model.gp.Query

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
 			val sitr = jenaModel.listStatements(null, OWL.distinctMembers, null as RDFNode)
 			assertTrue(!sitr.empty)
 			while (sitr.hasNext) {
 				val stmt = sitr.nextStatement
 				val obj = stmt.object
 				if (obj.isResource) {
 					val rsrcObj = obj.asResource
 					if (rsrcObj.canAs(RDF.List.class)) {
// 						val lst = rsrcObj.^as(RDF.List.class)
// 						val hd = (lst as RDFList).head
// 						assertTrue(hd.isResource)
// 						assertTrue(hd.asResource.localName.equals("CodeFile") ||
// 							hd.asResource.localName.equals("SomeModel"))
 					}
 					else {
 						fail
 					}
 				}
 			}
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
 			val sitr = jenaModel.listStatements(null, OWL.distinctMembers, null as RDFNode)
 			assertTrue(!sitr.empty)
 			while (sitr.hasNext) {
 				val stmt = sitr.nextStatement
 				val obj = stmt.object
 				if (obj.isResource) {
 					val rsrcObj = obj.asResource
 					if (rsrcObj.canAs(RDF.List.class)) {
// 						val lst = rsrcObj.^as(RDF.List.class)
// 						val hd = (lst as RDFList).head
// 						assertTrue(hd.isResource)
// 						assertTrue(hd.asResource.localName.equals("CodeFile") ||
// 							hd.asResource.localName.equals("SomeModel"))
 					}
 					else {
 						fail
 					}
 				}
 			}
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
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#age"))
 				assertTrue(rest.getPropertyValue(OWL2.onDataRange).asResource.URI.equals(XSD.xfloat.URI))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
  				return
 			}
 			fail()
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
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#age"))
 				assertTrue(rest.getPropertyValue(OWL2.onDataRange).asResource.URI.equals(XSD.xfloat.URI))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertySingleValue3() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by friend with a single value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertySingleValue4() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 friend describes Person with a single value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertySingleValue5() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by owns with a single value of type class.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#owns"))
 				assertTrue(rest.getPropertyValue(OWL2.cardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertySingleValue6() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 owns describes Person with a single value of type class.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#owns"))
 				assertTrue(rest.getPropertyValue(OWL2.cardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertySingleValue7() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by attr with a single value of type data.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#attr"))
 				assertTrue(rest.getPropertyValue(OWL2.cardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertySingleValue8() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 attr describes Person with a single value of type data.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#attr"))
 				assertTrue(rest.getPropertyValue(OWL2.cardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_01() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by friend with exactly 1 value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_02() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by friend with at least 1 value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.minQualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_03() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by friend with at most 1 value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.maxQualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_04() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 friend describes Person with exactly 1 value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_05() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 friend describes Person with at least 1 value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.minQualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_06() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 friend describes Person with at most 1 value of type Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#friend"))
 				assertTrue(rest.getPropertyValue(OWL2.onClass).asResource.URI.equals("http://sadl.org/test.sadl#Person"))
 				assertTrue(rest.getPropertyValue(OWL2.maxQualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_07() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Gender is a class, can only be one of {Male, Female}.
			 
			 gender is a property with values of type Gender.
			 
			 Person is a class described by gender with exactly 1 value .
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Person")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#gender"))
 				assertTrue(rest.getPropertyValue(OWL.cardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_08() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 OrderedChildren is a Person List.
			 Parent is a type of Person described by sortedChildren with exactly 1 value of type OrderedChildren.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Parent")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#sortedChildren"))
 				val onclass = rest.getPropertyValue(OWL2.onClass).asResource
 				assertTrue(onclass.URI.equals("http://sadl.org/test.sadl#OrderedChildren"))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_09() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class.
			 OrderedChildren is a Person List.
			 Parent is a type of Person.
			 sortedChildren describes Parent with exactly 1 value of type OrderedChildren.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Parent")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/test.sadl#sortedChildren"))
 				val onclass = rest.getPropertyValue(OWL2.onClass).asResource
 				assertTrue(onclass.URI.equals("http://sadl.org/test.sadl#OrderedChildren"))
 				assertTrue(rest.getPropertyValue(OWL2.qualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
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
	def void testInvalidPropertyRestrictionError() {
		val sadlModel = '''
			 uri "http://sadl.org/gh576.sadl" alias gh576.
			  
			 Gender is a class, can only be one of {Male, Female}.
			 
			 Person is a class described by friend with exactly 1 value of type Person,
			  	described by gender with exactly 1 value of type Gender.
			  
			 Woman is a type of Person, always has value Female.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
			assertTrue(issues.filter[severity === Severity.ERROR].size == 1)
 		]
	}
	
	@Test
	def void testNamedStructureAnnotationsRule() {
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
  			assertNotNull(cmds)
  			assertTrue(cmds.size == 4)
  			for (cmd:cmds) {
  				println(cmd.toString)
  				assertTrue(cmd.toString.startsWith("select i"))
  			}
		]
 	}
 	
 	@Test
 	def void testAmbiguousNamesInSparqlQuery() {
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
	def void testGH557_01() {
		val model = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Foo is a class described by bar with values of type Whim.
			 Whim is a class described by wham with values of type string.
			 MyFoo is a Foo with bar (a Whim MyWhim with wham "too bad").
			 Ask: select x where MyFoo is an x.
			 Ask: select c where MyFoo bar b and b is a c.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			val ask1 = cmds.get(0)
			println(ask1.toString)
			assertTrue(ask1 instanceof Query)
			assertTrue((ask1 as Query).variables.size == 1)
			assertTrue((ask1 as Query).variables.get(0).type.name.equals("Foo"))
			val ask2 = cmds.get(1)
			println(ask2.toString)
			assertTrue(ask2 instanceof Query)
			assertTrue((ask2 as Query).variables.size == 1)
			assertTrue((ask2 as Query).variables.get(0).type.name.equals("Whim"))
		]
	}
	
	@Test
	def void testGH557_02() {
		val model = '''
			 uri "http://sadl.imp/test_import_apvf" version "$Revision: 1.6 $ Last modified on $Date: 2015/06/30 21:27:34 $".
			 
			 Person is a class.
			 
			 Genius is a type of Person.
			 iq describes Person has values of type string. 
			 iq of Person must be one of {"low", "average", "high"}.
			 
			 a Person is a Genius only if iq always has value "high".
			 
			 //Rule GeniusRule: if p is a Person and p has iq "high" then p is a Genius.
			 
			 George is a Person, has iq "high".
			 
			 Test: George is a Genius.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			val ask1 = cmds.get(0)
			println(ask1.toString)
			assertTrue(ask1 instanceof com.ge.research.sadl.model.gp.Test)
			assertTrue((ask1 as com.ge.research.sadl.model.gp.Test).toString.equals("rdf(George, rdf:type, Genius)"))
		]
	}
		
	@Test
	def void testGH557_03() {
		val model = '''
			 uri "http://sadl.imp/test_import_apvf" version "$Revision: 1.6 $ Last modified on $Date: 2015/06/30 21:27:34 $".
			 
			 Person is a class.
			 
			 Genius is a type of Person.
			 iq describes Person has values of type string. 
			 iq of Person must be one of {"low", "average", "high"}.
			 
			 a Person is a Genius only if iq always has value "high".
			 
			 //Rule GeniusRule: if p is a Person and p has iq "high" then p is a Genius.
			 
			 George is a Person, has iq "high".
			 
			 Ask: George is a Genius.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			val ask1 = cmds.get(0)
			println(ask1.toString)
			assertTrue(ask1 instanceof Query)
			assertTrue((ask1 as Query).variables === null)
			assertTrue((ask1 as Query).toString.equals("rdf(George, rdf:type, Genius)"))
		]
	}
	
	@Test
	def void testGH597() {
		val model = '''
			 uri "http://sadl.org/GH-597.sadl" alias GH-597.
			 
			 Class1 is a class.
			 Class2 is a class.
			 Class3 is a class.
			 prop1 describes Class1 with values of type Class2.
			 prop2 describes prop1 with values of type Class3.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(issues)
			assertTrue(issues.size == 1)
			assertTrue(issues.get(0).message.equals("The domain of a property should be a class."))
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
