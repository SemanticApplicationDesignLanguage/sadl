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
import org.apache.jena.vocabulary.RDFS
import org.apache.jena.ontology.Individual

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
	def void testCase1() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 1
			 prop1 is a property.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop1")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
 			assertTrue(sitr2.isEmpty)
 		]
	}

	@Test
	def void testCase2() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 2
			 {Domain2, Range2} are classes.
			 relationship of Domain2 to Range2 is prop2.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain2")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop2")
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain2"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
 			val rng = sitr2.next.object
 			assertTrue(rng.asResource.URI.equals("http://sadl.org/test.sadl#Range2"))
 		]
	}

	@Test
	def void testCase3a() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 3 (3a)
			 Domain3a is a class.
			 Range3a is a class.
			 {RInst3aa, RInst3ab} are instances of Range3a.
			 prop3a describes Domain3a must be one of { RInst3aa, RInst3ab}.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain3a")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop3a")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr1.isEmpty)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain3a"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val svf = sc.getPropertyValue(OWL.someValuesFrom)
 			assertTrue(svf.^as(OntClass).isEnumeratedClass)
 			val eitr2 = svf.^as(OntClass).asEnumeratedClass.listOneOf
 			assertFalse(eitr2.isEmpty)
 			while (eitr2.hasNext) {
 				val el = eitr2.next
 				println(el.toString)
 				assertTrue(el.localName.equals("RInst3aa") || el.localName.equals("RInst3ab"))
 			}
 			
 		]
	}

	@Test
	def void testCase3b() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 3 (3b)
			 Domain3b is a class.
			 Range3b is a class.
			 RInst3b is a Range3b.
			 prop3b describes Domain3b always has value RInst3b.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain3b")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop3b")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr1.isEmpty)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain3b"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val svf = sc.getPropertyValue(OWL.hasValue)
 			assertTrue(svf.isResource)
 			assertTrue(svf.asResource.canAs(Individual))
			assertTrue(svf.asResource.URI.equals("http://sadl.org/test.sadl#RInst3b"))
 		]
	}

	@Test
	def void testCase3c() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 3 (3c)
			 Domain3c is a class.
			 Class3c is a class.
			 prop3c describes Domain3c only has values of type Class3c.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain3c")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop3c")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr1.isEmpty)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain3c"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val avf = sc.getPropertyValue(OWL.allValuesFrom)
 			assertTrue(avf.isResource)
 			assertTrue(avf.asResource.canAs(OntClass))
			assertTrue(avf.asResource.URI.equals("http://sadl.org/test.sadl#Class3c"))
 		]
	}

	@Test
	def void testCase3d() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 3 (3d)
			 Domain3d is a class.
			 Class3d is a class.
			 prop3d describes Domain3d has at least one value of type Class3d.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain3d")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop3d")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr1.isEmpty)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain3d"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val avf = sc.getPropertyValue(OWL.someValuesFrom)
 			assertTrue(avf.isResource)
 			assertTrue(avf.asResource.canAs(OntClass))
			assertTrue(avf.asResource.URI.equals("http://sadl.org/test.sadl#Class3d"))
 		]
	}

	@Test
	def void testCase3e() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 3 (3e)
			 Domain3e is a class.
			 Class3e is a class.
			 prop3e describes Domain3e has at least 1 value of type Class3e.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain3e")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop3e")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr1.isEmpty)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain3e"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val oc = sc.getPropertyValue(OWL2.onClass)
 			assertTrue(oc.isResource)
 			assertTrue(oc.asResource.canAs(OntClass))
			assertTrue(oc.asResource.URI.equals("http://sadl.org/test.sadl#Class3e"))
			val mqc = sc.getPropertyValue(OWL2.minQualifiedCardinality)
			assertTrue(mqc.literal)
			assertTrue(mqc.asLiteral.int == 1)
 		]
	}

	@Test
	def void testCase3f() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 3 (3f)
			 Domain3f is a class.
			 Class3f is a class.
			 prop3f describes Domain3f has at most 1 value of type Class3f.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain3f")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop3f")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr1.isEmpty)
 			val dmn = sitr1.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain3f"))
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val oc = sc.getPropertyValue(OWL2.onClass)
 			assertTrue(oc.isResource)
 			assertTrue(oc.asResource.canAs(OntClass))
			assertTrue(oc.asResource.URI.equals("http://sadl.org/test.sadl#Class3f"))
			val mqc = sc.getPropertyValue(OWL2.maxQualifiedCardinality)
			assertTrue(mqc.literal)
			assertTrue(mqc.asLiteral.int == 1)
 		]
	}
	
	@Test
	def void testCase4a() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4a
			 Domain4a is a class.
			 Range4a is a class.
			 {RInst4aa, RInst4ab, RInst4ac} are instances of Range4a.	
			 prop4a of Domain4a must be one of { RInst4aa, RInst4ab}.	// called case 6 in code comment
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4a")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4a")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val svf = sc.getPropertyValue(OWL.someValuesFrom)
 			assertTrue(svf.^as(OntClass).isEnumeratedClass)
 			val eitr2 = svf.^as(OntClass).asEnumeratedClass.listOneOf
 			assertFalse(eitr2.isEmpty)
 			while (eitr2.hasNext) {
 				val el = eitr2.next
 				println(el.toString)
 				assertTrue(el.localName.equals("RInst4aa") || el.localName.equals("RInst4ab"))
 			}
 		]
	}	

	@Test
	def void testCase4b() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4 (4b)
			 Domain4b is a class.
			 Range4b is a class.
			 RInst4b is a Range4b.
			 prop4b of Domain4b always has value RInst4b.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4b")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4b")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val svf = sc.getPropertyValue(OWL.hasValue)
 			assertTrue(svf.isResource)
 			assertTrue(svf.asResource.canAs(Individual))
			assertTrue(svf.asResource.URI.equals("http://sadl.org/test.sadl#RInst4b"))
 		]
	}

	@Test
	def void testCase4c() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4 (4c)
			 Domain4c is a class.
			 Class4c is a class.
			 prop4c of Domain4c only has values of type Class4c.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4c")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4c")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val avf = sc.getPropertyValue(OWL.allValuesFrom)
 			assertTrue(avf.isResource)
 			assertTrue(avf.asResource.canAs(OntClass))
			assertTrue(avf.asResource.URI.equals("http://sadl.org/test.sadl#Class4c"))
 		]
	}

	@Test
	def void testCase4d() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4 (4d)
			 Domain4d is a class.
			 Class4d is a class.
			 prop4d of Domain4d has at least one value of type Class4d.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4d")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4d")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val avf = sc.getPropertyValue(OWL.someValuesFrom)
 			assertTrue(avf.isResource)
 			assertTrue(avf.asResource.canAs(OntClass))
			assertTrue(avf.asResource.URI.equals("http://sadl.org/test.sadl#Class4d"))
 		]
	}

	@Test
	def void testCase4e() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4 (4e)
			 Domain4e is a class.
			 Class4e is a class.
			 prop4e of Domain4e has at least 1 value of type Class4e.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4e")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4e")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val oc = sc.getPropertyValue(OWL2.onClass)
 			assertTrue(oc.isResource)
 			assertTrue(oc.asResource.canAs(OntClass))
			assertTrue(oc.asResource.URI.equals("http://sadl.org/test.sadl#Class4e"))
			val mqc = sc.getPropertyValue(OWL2.minQualifiedCardinality)
			assertTrue(mqc.literal)
			assertTrue(mqc.asLiteral.int == 1)
 		]
	}

	@Test
	def void testCase4f() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4 (4f)
			 Domain4f is a class.
			 Class4f is a class.
			 prop4f of Domain4f has at most 1 value of type Class4f.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4f")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4f")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val oc = sc.getPropertyValue(OWL2.onClass)
 			assertTrue(oc.isResource)
 			assertTrue(oc.asResource.canAs(OntClass))
			assertTrue(oc.asResource.URI.equals("http://sadl.org/test.sadl#Class4f"))
			val mqc = sc.getPropertyValue(OWL2.maxQualifiedCardinality)
			assertTrue(mqc.literal)
			assertTrue(mqc.asLiteral.int == 1)
 		]
	}
	
	@Test
	def void testCase4g() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // case 4g
			 Domain4g is a class.
			 prop4g is a property.
			 Class4g is a class.
			 {Inst4ga, Inst4gb, Inst4gc} are instances of Class4g.
			 prop4g of Domain4g can only be one of {Inst4ga, Inst4gb}.	// called case 5
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain4g")
 			assertNotNull(dcls)
 			val nn = null as RDFNode
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop4g")
 			assertNotNull(prop)
 			val sitr1 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr1.isEmpty)
  			val sitr2 = jenaModel.listStatements(prop, RDFS.range, nn)
  			assertTrue(sitr2.isEmpty)
 			val eitr1 = dcls.listSuperClasses(true)
 			assertNotNull(eitr1)
 			val sc = eitr1.next
 			assertTrue(sc.canAs(Restriction))
 			val svf = sc.getPropertyValue(OWL.allValuesFrom)
 			assertTrue(svf.^as(OntClass).isEnumeratedClass)
 			val eitr2 = svf.^as(OntClass).asEnumeratedClass.listOneOf
 			assertFalse(eitr2.isEmpty)
 			while (eitr2.hasNext) {
 				val el = eitr2.next
 				println(el.toString)
 				assertTrue(el.localName.equals("Inst4ga") || el.localName.equals("Inst4gb"))
 			}
 		]
	}	
	@Test
	def void testCase7a() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // Case 7 (7a)
			 Domain7a is a class.
			 prop7aa is a property.
			 prop7ab is a property with values of type boolean.
			 prop7ab of prop7aa of Domain7a has default true.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain7a")
 			val paa = jenaModel.getProperty("http://sadl.org/test.sadl#prop7aa")
 			assertNotNull(paa)
 			// to make sure that it is an RDF property only, test for other types
 			val paaep = jenaModel.getObjectProperty("http://sadl.org/test.sadl#prop7aa")
 			assertNull(paaep)
 			val paaop = jenaModel.getDatatypeProperty("http://sadl.org/test.sadl#prop7aa")
 			assertNull(paaop)
 			val paaap = jenaModel.getAnnotationProperty("http://sadl.org/test.sadl#prop7aa")
 			assertNull(paaap)
 			val nn = null as RDFNode
 			val sitr = jenaModel.listStatements(dcls, RDFS.seeAlso, nn)
 			val dvbn = sitr.next.object
 			val sitr2 = jenaModel.listStatements(dvbn.asResource, jenaModel.getProperty("http://research.ge.com/Acuity/defaults.owl#hasDefault"), nn)
 			val dv = sitr2.next.object
 			val dvstr = dv.toString
 			assertTrue(dvstr.startsWith("true"))
 			// make sure that the property prop7ab doesn't have a domain
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop7ab")
 			val sitr3 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr3.isEmpty)
 			// now check interior of default value
 			val defns = "http://research.ge.com/Acuity/defaults.owl#"
 			val sitr4 = jenaModel.listStatements(dvbn.asResource, jenaModel.getProperty(defns+"appliesToPropertyChain"), nn)
 			assertFalse(sitr4.isEmpty)
 			val pce = sitr4.next.object
 			assertNotNull(pce)
 			val sitr5 = jenaModel.listStatements(pce.asResource, jenaModel.getProperty(defns + "propertyElement"), nn)
 			assertFalse(sitr5.isEmpty)
 			val pe = sitr5.next.object
 			assertTrue(pe.asResource.URI.equals(paa.URI))
 			val sitr6 = jenaModel.listStatements(pce.asResource, jenaModel.getProperty(defns + "nextPropertyChainElement"), nn)
 			assertFalse(sitr6.isEmpty)
 			val pce2 = sitr6.next.object
 			val sitr7 = jenaModel.listStatements(pce2.asResource, jenaModel.getProperty(defns + "propertyElement"), nn)
 			assertFalse(sitr7.isEmpty)
 			val pe2 = sitr7.next.object
 			assertTrue(pe2.asResource.URI.equals(prop.URI))
 		]
	}

	@Test
	def void testCase7b() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // Case 7 (7b)
			 Domain7b is a class.
			 Class7b is a class.
			 InstC7b is a Class7b.
			 prop7ba is a property.
			 prop7bb is a property with values of type Class7b.
			 prop7bb of prop7ba of Domain7b has default InstC7b.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			for (issue : issues) {
 				println(issue.toString)
 			}
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain7b")
 			val paa = jenaModel.getProperty("http://sadl.org/test.sadl#prop7ba")
 			assertNotNull(paa)
 			// to make sure that it is an RDF property only, test for other types
 			val paaep = jenaModel.getObjectProperty("http://sadl.org/test.sadl#prop7ba")
 			assertNull(paaep)
 			val paaop = jenaModel.getDatatypeProperty("http://sadl.org/test.sadl#prop7ba")
 			assertNull(paaop)
 			val paaap = jenaModel.getAnnotationProperty("http://sadl.org/test.sadl#prop7ba")
 			assertNull(paaap)
 			val nn = null as RDFNode
 			val sitr = jenaModel.listStatements(dcls, RDFS.seeAlso, nn)
 			val dvbn = sitr.next.object
 			val sitr2 = jenaModel.listStatements(dvbn.asResource, jenaModel.getProperty("http://research.ge.com/Acuity/defaults.owl#hasDefault"), nn)
 			val dv = sitr2.next.object
 			val dvstr = dv.toString
 			assertTrue(dvstr.equals("http://sadl.org/test.sadl#InstC7b"))
 			// make sure that the property prop7bb doesn't have a domain
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop7bb")
 			val sitr3 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertTrue(sitr3.isEmpty)
 			// now check interior of default value
 			val defns = "http://research.ge.com/Acuity/defaults.owl#"
 			val sitr4 = jenaModel.listStatements(dvbn.asResource, jenaModel.getProperty(defns+"appliesToPropertyChain"), nn)
 			assertFalse(sitr4.isEmpty)
 			val pce = sitr4.next.object
 			assertNotNull(pce)
 			val sitr5 = jenaModel.listStatements(pce.asResource, jenaModel.getProperty(defns + "propertyElement"), nn)
 			assertFalse(sitr5.isEmpty)
 			val pe = sitr5.next.object
 			assertTrue(pe.asResource.URI.equals(paa.URI))
 			val sitr6 = jenaModel.listStatements(pce.asResource, jenaModel.getProperty(defns + "nextPropertyChainElement"), nn)
 			assertFalse(sitr6.isEmpty)
 			val pce2 = sitr6.next.object
 			val sitr7 = jenaModel.listStatements(pce2.asResource, jenaModel.getProperty(defns + "propertyElement"), nn)
 			assertFalse(sitr7.isEmpty)
 			val pe2 = sitr7.next.object
 			assertTrue(pe2.asResource.URI.equals(prop.URI))
 		]
	}

	@Test
	def void testCase7c() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 // Case 7 (7c)
			 Domain7c is a class.
			 prop7c is a property with values of type boolean.
			 prop7c describes Domain7c has default true.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val dcls = jenaModel.getOntClass("http://sadl.org/test.sadl#Domain7c")
 			val nn = null as RDFNode
 			val sitr = jenaModel.listStatements(dcls, RDFS.seeAlso, nn)
 			val dvbn = sitr.next.object
 			val sitr2 = jenaModel.listStatements(dvbn.asResource, jenaModel.getProperty("http://research.ge.com/Acuity/defaults.owl#hasDefault"), nn)
 			val dv = sitr2.next.object
 			val dvstr = dv.toString
 			assertTrue(dvstr.startsWith("true"))
 			val prop = jenaModel.getProperty("http://sadl.org/test.sadl#prop7c")
 			val sitr3 = jenaModel.listStatements(prop, RDFS.domain, nn)
 			assertFalse(sitr3.isEmpty)
 			val dmn = sitr3.next.object
 			assertTrue(dmn.asResource.URI.equals("http://sadl.org/test.sadl#Domain7c"))
 			// now check interior of default value
 			val defns = "http://research.ge.com/Acuity/defaults.owl#"
 			val sitr4 = jenaModel.listStatements(dvbn.asResource, jenaModel.getProperty(defns+"appliesToPropertyChain"), nn)
 			assertFalse(sitr4.isEmpty)
 			val pce = sitr4.next.object
 			assertNotNull(pce)
 			val sitr5 = jenaModel.listStatements(pce.asResource, jenaModel.getProperty(defns + "propertyElement"), nn)
 			assertFalse(sitr5.isEmpty)
 			val pe = sitr5.next.object
 			assertTrue(pe.asResource.URI.equals(prop.URI))
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
	def void testPropertyInLinePropertyRestriction_10() {
		val sadlModel = '''
			 uri "http://sadl.org/GH-617.sadl" alias gh-617.
			 
			 D is a class.
			 C is a class,
			 	described by p2 with values of type string .
			 	
			 p2 of C has at least 1 value of type string.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/GH-617.sadl#C")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/GH-617.sadl#p2"))
 				val onclass = rest.getPropertyValue(OWL2.onDataRange).asResource
 				assertTrue(onclass.URI.equals("http://www.w3.org/2001/XMLSchema#string"))
 				assertTrue(rest.getPropertyValue(OWL2.minQualifiedCardinality).asLiteral.int == 1)
 				return
 			}
 			fail()
 		]
	}

	@Test
	def void testPropertyInLinePropertyRestriction_11() {
		val sadlModel = '''
			 uri "http://sadl.org/GH-617.sadl" alias gh-617.
			 
			 D is a class.
			 C is a class,
			 	described by p2 with values of type string .
			 	
			 p2 of C has at least one value of type string.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out)
 			assertTrue(issues.size == 0)
 			val pcls = jenaModel.getOntClass("http://sadl.org/GH-617.sadl#C")
 			val itr = pcls.listSuperClasses(true)
 			assertTrue(itr.hasNext)
 			val sprc = itr.next
 			if (sprc instanceof OntClass && (sprc as OntClass).canAs(Restriction)) {
 				val rest = (sprc as OntClass).^as(Restriction)
 				assertTrue(rest.onProperty.URI.equals("http://sadl.org/GH-617.sadl#p2"))
 				val onclass = rest.getPropertyValue(OWL.someValuesFrom).asResource
 				assertTrue(onclass.URI.equals("http://www.w3.org/2001/XMLSchema#string"))
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
 			for (issue : issues) {
 				println(issue.toString)
 			}
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

 	@Test
 	def void testGH_430() {
 		val sadlModel1 = '''
			 uri "http://sadl.org/base.sadl".
			 Base is a class.
 		'''.sadl
		val sadlModel2 = '''
			 uri "http://sadl.org/sub.sadl".
			 import "http://sadl.org/base.sadl" as base.
			 
			 MySub is a Base.
			 MySub is a base:Base.
			 base:Base is a class.
 		'''.sadl
 		val issues1 = _validationTestHelper.validate(sadlModel1)
		val issues2 = _validationTestHelper.validate(sadlModel2)
 		assertTrue(issues1.empty)
 		assertFalse(issues2.empty)
 		assertTrue(issues2.toString.contains("Declaration of concepts in another namespace not supported"))
 	
 	}
 	
  	@Test
 	def void testGH_518() {
 		val sadlModel1 = '''
			 uri "http://sadl.org/JavaExternal.sadl" alias javaexternal.
			 
			 External min(decimal n1, decimal n2) returns decimal : "java.lang.Math.min".
			 
			 Expr: min(2,3).
			 
			 Rule testRule: then print(min(2,3)).
 		'''.sadl
		val sadlModel2 = '''
			 uri "http://sadl.org/JavaExternalImported.sadl" alias javaexternalimported.
			 
			 import "http://sadl.org/JavaExternal.sadl" .
			 
			 Expr: min(2,3).
 		'''.sadl
 		val issues1 = _validationTestHelper.validate(sadlModel1)
		val issues2 = _validationTestHelper.validate(sadlModel2)
// 		assertTrue(issues1.empty)
// 		assertFalse(issues2.empty)
// 		assertTrue(issues2.toString.contains("Declaration of concepts in another namespace not supported"))
 		val numiss1 = issues1.size
 		val numiss2 = issues2.size
 	}
 	
 	@Test
 	def void testUserDefined01() {
 		val sadlModel = '''
			 uri "http://sadl.org/UserDefinedDataTypes.sadl" alias UserDefinedDataTypes.
			 
			 SL is a type of int [1,5].
			 
			 UDT is a type of {int or string}.
			 
			 SSN is a type of string "[0-9]{3}-[0-9]{2}-[0-9]{4}".
			 
			 MyClass is a class described by p1 with values of type SL, 
			 	described by p2 with values of type UDT,
			 	described by ssn with values of type SSN.
			 
			 mc1 is a MyClass with p1 5, with p2 "hi", with p2 4, with ssn "123-45-6789".
			 mc2 is a MyClass with p1 1, with p2 "a very long, long string".
			 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			if (issues !== null) {
 				for (iss : issues) {
 					println(iss.toString)
 				}
 			}
			assertTrue(issues.size == 0)
			
		]
 	}
 	
 	@Test
 	def void testUserDefined02() {
 		val sadlModel = '''
			 uri "http://sadl.org/UserDefinedDataTypes.sadl" alias UserDefinedDataTypes.
			 
			 SL is a type of int [1,5].
			 
			 UDT is a type of {int or string}.
			 
			 SSN is a type of string "[0-9]{3}-[0-9]{2}-[0-9]{4}".
			 
			 MyClass is a class described by p1 with values of type SL, 
			 	described by p2 with values of type UDT,
			 	described by ssn with values of type SSN.
			 
			 mc1 is a MyClass with p1 6, with ssn "12345".
			 mc2 is a MyClass with p1 0, with p2 false.		 
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			if (issues !== null) {
 				for (iss : issues) {
 					println(iss.toString)
 				}
 			}
			assertNotNull(issues)
			assertTrue(issues.size == 4)
		]
 	}
 	
 	@Test
 	def void testUserDefined03() {
 		val sadlModel = '''
			 uri "http://sadl.org/UserDefinedDataTypes.sadl" alias UserDefinedDataTypes.
			 
			 Requirement is a class.
			 reqName is a type of string "[a-Z0-9_]+".
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			if (issues !== null) {
 				for (iss : issues) {
 					println(iss.toString)
 				}
 			}
			assertNotNull(issues)
			assertTrue(issues.size == 1)
		]
 	}
 	
 	@Test
 	def void testUserDefined04() {
 		val sadlModel = '''
			 uri "http://sadl.org/UserDefinedDataTypes.sadl" alias UserDefinedDataTypes.
			 
			 Requirement is a class.
			 reqName is a type of string "^[a-z0-9_]+$".
			 name describes Requirement with a single value of type reqName.
			 r is a Requirement with name "This is an invalid name".
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			if (issues !== null) {
 				for (iss : issues) {
 					println(iss.toString)
 				}
 			}
			assertNotNull(issues)
			assertTrue(issues.size == 1)
		]
 	}
 	
 	@Test
 	def void testUserDefined05() {
 		val sadlModel = '''
			 uri "http://sadl.org/UserDefinedDataTypes.sadl" alias UserDefinedDataTypes.
			 
			 Requirement is a class.
			 name describes Requirement with a single value of type string "^[a-z0-9_]+$".
			 r is a Requirement with name "This is an invalid name".
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			if (issues !== null) {
 				for (iss : issues) {
 					println(iss.toString)
 				}
 			}
			assertNotNull(issues)
			assertTrue(issues.size == 1)
		]
 	}
 	
 	@Test
 	def void testUserDefined06() {
 		val sadlModel = '''
	 	 uri "http://sadl.org/ComplexUserdefinedDatatypes.sadl" alias complexuserdefineddatatypes.
	 	 
	 	 SmMedLg is a type of string {"small", "medium", "large"}.
	 	 
	 	 NumSize is a type of int [1,].
	 	 
	 	 MixedSize is a type of {SmMedLg or NumSize}.
	 	 
	 	 SizedThing is a class described by size with values of type MixedSize.
	 	 
	 	 GoodSized is a SizedThing with size "small".
	 	 GoodSized2 is a SizedThing with size 3.
	 	 
	 	 BadSized is a SizedThing with size "very small".
	 	 BadSized2 is a SizedThing with size 0.	
 	'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
 			if (issues !== null) {
 				for (iss : issues) {
 					println(iss.toString)
 				}
 			}
			assertNotNull(issues)
			assertTrue(issues.size == 2)
		]
	}

	@Test
	def void testGH_823a() {
		val sadlModel = '''
			 uri "http://sadl.org/Test.sadl" alias test.
			 
			 Agent is a class.
			 Person is a type of Agent.
			 Organization is a type of Agent.
			 
			 FileCreation is a class.
			 Package is a type of FileCreation.
			 
			 performedBy describes FileCreation with values of type Agent.
			 
			George is a Person.
			GE is an Organization.
			performedBy of Package must be one of {George, GE}.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertTrue(issues.empty)

			val pkgcls = jenaModel.getOntClass("http://sadl.org/Test.sadl#Package")
			val sitr = jenaModel.listStatements(pkgcls, RDFS.subClassOf, null as RDFNode)
			val restr = sitr.nextStatement.object
			assertTrue(restr.^as(OntClass).restriction)
//			jenaModel.write(System.out)
			val avfcls = jenaModel.getProperty(restr.^as(OntClass), OWL.allValuesFrom)
			assertNull(avfcls)
			val svfcls = jenaModel.getProperty(restr.^as(OntClass), OWL.someValuesFrom)
			assertNotNull(svfcls)
		]
	}

	@Test
	def void testGH_823b() {
		val sadlModel = '''
			 uri "http://sadl.org/Test.sadl" alias test.
			 
			 Agent is a class.
			 Person is a type of Agent.
			 Organization is a type of Agent.
			 
			 FileCreation is a class.
			 Package is a type of FileCreation.
			 
			 performedBy describes FileCreation with values of type Agent.
			 
			George is a Person.
			GE is an Organization.
			performedBy of Package can only be one of {George, GE}.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertTrue(issues.empty)

			val pkgcls = jenaModel.getOntClass("http://sadl.org/Test.sadl#Package")
			val sitr = jenaModel.listStatements(pkgcls, RDFS.subClassOf, null as RDFNode)
			val restr = sitr.nextStatement.object
			assertTrue(restr.^as(OntClass).restriction)
//			jenaModel.write(System.out)
			val avfcls = jenaModel.getProperty(restr.^as(OntClass), OWL.allValuesFrom)
			assertNotNull(avfcls)
			val svfcls = jenaModel.getProperty(restr.^as(OntClass), OWL.someValuesFrom)
			assertNull(svfcls)
		]
	}


	@Test
	def void testGH_823c() {
		val sadlModel = '''
			 uri "http://sadl.org/Test.sadl" alias test.
			 
			 Agent is a class.
			 Person is a type of Agent.
			 Organization is a type of Agent.
			 
			 FileCreation is a class.
			 Package is a type of FileCreation.
			 
			 performedBy describes FileCreation with values of type Agent.
			 
			performedBy of Package can only be one of {Person, Organization}.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertFalse(issues.empty)
			assertTrue(issues.size == 2)
			for (issue : issues) {
				println(issue.message)
				assertTrue(issue.message.contains("Expected an instance in the enumeration of the class"))
			}
		]
	}

	@Test
	def void testGH_823d() {
		val sadlModel = '''
			 uri "http://sadl.org/Test.sadl" alias test.
			 
			 Agent is a class.
			 Person is a type of Agent.
			 Organization is a type of Agent.
			 
			 FileCreation is a class.
			 Package is a type of FileCreation.
			 
			 performedBy describes FileCreation with values of type Agent.
			 
			 prop1 is a property.
			 prop2 is a property.
			performedBy of Package can only be one of {prop1, prop2}.
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertFalse(issues.empty)
			assertTrue(issues.size == 2)
			for (issue : issues) {
				println(issue.message)
				assertTrue(issue.message.contains("Expected an instance in the enumeration of the class"))
			}
		]
	}
	
	@Test
	def void testGH_828a() {
		val sadlModel = '''
			 uri "http://sadl.org/GH828.sadl" alias gh828.
			 Person is a class.
			 gender of Person must be one of {Male, Female}. 
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertFalse(issues.empty)
			assertTrue(issues.size == 2)
			for (issue : issues) {
				println(issue.message)
				assertTrue(issue.severity.equals(Severity.ERROR))
				assertTrue(issue.message.contains("Unable to determine type of instance, the property has no range"))
			}
		]
	}

	@Test
	def void testGH_828b() {
		val sadlModel = '''
			 uri "http://sadl.org/GH828.sadl" alias gh828.
			 Person is a class.
			 age of Person must be one of {"young", "old"}. 
		'''.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertFalse(issues.empty)
			assertTrue(issues.size == 2)
			for (issue : issues) {
				println(issue.message)
				assertTrue(issue.severity.equals(Severity.WARNING))
				assertTrue(issue.message.contains("Can't find range of property to create typed Literal"))
			}
		]
	}
	
	@Test
	def void testGH_904a() {
		val sadlModel = '''
			 uri "http://sadl.org/generalcase.sadl" alias generalcase.
			 
			 FinalClass is a class.
			 pn describes FinalClass with values of type Rn.
			 Rn is a class.
			 pm describes Rn with values of type Rm.
			 Rm is a class.
			 
			 pl describes Rm with values of type Rpl.
			 Rpl is a class.
			 p1 describes Rpl with values of type Rp1.
			 Rp1 is a class.
			 
			 qk describes Rm with values of type Rqk.
			 Rqk is a class.
			 q1 describes Rqk with values of type Rq1.
			 Rq1 is a class.
			 
			 FC1 is a FinalClass.
			 
			 Ask: p1 of pl and q1 of qk of pm of pn of FC1.
			 Ask: p1 of pl of pm of pn of FC1 and q1 of qk of pm of pn of FC1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor|
			assertTrue(issues.empty)
			assertNotNull(cmds);
			assertTrue(cmds.size == 2)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			assertEquals(cmds.get(0).toString, cmds.get(1).toString)
		]
	}
	
	@Test
	def void testGH_904b() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Part is a class described by processing with values of type Processing.
			 Processing is a class described by temperature with values of type float,
			 	described by volume with values of type float.
			 	
			 part1 is a Part. 	
			 	
			 Ask: temperature and volume of the processing of part1.
			 Ask: temperature of the processing of part1 and volume of the processing of part1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor|
			assertTrue(issues.empty)
			assertNotNull(cmds);
			assertTrue(cmds.size == 2)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			assertEquals(cmds.get(0).toString, cmds.get(1).toString)
		]
	}

	@Test
	def void testGH_904c() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Part is a class described by processing with values of type Processing.
			 Processing is a class described by temperature with values of type float,
			 	described by volume with values of type float.
			 	
			 part1 is a Part. 	
			 	
			 Ask: the temperature and volume of the processing of part1.
			 Ask: temperature of the processing of part1 and volume of the processing of part1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor|
			assertTrue(issues.empty)
			assertNotNull(cmds);
			assertTrue(cmds.size == 2)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			assertEquals(cmds.get(0).toString, cmds.get(1).toString)
		]
	}

	@Test
	def void testGH_904d() {
		val sadlModel = '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part. 	
			
			Ask: temperature and processNum of the processing of part1.
			Ask: temperature of the processing of part1 and processNum of the processing of part1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor|
			assertFalse(issues.empty)
			assertTrue(issues.get(0).toString.startsWith("WARNING:temperature, an object property with domain  http://sadl.org/test.sadl#SubProcess, may, but is not guaranteed to (because it is broader), operate (chained property) with processing, an object property with range  http://sadl.org/test.sadl#Process."))
			assertNotNull(cmds);
			assertTrue(cmds.size == 2)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			assertEquals(cmds.get(0).toString, cmds.get(1).toString)
		]
	}
	
	@Test
	def void testGH_904e() {
		val sadlModel = '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process,
				described by partID with values of type string.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
			
			part1 is a Part with partID "123". 	
			
			Ask: temperature of the processing of (a Part with partID "123").
			Ask: temperature and processNum of the processing of (a Part with partID "123").
			Ask: temperature of the processing of (a Part with partID "123") and processNum of the processing of (a Part with partID "123").
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor|
			assertFalse(issues.empty)
			assertTrue(issues.get(0).toString.startsWith("WARNING:temperature, an object property with domain  http://sadl.org/test.sadl#SubProcess, may, but is not guaranteed to (because it is broader), operate (chained property) with processing, an object property with range  http://sadl.org/test.sadl#Process."))
			assertNotNull(cmds);
			assertTrue(cmds.size == 3)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			assertEquals(cmds.get(1).toString, "select v0 v1 v2 v3 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v1), and(rdf(v1, test:temperature, v2), rdf(v1, test:processNum, v3))))")
			assertEquals(cmds.get(2).toString, "select v0 v2 v3 v1 v4 v5 where and(rdf(v0, test:partID, \"123\"), and(rdf(v0, test:processing, v2), and(rdf(v2, test:temperature, v3), and(rdf(v1, test:partID, \"123\"), and(rdf(v1, test:processing, v4), rdf(v4, test:processNum, v5))))))")
		]
	}
	
	@Test
	def void testGH_904f() {
		val sadlModel = '''
			uri "http://sadl.org/test.sadl" alias test.
			
			Part is a class described by processing with values of type Process,
				described by partID with values of type string.
			Process is a class
			described by processNum with a single value of type string .
			
			SubProcess is a type of Process
			described by temperature with values of type UnittedQuantity
			described by volume with values of type UnittedQuantity
			described by pressure with values of type UnittedQuantity .
						
			Ask: temperature of the processing of (a Part).
			Ask: temperature and processNum of the processing of (a Part).
			Ask: temperature of the processing of (a Part) and processNum of the processing of (the Part).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor|
			assertFalse(issues.empty)
			assertTrue(issues.get(0).toString.startsWith("WARNING:temperature, an object property with domain  http://sadl.org/test.sadl#SubProcess, may, but is not guaranteed to (because it is broader), operate (chained property) with processing, an object property with range  http://sadl.org/test.sadl#Process."))
			assertNotNull(cmds);
			assertTrue(cmds.size == 3)
			for (cmd : cmds) {
				println(cmd.toString)
			}
			assertEquals(cmds.get(1).toString, "select v0 v1 v2 v3 where and(rdf(v0, test:processing, v1), and(rdf(v1, test:temperature, v2), rdf(v1, test:processNum, v3)))")
			assertEquals(cmds.get(2).toString, "select v0 v2 v3 v1 v4 v5 where and(rdf(v0, test:processing, v2), and(rdf(v2, test:temperature, v3), and(rdf(v1, test:processing, v4), rdf(v4, test:processNum, v5))))")
		]
	}
	
}
