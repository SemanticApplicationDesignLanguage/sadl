/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.SadlConstants
import com.ge.research.sadl.processing.ValidationAcceptorImpl
import com.ge.research.sadl.reasoner.ConfigurationManager
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.ontology.Ontology
import com.hp.hpl.jena.query.QueryExecutionFactory
import com.hp.hpl.jena.rdf.model.RDFList
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.vocabulary.OWL
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.vocabulary.XSD
import java.util.ArrayList
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelManagerProviderTest  extends AbstractSADLModelProcessorTest {
	
/* Tests that should generate validation errors */	
	@Test def void testDuplicateUris() {
		'''
			uri "http://sadl.org.Tests/ModelName" alias foo.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
		]
		'''
			uri "http://sadl.org.Tests/ModelName" alias foo2.
		'''.assertValidatesTo[jenaModel2, rules2, cmds2, issues2, processor2 |
			assertNotNull(jenaModel2)
			assertTrue(issues2.size == 1)
			assertTrue(issues2.toString(), issues2.get(0).toString().contains("ERROR:This URI is already used in"))
		]
		
	}
	
	@Test def void testDuplicateAliases() {
		'''
			uri "http://sadl.org.Tests/ModelName1" alias foo.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
		]
		 '''
			uri "http://sadl.org.Tests/ModelName2" alias foo.
		'''.assertValidatesTo[jenaModel2, rules2, cmds2, issues2, processor2 |
			assertNotNull(jenaModel2)
			assertTrue(issues2.size == 1)
			assertTrue(issues2.toString(), issues2.get(0).toString().contains("ERROR:The alias 'foo' is already used in "))
		]
		
	}
	
/* Test that should not generate any validation errors */	
	@Test def void modelNameCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			assertTrue(jenaModel.listOntologies().hasNext())
			assertTrue(jenaModel.listOntologies().next().URI.toString().equals("http://sadl.org/model1"))
		]
	}
	
	@Test def void modelNameCase2() {
		'''
			uri "http://sadl.org/Tests/ModelName" alias mn version "1" (alias "This is an rdfs:label") (note "Note about the Model").		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			assertTrue(jenaModel.listOntologies().hasNext())
			var Ontology ont = jenaModel.listOntologies().next()
			var itr = ont.listComments("en").toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				var cmmnt = itr.next();
				if (cmmnt.toString().equals("Note about the Model@en")) {
					found = true;
				}
			}
			assertTrue(found)
			assertTrue(ont.getLabel("en").toString().equals("This is an rdfs:label"))
			assertTrue(ont.listVersionInfo().next().toString().equals("1"))
		]
	}
	
	@Test def void testImportSelfError() {
		'''
			uri "http://sadl.org/Tests/Import" alias imp.
			import "http://sadl.org/Tests/Import".
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertTrue(issues.size == 2)
			assertTrue(issues.get(0).toString().contains("cannot import itself") ||
				issues.get(1).toString().contains("cannot import itself")
			)
		]
	}
	
	@Test def void mySimpleClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Foo is a class.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			// some weird garbage values at the end of itr causing null pointer
			while (itr.hasNext() && found == false) {
				val nxt = itr.next;
				if (nxt.localName.equals("Foo")) {
					found = true;
				}
			}	
			assertTrue(found);
		]
	}
	
	@Test def void mustBeOnOfClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Season is a class, must be one of {Spring, Summer, Fall, Winter}.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listIndividuals().toIterable().iterator
			var found = 0
			while (itr.hasNext()) {
				val nxt = itr.next
				if (nxt.localName.equals("Spring")) found++
				if (nxt.localName.equals("Summer")) found++
				if (nxt.localName.equals("Fall")) found++
				if (nxt.localName.equals("Winter")) found++
			}
			assertTrue(found == 4);
		]
	}
	
	@Test def void mySimpleClassAsQnDeclarationCase() {
		'''
			uri "http://sadl.org/allqnames.sadl" alias aqn.
			 
			aqn:Shape is a class.
			aqn:area describes aqn:Shape with values of type float.
			
			aqn:MyShape is a aqn:Shape with aqn:area 23 .
			
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.join(',')[message], issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			// some weird garbage values at the end of itr causing null pointer
			while (itr.hasNext() && found == false) {
				val nxt = itr.next;
				if (nxt.localName.equals("Shape")) {
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
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
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
	
	@Test def void myClassWith50PropertiesCase() {
		'''
			uri "http://sadl.org/model50p" alias p50.
			MyClass is a class 
				described by p0 with values of type duration,
				described by p1 with values of type int,
				described by p2 with values of type float,
				described by p3 with values of type MyClass,
				described by p4 with values of type string,
				described by p5 with values of type date,
				described by p6 with values of type dateTime,
				described by p7 with values of type double,
				described by p8 with values of type long,
				described by p9 with values of type boolean,
				described by p0 with values of type duration,
				described by p1 with values of type int,
				described by p2 with values of type float,
				described by p3 with values of type MyClass,
				described by p4 with values of type string,
				described by p5 with values of type date,
				described by p6 with values of type dateTime,
				described by p7 with values of type double,
				described by p8 with values of type long,
				described by p9 with values of type boolean,
				described by p10 with values of type duration,
				described by p11 with values of type int,
				described by p12 with values of type float,
				described by p13 with values of type MyClass,
				described by p14 with values of type string,
				described by p15 with values of type date,
				described by p16 with values of type dateTime,
				described by p17 with values of type double,
				described by p18 with values of type long,
				described by p19 with values of type boolean,
				described by p20 with values of type duration,
				described by p21 with values of type int,
				described by p22 with values of type float,
				described by p23 with values of type MyClass,
				described by p24 with values of type string,
				described by p25 with values of type date,
				described by p26 with values of type dateTime,
				described by p27 with values of type double,
				described by p28 with values of type long,
				described by p29 with values of type boolean,
				described by p30 with values of type duration,
				described by p31 with values of type int,
				described by p32 with values of type float,
				described by p33 with values of type MyClass,
				described by p34 with values of type string,
				described by p35 with values of type date,
				described by p36 with values of type dateTime,
				described by p37 with values of type double,
				described by p38 with values of type long,
				described by p39 with values of type boolean,
				described by p40 with values of type duration,
				described by p41 with values of type int,
				described by p42 with values of type float,
				described by p43 with values of type MyClass,
				described by p44 with values of type string,
				described by p45 with values of type date,
				described by p46 with values of type dateTime,
				described by p47 with values of type double,
				described by p48 with values of type long,
				described by p49 with values of type boolean.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var prop = jenaModel.getOntProperty("http://sadl.org/model50p#p0")
			var itr = prop.listDomain().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt.localName.equals("MyClass")) {
					found = true;
				}
			}	
			if (!found) {
				jenaModel.write(System.out, "N3")
			}
			assertTrue(found);
			var q1 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select (count(distinct ?p) as ?cnt) where {?p rdfs:domain <http://sadl.org/model50p#MyClass>}"
    		assertTrue(queryResultContains(jenaModel, q1, "50^^http://www.w3.org/2001/XMLSchema#integer"))
		]
	}
	
	@Test def void mySubClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Food is a class.
			Pizza is a type of Food.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
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
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.toString, issues.empty)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt !== null && nxt.isURIResource) {
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
	
	@Test def void myUnionSuperClassWithRestrictionClassDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Person is a class described by child with values of type Person.
			Parent is a type of {Person and (child has at least 1 value)}.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt !== null && nxt.isURIResource) {
					if (nxt.localName.equals("Parent")) {
						found = true;
						var sciter = nxt.listSuperClasses(true);
						while (sciter.hasNext()) {
							var sc = sciter.next();
							var uc = sc.asIntersectionClass();
							var operands = uc.listOperands();
							var cnt = 0
							while (operands.hasNext) {
								var opcls = operands.next();
								assertTrue((opcls.URIResource && opcls.localName.equals("Person")) || opcls.isRestriction());
								cnt++	
							}
							assertTrue(cnt == 2)
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
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listClasses().toIterable().iterator
			var found = false
			while (itr.hasNext()) {
				val nxt = itr.next;
				if (nxt !== null && nxt.isURIResource) {
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
				jenaModel.write(System.out, "N3")
			}
			assertTrue(found);
		]
	}
	
	@Test def void myUserDefinedDatatypeDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			
			shortident is a type of string length 1-4 .
			over12 is a type of int [12,].				// an int >= 12
			clothingsize is a type of {int or string}.	// either an int or a string
			enumeratedheight is a type of string {"short", "medium", "tall"}.	// enumeration of 3 possible string values
			SSN is a type of string "[0-9]{3}-[0-9]{2}-[0-9]{4}".
			year is a type of int length 4 .
			
			SomeClass is a class, described by ident with values of type shortident.
			Adolescent is a class described by age with a single value of type over12.
			Clothing is a class described by size with values of type clothingsize.
			Person is a class described by ssn with values of type SSN.
			Artifact is a class described by cira with values of type year.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var prop = jenaModel.getDatatypeProperty("http://sadl.org/model1#ident")
			if (prop !== null) {
				var rng = prop.range;
				System.out.println("Range: " + rng.toString())
			}
			else {
				var prop2 = jenaModel.getObjectProperty("http://sadl.org/model1#ident")
				if (prop2 !== null) {
					var rng = prop2.range;
					System.out.println("Range: " + rng.toString())
				}	
			}
			jenaModel.write(System.out, "N3")
		]
	}
	
	@Test def void myUserDefinedDatatypeUseCase1() {
		'''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen version "$Revision: 1.1 $ Last modified on   $Date: 2015/02/02 22:11:13 $". 
			Airport_Ident is a type of string length 1-4 .
			Airport is a class, described by ident with values of type Airport_Ident.
			ALB is an Airport with ident "toolong".
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			for (issue:issues) {
				println(issue)
			}
			assertTrue(issues.size == 0)
			var found = false
			// look for something specific to the model; if found set found true
// TODO use datatype facets to check validity?			
			var stmtitr = jenaModel.listStatements(null, jenaModel.getProperty("http://www.w3.org/2001/XMLSchema#maxLength"), null as RDFNode).toIterable().iterator
			if (stmtitr !== null && stmtitr.hasNext) {
				found = true;
			}
//			if (!found) {
				jenaModel.write(System.out, ConfigurationManager.RDF_XML_ABBREV_FORMAT)
//			}
			assertTrue(found);
		]
	}

	@Test def void myUserDefinedDatatypeUseCase2() {
		'''
			uri "http://sadl.org/TestRequrements/StringLength" alias strlen version "$Revision: 1.1 $ Last modified on   $Date: 2015/02/02 22:11:13 $". 
			AnyThingGoes is a type of {string or decimal or int or date or time}.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var found = false
			// look for something specific to the model; if found set found true
// TODO use datatype facets to check validity?			
			var stmtitr = jenaModel.listStatements(null, OWL.unionOf, null as RDFNode).toIterable().iterator
			if (stmtitr !== null && stmtitr.hasNext) {
				var obj = stmtitr.next.object as RDFNode;
				if (obj.canAs(RDFList)) {
					var lst = obj.^as(RDFList)
					var jlst = lst.asJavaList
					if (jlst.contains(XSD.date))
					found = true;
				}
			}
			if (!found) {
				jenaModel.write(System.out)				
			}
			assertTrue(found);
		]
	}

	@Test def void mySimplePropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			prop is a property.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var p = jenaModel.getProperty("http://sadl.org/model1#prop");
			assertTrue(p !== null);
		]
	}
	
	@Test 
	def void testPropertyAllRangeSpecTypes() {
		'''
			uri "http://com.ge.research.sadl/proptypeonly". 
			Person is a class.
			prop is a property.
			dtpropwithrng is a property with values of type float.
			objpropwithrng is a property with values of type Person.		
			dtprop is a property with values of type data.
			objprop is a property with values of type class.
			dtpropwithlistrng is a property with values of type float List.
			objpropwithlistrng is a property with values of type Person List.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			jenaModel.write(System.out)
			assertTrue(issues.size == 0)
			var ns = "http://com.ge.research.sadl/proptypeonly#"
			var p = jenaModel.getProperty(ns+"prop")
			assertTrue(p !== null);
			var op = jenaModel.getObjectProperty(ns+"objprop")
			assertNotNull(op)
			var opwr = jenaModel.getObjectProperty(ns+"objpropwithrng")
			assertNotNull(opwr)
			var opwlr = jenaModel.getObjectProperty(ns+"objpropwithlistrng")
			assertNotNull(opwlr)
			var dtp = jenaModel.getDatatypeProperty(ns+"dtprop")
			assertNotNull(dtp)
			var dtpwr = jenaModel.getDatatypeProperty(ns+"dtpropwithrng")
			assertNotNull(dtpwr)
			var dtpwlr = jenaModel.getObjectProperty(ns+"dtpropwithlistrng")	// property type of a list range is always owl:ObjectProperty
			assertNotNull(dtpwlr)
		]
	}

	@Test def void myInversePropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			prop1 is a property with values of type class.
			prop2 is a property with values of type class.
			prop2 is the inverse of prop1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
		]
	}
	
	@Test def void myPropertyWithListDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Thingy is a class.
			multiValuedListProperty1 describes Thingy with values of type Thingy List.
			multiValuedListProperty2 describes Thingy with values of type float List.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			jenaModel.write(System.out,"N-TRIPLE")
			assertTrue(issues.size == 0)
			var objprop = jenaModel.getObjectProperty("http://sadl.org/model1#multiValuedListProperty1")
			val rngitr = objprop.listRange
			while (rngitr.hasNext) {
				val rng = rngitr.next
				assertTrue(rng.anon)
				assertTrue(rng.isClass)
				assertTrue((rng as OntClass).hasSuperClass(jenaModel.getOntClass(SadlConstants.SADL_LIST_MODEL_LIST_URI)))
	 			val eitr = (rng as OntClass).listSuperClasses
	 			while (eitr.hasNext) {
	 				val sprcls =eitr.next
	 				if (sprcls instanceof OntClass) {
	 					if ((sprcls as OntClass).restriction) {
		 					val onprop = (sprcls as OntClass).asRestriction.onProperty
		 					if (onprop.URI.equals(SadlConstants.SADL_LIST_MODEL_FIRST_URI)) {
		 						if ((sprcls as OntClass).asRestriction.allValuesFromRestriction) {
		 							val avfcls = (sprcls as OntClass).asRestriction.asAllValuesFromRestriction.allValuesFrom
		 							assertTrue(avfcls.URI.equals("http://sadl.org/model1#Thingy"))
		 						}
		 					}
		 				}
	 				}
	 			}
			}
		]
	}
	
	@Test def void mySubPropertyDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			prop1 is a property with values of type class.
			prop2 is a type of prop1.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
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
	
	@Test def void myDisjointClassDeclarationCase1() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Food is a class.
			Drink is a class.
			Poison is a class.
			Food and Drink and Poison are disjoint.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var food = jenaModel.getOntClass("http://sadl.org/model1#Food")
			var drink = jenaModel.getOntClass("http://sadl.org/model1#Drink")
			var poison = jenaModel.getOntClass("http://sadl.org/model1#Poison")
			assertTrue("Food is disjoint from Drink", food.isDisjointWith(drink))
			assertTrue("Food is disjoint from Poison", food.isDisjointWith(poison))
			assertTrue("Drink is disjoint from Poison", drink.isDisjointWith(poison))
		]
	}
	
	@Test def void myDisjointClassDeclarationCase2() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Food is a class.
			Drink is a class.
			Poison is a class.
			{Food, Drink, Poison} are disjoint.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var food = jenaModel.getOntClass("http://sadl.org/model1#Food")
			var drink = jenaModel.getOntClass("http://sadl.org/model1#Drink")
			var poison = jenaModel.getOntClass("http://sadl.org/model1#Poison")
			assertTrue("Food is disjoint from Drink", food.isDisjointWith(drink))
			assertTrue("Food is disjoint from Poison", food.isDisjointWith(poison))
			assertTrue("Drink is disjoint from Poison", drink.isDisjointWith(poison))
		]
	}
	
	@Test def void mySameAsComplimentOfCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Rich is a class.
			Poor is the same as not Rich.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var rich = jenaModel.getOntClass("http://sadl.org/model1#Rich")
			var poor = jenaModel.getOntClass("http://sadl.org/model1#Poor")
			var itr = jenaModel.listStatements(rich, jenaModel.getOntProperty("http://www.w3.org/2002/07/owl#complimentOf"), poor)
			var found = false
			while (itr.hasNext) {
				var nxt = itr.next()
				System.out.println("Nxt: " + nxt.toString())
				found = true
			}
			if (!found) {
				jenaModel.write(System.out, "N3")				
			}
		]
	}
	
	@Test def void mySameAsCase1() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Person is a class.
			Bill is a Person.
			William is a Person.
			Bill is the same as William.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var bill = jenaModel.getIndividual("http://sadl.org/model1#Bill")
			var william = jenaModel.getIndividual("http://sadl.org/model1#William")
			var itr = jenaModel.listStatements(bill, jenaModel.getOntProperty("http://www.w3.org/2002/07/owl#sameAs"), william)
			var found = false
			while (itr.hasNext) {
				var nxt = itr.next()
				System.out.println("Nxt: " + nxt.toString())
				found = true
			}
			if (!found) {
				jenaModel.write(System.out, "N3")				
			}
			assertTrue(found)
		]
	}
	
	@Test def void myNotSameAsCase1() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Person is a class.
			Bill is a Person.
			Hillary is a Person.
			Bill is not the same as Hillary.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var itr = jenaModel.listAllDifferent()
			var error = false
			while (itr.hasNext) {
				var dfrm = itr.next()
				var itr2 = dfrm.listDistinctMembers()
				while (itr2.hasNext) {
					var nxt = itr2.next
					if (!nxt.toString().contains("Bill") && !nxt.toString().contains("Hillary")) {
						System.out.println("Nxt: " + nxt.toString())	
						error = true
						}
				}
			}
			if (!error) {
				jenaModel.write(System.out, "N3")				
			}
			assertFalse(error)
		]
	}
	
	@Test def void myNotSameAsCase2() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Person is a class.
			Bill is a Person.
			Hillary is a Person.
			Chelsea is a Person.
			{Bill, Hillary, Chelsea} are not the same.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.filter[severity.name != "WARNING" ].size == 0)
			var itr = jenaModel.listAllDifferent()
			var error = false
			while (itr.hasNext) {
				var dfrm = itr.next()
				var itr2 = dfrm.listDistinctMembers()
				while (itr2.hasNext) {
					var nxt = itr2.next
					if (!nxt.toString().contains("Bill") && !nxt.toString().contains("Hillary") && !nxt.toString().contains("Chelsea")) {
						System.out.println("Nxt: " + nxt.toString())	
						error = true
						}
				}
			}
			if (!error) {
				jenaModel.write(System.out, "N3")				
			}
			assertFalse(error)
		]
	}
	
	@Test def void myNecesaryAndSufficientCase1() {
		'''
		uri "http://sadl.org/TestSadlIde/AnonRestrictions" alias anonrest 
			version "$Revision: 1.3 $ Last modified on   $Date: 2015/06/30 21:27:33 $". 
		Artifact is a class.
		Person is a class described by owns with values of type Artifact.
		Manufacturer is a class.
		Apple is a Manufacturer.
		Dell is a Manufacturer.
		Computer is a type of Artifact described by manufacturer with values of type Manufacturer.
		Student is a type of Person.
		Professor is a class described by teaches with values of type Student.
		A Professor is an AppleProfessor only if teaches has at least one value of type
			{Student and (owns has at least one value of type {Computer and (manufacturer always has value Apple)})}.
		A Student is an AppleLovingStudent only if owns only has values of type {Computer and (manufacturer always has value Apple)}.
		//AppleLovingStudent is a type of Student, 
		//	described by owns with values of type {Computer and (manufacturer always has value Apple)}.
		A Computer is an AppleComputer only if manufacturer always has value Apple.		// necessary and sufficient conditions
		manufacturer of AppleComputer always has value Apple.							// hasValue restriction only		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var error = true
			if (error) {
				jenaModel.write(System.out, "N3")				
			}
			assertFalse(!error)
		]
	}
	
	@Test def void myNecesaryAndSufficientCase2() {
		'''
		uri "http://sadl.org/TestSadlIde/AnonRestrictions" alias anonrest 
			version "$Revision: 1.3 $ Last modified on   $Date: 2015/06/30 21:27:33 $". 
		Person is a class described by owns with values of type Artifact.
		Artifact is a class.
		Manufacturer is a class.
		Apple is a Manufacturer.
		Dell is a Manufacturer.
		Computer is a type of Artifact described by manufacturer with values of type Manufacturer.
		Professor is a class described by teaches with values of type Student.
		Student is a type of Person.
		A Professor is an AppleProfessor only if teaches has at least one value of type
			{Student and (owns has at least one value of type {Computer and (manufacturer always has value Apple)})}.
		A Student is an AppleLovingStudent only if owns only has values of type {Computer and (manufacturer always has value Apple)}.
		//AppleLovingStudent is a type of Student, 
		//	described by owns with values of type {Computer and (manufacturer always has value Apple)}.
		A Computer is an AppleComputer only if manufacturer always has value Apple.		// necessary and sufficient conditions
		manufacturer of AppleComputer always has value Apple.							// hasValue restriction only		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var q1 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p ?v where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:hasValue ?v}"
    		assertTrue(queryResultContains(jenaModel, q1, "http://sadl.org/TestSadlIde/AnonRestrictions#manufacturer http://sadl.org/TestSadlIde/AnonRestrictions#Apple"))
			var q2 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:someValuesFrom ?v}"
    		assertTrue(queryResultContains(jenaModel, q2, "http://sadl.org/TestSadlIde/AnonRestrictions#teaches"))
    		assertTrue(queryResultContains(jenaModel, q2, "http://sadl.org/TestSadlIde/AnonRestrictions#owns"))
			var q3 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p ?v where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:allValuesFrom ?v}"
    		assertFalse(queryResultContains(jenaModel, q3, "http://sadl.org/TestSadlIde/AnonRestrictions#manufacturer http://sadl.org/TestSadlIde/AnonRestrictions#Apple"))

			var showModel = true
			if (showModel) {
				jenaModel.write(System.out, "N3")				
			}
		]
	}
		
	@Test def void myNecesaryAndSufficientCase3() {
		'''
		uri "http://sadl.org/TestSadlIde/AnonRestrictions" alias anonrest 
			version "$Revision: 1.3 $ Last modified on   $Date: 2015/06/30 21:27:33 $". 
		Person is a class described by owns with values of type Artifact.
		Artifact is a class.
		Manufacturer is a class.
		Apple is a Manufacturer.
		Dell is a Manufacturer.
		Computer is a type of Artifact described by manufacturer with values of type Manufacturer.
		Professor is a class described by teaches with values of type Student.
		Student is a type of Person.
		A Professor is an AppleProfessor only if teaches has at least one value of type
			{Student and (owns has at least one value of type {Computer and (manufacturer always has value Apple)})}.
		A Student is an AppleLovingStudent only if owns only has values of type {Computer and (manufacturer always has value Apple)}.
		//AppleLovingStudent is a type of Student, 
		//	described by owns with values of type {Computer and (manufacturer always has value Apple)}.		
		A Computer is an AppleComputer only if manufacturer always has value Apple.		// necessary and sufficient conditions
		manufacturer of AppleComputer always has value Apple.							// hasValue restriction only		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var q1 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p ?v where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:hasValue ?v}"
    		assertTrue(queryResultContains(jenaModel, q1, "http://sadl.org/TestSadlIde/AnonRestrictions#manufacturer http://sadl.org/TestSadlIde/AnonRestrictions#Apple"))
			var q2 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:someValuesFrom ?v}"
    		assertTrue(queryResultContains(jenaModel, q2, "http://sadl.org/TestSadlIde/AnonRestrictions#teaches"))
    		assertTrue(queryResultContains(jenaModel, q2, "http://sadl.org/TestSadlIde/AnonRestrictions#owns"))
			var q3 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p ?v where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:allValuesFrom ?v}"
    		assertFalse(queryResultContains(jenaModel, q3, "http://sadl.org/TestSadlIde/AnonRestrictions#manufacturer http://sadl.org/TestSadlIde/AnonRestrictions#Apple"))

			var showModel = true
			if (showModel) {
				jenaModel.write(System.out, "N3")				
			}
		]
	}
	
		@Test def void myNecesaryAndSufficientCase4() {
		'''
		uri "http://sadl.org/TestSadlIde/AnonRestrictions" alias anonrest 
			version "$Revision: 1.3 $ Last modified on   $Date: 2015/06/30 21:27:33 $". 
		Artifact is a class.
		Manufacturer is a class.
		Apple is a Manufacturer.
		Computer is a type of Artifact described by manufacturer with values of type Manufacturer.
		AppleComputer is a type of Computer.
		manufacturer of AppleComputer always has value Apple.							// hasValue restriction only		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var q1 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p ?v where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:hasValue ?v}"
    		assertTrue(queryResultContains(jenaModel, q1, "http://sadl.org/TestSadlIde/AnonRestrictions#manufacturer http://sadl.org/TestSadlIde/AnonRestrictions#Apple"))
			var q2 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:someValuesFrom ?v}"
    		assertFalse(queryResultContains(jenaModel, q2, "http://sadl.org/TestSadlIde/AnonRestrictions#teaches"))
    		assertFalse(queryResultContains(jenaModel, q2, "http://sadl.org/TestSadlIde/AnonRestrictions#owns"))
			var q3 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix ar: <http://sadl.org/TestSadlIde/AnonRestrictions#> " +
				"select distinct ?p ?v where {?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:allValuesFrom ?v}"
    		assertFalse(queryResultContains(jenaModel, q3, "http://sadl.org/TestSadlIde/AnonRestrictions#manufacturer http://sadl.org/TestSadlIde/AnonRestrictions#Apple"))

			var showModel = true
			if (showModel) {
				jenaModel.write(System.out, "N3")				
			}
		]
	}
	
	@Test def void myMaxCardinalityCase1() {
		'''
			uri "http://sadl.org/TestSadlIde/model1" alias m1.
			MyClass1 is a class.
			MyClass2 is a class.
			myProp describes MyClass1 with values of type MyClass2 List.
			myProp of MyClass1 has at most 10 values. 		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var q1 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> " +
				"prefix m1: <http://sadl.org/TestSadlIde/model1#> " +
				"select distinct ?c ?p ?v where {?c rdfs:subClassOf ?r . ?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:maxCardinality ?v}"
    		assertTrue(queryResultContains(jenaModel, q1, "http://sadl.org/TestSadlIde/model1#MyClass1 http://sadl.org/TestSadlIde/model1#myProp 10^^http://www.w3.org/2001/XMLSchema#int"))
			var showModel = true
			if (showModel) {
				jenaModel.write(System.out, "N3")				
			}
		]
	}

	@Test def void myMaxCardinalityCase2() {
		'''
			uri "http://sadl.org/TestSadlIde/model1" alias m1.
			MyClass1 is a class.
			MyClass2 is a class described by yourProp.
			myProp describes MyClass1 with values of type MyClass2 List.
			myProp of MyClass1 has at most 10 values. 		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var q1 = "prefix owl: <http://www.w3.org/2002/07/owl#> " +
				"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
				"prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> " +
				"prefix m1: <http://sadl.org/TestSadlIde/model1#> " +
				"select distinct ?c ?p ?v where {?c rdfs:subClassOf ?r . ?r rdf:type owl:Restriction . ?r owl:onProperty ?p . ?r owl:maxCardinality ?v}"
    		assertTrue(queryResultContains(jenaModel, q1, "http://sadl.org/TestSadlIde/model1#MyClass1 http://sadl.org/TestSadlIde/model1#myProp 10^^http://www.w3.org/2001/XMLSchema#int"))
			var showModel = true
			if (showModel) {
				jenaModel.write(System.out, "N3")				
			}
		]
	}

	@Test def void mySimpleInstanceDeclarationCase() {
		'''
			uri "http://sadl.org/model1" alias m1.
			Foo is a class.
			MyFoo is a Foo.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
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
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
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
			if (!found) {
				jenaModel.write(System.out, "N3")				
			}
			assertTrue(found);
		]
	}

	@Test def void myBoxesInstanceCase() {
		'''
			uri "http://sadl.org/TestRequrements/Boxes" alias boxes version "$Revision: 1.1 $ Last modified on   $Date: 2015/02/13 22:30:26 $". 
			
			Box is a class, 
				described by upper-right with a single value of type Point,
				described by lower-left with a single value of type Point.
				
			Point is a class, 
				described by x with values of type int,
				described by y with values of type int,
				described by inside (note "meaning inside both boxes") with values of type boolean.
			
			Box1 is a Box, 
				with lower-left (a Point with x 2, with y 12),
				with upper-right (a Point with x 8, with y 18).
			
			Box2 is a Box,
				with lower-left (a Point with x 0, with y 10),
				with upper-right (a Point with x 5, with y 12).
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
			var found = false
			// look for something specific to the model; if found set found true
			var boxitr = jenaModel.listStatements(null, RDF.type, jenaModel.getOntClass("http://sadl.org/TestRequrements/Boxes#Box"))
			var cntr = 0;
			while (boxitr.hasNext()) {
				var box = boxitr.nextStatement().subject;
				var ur = box.getPropertyResourceValue(jenaModel.getObjectProperty("http://sadl.org/TestRequrements/Boxes#upper-right"))
				var xitr = jenaModel.listStatements(ur, jenaModel.getDatatypeProperty("http://sadl.org/TestRequrements/Boxes#x"), null as RDFNode)
				var x = xitr.nextStatement().object.asLiteral.int
				if (box.URI.endsWith("#Box1")) {
					assertTrue(x == 8)
				}
				else if (box.URI.endsWith("#Box2")) {
					assertTrue(x == 5)
				}
				else {
					fail
				}
				cntr++
			}
			if (cntr == 2) {
				found = true
			}
			if (!found) {
				jenaModel.write(System.out, "N3")				
			}
			assertTrue(found);
		]
	}
	
	@Test def void RuleTest1() {
			'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy, described by color with values of type string.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy.
			
			Rule AllThingysConnect: if x is a Thingy and y is a Thingy and x != y then x has connectedTo y .
			Rule AllThingysAreBlue: if x is a Thingy then color of x is "blue".
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			
		]
	}

	@Test def void EquationTest1() {
			'''
			uri "http://com.ge.research.sadl/equations". 
			
			Thingy is a class 
				described by dateOfBirth with a single value of type date, 
				described by age with a single value of type int.
			
			Equation dateSubtractYears(dateTime x, dateTime y) returns float: x - y.		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			for (issue:issues) {
				println(issue.message)
			}
			val eqs =processor.equations
			assertEquals(1, eqs.size)
			for(eq:eqs) {
				processor.compareTranslations("float dateSubtractYears(dateTime x,dateTime y): -(x,y)",eq.toString)
			}
		]
	}

	@Test def void EquationTest2() {
			'''
			uri "http://com.ge.research.sadl/equations". 
			
			Equation PI_Controller(decimal K1, decimal K2, decimal Error) returns decimal:
			K1*Error + K2*Error^3/abs(Error).		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			for (issue:issues) {
				println(issue.message)
			}
			val eqs =processor.equations
			assertEquals(1, eqs.size)
			for(eq:eqs) {
				processor.compareTranslations("decimal PI_Controller(decimal K1,decimal K2,decimal Error): +((*(K1,Error)),(/((*(K2,(^(Error,3)))),(builtinfunctions:abs(Error)))))",eq.toString)
//				println(eq.toString)
			}
		]
	}

	@Test
	def void testListModel() {
		'''
			uri "http://sadl.org/sadllistmodel" alias sadllist.
			 
			^List is a class 
				described by ^first,
				described by rest with values of type ^List,
				described by lengthRestriction with values of type int,
				described by minLengthRestriction with values of type int,
				described by maxLengthRestriction with values of type int. 
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			jenaModel.write(System.out, "TURTLE")
			assertTrue(issues.size == 0)
		]
	}
	@Test			
	def void testLists_01() {
		'''
			uri "http://sadl.org/SadlList.sadl" alias SadlList.
			 
			Person is a class.
			PersonList is a type of Person List.
			
			MyChildren is the PersonList [Peter, Eileen, Janet, Sharon, Spencer, Lana].
			SpousesChildren is the Person List [Peter, Eileen, Janet, Sharon, Spencer, Lana].
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			jenaModel.write(System.out, "TURTLE")
			assertTrue(issues.size == 0)
		]
	}

	@Test 
	def void testLists_02() {
		'''
			uri "http://sadl.org/SadlList.sadl" alias SadlList.
			 
			Grades is a type of int List.
			
			MyGrades is the Grades [87, 43, 98, 100].
			YourGrades is the int List [87, 43, 98, 100].
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			jenaModel.write(System.out, "TURTLE")
			assertTrue(issues.size == 0)
		]
	}

	@Test 
	def void testLists_03() {
		'''
			uri "http://sadl.org/SadlList.sadl" alias SadlList.
			
			ITEM is a class.
			MarkerListType is a type of ITEM List length 0-100.
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			// expectations go here
			assertNotNull(jenaModel)
			jenaModel.write(System.out, "TURTLE")
			assertTrue(issues.size == 0)
		]
	}

	@Test
	def void testEquationScope() {
		'''
			 uri "http://sadl.org/eq.sadl" alias eq.
			 External sum(decimal X, decimal X) returns decimal:
			 "http://sadl.org/builtinfunctions#sum".
		'''.assertValidatesTo[jenaModel1, rules1, cmds1, issues1, processor1 |
			assertNotNull(jenaModel1)
			assertTrue(issues1.size == 0)
		]
		'''
			 uri "http://sadl.org/prop.sadl" alias prop.
			 import "http://sadl.org/eq.sadl".
			 Class1 is a class described by X with values of type float.
		'''.assertValidatesTo[jenaModel2, rules2, cmds2, issues2, processor2 |
		assertNotNull(jenaModel2)
		assertTrue(issues2.size == 0)		// shouldn't be an error
		]
	}

	@Test
	def void testEquationScope2() {
		'''
			uri "http://sadl.org/eq.sadl" alias eq.
			External sum(decimal X, decimal Y) returns decimal:
						 "http://sadl.org/builtinfunctions#sum".
			Equation circleArea(decimal r) returns decimal: PI*r^2.  
			Shape is a class described by area with values of type float,
				described by peremiter with values of type float.
			Rectangle is a type of Shape described by height with values of type float,
				described by width with values of type float.
			Rule RectPeremiter if x is a Rectangle then peremiter of x is 2*eq:sum(height of x, width of x). // without eq: prefix would use reasoner builtin and fail
		'''.assertValidatesTo[jenaModel1, rules1, cmds1, issues1, processor1 |
			assertNotNull(jenaModel1)
			assertTrue(issues1.size == 0)
		]
		'''
			  uri "http://sadl.org/prop.sadl" alias prop.
			  import "http://sadl.org/eq.sadl".
			  Circle is a type of Shape described by radius with values of type float.
			  Rule CircleArea if x is a Circle then area of x is circleArea(radius of x).
		'''.assertValidatesTo[jenaModel2, rules2, cmds2, issues2, processor2 |
		assertNotNull(jenaModel2)
		assertTrue(issues2.size == 0)		// shouldn't be an error
		]
	}

//	@Test def void my<younameit>Case() {
//		'''
//			// model goes here
//		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
//			// expectations go here
//			assertNotNull(jenaModel)
//			assertTrue(issues.size == 0)
//			var found = false
//			// look for something specific to the model; if found set found true
//
//			if (!found) {
//				jenaModel.write(System.out, "N3")				
//			}
//			assertTrue(found);
//		]
//	}
	
	protected def boolean queryResultContains(OntModel m, String q, String r) {
		var qe = QueryExecutionFactory.create(q, m)
		var results =  qe.execSelect()
		var vars = results.resultVars
    	var resultsList = new ArrayList<String>()
    	while (results.hasNext()) {
    		var result = results.next()
    		var sb = new StringBuffer();
    		var cntr = 0
    		for (var c = 0; c < vars.size(); c++) {
    			if (cntr++ > 0) {
    				sb.append(" ")
    			}
    			sb.append(result.get(vars.get(c)))
    		}
    		resultsList.add(sb.toString())
    	}
    	if (resultsList.contains(r)) {
    		return true
    	}
    	System.out.println("Query result does not contain '" + r + "':")
    	var itr = resultsList.iterator()
    	if (itr.hasNext()) {
    		while (itr.hasNext()) {
    			System.out.println("   " + itr.next().toString())
    		}
    	}
    	else {
    		System.out.println("    Query returned no results");
    	}
    	return false
	}
	
//	protected def Resource assertValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
//		val model = parser.parse(code)
//		validationTestHelper.assertNoErrors(model)
//		val processor = processorProvider.get
//		val List<Issue> issues= newArrayList
//		processor.onValidate(model.eResource, new ValidationAcceptorImpl([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
//		assertions.apply(processor.theJenaModel, issues)
//		return model.eResource
//	}
//
//	protected def Resource assertValidatesTo(ResourceSet resourceSet, CharSequence code, (OntModel, List<Issue>)=>void assertions) {
//		val model = parser.parse(code, resourceSet);
//		val xtextIssues = validationTestHelper.validate(model);
//		val processor = processorProvider.get
//		val List<Issue> issues= new ArrayList(xtextIssues);
//		processor.onValidate(model.eResource, new ValidationAcceptorImpl([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
//		assertions.apply(processor.theJenaModel, issues)
//		return model.eResource
//	}
}