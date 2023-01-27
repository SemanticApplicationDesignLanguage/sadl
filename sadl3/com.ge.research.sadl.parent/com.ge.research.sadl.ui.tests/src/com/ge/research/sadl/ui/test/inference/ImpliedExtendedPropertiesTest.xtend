/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.test.inference

import com.ge.research.sadl.model.gp.TestResult
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.reasoner.ConfigurationItem
import com.ge.research.sadl.reasoner.SadlCommandResult
import com.ge.research.sadl.ui.tests.AbstractSadlPlatformTest
import java.util.List
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

import static com.ge.research.sadl.ui.tests.GeneratedOutputFormat.*
import com.ge.research.sadl.reasoner.ResultSet
import org.junit.Ignore

/**
 * Test that demonstrate how to make assertions on the generated translator outputs, plus runs the inferencer too.
 * 
 * @author akos.kitta
 */
class ImpliedExtendedPropertiesTest extends AbstractSadlPlatformTest {

	@Test
	def void testImpliedPropertyInTest_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertyInTest.sadl" alias ImpliedPropertyInTest.
			 
			 Person is a class 
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Sue is a Person with age 23, with weight 125.
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Test: Sue is 23 .
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInTest_02() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertyInTest.sadl" alias ipt.
			 
			 Person is a class 
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Sue is a Person with child (a Person Jane).
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Rule R1: if x is a Person and x has child y then x is 23.0.
			 
			 Test: Sue is 23 .
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(x, rdf:type, ipt:Person) and rdf(x, ipt:child, y) then rdf(x, ipt:age, 23.0)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInTest_03() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/ImpliedPropertyInTest.sadl" alias ImpliedPropertyInTest.
				 
				 Person is a class 
				 	described by child with values of type Person,
				 	described by age with values of type decimal,
				 	described by weight with values of type decimal.
				 Person has impliedProperty age //, has impliedProperty weight
				 .
				 friend describes Person with values of type Person.
				 
				 Sue is a Person with age 23, with weight 125.
				 John is a Person with friend Sue.
				 
				 Test: Sue is 23 .
				 Test: friend of John is 23 .
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInRule_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule2.sadl" alias ipt.
			  
			 Person is a class 
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Adult is a type of Person.
			 
			 Sue is a Person with age 23, with weight 125.
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Rule R1: if x is a Person and x > 18 then x is an Adult.
			 
			 Test: Sue is an Adult .			 
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(x, rdf:type, ipt:Person) and rdf(x, ipt:age, v0) and >(v0,18) then rdf(x, rdf:type, ipt:Adult)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				if ((tr as TestResult).passed) 
					println("    passed\n")
				else {
					println("    failed\n")
				}
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInRule_02() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
			 uri "http://sadl.org/ImpliedPropertiesInRule2.sadl" alias ipt.
			  
			 Person is a class 
			 	described by dob with values of type date,
			 	described by child with values of type Person,
			 	described by age with values of type decimal,
			 	described by weight with values of type decimal.
			 Person has impliedProperty age //, has impliedProperty weight
			 .
			 
			 Adult is a type of Person.
			 
			 Sue is a Person with age 23, with weight 125.
			 
			 Parent is a type of (child has at least 1 value). 
			 Parent is the same as (child has at least 1 value).  
			 
			 Rule R1: if x is a Person and x > 18 then x is an Adult.
			 
			 Test: Sue is an Adult .			 
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(x, rdf:type, ipt:Person) and rdf(x, ipt:age, v0) and >(v0,18) then rdf(x, rdf:type, ipt:Adult)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				if ((tr as TestResult).passed) 
					println("    passed\n")
				else {
					println("    failed\n")
				}
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testImpliedPropertyInRule_03() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/test1.sadl" alias test1.
				 
				 Person is a class described by age with values of type UnittedQuantity.
				 Adult is a type of Person.
				 
				 verified describes UnittedQuantity with values of type boolean.
				 
				 UnittedQuantity has impliedProperty verified.
				 
				 Rule R1: if p is a Person and age of p >= 18 yrs and age of p is true then p is an Adult.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
//			assertTrue(
//				processor.compareTranslations(rules.get(0).toString(),
//					"Rule R1:  if rdf(x, rdf:type, ipt:Person) and rdf(x, ipt:age, v0) and >(v0,18) then rdf(x, rdf:type, ipt:Adult)."))
			}
		]

//		var List<ConfigurationItem> configItems = newArrayList
//		val String[] catHier = newArrayOfSize(1)
//		catHier.set(0, "Jena")
//		val ci = new ConfigurationItem(catHier)
//		ci.addNameValuePair("pModelSpec", "OWL_MEM")
//		configItems.add(ci)
//		assertInferencer(sfname, null, configItems) [
//			for (scr : it) {
//				println(scr.toString)
//				assertTrue(scr instanceof SadlCommandResult)
//				val tr = (scr as SadlCommandResult).results
//				assertTrue(tr instanceof TestResult)
//				if ((tr as TestResult).passed) 
//					println("    passed\n")
//				else {
//					println("    failed\n")
//				}
//				assertTrue((tr as TestResult).passed)
//			}
//		];
	}

	@Test
	def void testExpandedPropertyInRule_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				uri "http://sadl.org/test3.sadl" alias test3.
				 
				ConnectorType is a class described by numberOfProngs with values of type int, 
					described by maxCurrent with values of type float, 
					described by referenceVoltable with values of type float.
				ConnectorType has expandedProperty numberOfProngs.
				
				TypeB1 is a ConnectorType, has numberOfProngs 3.
				TypeB2 is a ConnectorType, has numberOfProngs 3.
					
				Connector is a class described by connectorType with values of type ConnectorType.
				
				{Socket, Plug} are types of Connector.
				
				Rule R1: if p is a Plug and s is a Socket and connectorType of p = connectorType of s then print(p, " is compatible with ", s).
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(p, rdf:type, test3:Plug) and rdf(s, rdf:type, test3:Socket) and rdf(p, test3:connectorType, v0) and rdf(v0, test3:numberOfProngs, v1) and rdf(s, test3:connectorType, v2) and rdf(v2, test3:numberOfProngs, v3) and is(v1,v3) then print(p,\" is compatible with \",s)."))
			}
		]
	}
		
	def void testExpandedPropertyInTest_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				uri "http://sadl.org/test3.sadl" alias test3.
				 
				ConnectorType is a class described by numberOfProngs with values of type int, 
					described by maxCurrent with values of type float, 
					described by referenceVoltable with values of type float.
				ConnectorType has expandedProperty numberOfProngs.
				
				TypeB1 is a ConnectorType, has numberOfProngs 3.
				TypeB2 is a ConnectorType, has numberOfProngs 3.
					
				Connector is a class described by connectorType with values of type ConnectorType.
				
				{Socket, Plug} are types of Connector.
				
				Rule R1: if p is a Plug and s is a Socket and connectorType of p = connectorType of s then print(p, " is compatible with ", s).

				TypeBPlug is a Plug, has connectorType TypeB1.
				
				TypeBSocket is a Socket, has connectorType TypeB2.
				
				Test: connectorType of TypeBPlug = connectorType of TypeBSocket.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(cmds.size == 1)
			assertTrue(
				processor.compareTranslations(cmds.get(0).toString, "[v1]: is [v3]:  where Conj(rdf(test3:TypeBPlug, test3:connectorType, v0)rdf(v0, test3:numberOfProngs, v1)rdf(test3:TypeBSocket, test3:connectorType, v2)rdf(v2, test3:numberOfProngs, v3))")
			)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(p, rdf:type, test3:Plug) and rdf(s, rdf:type, test3:Socket) and rdf(p, test3:connectorType, v0) and rdf(v0, test3:numberOfProngs, v1) and rdf(s, test3:connectorType, v2) and rdf(v2, test3:numberOfProngs, v3) and is(v1,v3) then print(p,\" is compatible with \",s)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Test
	def void testExpandedPropertyInQuery_01() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				uri "http://sadl.org/test3.sadl" alias test3.
				 
				ConnectorType is a class described by numberOfProngs with values of type int, 
					described by maxCurrent with values of type float, 
					described by referenceVoltable with values of type float.
				ConnectorType has expandedProperty numberOfProngs.
				
				TypeB1 is a ConnectorType, has numberOfProngs 3.
				TypeB2 is a ConnectorType, has numberOfProngs 3.
					
				Connector is a class described by connectorType with values of type ConnectorType.
				
				{Socket, Plug} are types of Connector.
				
				Rule R1: if p is a Plug and s is a Socket and connectorType of p = connectorType of s then print(p, " is compatible with ", s).
				
				TypeBPlug is a Plug, has connectorType TypeB1.
				
				TypeBSocket is a Socket, has connectorType TypeB2.
				
				Ask: connectorType of TypeBPlug = connectorType of TypeBSocket.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (cmds !== null) {
				for (cmd : cmds) {
					println(cmd.toString)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(cmds.size == 1)
//			assertTrue(
//				processor.compareTranslations(cmds.get(0).toString, "[v1]: is [v3]:  where Conj(rdf(test3:TypeBPlug, test3:connectorType, v0)rdf(v0, test3:numberOfProngs, v1)rdf(test3:TypeBSocket, test3:connectorType, v2)rdf(v2, test3:numberOfProngs, v3))")
//			)
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(p, rdf:type, test3:Plug) and rdf(s, rdf:type, test3:Socket) and rdf(p, test3:connectorType, v0) and rdf(v0, test3:numberOfProngs, v1) and rdf(s, test3:connectorType, v2) and rdf(v2, test3:numberOfProngs, v3) and is(v1,v3) then print(p,\" is compatible with \",s)."))
			}
		]

		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertNotNull(tr)
				println(tr.toString)
				assertTrue(tr instanceof ResultSet)
				assertEquals("\"v0\",\"v1\",\"v2\",\"v3\"
\"http://sadl.org/test3.sadl#TypeB1\",3,\"http://sadl.org/test3.sadl#TypeB2\",3", (tr as ResultSet).toString.trim)
			}
		];
	}

	@Ignore
	@Test
	def void testImpliedPropertyWithUQInRule_04() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/test2.sadl" alias test2.
				 
				 Person is a class described by age with values of type UnittedQuantity.
				 Adult is a type of Person.
				 
				 Status is a class, can only be one of {Verified, Unverified}.
				 status describes UnittedQuantity with values of type Status.
				 
				 UnittedQuantity has impliedProperty status.
				 
				 Rule R1: if p is a Person and age of p >= 18 yrs and age of p is Verified then p is an Adult.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(p, rdf:type, test2:Person) and rdf(p, test2:age, v0) and rdf(v0, sadlimplicitmodel:value, v1) and rdf(v0, sadlimplicitmodel:unit, \"yrs\") and >=(v1,18) and rdf(v0, test2:status, test2:Verified) then rdf(p, rdf:type, test2:Adult)."))
			}
		]
	}
		
	@Ignore
	@Test
	def void testImpliedPropertyWithUQInRule_05() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/test2.sadl" alias test2.
				 
				 Person is a class described by age with values of type UnittedQuantity.
				 Adult is a type of Person.
				 
				 Status is a class, can only be one of {Verified, Unverified}.
				 status describes UnittedQuantity with values of type Status.
				 
				 UnittedQuantity has impliedProperty status.
				 
				 Rule R1: if p is a Person and age of p >= 18 yrs and age of p is Verified then p is an Adult.
				 
				 Joan is a Person with age (a UnittedQuantity with ^value 20, with unit "yrs", with status Verified).
				 
				 Test: Joan is an Adult.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(p, rdf:type, test2:Person) and rdf(p, test2:age, v0) and rdf(v0, sadlimplicitmodel:value, v1) and rdf(v0, sadlimplicitmodel:unit, \"yrs\") and >=(v1,18) and rdf(v0, test2:status, test2:Verified) then rdf(p, rdf:type, test2:Adult)."))
			}
		]
		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
		
	@Ignore
	@Test
	def void testImpliedPropertyWithUQInRule_06() {
		updatePreferences(new PreferenceKey(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.id, Boolean.TRUE.toString));
		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));
		val sfname = 'JavaExternal.sadl'
		createFile(sfname, '''
				 uri "http://sadl.org/test2.sadl" alias test2.
				 
				 Person is a class described by age with values of type UnittedQuantity.
				 Adult is a type of Person.
				 
				 Status is a class, can only be one of {Verified, Unverified}.
				 status describes UnittedQuantity with values of type Status.
				 
				 UnittedQuantity has impliedProperty status.
				 
				 Rule R1: if p is a Person and age of p >= 18 yrs and age of p is Verified then p is an Adult.
				 
				 Joan is a Person with age (a UnittedQuantity with ^value 20, with unit "yrs", with status Verified).
				 
				 Test: Joan is an Adult.
			 ''').resource.assertValidatesTo [ jenaModel, rules, cmds, issues, processor |
			{ assertNotNull(jenaModel)

			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule R1:  if rdf(p, rdf:type, test2:Person) and rdf(p, test2:age, v0) and rdf(v0, sadlimplicitmodel:value, v1) and rdf(v0, sadlimplicitmodel:unit, \"yrs\") and >=(v1,18) and rdf(v0, test2:status, test2:Verified) then rdf(p, rdf:type, test2:Adult)."))
			}
		]
		var List<ConfigurationItem> configItems = newArrayList
		val String[] catHier = newArrayOfSize(1)
		catHier.set(0, "Jena")
		val ci = new ConfigurationItem(catHier)
		ci.addNameValuePair("pModelSpec", "OWL_MEM")
		configItems.add(ci)
		assertInferencer(sfname, null, configItems) [
			for (scr : it) {
				println(scr.toString)
				assertTrue(scr instanceof SadlCommandResult)
				val tr = (scr as SadlCommandResult).results
				assertTrue(tr instanceof TestResult)
				assertTrue((tr as TestResult).passed)
			}
		];
	}
 
}
