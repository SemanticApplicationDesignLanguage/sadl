/************************************************************************
 * Copyright © 2021 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemanticsllc.sadl.translator;

import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest;
import org.junit.runner.RunWith
import org.eclipse.xtext.testing.XtextRunner
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.eclipse.xtext.testing.InjectWith
import org.junit.Test
import static org.junit.Assert.*
import com.ge.research.sadl.model.gp.Rule
import com.ge.research.sadl.reasoner.ITranslator
import com.ge.research.sadl.jena.IJenaBasedModelProcessor
import java.lang.reflect.Method
import com.ge.research.sadl.builder.ConfigurationManagerForIDE
import java.util.Map
import java.util.List
import java.util.Iterator
import org.apache.jena.ontology.OntModel

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
public class TestAugmentedJenaTranslator extends AbstractSADLModelProcessorTest {

	override ITranslator getTranslator(IJenaBasedModelProcessor processor) {
		val gcm = processor.getClass.getDeclaredMethod("getConfigMgr") as Method
		gcm.accessible = true
		val cm = gcm.invoke(processor, null) as ConfigurationManagerForIDE;
		val rsnr = (cm as ConfigurationManager).getOtherReasoner("com.naturalsemanticsllc.sadl.reasoner.JenaAugmentedReasonerPlugin")
		val tr = cm.getTranslatorForReasoner(rsnr)
		tr
	}

	@Test
	def void testGetTranslatorInstance() {
		val cm = new ConfigurationManager
		val rsnr = (cm as ConfigurationManager).getOtherReasoner("com.naturalsemanticsllc.sadl.reasoner.JenaAugmentedReasonerPlugin")
		val tr = cm.getTranslatorForReasoner(rsnr)
		assertNotNull(tr)
		print(tr)
		assertTrue(tr.class.canonicalName.equals("com.naturalsemanticsllc.sadl.translator.JenaAugmentedTranslatorPlugin"))
	}
		
	@Test
	def void testStagedRules() {
		'''
			uri "http://sadl.imp/RuleStages".
			// In this test case we are trying to construct a model which
			//  will only give the value Black to op MyThingy1 
			//		because dp of MyThing1 is not > 2 so stage 1 rule does not fire
			//		so stage 2 rules 
			//  and only give the value Green to op of Thingy2
			//		because dp of MyThing2 is > 2 so StageOneRule fires 
			//  We fail to do so (the third test case fails).
			
			Thingy is a top-level class.
			Color is a top-level class, must be one of {Black, White, Green}.
			dp describes Thingy has values of type float.
			op describes Thingy has values of type Color. 
			
			Stage 1 Rule StageOneRule
				given
					x is a Thingy
				if
					dp of x > 2
				then
					op of x is Black.
					
			Stage 2 Rule StageTwoRule
				given
					x is a Thingy
				if
					op of x is not known
				then
					op of x is Green.		
		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(cmds !== null)
			
			val tr = getTranslator(processor)
			for (rule : rules) {
				println(rule.toString)
				if (rule instanceof Rule) {
					val ruleTrans = tr.translateRule(jenaModel, "http://sadl.imp/RuleStages", rule as Rule)
					println(ruleTrans)
					if (rule.ruleName.equals("StageOneRule")) {
						assertTrue(rule.stage == 1)
						val expected = "[StageOneRule: (?x rdf:type http://sadl.imp/RuleStages#Thingy), (?x http://sadl.imp/RuleStages#dp ?v0), greaterThan(?v0, 2) -> (?x http://sadl.imp/RuleStages#op http://sadl.imp/RuleStages#Black)]"
						assertTrue(processor.compareTranslations(expected.trim, ruleTrans.toString.trim))
					}
					else if (rule.ruleName.equals("StageTwoRule")) {
						assertTrue(rule.stage == 2)
						val expected = "[StageTwoRule: (?x rdf:type http://sadl.imp/RuleStages#Thingy), noValue(?x, http://sadl.imp/RuleStages#op) -> (?x http://sadl.imp/RuleStages#op http://sadl.imp/RuleStages#Green)]"
						assertTrue(processor.compareTranslations(expected.trim, ruleTrans.toString.trim))
					}
				}
			}
		]
	}
	
	
	@Test
	def void testDefaultValueAndStagedRules() {
		'''
			uri "http://sadl.imp/TestThreeLevelDefaults".
			// In this test case we are trying to construct a model which
			//  will only give the value Black to MyThingy1 and only give the value Green to op of Thingy2
			//  We do so successfully and all test cases pass.
			
			Thingy is a top-level class.
			Color is a top-level class, must be one of {Black, White, Green}.
			dp describes Thingy has values of type float.
			op describes Thingy has values of type Color. 
			ready describes Thingy has a single value of type boolean.
			
			dp of Thingy has level 0 default 2.3  . 
			ready of Thingy has level 1 default true.   
			op of Thingy has level 2 default White.
			
			Stage 1 Rule StageOneRule
				given
					x is a Thingy
				if
					dp of x > 2
				then
					op of x is Black.
					
			Stage 2 Rule StageTwoRule
				given
					x is a Thingy
				if
					ready of x is true and
					op of x is not Black
				then
					op of x is Green.		

		'''.assertValidatesTo[jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					println(issue.message)
				}
			}
			assertTrue(cmds !== null)
			
			val tr = getTranslator(processor)
			for (rule : rules) {
				println(rule.toString)
				if (rule instanceof Rule) {
					val ruleTrans = tr.translateRule(jenaModel, "http://sadl.imp/RuleStages", rule as Rule)
					println(ruleTrans)
					if (rule.ruleName.equals("StageOneRule")) {
						assertTrue(rule.stage == 1)
						val expected = "[StageOneRule: (?x rdf:type http://sadl.imp/TestThreeLevelDefaults#Thingy), (?x http://sadl.imp/TestThreeLevelDefaults#dp ?v0), greaterThan(?v0, 2) -> (?x http://sadl.imp/TestThreeLevelDefaults#op http://sadl.imp/TestThreeLevelDefaults#Black)]"
						assertTrue(processor.compareTranslations(expected.trim, ruleTrans.toString.trim))
					}
					else if (rule.ruleName.equals("StageTwoRule")) {
						assertTrue(rule.stage == 2)
						val expected = "[StageTwoRule: (?x rdf:type http://sadl.imp/TestThreeLevelDefaults#Thingy), (?x http://sadl.imp/TestThreeLevelDefaults#ready 'true'^^http://www.w3.org/2001/XMLSchema#boolean), noValue(?x, http://sadl.imp/TestThreeLevelDefaults#op, http://sadl.imp/TestThreeLevelDefaults#Black) -> (?x http://sadl.imp/TestThreeLevelDefaults#op http://sadl.imp/TestThreeLevelDefaults#Green)]"
						assertTrue(processor.compareTranslations(expected.trim, ruleTrans.toString.trim))
					}
				}
			}
			
			val gcm = tr.getClass.getDeclaredMethod("getDefaultValueRules", OntModel ) as Method
			gcm.accessible = true
			val dvrules = gcm.invoke(tr, jenaModel) as Map<Integer, List<String>>
			val lvlitr = dvrules.keySet.iterator as Iterator<Integer>
			while (lvlitr.hasNext) {
				val lv = lvlitr.next
				val ruleLst = dvrules.get(lv)
				for (rule : ruleLst) {
					println(rule)
					if (lv == 0) {
						val expected = "[http://sadl.imp/TestThreeLevelDefaults#Thingy_dp_default_lvl0: (?i rdf:type http://sadl.imp/TestThreeLevelDefaults#Thingy), noValue(?i,http://sadl.imp/TestThreeLevelDefaults#dp) -> (?i http://sadl.imp/TestThreeLevelDefaults#dp 2.3)]"
						assertTrue(processor.compareTranslations(expected.trim, rule.toString.trim))
					}
					else if (lv == 1) {
						val expected = "[http://sadl.imp/TestThreeLevelDefaults#Thingy_ready_default_lvl1: (?i rdf:type http://sadl.imp/TestThreeLevelDefaults#Thingy), noValue(?i,http://sadl.imp/TestThreeLevelDefaults#ready) -> (?i http://sadl.imp/TestThreeLevelDefaults#ready true)]"
						assertTrue(processor.compareTranslations(expected.trim, rule.toString.trim))
					}
					else if (lv == 2) {
						val expected = "[http://sadl.imp/TestThreeLevelDefaults#Thingy_op_default_lvl2: (?i rdf:type http://sadl.imp/TestThreeLevelDefaults#Thingy), noValue(?i,http://sadl.imp/TestThreeLevelDefaults#op) -> (?i http://sadl.imp/TestThreeLevelDefaults#op http://sadl.imp/TestThreeLevelDefaults#White)]"
						assertTrue(processor.compareTranslations(expected.trim, rule.toString.trim))
					}
				}
			}
		]
	}}
