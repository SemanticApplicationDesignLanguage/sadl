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
import com.ge.research.sadl.model.gp.Rule
import com.ge.research.sadl.model.gp.SadlCommand
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptorImpl
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import com.google.inject.Provider
import org.apache.jena.ontology.OntModel
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
import org.junit.Ignore

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorVariablesTest extends AbstractSADLModelProcessorTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> processorProvider
	@Inject IPreferenceValuesProvider preferenceProvider
	
//	@Ignore
	@Test
	def void testVariables_01() {
		val sadlModel = '''
			 uri "http://sadl.org/HasTests.sadl" alias ht.
			 
			 Person is a class.
			 age describes Person with values of type int.		// a "passive" property--a characteristic that a Person has; also a DatatypeProperty
			 child describes Person with values of type Person.	// another "passive" property, this time an ObjectProperty
			 A Person is a Parent only if child has at least 1 value.
			 
			 teaches describes Person with values of type Person. // an "action" property--something that a Person does
			 
			 // Instance declarations
			 George is a Person.
			 John is a Person, has age 23, has teaches George.	// this is currently valid SADL grammar but not good English
			 Julia is a Person, teaches George.	// this does not work but is desired
			 
			 Sue is a Person. 
			 Sue teaches George.	// this works currently, which is good
			 
			 Lana is a Parent.
			 
			 
			 knows describes Person with values of type Person.		// an "active" property
			 The relationship of Person to Person is acquaintance.	// a "passive" property
			 
			 Rule R1 if x is a Person and x has teaches y then x has acquaintance y.	// this works
			 
			 Rule R2 if x is a Person and x teaches y then x knows y.	// this didn't't but was desired

			 Rule R5: if x knows y then y knows x.
			 
			 Rule R6: if x is a Person and knows of x is y then knows of y is x.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
 			if (issues.size > 0) {
 				for (issue:issues) {
 					print(issue.toString)
 				}
 			}
 			assertTrue(issues.size == 0)
 			assertTrue(rules.size == 4)
 			for(rule:rules) {
 				println(rule.toString)
 			}
 			assertTrue(processorProvider.get.compareTranslations(rules.get(0).toString(),"Rule R1:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:teaches, y) then rdf(x, ht:acquaintance, y)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(1).toString(),"Rule R2:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:teaches, y) then rdf(x, ht:knows, y)."))
   			assertTrue(processorProvider.get.compareTranslations(rules.get(2).toString(),"Rule R5:  if rdf(x, ht:knows, y) then rdf(y, ht:knows, x)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(3).toString(),"Rule R6:  if rdf(x, rdf:type, ht:Person) and rdf(x, ht:knows, y) then rdf(y, ht:knows, x)."))
  		]
	}
	
	@Test // "GH-951"
	def void testVariables_02() {
		val sadlModel = '''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Person is a class described by likes with values of type Food.
			 Food is a class.
			 {indian, mild, chinese, italian} are types of Food.
			 
			 Sam is a Person.
			 {dahl,  curry, tandoori, kurma} are instances of indian.
			 {chop_suey, chow_mein, sweet_and_sour} are instances of chinese.
			 {pizza, spagetti} are instances of italian.
			 
			 {dahl, tandoori, kurma} are instances of mild.
			 
			 chips is a Food.
			 
			 Rule R1: if x is a mild and x is a indian then Sam likes x.
			 Rule R2: if x is a chinese then Sam likes x.
			 Rule R3: if x is an italian then Sam likes x.
			 Rule R4: if x is chips then Sam likes x.
			 
			 Ask: select x where x is an indian and x is a mild.
			 Ask: select x where Sam likes x.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
 			if (issues.size > 0) {
 				for (issue:issues) {
 					print(issue.toString)
 				}
 			}
 			assertTrue(issues.size == 0)
 			assertTrue(rules.size == 4)
 			for(rule:rules) {
 				println(rule.toString)
 			}
 			assertTrue(processorProvider.get.compareTranslations(rules.get(0).toString(),"Rule R1:  if rdf(x, rdf:type, test:mild) and rdf(x, rdf:type, test:indian) then rdf(test:Sam, test:likes, x)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(1).toString(),"Rule R2:  if rdf(x, rdf:type, test:chinese) then rdf(test:Sam, test:likes, x)."))
   			assertTrue(processorProvider.get.compareTranslations(rules.get(2).toString(),"Rule R3:  if rdf(x, rdf:type, test:italian) then rdf(test:Sam, test:likes, x)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(3).toString(),"Rule R4:  then rdf(test:Sam, test:likes, test:chips)."))
  		]
	}
	
	protected def Resource assertValidatesTo(CharSequence code, (OntModel, List<Rule>, List<SadlCommand>, List<Issue>)=>void assertions) {
		val model = parser.parse(code)
		validationTestHelper.assertNoErrors(model)
		val processor = processorProvider.get
		val List<Issue> issues= newArrayList
		processor.onValidate(model.eResource, new ValidationAcceptorImpl([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
		assertions.apply(processor.theJenaModel, processor.rules, processor.sadlCommands, issues)
		return model.eResource
	}

}
