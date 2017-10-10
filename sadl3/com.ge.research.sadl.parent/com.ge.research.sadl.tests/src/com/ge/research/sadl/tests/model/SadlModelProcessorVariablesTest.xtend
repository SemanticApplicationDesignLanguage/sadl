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
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.CardinalityRestriction
import com.hp.hpl.jena.ontology.HasValueRestriction
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.rdf.model.RDFNode
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
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import com.hp.hpl.jena.ontology.AllValuesFromRestriction
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.ontology.OntResource
import com.hp.hpl.jena.vocabulary.OWL
import com.ge.research.sadl.model.gp.SadlCommand
import com.ge.research.sadl.model.gp.Rule

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorVariablesTest extends AbstractProcessorTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> processorProvider
	@Inject IPreferenceValuesProvider preferenceProvider
	
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
			 
			 Rule R2 if x is a Person and x teaches y then x knows y.	// this doesn't but is desired
		 
«««			 Rule R3: if a Person knows a second Person then the second Person knows the first Person.
			 Rule R4: if a Person has knows a second Person then the second Person has knows the first Person.
			 Rule R5: if x knows y then y knows x.
			 
			 Rule R6: if x is a Person and knows of x is y then knows of y is x.
			 
			 Rule R7: if x is a Parent then there exists a Person and x has teaches the Person.
 		'''.assertValidatesTo [ jenaModel, rules, cmds, issues |
 			assertNotNull(jenaModel)
 			jenaModel.write(System.out, "RDF/XML-ABBREV")
 			if (issues.size > 0) {
 				for (issue:issues) {
 					print(issue.toString)
 				}
 			}
 			assertTrue(issues.size == 0)
 			assertTrue(rules.size == 6)
 			assertTrue(processorProvider.get.compareTranslations(rules.get(0).toString(),"Rule R1:  if and(rdf(x, rdf:type, ht:Person), rdf(x, ht:teaches, y)) then rdf(x, ht:acquaintance, y)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(1).toString(),"Rule R2:  if and(rdf(x, rdf:type, ht:Person), rdf(x, ht:teaches, y)) then rdf(x, ht:knows, y)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(2).toString(),"Rule R4:  if and(rdf(v0, rdf:type, ht:Person), and(rdf(v0, ht:knows, v1), and(rdf(v1, rdf:type, ht:Person), !=(v0,v1)))) then rdf(v1, ht:knows, v0)."))
  			assertTrue(processorProvider.get.compareTranslations(rules.get(3).toString(),"Rule R5:  if rdf(x, ht:knows, y) then rdf(y, ht:knows, x)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(4).toString(),"Rule R6:  if and(rdf(x, rdf:type, ht:Person), rdf(x, ht:knows, y)) then rdf(y, ht:knows, x)."))
 			assertTrue(processorProvider.get.compareTranslations(rules.get(5).toString(),"Rule R7:  if rdf(x, rdf:type, ht:Parent) then and(there exists(v2), and(rdf(x, ht:teaches, v2), rdf(v2, rdf:type, ht:Person)))."))
 			print(rules.get(5))
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
