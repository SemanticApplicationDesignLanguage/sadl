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
package com.ge.research.sadl.tests

import com.ge.research.sadl.jena.IJenaBasedModelProcessor
import com.ge.research.sadl.model.gp.Rule
import com.ge.research.sadl.model.gp.SadlCommand
import com.ge.research.sadl.processing.OntModelProvider
import com.ge.research.sadl.validation.ModelProcessorAdapter
import com.google.common.collect.Iterables
import com.hp.hpl.jena.ontology.OntModel
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.validation.Issue
import org.junit.Assert

import static org.eclipse.xtext.diagnostics.Severity.*
import static org.eclipse.xtext.util.CancelIndicator.*
import static org.eclipse.xtext.validation.CheckMode.*

/**
 * Provides a bunch of static assertion helpers for SADL tests.
 * 
 * @author akos.kitta
 */
abstract class SadlTestAssertions {

	/**
	 * Validates the Xtext resource then calls the assertion callback.
	 * If the assertion was successful, returns with the argument resource.
	 */
	static def Resource assertValidatesTo(XtextResource resource,
		(OntModel, List<Rule>, List<SadlCommand>, List<Issue>, IJenaBasedModelProcessor)=>void assertions) {

		val validator = resource.resourceServiceProvider.resourceValidator;
		val issues = validator.validate(resource, FAST_ONLY, NullImpl);
		val ontModel = OntModelProvider.find(resource);
		val processor = ModelProcessorAdapter.findInEmfObject(resource).processor;
		var rules = (processor as IJenaBasedModelProcessor).rules;
		if (rules === null) {
			rules = newArrayList();
		}
		var commands = (processor as IJenaBasedModelProcessor).sadlCommands;
		if (commands === null) {
			commands = newArrayList();
		}
		assertions.apply(ontModel, rules, commands, issues, processor as IJenaBasedModelProcessor);
		return resource;
	}

	/**
	 * Asserts no validation issues.
	 */
	static def void assertHasNoIssues(Iterable<? extends Issue> issues) {
		doAssertHasIssues(issues, [true], 0);
	}

	/**
	 * Asserts the all issues with all severities.
	 */
	static def void assertHasIssues(Iterable<? extends Issue> issues, int expectedCount) {
		doAssertHasIssues(issues, [true], expectedCount);
	}

	/**
	 * Asserts the errors.
	 */
	static def void assertHasErrors(Iterable<? extends Issue> issues, int expectedCount) {
		doAssertHasIssues(issues, [it === ERROR], expectedCount);
	}

	/**
	 * Asserts the warning level issues.
	 */
	static def void assertHasWarnings(Iterable<? extends Issue> issues, int expectedCount) {
		doAssertHasIssues(issues, [it === WARNING], expectedCount);
	}

	/**
	 * Asserts all info level issues.
	 */
	static def void assertHasInfos(Iterable<? extends Issue> issues, int expectedCount) {
		doAssertHasIssues(issues, [it === INFO], expectedCount);
	}

	private static def void doAssertHasIssues(Iterable<? extends Issue> issues, (Severity)=>boolean severityPredicate,
		int expectedCount) {

		val actualIssues = issues.filter[severityPredicate.apply(severity)];
		val plural = if(expectedCount === 1) '' else 's'; 
		Assert.assertEquals(
			'''Expected «expectedCount» issue«plural». Got «actualIssues.size» instead. [«Iterables.toString(actualIssues)»]''',
			expectedCount,
			actualIssues.size
		);
	}

	private new() {
		// NO
	}

}
