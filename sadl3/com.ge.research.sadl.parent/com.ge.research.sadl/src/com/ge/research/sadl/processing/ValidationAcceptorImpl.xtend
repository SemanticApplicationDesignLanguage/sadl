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
package com.ge.research.sadl.processing

import com.google.common.collect.ImmutableMap
import com.google.common.collect.Maps
import java.util.Map
import java.util.concurrent.atomic.AtomicInteger
import org.eclipse.emf.common.util.Diagnostic
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.validation.DiagnosticConverterImpl
import org.eclipse.xtext.validation.FeatureBasedDiagnostic
import org.eclipse.xtext.validation.Issue

import static org.eclipse.xtext.diagnostics.Severity.*
import static org.eclipse.xtext.validation.CheckType.NORMAL
import static org.eclipse.xtext.validation.ValidationMessageAcceptor.INSIGNIFICANT_INDEX

@FinalFieldsConstructor
class ValidationAcceptorImpl implements ValidationAcceptorExt {

	static val ISSUE_CODE = 'processor.issue';
	static val DIAGNOSTIC_MAPPING = ImmutableMap.of(ERROR, Diagnostic.ERROR, WARNING, Diagnostic.WARNING, INFO,
		Diagnostic.INFO);

	val IAcceptor<Issue> acceptor
	val counter = new IssueCounter
	val converter = new DiagnosticConverterImpl

	@Override
	override add(String message, EObject context, Severity severity) {
		add(message, context, severity, null)
	}

	def void addError(String message, EObject context, EStructuralFeature feature) {
		add(message, context, ERROR, feature);
	}

	def void addWarning(String message, EObject context, EStructuralFeature feature) {
		add(message, context, WARNING, feature);
	}

	def void addInfo(String message, EObject context, EStructuralFeature feature) {
		add(message, context, INFO, feature);
	}

	@Override
	override int getErrorCount() {
		return ERROR.issueCount;
	}

	def void clearErrorCount() {
		ERROR.reset;
	}

	@Override
	override int getWarningCount() {
		return WARNING.issueCount;
	}

	def void clearWarningCount() {
		WARNING.reset;
	}

	@Override
	override int getInfoCount() {
		return INFO.issueCount;
	}

	def void clearInfoCount() {
		INFO.reset;
	}

	def void clearMarkerCount() {
		#[ERROR, WARNING, INFO].forEach [
			reset;
		];
	}

	def int[] getMarkerCount() {
		return #[errorCount, warningCount, infoCount]
	}

	private def newDiagnosti(String message, EObject context, Severity severity, EStructuralFeature feature) {
		return new FeatureBasedDiagnostic(DIAGNOSTIC_MAPPING.get(severity), message, context, feature,
			INSIGNIFICANT_INDEX, NORMAL, ISSUE_CODE);
	}

	private def add(String message, EObject context, Severity severity, EStructuralFeature feature) {
		val diagnostic = newDiagnosti(message, context, severity, feature);
		converter.convertValidatorDiagnostic(diagnostic, acceptor);
		counter.increment(severity);
	}

	private def getIssueCount(Severity severity) {
		return counter.count(severity);
	}

	private def reset(Severity severity) {
		counter.reset(severity);
	}

	private static class IssueCounter {

		Map<Severity, AtomicInteger> counter;

		new() {
			counter = Maps.newEnumMap(Severity);
			Severity.values.forEach [
				counter.put(it, new AtomicInteger);
			];
		}

		private def void increment(Severity severity) {
			counter.get(severity).incrementAndGet;
		}

		private def void reset(Severity severity) {
			counter.get(severity).set(0);
		}

		private def count(Severity severity) {
			return counter.get(severity).intValue;
		}
	}

}
