package com.ge.research.sadl.processing

import org.eclipse.emf.common.util.Diagnostic
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.validation.CheckType
import org.eclipse.xtext.validation.DiagnosticConverterImpl
import org.eclipse.xtext.validation.FeatureBasedDiagnostic
import org.eclipse.xtext.validation.Issue

@Data class ValidationAcceptor {
	
	IAcceptor<Issue> acceptor
	
	DiagnosticConverterImpl converter = new DiagnosticConverterImpl
	
	def void addError(String message, EObject context) {
		addError(message, context, null)
	}
	
	def void addError(String message, EObject context, EStructuralFeature feature) {
		val diag = new FeatureBasedDiagnostic(Diagnostic.WARNING, message, context, feature, -1, CheckType.NORMAL, 'processor.issue')
		converter.convertValidatorDiagnostic(diag, acceptor)
	}
	
	def void addWarning(String message, EObject context) {
		addWarning(message, context, null)
	}
	def void addWarning(String message, EObject context, EStructuralFeature feature) {
		val diag = new FeatureBasedDiagnostic(Diagnostic.WARNING, message, context, feature, -1, CheckType.NORMAL, 'processor.issue')
		converter.convertValidatorDiagnostic(diag, acceptor)
	}
	
}