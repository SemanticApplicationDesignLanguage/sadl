/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.utils

import com.google.common.base.Preconditions
import com.google.inject.Singleton
import java.util.Arrays
import java.util.Collection
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.resource.XtextResource

import static org.eclipse.xtext.diagnostics.Severity.ERROR
import static org.eclipse.xtext.util.CancelIndicator.NullImpl
import static org.eclipse.xtext.validation.CheckMode.FAST_ONLY

import static extension com.google.common.base.Predicates.*

/**
 * SADL validation helper.
 * 
 * <p>
 * This class works without injection.
 * 
 * @author akos.kitta
 */
@Singleton
class ValidationHelper {

	/**
	 * Returns with {@code true} if the resource argument has any errors.
	 */
	def boolean hasErrors(Resource resource) {
		return hasErrors(resource, emptyList);
	}

	/**
	 * Returns with {@code true} if one of the following is {@code true} for the resource argument.
	 * <p>
	 * <ul>
	 * <li>The resource has either syntax and/or linking error.</li>
	 * <li>The resource has errors and any of the errors belong to the expected issue codes.</li>
	 * </ul>
	 * 
	 * If the expected {@code expectedIssueCodes} argument is either {@code null} or empty,
	 * then all issue codes will be accepted.
	 */
	def boolean hasErrors(Resource resource, Collection<String> expectedIssueCodes) {
		Preconditions.checkNotNull(resource, 'resource');
		// Already have linking or syntax errors. We are doomed anyway.
		if (!resource.errors.nullOrEmpty) {
			return true;
		}
		if (resource instanceof XtextResource) {
			val issueCodePredicate = if(expectedIssueCodes.nullOrEmpty) alwaysTrue else expectedIssueCodes.toSet.in;
			return !resource.resourceServiceProvider.resourceValidator.validate(resource, FAST_ONLY, NullImpl).filter [
				severity === ERROR
			].filter [
				issueCodePredicate.apply(code)
			].nullOrEmpty;
		}
		return false;
	}

	/**
	 * Sugar for narrowing the errors into a subset of issue codes.
	 */
	def boolean hasErrors(Resource resource, String... expectedIssueCodes) {
		return hasErrors(resource, Arrays.asList(expectedIssueCodes));
	}

}
