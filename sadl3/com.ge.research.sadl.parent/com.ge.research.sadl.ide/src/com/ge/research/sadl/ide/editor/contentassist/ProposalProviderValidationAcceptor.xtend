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
package com.ge.research.sadl.ide.editor.contentassist

import com.ge.research.sadl.processing.ValidationAcceptor
import com.google.common.base.Predicate
import com.google.common.collect.HashMultimap
import com.google.common.collect.Maps
import com.google.common.collect.Multimap
import com.google.common.collect.Multimaps
import java.util.Map
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.resource.IEObjectDescription

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

/**
 * Validation acceptor for the content assist. Stores a multi-map of objects with the 
 * validation messages per severities. Also acts as a EObject description predicate,
 * so that it can be directly used for cross references in proposal provider.
 * 
 * @author akos.kitta 
 */
class ProposalProviderValidationAcceptor implements ValidationAcceptor, Predicate<IEObjectDescription> {

	val Map<Severity, Multimap<EObject, String>> issues;

	new() {
		issues = Maps.<Severity, Multimap<EObject, String>>newHashMap;
		Severity.values.forEach [
			issues.put(it, HashMultimap.create);
		];
	}

	override add(String message, EObject context, Severity severity) {
		issues.get(severity).put(context, message);
	}

	override apply(IEObjectDescription it) {
		if (SADL_RESOURCE == EClass) {
			val errors = Multimaps.unmodifiableMultimap(issues.get(Severity.ERROR)).asMap;
			return errors.get(EObjectOrProxy).nullOrEmpty;
		}
		return true;
	}

}
