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
package com.ge.research.sadl.ui.contentassist;

import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;

import com.ge.research.sadl.processing.ISadlOntologyHelper.Context;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.google.common.base.Optional;

/**
 * Representation of a service for converting an Eclipse-based content assist
 * context into an {@link Context ontology helper context}.
 * 
 * @author akos.kitta
 *
 */
public interface IOntologyContextProvider {

	/**
	 * Transforms the content assist context into a ontology helper context and
	 * returns with it. May return with an absent when the transformation is not
	 * viable.
	 */
	default Optional<Context> getOntologyContext(ContentAssistContext it) {
		return getOntologyContext(it, ValidationAcceptor.NOOP);
	}

	/**
	 * Transforms the content assist context into a ontology helper context with
	 * the given acceptor.
	 */
	Optional<Context> getOntologyContext(ContentAssistContext it, ValidationAcceptor acceptor);

}
