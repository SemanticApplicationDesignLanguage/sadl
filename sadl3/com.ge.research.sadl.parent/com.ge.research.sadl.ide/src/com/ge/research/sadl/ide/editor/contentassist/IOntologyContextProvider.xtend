/** 
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
 * Project: SADL
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 */
package com.ge.research.sadl.ide.editor.contentassist

import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import com.ge.research.sadl.processing.IModelProcessor
import com.ge.research.sadl.processing.ISadlOntologyHelper.Context
import com.ge.research.sadl.processing.ValidationAcceptor
import com.google.common.base.Optional
import com.google.inject.ImplementedBy

/** 
 * Representation of a service for converting an Eclipse-based content assist
 * context into an {@link Context ontology helper context}.
 * @author akos.kitta
 */
@ImplementedBy(SadlOntologyContextProvider)
interface IOntologyContextProvider {
	/** 
	 * Transforms the content assist context into a ontology helper context and
	 * returns with it. May return with an absent when the transformation is not
	 * viable.
	 */
	def Optional<Context> getOntologyContext(ContentAssistContext it, IModelProcessor processor) {
		return getOntologyContext(it, processor, ValidationAcceptor.NOOP)
	}

	/** 
	 * Transforms the content assist context into a ontology helper context with the
	 * given acceptor.
	 */
	def Optional<Context> getOntologyContext(ContentAssistContext it, IModelProcessor processor,
		ValidationAcceptor acceptor)

}
