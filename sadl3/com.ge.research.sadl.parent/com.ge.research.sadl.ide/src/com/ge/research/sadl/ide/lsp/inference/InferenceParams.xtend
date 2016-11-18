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
package com.ge.research.sadl.ide.lsp.inference

import java.util.Map
import org.eclipse.lsp4j.generator.LanguageServerAPI
import org.eclipse.lsp4j.jsonrpc.validation.NonNull

/**
 * Parameter, sent by the client which invokes the {@code SADL} inferencer.
 */
@LanguageServerAPI
class InferenceParams {
	
	/**
	 * The unique URI of the resource/text document that was evaluated.
	 */
	@NonNull
	String uri;
	
	/**
	 * An optional map of properties for the inferencer.
	 */
	Map<String, String> properties;
	
}
