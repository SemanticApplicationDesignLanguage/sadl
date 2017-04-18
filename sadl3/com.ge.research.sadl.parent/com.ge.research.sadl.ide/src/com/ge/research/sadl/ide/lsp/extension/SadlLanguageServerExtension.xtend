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
package com.ge.research.sadl.ide.lsp.^extension

import com.ge.research.sadl.ide.lsp.inference.InferenceParams
import com.ge.research.sadl.ide.lsp.inference.SadlInferencer
import com.google.inject.Inject
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints
import org.eclipse.xtext.ide.server.ILanguageServerAccess
import org.eclipse.xtext.resource.XtextResource

import static java.util.Collections.emptyMap
import static extension java.util.Collections.unmodifiableMap

/**
 * {@code SADL} language server extension implementation.
 * 
 * @author akos.kitta
 */
class SadlLanguageServerExtension implements ISadlLanguageServerExtension {

	ILanguageServerAccess access;

	@Inject
	SadlInferencer inferencer;

	@Override
	override initialize(ILanguageServerAccess access) {
		this.access = access;
	}

	@Override
	override getInferenceResults(InferenceParams param) {
		return access.doRead(param.uri) [ ctx |
			val resource = ctx.resource;
			if (resource instanceof XtextResource) {
				if (resource.errors.nullOrEmpty) {
					val doc = ctx.document;
					return inferencer.runTests(resource, doc, param.properties ?: emptyMap);
				}
			}
		];
	}
	
	@Override
	override supportedMethods() {
		return ServiceEndpoints.getSupportedMethods(ISadlLanguageServerExtension).unmodifiableMap;
	}

}
