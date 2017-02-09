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
import com.ge.research.sadl.ide.lsp.inference.InferenceResult
import com.google.inject.ImplementedBy
import java.util.List
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethodProvider
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.xtext.ide.server.ILanguageServerExtension

/**
 * Representation of the {@code SADL} specific language server protocol extension.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SadlLanguageServerExtension)
interface ISadlLanguageServerExtension extends ILanguageServerExtension, JsonRpcMethodProvider {

	/**
	 * Calculates and returns with a list of inferencer results for a text document.
	 * 
	 * @param param the bare minimum information for the inferencer sent by the client-side.
	 * 
	 * @return a list of inferencer results. Could be empty but never {@code null}.
	 */
	@JsonRequest('sadl/inferenceResults')
	def CompletableFuture<List<? extends InferenceResult>> getInferenceResults(InferenceParams param);

}
