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

import com.google.inject.ImplementedBy
import java.util.List
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.generator.JsonRpcData
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethodProvider
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import org.eclipse.lsp4j.jsonrpc.validation.NonNull
import org.eclipse.xtext.ide.server.ILanguageServerExtension

/**
 * Representation of the {@code SADL} specific language server protocol extension.
 * 
 * @author akos.kitta
 */
@JsonSegment('sadl')
@ImplementedBy(SadlLanguageServerExtension)
interface ISadlLanguageServerExtension extends ILanguageServerExtension, JsonRpcMethodProvider {

	/**
	 * Tests the model by running the inferencer on the given text document.
	 */
	@JsonNotification('testModel')
	def void testModel(TestModelParams param);

	/**
	 * Runs a SADL/SPARQL query on the given model.
	 */
	@JsonNotification('runQuery')
	def void runQuery(RunQueryParams param);

	/**
	 * Client request to reload all the necessary external SADL model files defined in the given definition files. Resolves to a list of files
	 * that has to be rebuild after downloading the externals.
	 */
	@JsonRequest('reloadExternals')
	def CompletableFuture<ReloadExternalsResult> reloadExternals(ReloadExternalsParams params);

	/**
	 * Resolves to the location of the container SADL project or {@code null} if the requesting location URI is not in a SADL project.
	 */
	@JsonRequest('projectRoot')
	def CompletableFuture<ProjectRootResult> projectRoot(ProjectRootParams params);

}

/**
 * Parameter for testing the SADL model.
 */
@JsonRpcData
class TestModelParams {

	@NonNull
	String uri;

}

/**
 * Parameter for running a SADL/SPARQL query on the given SADL model.
 */
@JsonRpcData
class RunQueryParams {

	@NonNull
	String uri;

	@NonNull
	String query;

}

/**
 * Parameters for reloading the external SADL models.
 */
@JsonRpcData
class ReloadExternalsParams {

	@NonNull
	String uri;

}

/**
 * Result of the external reload request. Contains a list of model resource URIs that have to be rebuild after the request.
 */
@JsonRpcData
class ReloadExternalsResult {

	@NonNull
	List<String> uris;

}

/**
 * Parameters for determining the SADL project root location for the given URI.
 */
@JsonRpcData
class ProjectRootParams {

	@NonNull
	String uri;

}

/**
 * Result that is showing the root of the container SADL project.
 */
@JsonRpcData
class ProjectRootResult {

	/**
	 * The location of the SADL project root or {@code null} if the location is not in a SADL project.
	 */
	String uri;

}
