/************************************************************************
 * Copyright 2007-2018 - General Electric Company, All Rights Reserved
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

import com.ge.research.sadl.builder.MessageManager.MessageType
import com.ge.research.sadl.external.ExternalEmfModelDownloader
import com.ge.research.sadl.external.ExternalEmfResourcePredicate
import com.ge.research.sadl.ide.handlers.SadlRunInferenceHandler
import com.ge.research.sadl.ide.handlers.SadlRunQueryHandler
import com.ge.research.sadl.utils.SadlConsole
import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.inject.Inject
import com.google.inject.Provider
import com.google.inject.Singleton
import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.CompletableFuture
import java.util.function.BiPredicate
import java.util.stream.Collectors
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints
import org.eclipse.xtext.ide.server.ILanguageServerAccess
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.UriExtensions

import static extension java.util.Collections.unmodifiableMap

/**
 * {@code SADL} language server extension implementation.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlLanguageServerExtension implements ISadlLanguageServerExtension, SadlConsole {

	ILanguageServerAccess access;

	@Inject
	extension UriExtensions;

	@Inject
	SadlProjectHelper projectHelper;

	@Inject
	ExternalEmfModelDownloader modelDownloader;

	@Inject
	Provider<SadlRunQueryHandler> queryHandlerProvider;

	@Inject
	Provider<SadlRunInferenceHandler> inferenceHandlerProvider;

	@Inject
	ExternalEmfResourcePredicate externalEmfResourcePredicate;

	override initialize(ILanguageServerAccess access) {
		this.access = access;
	}

	override testModel(TestModelParams param) {
		access.doRead(param.uri) [ ctx |
			val resource = ctx.resource;
			if (resource instanceof XtextResource) {
				val path = Paths.get(new URI(param.uri));
				inferenceHandlerProvider.get.run(path, [resource]);
				return /* void */ null;
			}
		];
	}

	override runQuery(RunQueryParams param) {
		access.doRead(param.uri) [ ctx |
			val resource = ctx.resource;
			if (resource instanceof XtextResource) {
				val path = Paths.get(new URI(param.uri));
				queryHandlerProvider.get.run(path, [resource], param.query);
				return /* void */ null;
			}
		];
	}

	override reloadExternals(ReloadExternalsParams params) {
		modelDownloader.downloadModels(new URI(params.uri));
		val root = projectHelper.getRoot(new URI(params.uri));
		// Rebuild the content of the current project after downloading the externals.
		if (root !== null) {
			val maxDepth = 20;
			val matcher = [ Path path, BasicFileAttributes attribute |
				if (attribute.regularFile) {
					if (path.fileName.toString.endsWith('sadl')) {
						return true;
					}
					val uri = projectHelper.toUri(path);
					if (uri !== null && externalEmfResourcePredicate.apply(uri.toEmfUri)) {
						return true;
					}
				}
				return false;
			] as BiPredicate<Path, BasicFileAttributes>;
			val result = new ReloadExternalsResult();
			result.uris = Files.find(Paths.get(root), maxDepth, matcher).map[toFile.toURI.toString].collect(
				Collectors.toList);
			return CompletableFuture.completedFuture(result);
		}
	}

	override projectRoot(ProjectRootParams params) {
		val root = projectHelper.getRoot(new URI(params.uri));
		val result = new ProjectRootResult;
		if (root !== null) {
			result.uri = root.toString();
		}
		return CompletableFuture.completedFuture(result);
	}

	override print(MessageType type, String message) {
		val messageType = switch (type) {
			case ERROR: org.eclipse.lsp4j.MessageType.Error
			case WARN: org.eclipse.lsp4j.MessageType.Warning
			case INFO: org.eclipse.lsp4j.MessageType.Info
			default: org.eclipse.lsp4j.MessageType.Log
		}
		this.access.languageClient.logMessage(new MessageParams(messageType, message));
	}

	override supportedMethods() {
		return ServiceEndpoints.getSupportedMethods(ISadlLanguageServerExtension).unmodifiableMap;
	}

}
