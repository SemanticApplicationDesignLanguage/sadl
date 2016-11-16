package io.typefox.lsp.endpoint

import com.google.common.util.concurrent.ThreadFactoryBuilder
import io.typefox.lsp.endpoint.nio.file.FileManager
import java.io.IOException
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import org.apache.log4j.Logger
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.jsonrpc.Endpoint
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethodProvider
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.xtend.lib.annotations.Delegate

import static extension java.nio.file.Files.*

class WorkspaceServerImpl implements LanguageServer, LanguageClientAware, JsonRpcMethodProvider, WorkspaceServer {

	val static DELAY = 500

	val static LOGGER = Logger.getLogger(WorkspaceServerImpl)

	val FileManager fileManager

	@Delegate
	val LanguageServer languageServer

	val ScheduledExecutorService executorService
	
	WorkspaceClient client

	new(LanguageServer languageServer) {
		this.languageServer = languageServer

		this.fileManager = new FileManager
		fileManager.eventEmitter = [
			new DidChangeWatchedFilesEvent[ params |
				languageServer.workspaceService.didChangeWatchedFiles(params)
				client?.didChangeWatchedFiles(params)
			]
		]

		val builder = new ThreadFactoryBuilder
		builder.nameFormat = WorkspaceServerImpl.simpleName + ' %d'
		val threadFactory = builder.build
		this.executorService = Executors.newSingleThreadScheduledExecutor(threadFactory)
		executorService.scheduleAtFixedRate([
			try {
				fileManager.processEvents
			} catch (Throwable t) {
				LOGGER.error(t.message, t)
			}
		], DELAY, DELAY, TimeUnit.MILLISECONDS)
	}

	override initialize(InitializeParams params) {
		fileManager.close

		val promise = languageServer.initialize(params)
		val rootPath = params.rootPath
		if (rootPath !== null) {
			promise.thenRunAsync([
				val root = Paths.get(rootPath)
				try {
					fileManager.open(root)
				} catch (IOException e) {
					LOGGER.error("Failed to open file manager for watching: " + root, e)
				}
			], this.executorService)
		}
		return promise
	}

	override resolveFile(ResolveFileParams params) {
		return CompletableFutures.computeAsync [ cancelChecker |
			val rootPath = params.uri.toPath
			val maxDepth = params.maxDepth ?: Integer.MAX_VALUE

			val fileAcceptor = new FileAcceptor(cancelChecker)
			fileManager.resolve(rootPath, maxDepth, fileAcceptor)
			return fileAcceptor.rootFile
		]
	}

	override resolveContent(ResolveContentParams params) {
		return CompletableFutures.computeAsync [ cancelChecker |
			val path = params.uri.toPath
			if (path.regularFile) {
				val charset = Charset.defaultCharset
				val bytes = path.readAllBytes
				
				val content = new FileContent
				content.value = new String(bytes, charset)
				content.encoding = charset.name
				return content
			}
			return null
		]
	}

	protected def Path toPath(String uri) {
		return Paths.get(URI.create(uri))
	}

	override shutdown() {
		fileManager.close

		this.executorService.shutdown()
		languageServer.shutdown
	}

	override exit() {
		this.executorService.shutdownNow
		languageServer.exit
	}

	override connect(LanguageClient client) {
		if (languageServer instanceof LanguageClientAware) {
			languageServer.connect(client)
		}
		if (client instanceof Endpoint) {
			this.client = ServiceEndpoints.toServiceObject(client, WorkspaceClient)
		}
	}

	override supportedMethods() {
		val supportedMethods = ServiceEndpoints.getSupportedMethods(this.class)
		if (languageServer instanceof JsonRpcMethodProvider) {
			supportedMethods.putAll(languageServer.supportedMethods)
		}
		return supportedMethods
	}

}
