package io.typefox.lsp.endpoint

import com.google.common.util.concurrent.ThreadFactoryBuilder
import io.typefox.lsp.endpoint.nio.file.FileManager
import java.io.IOException
import java.nio.file.Paths
import java.util.Collections
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import org.apache.log4j.Logger
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethodProvider
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.xtend.lib.annotations.Delegate

class FileWatchingLanguageServer implements LanguageServer, LanguageClientAware, JsonRpcMethodProvider {

    val static DELAY = 500

    val static LOGGER = Logger.getLogger(FileWatchingLanguageServer)

    val FileManager fileManager

    @Delegate
    val LanguageServer languageServer

    val ScheduledExecutorService executorService

    new(LanguageServer languageServer) {
        this.languageServer = languageServer

        this.fileManager = new FileManager
        fileManager.eventEmitter = [
            new LanguageServerFileChangedEvent(languageServer)
        ]

        val builder = new ThreadFactoryBuilder
        builder.nameFormat = FileWatchingLanguageServer.simpleName + ' %d'
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
    }
				
	override supportedMethods() {
		if (languageServer instanceof JsonRpcMethodProvider) {
			return languageServer.supportedMethods; 
		}
		return Collections.emptyMap;
	}

}