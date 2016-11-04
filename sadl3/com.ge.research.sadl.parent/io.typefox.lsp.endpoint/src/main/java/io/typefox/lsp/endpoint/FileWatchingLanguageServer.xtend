package io.typefox.lsp.endpoint

import com.google.common.util.concurrent.ThreadFactoryBuilder
import io.typefox.lsapi.InitializeParams
import io.typefox.lsapi.services.LanguageServer
import io.typefox.lsp.endpoint.nio.file.FileManager
import java.nio.file.Paths
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import org.apache.log4j.Logger
import org.eclipse.xtend.lib.annotations.Delegate

class FileWatchingLanguageServer implements LanguageServer {

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
                fileManager.open(root)
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

}
