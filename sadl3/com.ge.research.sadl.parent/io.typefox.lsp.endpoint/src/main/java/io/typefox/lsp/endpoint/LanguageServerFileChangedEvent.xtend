package io.typefox.lsp.endpoint

import io.typefox.lsapi.FileChangeType
import io.typefox.lsapi.builders.DidChangeWatchedFilesParamsBuilder
import io.typefox.lsapi.services.LanguageServer
import io.typefox.lsp.endpoint.nio.file.FileChangedEvent
import java.nio.file.Path
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@FinalFieldsConstructor
class LanguageServerFileChangedEvent implements FileChangedEvent {

    val LanguageServer languageServer

    val builder = new DidChangeWatchedFilesParamsBuilder()

    override addCreated(Path path) {
        addChange(path, FileChangeType.Created)
    }

    override addChanged(Path path) {
        addChange(path, FileChangeType.Changed)
    }

    override addDeleted(Path path) {
        addChange(path, FileChangeType.Deleted)
    }

    protected def addChange(Path file, FileChangeType type) {
        builder.change(file.toUri.toString, type)
    }

    override fire() {
        val params = builder.build
        if (!params.changes.empty) {
            languageServer.workspaceService.didChangeWatchedFiles(params)
        }
    }

}