package io.typefox.lsp.endpoint

import io.typefox.lsp.endpoint.nio.file.FileChangedEvent
import java.nio.file.Path
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.FileChangeType
import org.eclipse.lsp4j.FileEvent
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@FinalFieldsConstructor
class DidChangeWatchedFilesEvent implements FileChangedEvent {

	var params = new DidChangeWatchedFilesParams()

	val (DidChangeWatchedFilesParams)=>void eventEmitter

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
		params.changes += new FileEvent(file.toUri.toString, type)
	}

	override fire() {
		if (!params.changes.empty) {
			eventEmitter.apply(params)
			params = new DidChangeWatchedFilesParams()
		}
	}

}
