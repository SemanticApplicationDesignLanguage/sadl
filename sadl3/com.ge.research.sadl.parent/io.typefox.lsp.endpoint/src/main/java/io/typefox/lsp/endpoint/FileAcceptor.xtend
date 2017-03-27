package io.typefox.lsp.endpoint

import io.typefox.lsp.endpoint.nio.file.PathAcceptor
import java.nio.file.Files
import java.nio.file.Path
import java.util.Deque
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@FinalFieldsConstructor
class FileAcceptor implements PathAcceptor {
	
	val CancelChecker cancelChecker

	val Deque<File> fileStack = newLinkedList

	def File getRootFile() {
		return fileStack.head
	}

	override onStartDirectory(Path path) {
		cancelChecker?.checkCanceled

		val file = path.asFile
		addChild(file)

		file.children = newArrayList
		fileStack.push(file)
	}

	override onFile(Path path) {
		cancelChecker?.checkCanceled

		val file = path.asFile
		addChild(file)
		if (fileStack.empty) {
			fileStack.push(file)
		}
	}

	override onEndDirectory(Path dir) {
		cancelChecker?.checkCanceled

		if (fileStack.size > 1) {
			fileStack.pop
		}
	}

	protected def File asFile(Path path) {
		val file = new File
		file.uri = path.toUri.toString
		file.directory = Files.isDirectory(path)
		return file
	}
	
	protected def void addChild(File file) {
		if (!fileStack.empty) {
			fileStack.peek.children.add(file)
		}
	}

}
