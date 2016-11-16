package io.typefox.lsp.endpoint.nio.file

import java.io.Closeable
import java.io.IOException
import java.nio.file.FileVisitOption
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardWatchEventKinds
import java.nio.file.attribute.BasicFileAttributes
import java.util.EnumSet
import org.eclipse.xtend.lib.annotations.Accessors

class FileManager implements Closeable {

	PathWatcher watcher

	@Accessors
	var ()=>FileChangedEvent eventEmitter

	def void open(Path path) {
		val index = new FileIndex
		index.add(path)
		index.eventEmitter = eventEmitter
		watcher = new PathWatcher(path)
		watcher.eventAcceptor = [ event |
			process(index, event)
		]
	}

	protected def void process(FileIndex index, PathChangedEvent event) {
		if (event.kind === StandardWatchEventKinds.ENTRY_DELETE) {
			index.remove(event.path)
		} else {
			index.add(event.path)
		}
	}

	def void processEvents() {
		watcher?.processEvents
	}

	override void close() {
		watcher?.close
		watcher = null
	}

	def void resolve(Path path, int maxDepth, PathAcceptor acceptor) {
		Files.walkFileTree(path, EnumSet.noneOf(FileVisitOption), maxDepth, new SimpleFileVisitor<Path>() {

			override preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
				acceptor.onStartDirectory(dir)
				super.preVisitDirectory(dir, attrs)
			}

			override visitFile(Path file, BasicFileAttributes attrs) throws IOException {
				acceptor.onFile(file)
				return super.visitFile(file, attrs)
			}

			override postVisitDirectory(Path dir, IOException exc) throws IOException {
				acceptor.onEndDirectory(dir)
				super.postVisitDirectory(dir, exc)
			}

			override visitFileFailed(Path file, IOException exc) throws IOException {
				return FileVisitResult.SKIP_SUBTREE
			}

		})
	}

}
