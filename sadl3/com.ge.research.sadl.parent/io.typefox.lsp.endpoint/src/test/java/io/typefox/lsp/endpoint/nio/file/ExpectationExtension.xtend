package io.typefox.lsp.endpoint.nio.file

import io.typefox.lsp.endpoint.File
import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Collection
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

import static org.junit.Assert.*

@FinalFieldsConstructor
class ExpectationExtension {

	@Accessors
	val Path rootPath
	
	def Path createFile(String path) {
		val filePath = rootPath.resolve(path)
		Files.createDirectories(filePath.parent)
		Files.createFile(filePath)
		return filePath
	}
	
	def Path createDirectory(String path) {
		val directoryPath = rootPath.resolve(path)
		Files.createDirectories(directoryPath.parent)
		Files.createDirectory(directoryPath)
		return directoryPath
	}

	def void assertFile(String expectation, File file) {
		assertEquals(expectation, file.toExpectation)
	}

	def void assertIndex(String expectation, FileIndex fileIndex) {
		assertEquals(expectation, fileIndex.toExpectation)
	}

	def void assertEvent(String expectation, MockFileChangedEvent event) {
		assertEquals(expectation, event.toExpectation)
	}

	def void assertEvents(String expectation, Iterable<PathChangedEvent> events) {
		assertEquals(expectation, events.toExpectation)
	}

	def String toExpectation(File file) {
		if (file === null) return 'inexistent'
		'''
			«IF file.children !== null && !file.children.empty»
				«file.toSignature»
					«FOR child: file.children»
						«child.toExpectation»
					«ENDFOR»
			«ELSE»
				«file.toSignature»
			«ENDIF»
		'''
	}

	protected def String toSignature(File file) {
		val result = new StringBuilder
		if (file.directory) {
			if (file.children !== null) {
				result.append('- ')	
			} else {
				result.append('+ ')
			}
		}
		result.append(file.toPath.last)
		return result.toString
	}

	protected def String toModifier(String name, boolean value) {
		return if(value) name + ' ' else ''
	}

	def String toExpectation(FileIndex fileIndex) {
		return toExpectation(fileIndex.files)
	}

	def String toExpectation(MockFileChangedEvent event) '''
		«toExpectation('created', event.createdFiles)»
		«toExpectation('changed', event.changedFiles)»
		«toExpectation('deleted', event.deletedFiles)»
	'''

	def String toExpectation(Iterable<PathChangedEvent> events) '''
		«FOR event : events.map[toExpectation].sort»
			«event»
		«ENDFOR»
	'''

	def String toExpectation(PathChangedEvent event) {
		return event.kind + ': ' + event.path.toExpectation
	}

	def String toExpectation(String type, Collection<Path> files) {
		if(files.empty) return ''
		'''
			«type» {
			    «toExpectation(files)»
			}
		'''
	}

	def String toExpectation(Collection<Path> files) '''
		«FOR file : files.map[toExpectation].sort»
			«file»
		 «ENDFOR»
	'''

	def Path toPath(File file) {
		return file.uri.toPath
	}

	def Path toPath(String uri) {
		return Paths.get(URI.create(uri))
	}

	def String toExpectation(Path file) {
		return rootPath.relativize(file).toString
	}

}
