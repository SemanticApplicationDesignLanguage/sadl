package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Path
import java.util.Collection
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

import static org.junit.Assert.*

@FinalFieldsConstructor
class ExpectationExtension {

    @Accessors
    val Path rootPath

    protected def void assertIndex(String expectation, FileIndex fileIndex) {
        assertEquals(expectation, fileIndex.toExpectation)
    }

    protected def void assertEvent(String expectation, MockFileChangedEvent event) {
        assertEquals(expectation, event.toExpectation)
    }

    protected def void assertEvents(String expectation, Iterable<PathChangedEvent> events) {
        assertEquals(expectation, events.toExpectation)
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

    def String toExpectation(Path file) {
        return rootPath.relativize(file).toString
    }

}
