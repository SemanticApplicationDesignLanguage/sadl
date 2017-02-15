package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Files
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

import static org.junit.Assert.*

class PathWatcherTest {

    @Rule
    public val folder = new TemporaryFolder
    
    extension ExpectationExtension
    
    @Before
    def void setUp() {
        this._expectationExtension = new ExpectationExtension(folder.root.toPath) 
    }

    @Test(timeout=30000)
    def void testProcessAddFile() {
        val events = newArrayList
        val watcher = new PathWatcher(rootPath)
        watcher.eventAcceptor = [ event |
            events += event
        ]

        Files.createFile(rootPath.resolve('Foo.txt'))

        assertTrue(watcher.processEvents)
        assertEvents('''
            ENTRY_CREATE: Foo.txt
        ''', events)
    }

    @Test(timeout=30000)
    def void testProcessAddFolder() {
        val events = newArrayList
        val watcher = new PathWatcher(rootPath)
        watcher.eventAcceptor = [ event |
            events += event
        ]

        Files.createDirectories(rootPath.resolve('foo'))
        Files.createFile(rootPath.resolve('foo/Foo.txt'))

        assertTrue(watcher.processEvents)
        assertEvents('''
            ENTRY_CREATE: foo
        ''', events)
    }

    @Test(timeout=30000)
    def void testProcessMove() {
        val folderPath = Files.createDirectories(rootPath.resolve('foo'))
        Files.createFile(folderPath.resolve('Foo.txt'))

        val events = newArrayList
        val watcher = new PathWatcher(rootPath)
        watcher.eventAcceptor = [ event |
            events += event
        ]

        Files.move(folderPath, folderPath.resolveSibling('bar'))

        val expectedEvents = newArrayList('ENTRY_CREATE: bar', 'ENTRY_DELETE: foo')
        while(!expectedEvents.empty) {
            assertTrue(watcher.processEvents)
            expectedEvents.removeAll(events.map[toExpectation])
        }
    }

}
