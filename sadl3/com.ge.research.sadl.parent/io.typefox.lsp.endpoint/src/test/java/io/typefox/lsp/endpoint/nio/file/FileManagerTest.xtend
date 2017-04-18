package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Files
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class FileManagerTest {

    @Rule
    public val folder = new TemporaryFolder
    
    extension ExpectationExtension
    
    @Before
    def void setUp() {
        this._expectationExtension = new ExpectationExtension(folder.root.toPath) 
    }

    @Test(timeout=30000)
    def void testProcessEvents() {
        Files.createFile(rootPath.resolve('Foo.txt'))

        val event = new MockFileChangedEvent
        val manager = new FileManager
        manager.eventEmitter = [event]
        manager.open(rootPath)

        Files.createFile(rootPath.resolve('Bar.txt'))
        manager.processEvents
        
        assertEvent('''
            created {
                Bar.txt
            }
        ''', event)
    }

}
