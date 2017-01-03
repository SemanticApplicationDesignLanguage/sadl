package io.typefox.lsp.endpoint.nio.file

import com.google.common.base.StandardSystemProperty
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import org.eclipse.xtext.util.Wrapper
import org.junit.Assume
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

import static org.junit.Assert.*

class FileIndexTest {

    @Rule
    public val folder = new TemporaryFolder

    extension ExpectationExtension

    @Before
    def void setUp() {
        this._expectationExtension = new ExpectationExtension(folder.root.toPath)
    }

    @Test
    def void testAddFolder() {
        val fileIndex = new FileIndex
        val event = new MockFileChangedEvent
        fileIndex.eventEmitter = [event]
        assertTrue(fileIndex.files.empty)

        Files.createDirectories(rootPath.resolve('foo'))
        Files.createDirectories(rootPath.resolve('bar'))
        Files.createFile(rootPath.resolve('foo/Foo.txt'))
        Files.createFile(rootPath.resolve('bar/Bar.txt'))
        Files.createFile(rootPath.resolve('Baz.txt'))

        fileIndex.add(rootPath)

        assertIndex('''
            Baz.txt
            bar«File.separatorChar»Bar.txt
            foo«File.separatorChar»Foo.txt
        ''', fileIndex)

        assertEvent('''
            created {
                Baz.txt
                bar«File.separatorChar»Bar.txt
                foo«File.separatorChar»Foo.txt
            }
        ''', event)
    }

    @Test
    def void testAddFile() {
        val fileIndex = new FileIndex
        assertTrue(fileIndex.files.empty)

        val event = new MockFileChangedEvent
        fileIndex.eventEmitter = [event]

        val filePath = Files.createFile(rootPath.resolve('Baz.txt'))

        fileIndex.add(filePath)

        assertIndex('''
            Baz.txt
        ''', fileIndex)

        assertEvent('''
            created {
                Baz.txt
            }
        ''', event)
    }

    @Test
    def void testRemoveFolder() {
        val fileIndex = new FileIndex
        assertTrue(fileIndex.files.empty)

        val fooPath = Files.createDirectories(rootPath.resolve('foo'))
        Files.createDirectories(rootPath.resolve('bar'))
        Files.createFile(rootPath.resolve('foo/Foo.txt'))
        Files.createFile(rootPath.resolve('bar/Bar.txt'))

        fileIndex.add(rootPath)

        val event = new MockFileChangedEvent
        fileIndex.eventEmitter = [event]
        fileIndex.remove(fooPath)

        assertIndex('''
            bar«File.separatorChar»Bar.txt
        ''', fileIndex)

        assertEvent('''
            deleted {
                foo«File.separatorChar»Foo.txt
            }
        ''', event)
    }

    @Test
    def void testChnagedFile() {
        val fileIndex = new FileIndex
        assertTrue(fileIndex.files.empty)

        val filePath = Files.createFile(rootPath.resolve('Baz.txt'))
        fileIndex.add(filePath)

        val event = new MockFileChangedEvent
        fileIndex.eventEmitter = [event]
        fileIndex.add(filePath)

        assertIndex('''
            Baz.txt
        ''', fileIndex)

        assertEvent('''
            changed {
                Baz.txt
            }
        ''', event)
    }

    @Test
    def void testFailedAddFile_01() {
    	assumeUnixOS;
    	
        val fileIndex = new FileIndex

        val filePath = Files.createFile(rootPath.resolve('Foo.txt'))
        val filePath2 = Files.createFile(rootPath.resolve('Bar.txt'))
        val addedPath = new Wrapper<String>
        val event = new MockFileChangedEvent() {

            override addCreated(Path file) {
                super.addCreated(file)

                Files.delete(if(file == filePath) filePath2 else filePath)
                addedPath.set(file.toExpectation)
            }

        }
        fileIndex.eventEmitter = [event]
        fileIndex.add(rootPath)

        assertIndex('''
            «addedPath.get»
        ''', fileIndex)

        assertEvent('''
            created {
                «addedPath.get»
            }
        ''', event)
    }

    @Test
    def void testFailedAddFile_02() {
        val fileIndex = new FileIndex

        Files.createDirectory(rootPath.resolve('foo'))
        Files.createDirectory(rootPath.resolve('bar'))
        val filePath = Files.createFile(rootPath.resolve('foo/Foo.txt'))
        val filePath2 = Files.createFile(rootPath.resolve('bar/Bar.txt'))
        val addedPath = new Wrapper<String>
        val event = new MockFileChangedEvent() {

            override addCreated(Path file) {
                super.addCreated(file)

                val fileToDelete = if(file == filePath) filePath2 else filePath
                Files.delete(fileToDelete)
                Files.delete(fileToDelete.parent)
                addedPath.set(file.toExpectation)
            }

        }
        fileIndex.eventEmitter = [event]
        fileIndex.add(rootPath)

        assertIndex('''
            «addedPath.get»
        ''', fileIndex)

        assertEvent('''
            created {
                «addedPath.get»
            }
        ''', event)
    }

    @Test
    def void testFailedAddFile_03() {
    	assumeUnixOS;
    	
        val fileIndex = new FileIndex

        Files.createDirectory(rootPath.resolve('foo'))
        val filePath = Files.createFile(rootPath.resolve('foo/Foo.txt'))
        val filePath2 = Files.createFile(rootPath.resolve('foo/Bar.txt'))
        val addedPath = new Wrapper<String>
        val event = new MockFileChangedEvent() {

            override addCreated(Path file) {
                super.addCreated(file)

                Files.delete(filePath)
                Files.delete(filePath2)
                Files.delete(filePath.parent)
                addedPath.set(file.toExpectation)
            }

        }
        fileIndex.eventEmitter = [event]
        fileIndex.add(rootPath)

        assertIndex('''
            «addedPath.get»
        ''', fileIndex)

        assertEvent('''
            created {
                «addedPath.get»
            }
        ''', event)
    }

    @Test
    def void testFailedAddFile_04() {
        val fileIndex = new FileIndex

        Files.createDirectories(rootPath.resolve('root/foo'))
        Files.createDirectories(rootPath.resolve('root/bar'))
        val filePath = Files.createFile(rootPath.resolve('root/foo/Foo.txt'))
        val filePath2 = Files.createFile(rootPath.resolve('root/bar/Bar.txt'))
        val addedPath = new Wrapper<String>
        val event = new MockFileChangedEvent() {

            override addCreated(Path file) {
                super.addCreated(file)

                val fileToDelete = if(file == filePath) filePath2 else filePath
                Files.delete(fileToDelete)
                Files.delete(fileToDelete.parent)
                addedPath.set(file.toExpectation)
            }

        }
        fileIndex.eventEmitter = [event]
        fileIndex.add(rootPath)

        assertIndex('''
            «addedPath.get»
        ''', fileIndex)

        assertEvent('''
            created {
                «addedPath.get»
            }
        ''', event)
    }
    
    /**
     * Windows handles differently deleted files and folders than UNIX. Instead of silently 
     * handling the no such file exception in the file visitor (just like we do in the file
     * indexer) an access denied exception is thrown on Windows which cannot be just ignored
     * as it is ignored on UNIX.
     */
    private def void assumeUnixOS() {
		Assume.assumeTrue('Assumed UNIX OS.', StandardSystemProperty.OS_NAME.value.indexOf('win') < 0);
	}

}
