package io.typefox.lsp.endpoint.nio.file

import java.io.IOException
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.Set
import org.apache.log4j.Logger
import org.eclipse.xtend.lib.annotations.Accessors

class FileIndex {
    
    static val LOGGER = Logger.getLogger(FileIndex)

    @Accessors(PUBLIC_GETTER)
    val Set<Path> files = <Path>newHashSet

    @Accessors
    var ()=>FileChangedEvent eventEmitter

    def void add(Path path) {
        val event = eventEmitter?.apply
        Files.walkFileTree(path, new SimpleFileVisitor<Path>() {

            override visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                addFile(file.toAbsolutePath, event)
                return super.visitFile(file, attrs)
            }

            override visitFileFailed(Path file, IOException exc) throws IOException {
                LOGGER.warn('''«exc.class.simpleName»: «exc.message»''')
                return FileVisitResult.SKIP_SUBTREE
            }

        })
        event?.fire
    }

    protected def void addFile(Path file, FileChangedEvent event) {
        if (files.add(file)) {
            event?.addCreated(file)
        } else {
            event?.addChanged(file)
        }
    }

    def void remove(Path path) {
        val event = eventEmitter?.apply
        val i = files.iterator
        while (i.hasNext) {
            val file = i.next
            if (file.startsWith(path)) {
                i.remove
                event?.addDeleted(file)
            }
        }
        event?.fire
    }

}
