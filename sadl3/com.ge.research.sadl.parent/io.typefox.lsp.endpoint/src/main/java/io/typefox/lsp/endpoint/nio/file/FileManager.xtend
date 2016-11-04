package io.typefox.lsp.endpoint.nio.file

import java.io.Closeable
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
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

}
