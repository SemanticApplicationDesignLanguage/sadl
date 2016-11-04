package io.typefox.lsp.endpoint.nio.file

import java.io.Closeable
import java.nio.file.ClosedWatchServiceException
import java.nio.file.Path
import java.nio.file.WatchEvent
import java.nio.file.WatchService
import org.eclipse.xtend.lib.annotations.Accessors

import static java.nio.file.StandardWatchEventKinds.*

class PathWatcher implements Closeable {
    
    val Path path

    val WatchService watchService
    
    var volatile closed = false

    @Accessors
    var (PathChangedEvent)=>void eventAcceptor

    new(Path path) {
        this.path = path
        watchService = path.fileSystem.newWatchService
        path.register(
            watchService,
            ENTRY_CREATE,
            ENTRY_MODIFY,
            ENTRY_DELETE
        )
    }

    override void close() {
        this.closed = true
        watchService.close
    }

    def boolean processEvents() {
        if (this.closed) return false
        try {
            val watchKey = watchService.take
            for (event : watchKey.pollEvents) {
                if (!watchKey.valid) return false
                processEvent(event)
            }
            return watchKey.reset
        } catch (ClosedWatchServiceException e) {
            return false
        } catch (InterruptedException e) {
            return false
        }
    }

    protected def void processEvent(WatchEvent<?> event) {
        if(event.kind === OVERFLOW) return;
        val path = event.context
        if (path instanceof Path) {
            val childPath = this.path.resolve(path)
            if (!this.closed) {
                val pathChangedEvent = new PathChangedEvent(childPath, event.kind)
                eventAcceptor?.apply(pathChangedEvent)
            }
        }
    }

}
