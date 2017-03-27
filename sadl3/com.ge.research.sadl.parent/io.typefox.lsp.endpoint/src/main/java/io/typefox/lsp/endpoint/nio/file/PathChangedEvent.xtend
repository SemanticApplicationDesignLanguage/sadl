package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Path
import java.nio.file.WatchEvent
import org.eclipse.xtend.lib.annotations.Data

@Data
class PathChangedEvent {
    Path path
    WatchEvent.Kind<?> kind
}