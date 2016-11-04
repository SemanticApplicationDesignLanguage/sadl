package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Path

interface FileChangedEvent {
    def void addCreated(Path file)

    def void addChanged(Path file)

    def void addDeleted(Path file)

    def void fire()
}