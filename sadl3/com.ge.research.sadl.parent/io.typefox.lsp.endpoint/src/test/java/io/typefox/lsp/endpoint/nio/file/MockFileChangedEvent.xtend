package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Path
import org.eclipse.xtend.lib.annotations.Accessors

@Accessors(PUBLIC_GETTER)
class MockFileChangedEvent implements FileChangedEvent {

    val createdFiles = <Path>newArrayList
    val changedFiles = <Path>newArrayList
    val deletedFiles = <Path>newArrayList

    override addCreated(Path file) {
        createdFiles.add(file)
    }

    override addChanged(Path file) {
        changedFiles.add(file)
    }

    override addDeleted(Path file) {
        deletedFiles.add(file)
    }
    
    override fire() {
    }

}
