package io.typefox.lsp.endpoint.nio.file

import java.nio.file.Path

interface PathAcceptor {
	def void onStartDirectory(Path dir)
	def void onFile(Path file)
	def void onEndDirectory(Path dir)
}
