package io.typefox.lsp.endpoint;

import org.eclipse.lsp4j.DidChangeWatchedFilesParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.services.LanguageClientExtensions;

public interface WorkspaceClient extends LanguageClientExtensions {

	@JsonNotification("workspace/didChangeWatchedFiles")
	void didChangeWatchedFiles(DidChangeWatchedFilesParams params);

}
