package io.typefox.lsp.endpoint;

import org.eclipse.lsp4j.DidChangeWatchedFilesParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;

public interface WorkspaceClient {

	@JsonNotification
	void didChangeWatchedFiles(DidChangeWatchedFilesParams params);

}
