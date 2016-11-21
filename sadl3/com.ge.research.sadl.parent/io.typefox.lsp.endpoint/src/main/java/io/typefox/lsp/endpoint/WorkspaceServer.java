package io.typefox.lsp.endpoint;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment;

@JsonSegment("workspace")
public interface WorkspaceServer {

	/**
	 * <ol>
	 * <li>if a file or directory for {@link CreateFileParams#uri params.uri}
	 * already exists then do nothing</li>
	 * <li>otherwise:</li>
	 * <ol>
	 * <li>creates a new file and all non-existing parent directories</li>
	 * <li>if {@link CreateFileParams#content params.content} != null then
	 * updates a created file with the given content</li>
	 * </ol>
	 * </ol>
	 */
	CompletableFuture<Void> createFile(CreateFileParams params);

	/**
	 * <ol>
	 * <li>if a file or directory for {@link CreateDirectoryParams#uri params.uri}
	 * already exists then do nothing</li>
	 * <li>otherwise creates a new directory and all non-existing parent
	 * directories</li>
	 * </ol>
	 */
	CompletableFuture<Void> createDirectory(CreateDirectoryParams params);

	/**
	 * <ol>
	 * <li>if a file or directory for {@link DeleteFileParams#uri params.uri}
	 * does not exist then do nothing</li>
	 * <li>otherwise:</li>
	 * <ol>
	 * <li>if {@link DeleteFileParams#uri params.uri} corresponds to a file
	 * then deletes a file</li>
	 * <li>if {@link DeleteFileParams#uri params.uri} corresponds to a
	 * directory then deletes a directory and its children</li>
	 * </ol>
	 */
	CompletableFuture<Void> deleteFile(DeleteFileParams params);

	/**
	 * <p>
	 * Computes a file state for the given URI and its children.
	 * </p>
	 * 
	 * @return
	 *         <ol>
	 *         <li>{@code null} if a file does not exist for
	 *         {@link ResolveFileParams#uri params.uri}</li>
	 *         <li>otherwise a file if {@link ResolveFileParams#uri params.uri}
	 *         corresponds to an existing file</li>
	 *         <li>otherwise:
	 *         <ol>
	 *         <li>a directory if {@link ResolveFileParams#maxDepth
	 *         params.maxDepth} = 0;<br>
	 *         a directory is not resolved</li>
	 *         <li>a directory and its children if
	 *         {@link ResolveFileParams#maxDepth params.maxDepth} = 1;<br>
	 *         child directories are not resolved</li>
	 *         <li>a directory and its children if
	 *         {@link ResolveFileParams#maxDepth params.maxDepth} = N where N >
	 *         1;<br>
	 *         child directories are resolved with
	 *         {@link ResolveFileParams#maxDepth params.maxDepth} = N - 1</li>
	 *         </ol>
	 *         </ol>
	 */
	@JsonRequest
	CompletableFuture<File> resolveFile(ResolveFileParams params);

	/**
	 * <p>
	 * Computes a file content for the given URI.
	 * </p>
	 * 
	 * @return {@code null} if a file does exist for
	 *         {@link ResolveFileContentParams#uri params.uri}; otherwise a file
	 *         content
	 */
	@JsonRequest
	CompletableFuture<FileContent> resolveFileContent(ResolveFileContentParams params);

	/**
	 * <p>
	 * Updates a file content for the given URI.
	 * </p>
	 */
	@JsonRequest
	CompletableFuture<Void> updateFileContent(UpdateFileContentParams params);

}