package io.typefox.lsp.endpoint

import java.util.List
import javax.validation.constraints.NotNull
import org.eclipse.lsp4j.generator.LanguageServerAPI
import org.eclipse.lsp4j.jsonrpc.validation.NonNull

@LanguageServerAPI
class File {

	@NonNull
	String uri

	boolean directory

	/**
	 * <p>
	 * {@code null} if {@link File#directory directory} = false or the directory has not been resolved.
	 * </p>
	 * 
	 * <p>
	 * Use {@link WorkspaceServer#resolveFile workspace/resolveFile} request
	 * with {@link ResolveFileParams#uri params.uri} = {@link File#uri uri}
	 * and {@link ResolveFileParams#maxDepth params.maxDepth} != 0 to resolve the directory.
	 * </p>
	 */
	List<File> children

}

@LanguageServerAPI
class FileContent {

	@NotNull
	String value

}

@LanguageServerAPI
class ResolveFileParams {
	@NonNull
	String uri

	/**
	 * <p>
	 * {@code maxDepth = null} or {@code maxDepth >= 0}
	 * </p>
	 * <p>
	 * {@code maxDepth = null} means that all directories should be resolved,
	 * see {@link WorkspaceServer#resolveFile workspace/resolveFile}
	 * </p>
	 */
	Integer maxDepth
}

@LanguageServerAPI
class ResolveFileContentParams {
	@NonNull
	String uri
}

@LanguageServerAPI
class UpdateFileContentParams {
	
	@NonNull
	String uri
	
	@NonNull
	FileContent content
	
}
