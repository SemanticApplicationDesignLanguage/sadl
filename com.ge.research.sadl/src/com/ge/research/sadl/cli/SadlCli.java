package com.ge.research.sadl.cli;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.UUID;

import org.eclipse.emf.common.util.URI;

import com.google.common.base.Optional;

public class SadlCli {

	public static Optional<URI> projectRootUri = Optional.absent();

	public static void main(final String[] args) throws Exception {
		try {
			Path file = copyProjectContentToTmp(getProjectRootPath(args));
			projectRootUri = Optional.fromNullable(URI.createFileURI(file
					.toFile().getAbsolutePath()));
			new SadlCliHelper().transform(file);
		} finally {
			projectRootUri = Optional.absent();
		}
	}

	private static Path getProjectRootPath(final String[] args) {
		if (args == null || args.length == 0) {
			throw new IllegalArgumentException(
					"The absolute path to the project root should be specified.");
		}

		if (args.length > 1) {
			throw new IllegalArgumentException(
					"Only one project can be converted in a time. args was: "
							+ Arrays.toString(args));
		}

		final File file = new File(args[0]);
		if (!file.exists()) {
			throw new IllegalArgumentException("Project root " + file
					+ " does not exist.");
		}

		if (file.isFile()) {
			throw new IllegalArgumentException(
					"Expected a folder as the project root. Got a file instead:"
							+ file);
		}

		return file.toPath();
	}

	private static Path copyProjectContentToTmp(Path projectRoot)
			throws IOException {
		
		String tmpDirStr = System.getProperty("java.io.tmpdir");
		if (tmpDirStr == null) {
			throw new IllegalStateException(
					"System property 'java.io.tmpdir' does not specify a temporary directory.");
		}
		final File tmpDirRoot = new File(tmpDirStr);
		if (!tmpDirRoot.exists()) {
			throw new IllegalStateException(
					"Temporary directory does not exist. " + tmpDirRoot);
		}

		if (!tmpDirRoot.isDirectory()) {
			throw new IllegalStateException(
					"Expected a directory location for the temporary directory. "
							+ tmpDirRoot);
		}

		if (!tmpDirRoot.canWrite()) {
			throw new IllegalStateException(
					"Cannot write temporary directory content. " + tmpDirRoot);
		}

		
		
		Path tmpDir = Paths.get(System.getProperty("java.io.tmpdir") + "/"
				+ UUID.randomUUID() + "/" + projectRoot.getFileName());
		
		Files.createDirectories(tmpDir);

		Files.walkFileTree(projectRoot, new CopyFileVisitor(tmpDir));
		return tmpDir.toFile().getCanonicalFile().toPath();

	}

}
