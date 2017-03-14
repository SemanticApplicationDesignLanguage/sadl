package com.ge.research.sadl.cli;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;

import org.eclipse.emf.common.util.URI;

import com.google.common.base.Optional;

public class SadlCli {

	public static Optional<URI> projectRootUri = Optional.absent();

	public static void main(final String[] args) throws Exception {
		try {
			final Path file = getProjectRootPath(new String[] { "/Users/akos.kitta/Downloads/TestSadlIde" });
			// val file =
			// getProjectRootPath(newArrayList("/Users/akos.kitta/dev/wss/sadl_jar/runtime-New_configuration/Test"));
			projectRootUri = Optional.fromNullable(URI.createFileURI(file
					.toFile().getAbsolutePath()));
			new SadlCliHelper().transform(file);
		} finally {
			projectRootUri = Optional.absent();
		}
	}

	private static Path getProjectRootPath(final String[] args) {
		if (args == null || args.length == 0) {
			throw new IllegalArgumentException("The absolute path to the project root should be specified.");
		}

		if (args.length > 1) {
			throw new IllegalArgumentException("Only one project can be converted in a time. args was: " + Arrays.toString(args));
		}
		
		final File file = new File(args[0]);
		if (!file.exists()) {
			throw new IllegalArgumentException("Project root " +file+" does not exist.");
		}
		
		if (file.isFile()) {
			throw new IllegalArgumentException("Expected a folder as the project root. Got a file instead:" + file);
		}
		
		return file.toPath();
	}
}
