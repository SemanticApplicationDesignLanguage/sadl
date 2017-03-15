package com.ge.research.sadl.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Date;
import java.util.UUID;

import org.eclipse.emf.common.util.URI;

import com.ge.research.sadl.builder.ResourceManager;
import com.google.common.base.Optional;
import com.ibm.icu.text.SimpleDateFormat;

public class SadlCli {

	public static Optional<URI> projectRootUri = Optional.absent();

	public static void main(final String[] args) throws Exception {
		try {
			Path originalPath = getProjectRootPath(args);
			Path file = copyProjectContentToTmp(originalPath);
			projectRootUri = Optional.fromNullable(URI.createFileURI(file
					.toFile().getAbsolutePath()));
			new SadlCliHelper().transform(file);

			Path outputPath = Paths.get(originalPath.toString()
					+ "_transformed_"
					+ new SimpleDateFormat("yyyy-mm-dd_hhMMss")
							.format(new Date()));
			info("Copying generated output to " + outputPath);
			Files.walkFileTree(file, new CopyFileVisitor(outputPath));

		} finally {
			URI uri = projectRootUri.orNull();
			if (uri != null) {
				info("Cleaning up temporary resources.");
				delete(new File(uri.toFileString()));
			}
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
			throws IOException, Exception {

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

		Path tmpDir = Paths
				.get(System.getProperty("java.io.tmpdir") + "/"
						+ UUID.randomUUID() + "/" + projectRoot.getFileName())
				.toFile().getCanonicalFile().toPath();

		Files.createDirectories(tmpDir);

		info("Copying SADL project content to " + tmpDir + "...");

		Files.walkFileTree(projectRoot, new CopyFileVisitor(tmpDir));

		File owlDir = new File(tmpDir.toFile(), ResourceManager.OWLDIR);
		if (!owlDir.exists()) {
			info("Creating OwlModels folder to " + owlDir + "..");
			Files.createDirectory(owlDir.toPath());

			for (String fileName : new String[] { "ont-policy.rdf",
					"configuration.rdf" }) {
				info("Copying " + fileName + " into the OwlModels folder...");
				final URL resourceUrl = SadlCli.class.getClassLoader()
						.getResource("Models/" + fileName);
				if (resourceUrl != null) {
					final File resource = new File(resourceUrl.toURI());
					if (resource.exists()) {
						final File target = new File(owlDir, fileName);
						Files.createFile(target.toPath());
						ResourceManager.copyFile(resource, target);
						info("The " + fileName
								+ "resource has been successfully copied.");
					}
				}
			}

		}

		return tmpDir;

	}

	private static void delete(File f) throws Exception {
		if (f.isDirectory()) {
			for (File c : f.listFiles()) {
				delete(c);
			}
		}
		info("Deleting " + f + "...");
		if (!f.delete()) {
			throw new FileNotFoundException("Failed to delete file: " + f);
		}
	}

	static void info(Object it) {
		System.out.println(getTimestamp() + " INFO  [SADL-CLI]: " + it);
	}

	static void error(Object it) {
		System.err.println(getTimestamp() + " ERROR [SADL-CLI]: " + it);
	}

	static private String getTimestamp() {
		return new SimpleDateFormat("yyyy-mm-dd hh:MM:ss").format(new Date());
	}

}
