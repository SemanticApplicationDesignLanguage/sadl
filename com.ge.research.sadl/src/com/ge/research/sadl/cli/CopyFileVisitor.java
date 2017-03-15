package com.ge.research.sadl.cli;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

public class CopyFileVisitor extends SimpleFileVisitor<Path> {

	private final Path targetPath;
	private Path sourcePath = null;

	public CopyFileVisitor(Path targetPath) {
		this.targetPath = targetPath;
	}

	@Override
	public FileVisitResult preVisitDirectory(final Path dir,
			final BasicFileAttributes attrs) throws IOException {

		if (sourcePath == null) {
			sourcePath = dir;
		} else {
			Files.createDirectories(targetPath.resolve(sourcePath
					.relativize(dir)));
		}
		return FileVisitResult.CONTINUE;
	}

	@Override
	public FileVisitResult visitFile(final Path file,
			final BasicFileAttributes attrs) throws IOException {
		
		if (!".DS_Store".equals(file.getFileName().toString()) && !".project".equals(file.getFileName().toString())) {
			Files.copy(file, targetPath.resolve(sourcePath.relativize(file)));
		}
		return FileVisitResult.CONTINUE;
	}

}
