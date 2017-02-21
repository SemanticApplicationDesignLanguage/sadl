/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.utils

import com.google.inject.Inject
import com.google.inject.Singleton
import java.io.File
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.util.IFileSystemScanner.JavaIoFileSystemScanner

/**
 * Class for converting {@link Path path} and {@link File file} instances
 * into {@link URI EMF based URI}s. 
 * 
 * <p>
 * This class, unlike the {@link URI#createFileURI EMF based factory method},
 * will provide URIs with the following pattern:
 * <ul>
 * <li>{@code file:///Users/x/y/z} or</li>
 * <li>{@code file:///C:/x/y/z}</li>.
 * </ul>
 * 
 * <p>
 * Internally, this class delegates into the {@link JavaIoFileSystemScanner}
 * and reuses its logic.
 * 
 * @author akos.kitta
 */
@Singleton
class PathToFileUriConverter {

	@Inject
	extension JavaIoFileSystemScanner;

	/**
	 * Converts the path argument into a EMF based file URI.
	 */
	def URI createFileUri(Path it) {
		return toFile.createFileUri;
	}

	/**
	 * Converts the file argument into a EMF based file URI.
	 */
	def URI createFileUri(File it) {
		val acceptor = new SingleElementAcceptor
		scanRec(it, acceptor);
		return acceptor.ref.get;
	}

	/**
	 * URI acceptor that accepts only the first visited URI. Once the first URI is visited and accepted, instances of this class do not change their internal states.
	 * 
	 * @author akos.kitta
	 */
	private static final class SingleElementAcceptor implements IAcceptor<URI> {

		AtomicReference<URI> ref

		new() {
			ref = new AtomicReference<URI>;
		}

		@Override
		override accept(URI uri) {
			if (ref.get === null) {
				ref.set(uri);
			}
		}

	}

}
