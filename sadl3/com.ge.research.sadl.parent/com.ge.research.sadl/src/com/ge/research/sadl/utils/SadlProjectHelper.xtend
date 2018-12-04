/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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

import com.google.inject.ImplementedBy
import java.net.URI
import java.nio.file.Path

/**
 * Helper service for validating and locating SADL projects.
 */
@ImplementedBy(Noop)
interface SadlProjectHelper {

	static val DOT_PROJECT = '.project';

	/**
	 * Returns {@code true} if the URI argument points to a SADL project root. Otherwise, {@code false}.
	 */
	def boolean isRoot(URI uri);

	/**
	 * Returns with the URI of the container project of the argument. Returns with the argument if that is a SADL project root.
	 * Returns with {@code null} if the SADL project cannot be found, accessed or does not exist.
	 */
	def URI getRoot(URI nested);

	/**
	 * Converts the FS path to the appropriate URI format.
	 */
	def URI toUri(Path path) {
		return path.toFile.toURI;
	}

	/**
	 * Converts the URI to a file-system path.
	 */
	def Path toPath(URI uri);

	/**
	 * The default project helper. Does not help too much. This is the default and should be used for the headless tool-chain. 
	 */
	class Noop implements SadlProjectHelper {

		override isRoot(URI uri) {
			return false;
		}

		override getRoot(URI nested) {
			return null;
		}

		override toPath(URI uri) {
			return null;
		}

	}

}
