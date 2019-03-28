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
import com.google.inject.Inject
import com.google.inject.Singleton
import java.net.URI
import java.nio.file.Path
import org.eclipse.emf.common.EMFPlugin
import org.eclipse.xtext.util.internal.Log

import static extension com.google.common.base.Strings.nullToEmpty

/**
 * Helper service for validating and locating SADL projects.
 */
@ImplementedBy(DispatchingProjectHelper)
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

	@Log
	@Singleton
	class DispatchingProjectHelper implements SadlProjectHelper {

		@Inject
		Noop headless;

		@Inject
		SadlIdeProjectHelper ide;

		@Inject
		EclipseSadlProjectHelper eclipse;

		override isRoot(URI uri) {
			return uri.dispatch.isRoot(uri);
		}

		override getRoot(URI nested) {
			return nested.dispatch.getRoot(nested);
		}

		override toPath(URI uri) {
			return uri.dispatch.toPath(uri);
		}

		override toUri(Path path) {
			return (if(EMFPlugin.IS_ECLIPSE_RUNNING) eclipse else headless).toUri(path);
		}

		protected def SadlProjectHelper ^dispatch(URI uri) {
			val isFile = uri.scheme.nullToEmpty.equalsIgnoreCase('file');
			if (EMFPlugin.IS_ECLIPSE_RUNNING) {
				if (isFile) {
					LOG.info('''Got a file URI while running in Eclipse. Falling back to headless case. URI: «uri»''');
					return headless;
				}
				return eclipse;
			} else {
				if (isFile) {
					return ide;
				}
				return headless;
			}
		}

	}

	/**
	 * Does not help too much. This is the default and should be used for the headless tool-chain. 
	 */
	@Singleton
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
