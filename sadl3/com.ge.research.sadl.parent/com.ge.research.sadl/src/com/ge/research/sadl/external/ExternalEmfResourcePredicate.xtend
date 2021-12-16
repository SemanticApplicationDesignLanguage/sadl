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
package com.ge.research.sadl.external

import com.google.common.base.Predicate
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import org.eclipse.emf.common.EMFPlugin
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.util.internal.Log
import org.slf4j.LoggerFactory

import static extension com.google.common.base.Strings.nullToEmpty

/**
 * Predicate that evaluates to {@code true} if the resource URI is an external EMF resource and has to be loaded.
 * 
 * @author akos.kitta
 */
@ImplementedBy(DispatchingExternalEmfResourcePredicate)
interface ExternalEmfResourcePredicate extends Predicate<URI> {

	static val EXTERNAL_EXTENSION = 'url';

	/**
	 * {@code true} if the resource given with its URI is an external resource. Otherwise, {@code false}. 
	 */
	override boolean apply(URI resourceUri);

	@Singleton
	static class DispatchingExternalEmfResourcePredicate implements ExternalEmfResourcePredicate {

		static val LOGGER = LoggerFactory.getLogger(DispatchingExternalEmfResourcePredicate);

		@Inject
		Default headless;

		@Inject
		SadlIdeExternalEmfResourcePredicate ide;

		@Inject
		EclipseExternalEmfResourcePredicate eclipse;

		override apply(URI resourceUri) {
			return resourceUri.dispatch.apply(resourceUri);
		}

		protected def ExternalEmfResourcePredicate ^dispatch(URI uri) {
			val isFile = uri.scheme.nullToEmpty.equalsIgnoreCase('file');
			if (EMFPlugin.IS_ECLIPSE_RUNNING) {
				if (isFile) {
					LOGGER.info('''Got a file URI while running in Eclipse. Falling back to headless case. URI: «uri»''');
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

	static class Default implements ExternalEmfResourcePredicate {

		static val IGNORED_FILES = #[
			'SadlBaseModel.owl',
			'SadlListModel.owl',
			'SadlBuiltinFunctions.owl',
			'SadlImplicitModel.owl',
			'metrics.owl'
		];

		override apply(URI it) {
			if (!ExternalEmfResourceFactory.EXTERNAL_EXTENSIONS.contains(fileExtension)) {
				return false;
			}
			val name = lastSegment;
			if (IGNORED_FILES.exists[name.endsWith(it)]) {
				return false
			}
			return true
		}

	}

}
