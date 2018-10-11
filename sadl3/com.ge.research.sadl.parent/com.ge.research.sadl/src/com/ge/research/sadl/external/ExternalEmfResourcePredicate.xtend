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
import com.google.inject.Singleton
import org.eclipse.emf.common.util.URI

/**
 * Predicate that evaluates to {@code true} if the resource URI is an external EMF resource and has to be loaded.
 * 
 * @author akos.kitta
 */
@ImplementedBy(Default)
interface ExternalEmfResourcePredicate extends Predicate<URI> {
	
	static val EXTERNAL_EXTENSION = 'url';

	/**
	 * {@code true} if the resource given with its URI is an external resource. Otherwise, {@code false}. 
	 */
	override boolean apply(URI resourceUri);

	@Singleton
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
