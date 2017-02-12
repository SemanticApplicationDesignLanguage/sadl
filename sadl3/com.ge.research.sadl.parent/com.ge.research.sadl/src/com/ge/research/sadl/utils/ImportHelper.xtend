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

import com.ge.research.sadl.sADL.SadlModel
import com.google.common.collect.ImmutableCollection.Builder
import com.google.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.util.EcoreUtil

import static com.ge.research.sadl.processing.SadlConstants.SADL_IMPLICIT_MODEL_URI
import static com.google.common.collect.ImmutableSet.builder

/**
 * Singleton helper for imports and import URIs.
 * 
 * @author akos.kitta
 */
@Singleton
class ImportHelper {

	/**
	 * Returns with an iterable containing the URI of the SADL model argument 
	 * and all explicitly declared imported resource URIs including the 
	 * transitive ones. Also contains the implicit SADL model URI.
	 */
	def Iterable<String> getAllImportedResourceUris(SadlModel model) {
		return builder.addAll(model.getImportedModels(builder).build.map[baseUri]).add(SADL_IMPLICIT_MODEL_URI).
			build;
	}

	private def Builder<SadlModel> getImportedModels(SadlModel it, Builder<SadlModel> builder) {
		builder.add(it);
		imports.map[importedResource].filterNull.fold(builder, [ b, a |
			b.add(a.getImportedModels(b).build);
			return b;
		]);
	}

}
