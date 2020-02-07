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

import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.ImplementedBy

/**
 * Provides RDF comments for SADL resources.
 * 
 * @author akos.kitta 
 */
@ImplementedBy(Default)
interface SadlResourceCommentProvider {

	/**
	 * Returns with the comment extracted from the {@code note} annotation from the given SADL resource.
	 * Returns with an empty iterable if no comments are attached to the SADL resource argument.
	 */
	def Iterable<String> getComment(SadlResource it);

}


/**
 * The default comments provider.
 */
class Default implements SadlResourceCommentProvider {

	override getComment(SadlResource it) {
		return it.annotations.filter[annotations.type == 'note'].map[annotations.contents].flatten;
	}

}
