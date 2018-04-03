/************************************************************************
 * Copyright © 2007-2018 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.hover

import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.utils.SadlResourceCommentProvider
import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.documentation.IEObjectDocumentationProvider

/**
 * EObject documentation provider for showing an arbitrary documentation for a particular AST node element.
 * 
 * @author akos.kitta
 */
class SadlEObjectDocumentationProvider implements IEObjectDocumentationProvider {

	@Inject
	extension SadlResourceCommentProvider

	override getDocumentation(EObject it) {
		if (it instanceof SadlResource) {
			val comments = comment;
			if (!comments.nullOrEmpty) {
				return comments.map['''<p>«it.replaceAll('\r?\n', '<br>')»</p>'''].join('');	
			}
		}
		return null;
	}

}
