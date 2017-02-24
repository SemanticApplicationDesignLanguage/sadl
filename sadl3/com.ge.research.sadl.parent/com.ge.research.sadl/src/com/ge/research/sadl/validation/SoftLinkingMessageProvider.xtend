/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.validation

import com.ge.research.sadl.sADL.SadlImport
import com.ge.research.sadl.sADL.SadlModel
import org.eclipse.xtext.linking.impl.LinkingDiagnosticMessageProvider

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

class SoftLinkingMessageProvider extends LinkingDiagnosticMessageProvider {

	@Override
	override getUnresolvedProxyMessage(ILinkingDiagnosticContext it) {
		if (reference.EReferenceType === SADL_RESOURCE) {
			// treated as declaration. 
			return null
		} else if (reference === SADL_IMPORT__IMPORTED_RESOURCE && context instanceof SadlImport) {
			// Importing self causes linker error but that is handles already in SADL validator.
			val container = (context as SadlImport).eContainer;
			if (container instanceof SadlModel) {
				val baseUri = container.baseUri;
				println(baseUri)
				println(linkText)
				if (baseUri == linkText) {
					return null;
				}
			}
		}
		super.getUnresolvedProxyMessage(it);
	}

}