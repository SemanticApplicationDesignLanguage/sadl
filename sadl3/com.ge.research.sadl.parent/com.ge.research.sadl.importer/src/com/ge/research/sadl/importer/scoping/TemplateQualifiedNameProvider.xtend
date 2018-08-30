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
package com.ge.research.sadl.importer.scoping

import com.ge.research.sadl.scoping.SadlQualifiedNameProvider
import com.google.common.base.Preconditions
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.QualifiedName
import com.ge.research.sadl.importer.template.TemplateModel

class TemplateQualifiedNameProvider extends SadlQualifiedNameProvider {

	override apply(EObject input) {
		if (input instanceof TemplateModel) {
			Preconditions.checkNotNull(input.uri.baseUri, '''Base URI was null for SADL model in resource: «input.eResource.URI».''');
			return QualifiedName.create(input.uri.baseUri)
		}
		else {	
			return getFullyQualifiedName(input)			
		}
	}

}
