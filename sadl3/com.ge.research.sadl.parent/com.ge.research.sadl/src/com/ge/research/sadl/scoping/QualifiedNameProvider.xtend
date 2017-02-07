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
package com.ge.research.sadl.scoping

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.naming.QualifiedName

class QualifiedNameProvider implements IQualifiedNameProvider {

	@Inject extension DeclarationExtensions
	@Inject IQualifiedNameConverter nameConverter

	override getFullyQualifiedName(EObject obj) {
		if (obj instanceof SadlModel) {
			return QualifiedName.create(obj.baseUri)
		}
		if (obj instanceof SadlResource) {
			if (obj.isExternal) {
				val uri = URI.createURI(obj.conceptUri)
				if (uri.fragment === null) {
					return null
				} else {
					return QualifiedName.create(uri.trimFragment.toString, uri.fragment)
				}
			}
			val concreteName = obj.concreteName
			if (concreteName === null) {
				return null;
			}
			if (concreteName.indexOf(':') != -1) {
				return nameConverter.toQualifiedName(concreteName)
			}
			val model = EcoreUtil2.getContainerOfType(obj, SadlModel)
			return model.fullyQualifiedName.append(concreteName)
		}
		return null
	}
	
	override apply(EObject input) {
		return getFullyQualifiedName(input)
	}

}