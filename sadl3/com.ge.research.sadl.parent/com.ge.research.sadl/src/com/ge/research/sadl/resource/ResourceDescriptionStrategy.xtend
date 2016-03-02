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
package com.ge.research.sadl.resource

import com.ge.research.sadl.sADL.SadlModel
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.EObjectDescription
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionStrategy
import org.eclipse.xtext.util.IAcceptor

class ResourceDescriptionStrategy extends DefaultResourceDescriptionStrategy {
	
	public static val USER_DATA_ALIAS = "alias"
	
	override createEObjectDescriptions(EObject eObject, IAcceptor<IEObjectDescription> acceptor) {
		val qualifiedName = qualifiedNameProvider.getFullyQualifiedName(eObject);
		if (qualifiedName != null) {
			if (eObject instanceof SadlModel) {
				acceptor.accept(EObjectDescription.create(qualifiedName, eObject, #{USER_DATA_ALIAS -> eObject.alias}));
			} else {
				acceptor.accept(EObjectDescription.create(qualifiedName, eObject));
			}
		}
		return true;
	}
	
}