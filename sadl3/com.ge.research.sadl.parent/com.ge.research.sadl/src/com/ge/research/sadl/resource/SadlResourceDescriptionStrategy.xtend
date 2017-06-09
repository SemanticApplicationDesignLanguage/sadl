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

import com.google.inject.Inject
import com.google.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.EObjectDescription
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionStrategy
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.resource.XtextResource

@Singleton
class SadlResourceDescriptionStrategy extends DefaultResourceDescriptionStrategy {
	
	@Inject
	extension UserDataHelper;
	
	override createEObjectDescriptions(EObject eObject, IAcceptor<IEObjectDescription> acceptor) {
		val qualifiedName = qualifiedNameProvider.getFullyQualifiedName(eObject);
		if (qualifiedName !== null) {
			acceptor.accept(EObjectDescription.create(qualifiedName, eObject, eObject.createUserData));
			if (qualifiedName.segmentCount > 1 && eObject.eResource instanceof XtextResource) {
				// Export the simple name of the SADL resources, so that the resource description manager
				// can mark a resource as `affected` when comparing the imported names of the candidates
				// with the exported name of the deltas. Ignore external resources they do not participate
				// in the incremental builder anyway.
				acceptor.accept(EObjectDescription.create(qualifiedName.lastSegment, eObject, eObject.createUserData));	
			}
		}
		return true;
	}
	
}