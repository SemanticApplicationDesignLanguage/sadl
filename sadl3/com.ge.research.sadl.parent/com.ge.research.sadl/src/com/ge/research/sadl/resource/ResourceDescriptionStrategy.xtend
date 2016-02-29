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