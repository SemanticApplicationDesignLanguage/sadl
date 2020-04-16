package com.ge.research.sadl.ui.refactoring

import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.IReferenceDescription
import org.eclipse.xtext.ui.refactoring.impl.DefaultReferenceUpdater

class SadlReferenceUpdater extends DefaultReferenceUpdater {

	override EObject resolveReference(EObject referringElement, IReferenceDescription referenceDescription) {
		var resolvedValue = referringElement.eGet(referenceDescription.getEReference());
		val isMany = referenceDescription.getEReference().isMany()
		if (isMany) {
			val list = resolvedValue as List<?>;
			resolvedValue = list.get(referenceDescription.getIndexInList());
		}
		if (resolvedValue instanceof EObject) {
			if (resolvedValue.eIsProxy && !isMany && referringElement.eResource !== null) {
				resolvedValue = referringElement.eResource.resourceSet.getEObject(referenceDescription.targetEObjectUri,
					true)
			}
		}
		return resolvedValue as EObject;
	}

}
