package com.ge.research.sadl.ui.refactoring

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.IReferenceDescription
import org.eclipse.xtext.ui.refactoring.impl.DefaultReferenceUpdater

class SadlReferenceUpdater extends DefaultReferenceUpdater {

	override EObject resolveReference(EObject referringElement, IReferenceDescription referenceDescription) {
		var resolvedValue = super.resolveReference(referringElement, referenceDescription);
		val isMany = referenceDescription.getEReference().isMany()
		if (resolvedValue instanceof EObject) {
			if (resolvedValue.eIsProxy && !isMany && referringElement.eResource !== null) {
				val resourceSet = referringElement.eResource.resourceSet
				resolvedValue = resourceSet.getEObject(referenceDescription.targetEObjectUri, true)
			}
		}
		return resolvedValue as EObject;
	}

}
