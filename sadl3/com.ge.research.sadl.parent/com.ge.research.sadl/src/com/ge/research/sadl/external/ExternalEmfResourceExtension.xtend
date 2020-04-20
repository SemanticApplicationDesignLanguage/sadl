package com.ge.research.sadl.external

import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject

class ExternalEmfResourceExtension {

	@Inject
	ExternalEmfResourcePredicate externalResourcePredicate

	def isExternal(EObject it) {
		if (it === null || eIsProxy) {
			return false
		}
		if (eResource instanceof ExternalEmfResource) {
			return true
		}
		if (eResource.URI !== null && externalResourcePredicate.apply(eResource.URI)) {
			return true
		}
		return false
	}

}
