package com.ge.research.sadl.scoping

import com.ge.research.sadl.sADL.Declaration
import com.ge.research.sadl.sADL.Model
import com.ge.research.sadl.validation.SoftDeclarationReference
import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.naming.QualifiedName

class QualifiedNameProvider implements IQualifiedNameProvider {

	@Inject extension SoftDeclarationReference declarationReference

	override getFullyQualifiedName(EObject obj) {
		if (obj instanceof Model) {
			if (obj.alias !== null) {
				return QualifiedName.create(obj.alias)
			} else {
				return QualifiedName.create(obj.baseUri)
			}
		}
		if (obj instanceof Declaration) {
			val model = EcoreUtil2.getContainerOfType(obj, Model)
			return model.fullyQualifiedName.append(obj.concreteName)
		}
		return null
	}
	
	override apply(EObject input) {
		return getFullyQualifiedName(input)
	}

}