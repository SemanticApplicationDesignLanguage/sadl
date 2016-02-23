package com.ge.research.sadl.scoping

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.naming.IQualifiedNameConverter

class QualifiedNameProvider implements IQualifiedNameProvider {

	@Inject extension DeclarationExtensions
	@Inject IQualifiedNameConverter nameConverter

	override getFullyQualifiedName(EObject obj) {
		if (obj instanceof SadlModel) {
			return QualifiedName.create(obj.baseUri)
		}
		if (obj instanceof SadlResource) {
			val model = EcoreUtil2.getContainerOfType(obj, SadlModel)
			val concreteName = obj.concreteName
			if (concreteName.indexOf(':') != -1) {
				return nameConverter.toQualifiedName(concreteName)
			}
			return model.fullyQualifiedName.append(concreteName)
		}
		return null
	}
	
	override apply(EObject input) {
		return getFullyQualifiedName(input)
	}

}