package com.ge.research.sadl.ui.refactoring

import com.ge.research.sadl.model.DeclarationExtensions.NewNameAdapter
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.scoping.SadlQualifiedNameConverter
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.xtext.resource.IReferenceDescription
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor
import org.eclipse.xtext.ui.refactoring.impl.DefaultReferenceUpdater
import org.eclipse.xtext.ui.refactoring.impl.RefactoringCrossReferenceSerializer

class SadlReferenceUpdater extends DefaultReferenceUpdater {

	@Inject
	SadlQualifiedNameConverter qualifiedNameConverter;

	@Inject
	RefactoringCrossReferenceSerializer crossReferenceSerializer;

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

	override createReferenceUpdate(EObject referringElement, URI referringResourceURI, EReference reference,
		int indexInList, EObject newTargetElement, IRefactoringUpdateAcceptor updateAcceptor) {

		if (!transientValueService.isValueInListTransient(referringElement, indexInList, reference)) {
			val referenceTextRegion = locationInFileProvider.getFullTextRegion(referringElement, reference,
				indexInList);
			if (referenceTextRegion !== null) {
				val crossReference = getCrossReference(referringElement, referenceTextRegion.getOffset());
				if (crossReference !== null) {
					val refTextComparator = getRefTextEvaluator(referringElement, referringResourceURI, reference,
						indexInList, newTargetElement);
					var String newReferenceText = null;
					if (newTargetElement instanceof SadlResource) {
						val adapter = NewNameAdapter.findInEmfObject(newTargetElement)
						if (adapter !== null && !adapter.name.nullOrEmpty) {
							val qname = qualifiedNameConverter.toQualifiedName(adapter.name);
							newReferenceText = qualifiedNameConverter.toString(qname);
						}
					}
					if (newReferenceText === null) {
						newReferenceText = crossReferenceSerializer.getCrossRefText(referringElement, crossReference,
							newTargetElement, refTextComparator, referenceTextRegion,
							updateAcceptor.getRefactoringStatus());
					}
					if (newReferenceText === null) {
						newReferenceText = resolveNameConflict(referringElement, reference, newTargetElement,
							updateAcceptor);
					}
					if (newReferenceText === null) {
						updateAcceptor.getRefactoringStatus().add(RefactoringStatus.ERROR,
							"Refactoring introduces a name conflict.", referringElement, referenceTextRegion);
					}
					createTextChange(referenceTextRegion, newReferenceText, referringElement, newTargetElement,
						reference, referringResourceURI, updateAcceptor);
				}
			}
		}
	}

}
