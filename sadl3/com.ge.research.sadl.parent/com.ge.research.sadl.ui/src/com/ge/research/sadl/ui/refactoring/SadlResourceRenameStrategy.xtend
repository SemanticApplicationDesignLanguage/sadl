package com.ge.research.sadl.ui.refactoring

import com.ge.research.sadl.external.ExternalEmfResourceExtension
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.model.DeclarationExtensions.NewNameAdapter
import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import org.eclipse.xtext.resource.ILocationInFileProvider
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor
import org.eclipse.xtext.ui.refactoring.impl.DefaultRenameStrategy
import org.eclipse.xtext.ui.refactoring.impl.DefaultRenameStrategyProvider
import org.eclipse.xtext.ui.refactoring.impl.RefactoringException
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext
import org.eclipse.xtext.util.ITextRegion

import static com.ge.research.sadl.processing.SadlConstants.*

class SadlResourceRenameStrategy implements DefaultRenameStrategyProvider.IInitializable {

	@Inject
	extension DeclarationExtensions

	@Inject
	extension RefactoringExtensions

	@Inject
	extension ILocationInFileProvider

	@Inject
	extension ExternalEmfResourceExtension

	@Inject
	DefaultRenameStrategy delegate

	String originalName
	ITextRegion originalNameRegion
	URI declarationUri
	boolean shouldUseDelegate = true

	override initialize(EObject targetElement, IRenameElementContext renameElementContext) {
		if (targetElement instanceof SadlResource) {
			shouldUseDelegate = false
			val declaration = targetElement.declaration
			if (declaration === null || declaration.isBuiltIn || _externalEmfResourceExtension.isExternal(declaration)) {
				return false
			}
			declarationUri = EcoreUtil.getURI(declaration)
			originalName = declaration.concreteName
			originalNameRegion = declaration.significantTextRegion
			return !originalName.nullOrEmpty && !originalNameRegion.nullOrEmpty
		}
		return delegate.initialize(targetElement, renameElementContext)
	}

	override applyDeclarationChange(String newName, ResourceSet resourceSet) {
		if (shouldUseDelegate) {
			delegate.applyDeclarationChange(newName, resourceSet)
		} else {
			val toRename = resourceSet.getEObject(declarationUri, true)
			if (!(toRename instanceof SadlResource)) {
				throw new RefactoringException('''Expected a SADL resource for URI '«declarationUri»'. Was «toRename»''')
			}
			if (toRename === null || toRename.eIsProxy) {
				throw new RefactoringException('''«toRename» was a proxy.''')
			}
			new NewNameAdapter(newName).attachToEmfObject(toRename)
		}
	}

	override createDeclarationUpdates(String newName, ResourceSet resourceSet,
		IRefactoringUpdateAcceptor updateAcceptor) {

		if (shouldUseDelegate) {
			delegate.createDeclarationUpdates(newName, resourceSet, updateAcceptor)
		} else {
			// Nothing! The declaration of the SADL resource references itself.
			// We do not have to make the original replace edit here, the default reference updater will make sure
			// we update everything, including the "name" of the declaration.
		}
	}

	override getOriginalName() {
		return shouldUseDelegate ? delegate.originalName : originalName
	}

	override revertDeclarationChange(ResourceSet resourceSet) {
		if (shouldUseDelegate) {
			delegate.revertDeclarationChange(resourceSet)
		} else {
			val toRename = resourceSet.getEObject(declarationUri, true)
			if (!(toRename instanceof SadlResource)) {
				throw new RefactoringException('''Expected a SADL resource for URI '«declarationUri»'. Was «toRename»''')
			}
			if (toRename === null || toRename.eIsProxy) {
				throw new RefactoringException('''«toRename» was a proxy.''')
			}
			NewNameAdapter.removeFromEmfObject(toRename)
		}
	}

	override validateNewName(String newName) {
		if (shouldUseDelegate) {
			return delegate.validateNewName(newName)
		}
		val status = new RefactoringStatus
		// TODO: check name conflicts. Make sure we cannot rename built-ins 
		return status
	}

	private def boolean isBuiltIn(EObject it) {
		if (it === null || eIsProxy) {
			return false
		}
		val resource = eResource
		if (resource === null) {
			return false
		}
		return #[SADL_IMPLICIT_MODEL_FILENAME, SADL_BUILTIN_FUNCTIONS_FILENAME].exists [
			resource.URI.toString.endsWith(it)
		]
	}

}
