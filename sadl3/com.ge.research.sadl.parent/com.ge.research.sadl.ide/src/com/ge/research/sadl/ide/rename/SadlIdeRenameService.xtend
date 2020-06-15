package com.ge.research.sadl.ide.rename

import com.ge.research.sadl.external.ExternalEmfResourceExtension
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlResource
import com.google.common.base.Throwables
import com.google.inject.Inject
import java.io.FileNotFoundException
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.PrepareRenameParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.WorkspaceEdit
import org.eclipse.xtext.ide.refactoring.IRenameStrategy2
import org.eclipse.xtext.ide.refactoring.RenameChange
import org.eclipse.xtext.ide.refactoring.RenameContext
import org.eclipse.xtext.ide.serializer.IChangeSerializer
import org.eclipse.xtext.ide.server.ILanguageServerAccess
import org.eclipse.xtext.ide.server.rename.ChangeConverter2
import org.eclipse.xtext.ide.server.rename.IRenameService2
import org.eclipse.xtext.ide.server.rename.RenameService2
import org.eclipse.xtext.resource.XtextResource

import static com.ge.research.sadl.processing.SadlConstants.*
import static org.eclipse.xtext.ide.refactoring.RefactoringIssueAcceptor.Severity.*

class SadlIdeRenameService extends RenameService2 {

	@Inject
	extension DeclarationExtensions

	@Inject
	extension ExternalEmfResourceExtension

	@Inject
	SadlResourceRenameHelper renameHelper

	override rename(IRenameService2.Options options) {
		val textDocument = options.renameParams.textDocument
		val uri = textDocument.uri
		val issueAcceptor = issueProvider.get
		val shouldPrepareRename = options.languageServerAccess.shouldPrepareRename
		return options.languageServerAccess.doRead(uri) [ context |

			if (shouldPrepareRename) {
				val identifier = new TextDocumentIdentifier(textDocument.uri)
				val position = options.renameParams.position
				val positionParams = new PrepareRenameParams(identifier, position)
				val resource = context.resource
				val document = context.document
				val cancelIndicator = options.cancelIndicator

				val prepareRenameResult = doPrepareRename(resource, document,
					positionParams as TextDocumentPositionParams, cancelIndicator)
				if (!mayPerformRename(prepareRenameResult, options.renameParams)) {
					return null
				}
			}

			val workspaceEdit = new WorkspaceEdit

			val resourceSet = options.languageServerAccess.newLiveScopeResourceSet(context.resource.URI)
			val xtextResource = resourceSet.getResource(context.resource.URI, true)
			if (xtextResource instanceof XtextResource) {
				val position = options.renameParams.position
				var EObject element
				try {
					element = xtextResource.getElementAtOffset(context.document, position)
				} catch (IndexOutOfBoundsException exc) {
					issueAcceptor.add(FATAL, '''Invalid document «position.toPositionFragment(uri)»''')
				}
				if (element === null || element.eIsProxy) {
					issueAcceptor.add(FATAL, '''No element found at «position.toPositionFragment(uri)»''')
				} else {
					if (element instanceof SadlResource) {
						renameHelper.rename(
							element,
							options.renameParams.newName,
							issueAcceptor,
							workspaceEdit,
							options.languageServerAccess,
							options.cancelIndicator
						)
					} else {
						val services = serviceProviderRegistry.getResourceServiceProvider(element.eResource.URI)
						val changeSerializer = services.get(IChangeSerializer)
						val change = new RenameChange(options.renameParams.newName, EcoreUtil.getURI(element))
						val renameContext = new RenameContext(#[change], resourceSet, changeSerializer, issueAcceptor)
						val renameStrategy = services.get(IRenameStrategy2)
						renameStrategy.applyRename(renameContext)
						val converterFactory = services.get(ChangeConverter2.Factory)
						val changeConverter = converterFactory.create(workspaceEdit, options.languageServerAccess)
						changeSerializer.applyModifications(changeConverter)
					}
				}
			} else {
				issueAcceptor.add(FATAL, 'Loaded resource is not an XtextResource', context.resource.URI)
			}
			issueAcceptor.checkSeverity
			return workspaceEdit
		].exceptionally [ exception |
			val rootCause = Throwables.getRootCause(exception)
			if (rootCause instanceof FileNotFoundException) {
				if (shouldPrepareRename) {
					return null
				}
			}
			throw exception
		].get
	}

	override protected getElementName(EObject element) {
		if (element instanceof SadlResource) {
			val declaration = element.declaration
			if (declaration === null || declaration.isBuiltIn ||
				_externalEmfResourceExtension.isExternal(declaration)) {
				return null;
			}
			return element.concreteName;
		}
		return super.getElementName(element);
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

	private def toPositionFragment(Position it, String uri) {
		return '''position line: «line» column: «character» in resource: «uri»'''
	}

	private def shouldPrepareRename(ILanguageServerAccess access) {
		val provider = access?.initializeResult?.capabilities?.renameProvider
		return if(provider !== null && provider.isRight) Boolean.TRUE == provider.getRight.prepareProvider else false
	}

}
