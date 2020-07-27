/************************************************************************
 * Copyright 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.quickfix

import com.ge.research.sadl.markers.SadlMarkerConstants
import com.ge.research.sadl.markers.SadlMarkerRefType
import com.ge.research.sadl.resource.UserDataHelper
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlModelElement
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription
import com.ge.research.sadl.validation.SADLValidator
import com.google.inject.Inject
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.Path
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.ide.IDE
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.EObjectAtOffsetHelper
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.scoping.IGlobalScopeProvider
import org.eclipse.xtext.ui.editor.IURIEditorOpener
import org.eclipse.xtext.ui.editor.model.edit.IModification
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext
import org.eclipse.xtext.ui.editor.model.edit.IssueModificationContext
import org.eclipse.xtext.ui.editor.quickfix.DefaultQuickfixProvider
import org.eclipse.xtext.ui.editor.quickfix.Fix
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.validation.Issue

import static com.ge.research.sadl.markers.SadlMarkerConstants.*
import static com.ge.research.sadl.sADL.SADLPackage.Literals.*
import static java.lang.System.*

/**
 * Quick fix provider for SADL.
 */
class SADLQuickfixProvider extends DefaultQuickfixProvider {

	@Inject
	IssueModificationContext.Factory factory;
	
	@Inject
	IGlobalScopeProvider globalScopeProvider;
	
	@Inject
	UserDataHelper userDataHelper;
	
	@Inject
	IURIEditorOpener editorOpener;
	
	@Inject
	EObjectAtOffsetHelper offsetHelper;
	
	@Fix("REPLACE_ALIAS")
	def void replaceAlias(Issue issue, IssueResolutionAcceptor acceptor) {
		val data = issue.data
		if (data !== null && data.size === 2) {
			// 0th item is what to fix
			// 1st item is the fix
			val alias = data.get(0);
			val fix = data.get(1)
			if (!alias.nullOrEmpty && !fix.nullOrEmpty) {
				val label = "Replace '" + alias + "' alias with '" + fix + "' resource name"
				acceptor.accept(issue, label, label, null, [ context |
					val xtextDocument = context.xtextDocument
					xtextDocument.replace(issue.offset, issue.length, fix)
				])
			}
		}
	}

	@Fix(AmbiguousNameErrorEObjectDescription.AMBIGUOUS_NAME_ISSUE_CODE)
	def fixAmbigupusNames(Issue issue, IssueResolutionAcceptor acceptor) {
		val uri2AliasMap = factory.createModificationContext(issue)?.xtextDocument?.readOnly([
			// But the current resources into the map.
			val map = newHashMap();
			val currentModel = it.contents.head as SadlModel
			if (!currentModel.alias.nullOrEmpty) {
				map.put(currentModel.baseUri, currentModel.alias);
			}
			// Put all imported resources with the local `as` into the map. Later we will shadow those with the definition site aliases. 
			map.putAll(newHashMap(currentModel.imports.map[importedResource?.baseUri -> alias].filter [
				key !== null && value !== null
			]));
			globalScopeProvider.getScope(it, SADL_IMPORT__IMPORTED_RESOURCE, [EClass === SADL_MODEL]).allElements.
				forEach [
					val alias = userDataHelper.getAlias(it);
					if (alias.present) {
						map.put(it.name.toString, alias.get);
					}
				];
			return map;
		]);
		for (baseUriWithFqn : issue.data.head?.split(",").toList.filter[!isEmpty] ?: emptyList) {
			val raw = baseUriWithFqn.split(' ');
			val baseUri = raw.head;
			val name = raw.last;
			val alias = if (uri2AliasMap.containsKey(baseUri)) '''«uri2AliasMap.get(baseUri)»:«name»''' else name;
			acceptor.accept(issue, "Change to '" + alias + "'", "Change to '" + alias + "'", 'upcase.png') [ context |
				val xtextDocument = context.xtextDocument
				xtextDocument.replace(issue.offset, issue.length, alias)
			]
		}
	}

	@Fix(SADLValidator.UNRESOLVED_SADL_RESOURCE)
	def resolveSadlResource(Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Create class definition", "New class definition",
			'upcase.png', [ EObject element, IModificationContext context |
				if (!(element.eResource instanceof XtextResource)) {
					return;
				}
				val toFix = offsetHelper.resolveElementAt(element.eResource as XtextResource, issue.offset)
				if (toFix instanceof SadlResource) {
					// This is the model element that has the issue.
					// We have to insert the new class before this element.
					val modelElement = EcoreUtil2.getAllContainers(element).findLast[it instanceof SadlModelElement];
					val model = EcoreUtil2.getContainerOfType(element, SadlModel);
					val indexOf = model.elements.indexOf(modelElement);
					val insertBeforeModelElement = model.elements.get(indexOf);
					val node = NodeModelUtils.findActualNodeFor(insertBeforeModelElement);
					if (node !== null) {
						val name = context.xtextDocument.get(issue.offset, issue.length)
						if (!name.nullOrEmpty) {
							context.xtextDocument.replace(node.offset, 0, '''«name» is a class.«lineSeparator»''');
						}
					}
				}
			]);
	}

	@Fix(SadlMarkerConstants.SADL_REFS)
	def showSadlMarkerRefs(Issue it, IssueResolutionAcceptor acceptor) {
		if (data !== null) {
			data.forEach [ dataEntry |
				val segments = dataEntry.split('''\«SADL_REFS_SEPARATOR»''');
				val type = GET_TYPE_REF_BY_NAME.apply(segments.head);
				// The rest might contain colons (we have a URL), so we need to join them together.
				val refId = segments.drop(1).join(SADL_REFS_SEPARATOR);
				if (!refId.nullOrEmpty) {
					val label = type.getLabel(refId);
					acceptor.accept(it, label, "Open references", null /* image */ , type.getModification(refId));
				}
			];
		}
	}

	def getLabel(SadlMarkerRefType type, String refId) {
		return switch (type) {
			case File: '''Open «refId»'''
			case ModelElement: {
				val segments = refId.split('''\«SADL_REFS_SEPARATOR»''');
				val astNodeName = segments.head;
				'''Jump to «IF !astNodeName.nullOrEmpty»«astNodeName» in «ENDIF»«segments.get(1)»'''
			}
		}
	}
	
	def IModification getModification(SadlMarkerRefType type, String refId) {
		return switch (type) {
			case File: {
				[
					val path = new Path(refId);
					val file = ResourcesPlugin.workspace.root.getFile(path);
					if (file.accessible) {
						val page = PlatformUI.workbench.activeWorkbenchWindow.activePage;
						IDE.openEditor(page, file, true);
					} else {
						val shell = activeShell;
						val title = "File does not exist";
						val message = '''File does not exist at «path».''';
						MessageDialog.openError(shell, title, message);
					}
				];
			}
			case ModelElement: {
				[
					val segments = refId.split('''\«SADL_REFS_SEPARATOR»''');
					val uri = URI.createURI(segments.last);
					editorOpener.open(uri, true);
				]
			}
		}
	}
	
	def getActiveShell() {
		val display = if(Display.current === null) Display.^default else Display.current;
		return display.activeShell;
	}
	
	def SadlModel getSadlModel(EObject object) {
		if (object === null) {
			return null
		}
		val cont = object.eContainer
		if (cont instanceof SadlModel) {
			return cont as SadlModel
		}
		return getSadlModel(cont)
	}
}
