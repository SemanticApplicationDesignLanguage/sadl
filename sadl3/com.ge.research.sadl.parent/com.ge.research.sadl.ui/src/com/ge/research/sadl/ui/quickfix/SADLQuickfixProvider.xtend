
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

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription
import com.ge.research.sadl.validation.SADLValidator
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EOF
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext
import org.eclipse.xtext.ui.editor.quickfix.DefaultQuickfixProvider
import org.eclipse.xtext.ui.editor.quickfix.Fix
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.ui.editor.utils.EditorUtils
import org.eclipse.xtext.validation.Issue

/**
 * Custom quickfixes.
 *
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes
 */
class SADLQuickfixProvider extends DefaultQuickfixProvider {

	@Fix(AmbiguousNameErrorEObjectDescription.AMBIGUOUS_NAME_ISSUE_CODE)
	def capitalizeName(Issue issue, IssueResolutionAcceptor acceptor) {
		for (alternative: issue.data.head?.split(",").toList.filter[!isEmpty] ?: emptyList) {
			acceptor.accept(issue, "Change to '"+alternative+"'", "Change to '"+alternative+"'", 'upcase.png') [
				context |
				val xtextDocument = context.xtextDocument
				xtextDocument.replace(issue.offset, issue.length, alternative)
			]
		}
	}
	
	@Fix(SADLValidator.UNRESOLVED_SADL_RESOURCE)
	def resolveSadlResource(Issue issue, IssueResolutionAcceptor acceptor) {

		acceptor.accept(issue, "Create class definition", "New class definition", 'upcase.png',
			[ EObject element, IModificationContext context |
			
			val xtextDocument = context.xtextDocument
			val varNm = xtextDocument.get(issue.offset, issue.length)
			val cont = element.eContainer
			if (cont == null) {
				val reqNode = NodeModelUtils.getNode(element)
				var afterReqLocation = reqNode.endOffset
				var char ch = xtextDocument.getChar(afterReqLocation)
				while (ch == '.' || ch == '\n' || ch == '\r') {
					afterReqLocation++
					ch = xtextDocument.getChar(afterReqLocation)
				} 
				val replacement = " \n  where " + varNm + " is "
				xtextDocument.replace(afterReqLocation, 0, replacement)
				EditorUtils.activeXtextEditor.selectAndReveal(afterReqLocation + replacement.length, 1)
			}
			else {
				val ctxNode = NodeModelUtils.getNode(cont)
				var afterContext = ctxNode.endOffset + 1
				var char ch = xtextDocument.getChar(afterContext)
				while (ch != EOF && (ch == '.' || ch == '\n' || ch == '\r')) {
					afterContext++
					ch = xtextDocument.getChar(afterContext)
				} 
				val replacement = "\nShape is "
				xtextDocument.replace(afterContext, 0, replacement)
				EditorUtils.activeXtextEditor.selectAndReveal(afterContext + replacement.length, 1)		// this doesn't work--how to locate cursor after replacement?
			}
		]);	
		
	}

	def SadlModel getSadlModel(EObject object) {
		if (object == null) {
			return null
		}
		val cont = object.eContainer
		if (cont instanceof SadlModel) {
			return cont as SadlModel
		}
		return getSadlModel(cont)
	}
}
