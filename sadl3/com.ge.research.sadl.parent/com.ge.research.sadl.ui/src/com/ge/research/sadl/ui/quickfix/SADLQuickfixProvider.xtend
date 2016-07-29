
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

import org.eclipse.xtext.ui.editor.quickfix.DefaultQuickfixProvider
import org.eclipse.xtext.ui.editor.quickfix.Fix
import com.ge.research.sadl.scoping.ErrorAddingLinkingService
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.validation.Issue

/**
 * Custom quickfixes.
 *
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes
 */
class SADLQuickfixProvider extends DefaultQuickfixProvider {

	@Fix(ErrorAddingLinkingService.ISSUE_CODE)
	def capitalizeName(Issue issue, IssueResolutionAcceptor acceptor) {
		for (alternative: issue.data.head?.split(",").toList.filter[!isEmpty] ?: emptyList) {
			acceptor.accept(issue, "Change to '"+alternative+"'", "Change to '"+alternative+"'", 'upcase.png') [
				context |
				val xtextDocument = context.xtextDocument
				xtextDocument.replace(issue.offset, issue.length, alternative)
			]
		}
	}
}
