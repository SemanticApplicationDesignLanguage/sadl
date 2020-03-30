package com.ge.research.sadl.ide.editor.contentassist

import com.google.inject.Singleton
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalPriorities

@Singleton
class SadlIdeContentProposalPriorities extends IdeContentProposalPriorities {

	int keywordPriority = 600; // We want to see keywords before cross-references.

	override int getKeywordPriority(String keyword, ContentAssistEntry entry) {
		adjustPriority(entry, keywordPriority)
	}

}
