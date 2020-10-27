package com.ge.research.sadl.ui.contentassist;

import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;

import com.google.inject.Inject;

public class SADLProposalProvider extends AbstractSADLProposalProvider {

	@Inject
	protected SADLUiToIdeContentProposalProvider delegate;

	@Override
	public void createProposals(ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		delegate.createProposals(context, acceptor);
	}

}
