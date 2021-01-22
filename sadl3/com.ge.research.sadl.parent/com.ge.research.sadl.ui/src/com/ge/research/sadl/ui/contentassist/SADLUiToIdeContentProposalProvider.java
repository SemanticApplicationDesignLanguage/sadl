package com.ge.research.sadl.ui.contentassist;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jface.text.Region;
import org.eclipse.xtext.AbstractElement;
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext.Builder;
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry;
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor;
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider;
import org.eclipse.xtext.ui.editor.contentassist.AbstractContentProposalProvider.NullSafeCompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.ConfigurableCompletionProposal;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.UiToIdeContentProposalProvider;
import org.eclipse.xtext.util.TextRegion;
import org.eclipse.xtext.xbase.lib.Pair;

import com.ge.research.sadl.ide.editor.contentassist.DataTypePropertyInterruptException;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class SADLUiToIdeContentProposalProvider extends UiToIdeContentProposalProvider {

	@Inject
	protected IdeContentProposalProvider ideProvider;

	@Inject
	protected Provider<Builder> builderProvider;

	@Override
	public void createProposals(ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		ArrayList<Pair<ContentAssistEntry, Integer>> entries = new ArrayList<>();
		IIdeContentProposalAcceptor ideAcceptor = new IIdeContentProposalAcceptor() {

			@Override
			public boolean canAcceptMoreProposals() {
				return entries.size() < getMaxProposals();
			}

			@Override
			public void accept(ContentAssistEntry entry, int priority) {
				if (entry != null) {
					entries.add(Pair.of(entry, priority));
				}
			}

		};
		Collection<org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext> contexts = Collections
				.singletonList(getIdeContext(context, builderProvider.get()));
		try {
			ideProvider.createProposals(contexts, ideAcceptor);
		}
		catch (Exception e) {
			if (e instanceof DataTypePropertyInterruptException) {
				ContentAssistEntry entry = ((DataTypePropertyInterruptException)e).getDatatypePropertySuggestion();
				ICompletionProposalAcceptor uiAcceptor = new NullSafeCompletionProposalAcceptor(acceptor);
				ConfigurableCompletionProposal proposal = doCreateProposal(entry.getProposal(), getDisplayString(entry),
							getImage(entry), 0, context);
				applySelection(entry, proposal);
				uiAcceptor.accept(proposal);
				return;
			}
		}
		ICompletionProposalAcceptor uiAcceptor = new NullSafeCompletionProposalAcceptor(acceptor);
		for (Pair<ContentAssistEntry, Integer> pair : entries) {
			ContentAssistEntry entry = pair.getKey();
			ConfigurableCompletionProposal proposal = doCreateProposal(entry.getProposal(), getDisplayString(entry),
					getImage(entry), pair.getValue(), context);
			applySelection(entry, proposal);
			uiAcceptor.accept(proposal);
		}
	}

	private void applySelection(ContentAssistEntry from, ConfigurableCompletionProposal to) {
		List<TextRegion> editPositions = from.getEditPositions();
		if ((editPositions != null && !editPositions.isEmpty()) && editPositions.size() == 1) {
			to.setSelectionStart(editPositions.get(0).getOffset());
			to.setSelectionLength(editPositions.get(0).getLength());
		}
	}

	public static org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext getIdeContext(ContentAssistContext c,
			Builder builder) {

		Region replaceRegion = c.getReplaceRegion();
		builder.setPrefix(c.getPrefix());
		builder.setSelectedText(c.getSelectedText());
		builder.setRootModel(c.getRootModel());
		builder.setRootNode(c.getRootNode());
		builder.setCurrentModel(c.getCurrentModel());
		builder.setPreviousModel(c.getPreviousModel());
		builder.setCurrentNode(c.getCurrentNode());
		builder.setLastCompleteNode(c.getLastCompleteNode());
		builder.setOffset(c.getOffset());
		builder.setReplaceRegion(new TextRegion(replaceRegion.getOffset(), replaceRegion.getLength()));
		builder.setResource(c.getResource());
		for (AbstractElement grammarElement : c.getFirstSetGrammarElements()) {
			builder.accept(grammarElement);
		}
		return builder.toContext();
	}

}
