package com.ge.research.sadl.ui.quickfix;

import java.util.Map;

import org.eclipse.emf.common.util.WrappedException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext.Builder;
import org.eclipse.xtext.ui.editor.contentassist.ITemplateProposalProvider;
import org.eclipse.xtext.ui.editor.model.XtextDocumentUtil;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.ui.editor.quickfix.XtextQuickAssistProcessor;
import org.eclipse.xtext.ui.editor.templates.DefaultTemplateProposalProvider;
import org.eclipse.xtext.ui.editor.templates.XtextTemplateContext;
import org.eclipse.xtext.ui.editor.templates.XtextTemplateContextType;
import org.eclipse.xtext.ui.editor.templates.XtextTemplateProposal;

import com.ge.research.sadl.ui.quickfix.TemplateIssueResolutionAcceptor.TemplateIssueResolution;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class TemplateQuickAssistProcessor extends XtextQuickAssistProcessor {

	/**
	 * A TeamplateContext that also applies a prefix before the template and a
	 * postfix after the template.
	 */
	protected static class QuickfixTemplateContext extends XtextTemplateContext {
		private String postfix;

		private String prefix;

		public QuickfixTemplateContext(TemplateContextType type,
				IDocument document, Position position,
				ContentAssistContext contentAssistContext,
				IScopeProvider scopeProvider, String prefix, String postfix) {
			super(type, document, position, contentAssistContext, scopeProvider);
			this.prefix = prefix;
			this.postfix = postfix;
		}

		@Override
		public TemplateBuffer evaluate(Template template)
				throws BadLocationException, TemplateException {
			TemplateBuffer result = super.evaluate(template);
			result.setContent(prefix + result.getString() + postfix,
					result.getVariables());
			return result;
		}

	}

	private IQuickAssistInvocationContext qaCtx;

	@Inject
	private IScopeProvider scopeProvider;

	@Inject
	private Provider<XtextTemplateContextType> templateCtxTypeProvider;

	// needed to get a template's Icon
	@Inject
	private ITemplateProposalProvider templatePP;

	@Inject
	private Provider<Builder> builderProvider;


	@Override
	public ICompletionProposal[] computeQuickAssistProposals(
			IQuickAssistInvocationContext invocationContext) {
		this.qaCtx = invocationContext;
		return super.computeQuickAssistProposals(invocationContext);
	}

	@Override
	protected ICompletionProposal create(Position posisition,
			IssueResolution resolution) {
		if (resolution instanceof TemplateIssueResolution)
			return createTemplateProposal((TemplateIssueResolution) resolution);
		else
			return super.create(posisition, resolution);
	}

	protected ICompletionProposal createTemplateProposal(
			TemplateIssueResolution res) {
		try {
			TemplateContextSupplier fac = res.getContextFactory();
			IDocument doc = XtextDocumentUtil.get(qaCtx.getSourceViewer());
			Position pos = fac.getPosition(doc, qaCtx);
			String prefix = fac.getPrefix(doc, qaCtx);
			String postfix = fac.getPostfix(doc, qaCtx);
			Map<String, String> vars = fac.getVariables(doc, qaCtx);
			Region r = new Region(pos.getOffset(), pos.getLength());
			ContentAssistContext cactx = builderProvider.get()
					.setOffset(pos.getOffset()).setPrefix(prefix)
					.setViewer(qaCtx.getSourceViewer()).toContext();

			// create a TemplateProposalContext
			Image i = ((DefaultTemplateProposalProvider) templatePP)
					.getImage(res.getTemplate());
			XtextTemplateContextType type = templateCtxTypeProvider.get();
			XtextTemplateContext tplctx = new QuickfixTemplateContext(type,
					doc, pos, cactx, scopeProvider, prefix, postfix);
			for (Map.Entry<String, String> e : vars.entrySet())
				tplctx.setVariable(e.getKey(), e.getValue());

			// create a TemplateProposal
			XtextTemplateProposal proposal = new XtextTemplateProposal(
					res.getTemplate(), tplctx, r, i);
			return proposal;
		} catch (RuntimeException e1) {
			throw e1;
		} catch (Exception e2) {
			throw new WrappedException(e2);
		}
	}

}
