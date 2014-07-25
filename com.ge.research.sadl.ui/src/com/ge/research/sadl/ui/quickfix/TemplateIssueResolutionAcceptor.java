package com.ge.research.sadl.ui.quickfix;

import java.util.List;

import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.xtext.ui.editor.model.edit.IssueModificationContext.Factory;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;

import com.google.common.collect.Lists;
import com.google.inject.Inject;

/**
 * This IssueResolutionAcceptor adds support for TemplateProposals.
 * 
 * @see TemplateIssueResolutionAcceptor#accept(Issue, Template,
 *      TemplateContextSupplier)
 */
public class TemplateIssueResolutionAcceptor extends IssueResolutionAcceptor {

	protected static class TemplateIssueResolution extends IssueResolution {

		private TemplateContextSupplier contextFactory;

		private Template template;

		public TemplateIssueResolution(Template tpl,
				TemplateContextSupplier ctxfac) {
			super(tpl.getName(), tpl.getDescription(), null, null, null);
			this.template = tpl;
			this.contextFactory = ctxfac;
		}

		@Override
		public void apply() {
			super.apply();
		}

		public TemplateContextSupplier getContextFactory() {
			return contextFactory;
		}

		public Template getTemplate() {
			return template;
		}

	}

	private List<TemplateIssueResolution> resolutions = Lists.newArrayList();

	@Inject
	private TemplateStore templateStore;

	@Inject
	public TemplateIssueResolutionAcceptor(Factory factory) {
		super(factory);
	}

	public void accept(Issue issue, Template tpl, TemplateContextSupplier supp) {
		resolutions.add(new TemplateIssueResolution(tpl, supp));
	}

	/**
	 * Find a Template by name.
	 */
	public Template findTemplate(String name) {
		for (Template t : getTemplateStore().getTemplates())
			if (name.equals(t.getName()))
				return t;
		return null;
	}

	@Override
	public List<IssueResolution> getIssueResolutions() {
		List<IssueResolution> result = Lists.newArrayList(super
				.getIssueResolutions());
		result.addAll(resolutions);
		return result;
	}

	public TemplateStore getTemplateStore() {
		return templateStore;
	}
}
