package com.ge.research.sadl.ui.quickfix;

import static java.util.Arrays.asList;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.texteditor.spelling.SpellingAnnotation;
import org.eclipse.ui.texteditor.spelling.SpellingProblem;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.XtextDocumentUtil;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.ui.editor.quickfix.XtextQuickAssistProcessor;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.validation.IssueUtils;
import com.google.common.collect.Lists;

public class SadlQuickAssistProcessor extends XtextQuickAssistProcessor {

	// Method was copied from `XtextQuickAssistProcessor` as is
	protected List<ICompletionProposal> createQuickfixes(IQuickAssistInvocationContext invocationContext,
			Set<Annotation> applicableAnnotations) {

		List<ICompletionProposal> result = Lists.newArrayList();
		ISourceViewer sourceViewer = invocationContext.getSourceViewer();
		IAnnotationModel annotationModel = sourceViewer.getAnnotationModel();
		IXtextDocument xtextDocument = XtextDocumentUtil.get(sourceViewer);

		// !Customization!
		// `org.eclipse.ui.texteditor.MarkerAnnotation` and XtextAnnotation references
		// the same `Issue`, we collect them here,
		// and calculate a distinct set of the issues
		Map<Issue, Position> visitedIssues = new HashMap<>();

		for (Annotation annotation : applicableAnnotations) {
			if (annotation instanceof SpellingAnnotation) {
				SpellingProblem spellingProblem = ((SpellingAnnotation) annotation).getSpellingProblem();
				ICompletionProposal[] proposals = spellingProblem.getProposals();
				if (proposals != null) {
					result.addAll(asList(proposals));
				}
			} else {
				final Issue issue = getIssueUtil().getIssueFromAnnotation(annotation);
				Position pos = annotationModel.getPosition(annotation);
				if (issue != null && pos != null) {
					// Instead of adding the quick fix proposal to the `result`, we collect the
					// issues, and convert them into a distinct set.
					visitedIssues.put(issue, pos);
				}
			}
		}

		// On the distinct issues, we do the same as in the parent,
		// `XtextQuickAssistProcessor`, class:
		// convert the issues to quick-fix completion proposals.
		for (Issue issue : IssueUtils.distinct(visitedIssues.keySet())) {
			@SuppressWarnings("deprecation")
			Iterable<IssueResolution> resolutions = getResolutions(issue, xtextDocument);
			if (resolutions.iterator().hasNext()) {
				for (IssueResolution resolution : resolutions) {
					result.add(create(visitedIssues.get(issue), resolution));
				}
			}
		}

		return result;
	}

}
