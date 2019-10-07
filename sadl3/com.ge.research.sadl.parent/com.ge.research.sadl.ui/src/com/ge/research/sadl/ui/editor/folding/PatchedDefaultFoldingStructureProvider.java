/************************************************************************
 * Copyright © 2007-2019 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.editor.folding;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.folding.DefaultFoldingStructureProvider;
import org.eclipse.xtext.ui.editor.folding.FoldedPosition;
import org.eclipse.xtext.ui.editor.folding.IFoldingRegionProvider;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.util.Tuples;

import com.google.common.collect.Sets;
import com.google.inject.Inject;

/**
 * Patched {@link DefaultFoldingStructureProvider} instead of bumping up to
 * Xtext 2.17. The current code was shamelessly copied from
 * https://github.com/eclipse/xtext-eclipse/pull/944/commits/60be2628f630c61b19efb8f7d0c62b806c1e0db2
 * with the matching EPL v1.0 license.
 * 
 * @see https://github.com/eclipse/xtext-eclipse/pull/944
 * @see https://github.com/eclipse/xtext-eclipse/issues/863
 */
public class PatchedDefaultFoldingStructureProvider extends DefaultFoldingStructureProvider {

	@Inject
	private IFoldingRegionProvider foldingRegionProvider;

	private XtextEditor editor;
	private ProjectionViewer viewer;

	@Override
	public void install(XtextEditor editor, ProjectionViewer viewer) {
		super.install(editor, viewer);
		this.editor = editor;
		this.viewer = viewer;
	}

	protected void calculateProjectionAnnotationModel(boolean allowCollapse) {
		ProjectionAnnotationModel projectionAnnotationModel = this.viewer.getProjectionAnnotationModel();
		if (projectionAnnotationModel != null) {
			// make a defensive copy as we modify the folded positions in subsequent
			// operations
			Collection<FoldedPosition> foldedPositions = Sets
					.newLinkedHashSet(foldingRegionProvider.getFoldingRegions(editor.getDocument()));
			foldedPositions = filterFoldedPositions(foldedPositions);
			Annotation[] newRegions = mergeFoldingRegions(foldedPositions, projectionAnnotationModel);
			updateFoldingRegions(allowCollapse, projectionAnnotationModel, foldedPositions, newRegions);
		}
	}

	/**
	 * @since 2.17
	 */
	protected Collection<FoldedPosition> filterFoldedPositions(Collection<FoldedPosition> foldedPositions) {
		Map<Pair<Integer, Integer>, FoldedPosition> acceptedPositions = new HashMap<>();

		for (FoldedPosition foldedPosition : foldedPositions) {
			int startLineNumber = getLineNumber(foldedPosition.getOffset());
			int endLineNumber = getLineNumber(foldedPosition.getOffset() + foldedPosition.getLength());
			Pair<Integer, Integer> startAndEndLineNumbers = Tuples.create(startLineNumber, endLineNumber);

			if (!acceptedPositions.containsKey(startAndEndLineNumbers)) {
				acceptedPositions.put(startAndEndLineNumbers, foldedPosition);
			} else {
				// if the folded position has already been accepted, keep the one with the
				// smaller region
				FoldedPosition alreadyAcceptedPosition = acceptedPositions.get(startAndEndLineNumbers);
				if (foldedPosition.getLength() < alreadyAcceptedPosition.getLength()) {
					acceptedPositions.put(startAndEndLineNumbers, foldedPosition);
				}
			}
		}

		return acceptedPositions.values();
	}

	/**
	 * @since 2.17
	 */
	protected int getLineNumber(int offset) {
		IDocument document = viewer.getDocument();
		int lineNumber = -1;
		try {
			lineNumber = document.getLineOfOffset(offset);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		return lineNumber;
	}
}
