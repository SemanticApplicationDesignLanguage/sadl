package com.ge.research.sadl.externalmodels.editors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

public class UrlListSourceViewerConfig extends SourceViewerConfiguration {
	private static final Color DEFAULT_TAG_COLOR = new Color(Display.getCurrent(), new RGB(0,0,0));
	private UrlListScanner scanner;

	protected UrlListScanner getUrlScanner() {
		if (scanner == null)
		{
			scanner = new UrlListScanner();
			scanner.setDefaultReturnToken(new Token(new TextAttribute(DEFAULT_TAG_COLOR)));
		}
		return scanner;
	}
	
	public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer){
		PresentationReconciler reconciler = new PresentationReconciler();
		DefaultDamagerRepairer dr =
				new DefaultDamagerRepairer(getUrlScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
		return reconciler;
	}
	
}
