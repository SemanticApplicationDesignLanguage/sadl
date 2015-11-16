package com.ge.research.sadl.externalmodels.editors;

import org.eclipse.ui.editors.text.TextEditor;

public class UrlListTextEditor extends TextEditor {
	public UrlListTextEditor() {
		super();
		setSourceViewerConfiguration(new UrlListSourceViewerConfig());
	}
}
