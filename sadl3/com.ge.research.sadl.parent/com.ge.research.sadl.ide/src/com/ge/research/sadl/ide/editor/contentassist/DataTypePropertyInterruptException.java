package com.ge.research.sadl.ide.editor.contentassist;

import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry;

public class DataTypePropertyInterruptException extends Exception {

	private static final long serialVersionUID = 1L;
	
	private ContentAssistEntry datatypePropertySuggestion = null;
	
	public DataTypePropertyInterruptException(ContentAssistEntry suggestion) {
		setDatatypePropertySuggestion(suggestion);
	}

	public ContentAssistEntry getDatatypePropertySuggestion() {
		return datatypePropertySuggestion;
	}

	private void setDatatypePropertySuggestion(ContentAssistEntry datatypePropertySuggestion) {
		this.datatypePropertySuggestion = datatypePropertySuggestion;
	}
}
