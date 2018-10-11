package com.ge.research.sadl.owl2sadl;

public class OwlImportException extends Exception {

	private static final long serialVersionUID = 1L;

	public OwlImportException(String message) {
		super (message);
	}

	public OwlImportException(String message, Exception cause) {
		super(message, cause);
	}
}
