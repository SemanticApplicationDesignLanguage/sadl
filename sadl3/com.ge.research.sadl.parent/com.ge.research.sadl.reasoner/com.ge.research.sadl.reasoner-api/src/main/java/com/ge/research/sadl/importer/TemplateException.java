package com.ge.research.sadl.importer;

public class TemplateException extends Exception {

    private static final long serialVersionUID = 1L;

	public TemplateException(String msg) {
		super(msg);
	}
	
	public TemplateException(String msg, Throwable t) {
		super(msg, t);
	}
}
