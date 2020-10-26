package com.ge.research.sadl.importer;

public class TemplateException extends Exception {
	public TemplateException(String msg) {
		super(msg);
	}
	
	public TemplateException(String msg, Throwable t) {
		super(msg, t);
	}
}
