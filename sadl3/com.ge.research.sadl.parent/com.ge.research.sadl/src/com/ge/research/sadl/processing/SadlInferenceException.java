package com.ge.research.sadl.processing;

public class SadlInferenceException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public SadlInferenceException(String msg) {
		super(msg);
	}
	
	public SadlInferenceException(String msg, Throwable t) {
		super(msg, t);
	}
}
