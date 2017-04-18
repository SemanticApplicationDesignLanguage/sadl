package com.ge.research.sadl.processing;

public class SadlInferenceException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public SadlInferenceException(String msg) {
		super(msg);
	}

	/**
	 * Registers a new processor into the underlying cache. If already
	 * registered processor instance will be discarded and the argument will be
	 * registered instead.
	 * 
	 * @param processor
	 *            the new processor to register. Cannot be {@code null}.
	 */
	public SadlInferenceException(String msg, Throwable t) {
		super(msg, t);
	}
}
