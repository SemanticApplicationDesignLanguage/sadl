package com.ge.research.sadl.swi_prolog.plinterface;

public class PlServiceFailedException extends Exception {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public PlServiceFailedException(long time) {
		super("PlService failed after " + time + " ms.");
	}
}
