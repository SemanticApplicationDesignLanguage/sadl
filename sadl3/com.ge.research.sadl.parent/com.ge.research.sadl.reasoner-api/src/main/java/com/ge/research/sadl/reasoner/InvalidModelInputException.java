package com.ge.research.sadl.reasoner;

public class InvalidModelInputException extends Exception {
	private static final long serialVersionUID = 1L;

	public InvalidModelInputException (String message) { 
		super(message); 
	}
	
	public InvalidModelInputException (String message, Exception cause) {
		super(message, cause); 
	}
}
