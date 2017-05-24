package com.ge.research.sadl.jena;
/*
 * Author: 212325165
 * Exception when Property does not have a Range and type-checking based on that should be aborted. 
 */
public class PropertyWithoutRangeException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public PropertyWithoutRangeException(){
		super("Property does not have a range");
	}
}
