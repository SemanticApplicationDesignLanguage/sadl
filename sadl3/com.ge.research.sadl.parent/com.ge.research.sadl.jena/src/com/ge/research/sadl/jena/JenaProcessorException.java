package com.ge.research.sadl.jena;

/**
 * Class to handle Jena processor Exceptions
 * @author 200005201
 *
 */
public class JenaProcessorException extends Exception {
	
	public JenaProcessorException(String errMsg) {
		super(errMsg);
	}
	
	public JenaProcessorException(String errMsg, Throwable t) {
		super(errMsg, t);
	}

}
