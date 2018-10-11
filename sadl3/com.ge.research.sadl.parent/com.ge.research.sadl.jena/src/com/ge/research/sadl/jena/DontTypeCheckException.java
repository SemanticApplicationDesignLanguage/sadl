package com.ge.research.sadl.jena;
/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */

/**
 * This exception is thrown when type checking should be aborted and pass
 * 
 * @author 200005201
 *
 */
public class DontTypeCheckException extends Exception {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public DontTypeCheckException() {
		super();
	}

	public DontTypeCheckException(String msg) {
		super(msg);
	}

}
