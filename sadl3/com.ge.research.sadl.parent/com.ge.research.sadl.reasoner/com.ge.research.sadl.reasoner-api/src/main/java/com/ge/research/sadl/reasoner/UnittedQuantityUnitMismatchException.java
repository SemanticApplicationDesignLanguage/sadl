package com.ge.research.sadl.reasoner;

public class UnittedQuantityUnitMismatchException extends UnittedQuantityHandlerException {
    // Needed to avoid a warning.
    private static final long serialVersionUID = 1L;

    public UnittedQuantityUnitMismatchException(String msg) {
        super(msg);
    }
    
    public UnittedQuantityUnitMismatchException(String msg, Exception e) {
    	super(msg, e);
    }
}
