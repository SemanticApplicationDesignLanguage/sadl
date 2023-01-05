package com.ge.research.sadl.reasoner;

public class ArgumentTypeValidationException extends TranslationException {

	// Needed to avoid a warning.
    private static final long serialVersionUID = 1L;

    public ArgumentTypeValidationException(String msg) {
        super(msg);
    }
    
    public ArgumentTypeValidationException(String msg, Exception e) {
    	super(msg, e);
    }

}
