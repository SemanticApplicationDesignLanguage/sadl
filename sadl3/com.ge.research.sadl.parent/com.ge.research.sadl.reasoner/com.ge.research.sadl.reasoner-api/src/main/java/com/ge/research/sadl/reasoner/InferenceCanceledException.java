package com.ge.research.sadl.reasoner;

import org.apache.jena.reasoner.rulesys.Builtin;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;

@SuppressWarnings("serial")
public class InferenceCanceledException extends BuiltinException {

	/**
	 * 
	 */
	public InferenceCanceledException(Builtin builtin, RuleContext context,
			String message) {
		super(builtin, context, message);
	}

}
