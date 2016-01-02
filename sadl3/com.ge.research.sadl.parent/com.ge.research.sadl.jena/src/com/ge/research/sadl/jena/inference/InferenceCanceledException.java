package com.ge.research.sadl.jena.inference;

import com.hp.hpl.jena.reasoner.rulesys.Builtin;
import com.hp.hpl.jena.reasoner.rulesys.BuiltinException;
import com.hp.hpl.jena.reasoner.rulesys.RuleContext;

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
