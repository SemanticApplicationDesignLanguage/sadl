package com.ge.research.sadl.jena.reasoner.builtin;

import com.ge.research.sadl.model.gp.FunctionSignature;
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin;

public abstract class TypedBaseBuiltin extends BaseBuiltin {
	
	abstract public String getFunctionSignatureString();
	
	public FunctionSignature getFunctionSignature() {
		return new FunctionSignature(getFunctionSignatureString(), this.getURI());
	}
}
