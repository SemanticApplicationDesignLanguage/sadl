package com.ge.research.sadl.jena.reasoner.builtin;

import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;

public interface ITypedBaseBuiltin {
	/**
	 * Method to get the type signature of the built-in function
	 * @return
	 */
	abstract public String getFunctionSignatureString();
	
	/**
	 * Method to answer the type of UnittedQuantity enabled built-in function this is
	 * @return
	 */
	abstract BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatus();

	/**
	 * Method to answer if the built-in function can accept a List as an input argument and
	 * treat list members as if they were inputs to the built-in function.
	 * @return
	 */
	abstract boolean canProcessListArgument();

}