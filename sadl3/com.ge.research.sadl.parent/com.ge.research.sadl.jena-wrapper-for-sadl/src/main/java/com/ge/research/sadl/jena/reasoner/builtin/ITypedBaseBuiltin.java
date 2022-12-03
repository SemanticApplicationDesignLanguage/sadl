package com.ge.research.sadl.jena.reasoner.builtin;

import org.apache.jena.ontology.OntModel;

import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;

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
	
	/**
	 * Method to validate the input argument types and return the output type. 
	 * @param model -- the OntModel providing the context for validation
	 * @param argTypes -- the type (identified by URI) of the argument(s)
	 * @return -- the type of the return value (identified by URI)
	 * @throws UnittedQuantityHandlerException, TranslationException
	 */
	abstract Node validateArgumentTypes(OntModel model, java.util.List<Node> argTypes) throws UnittedQuantityHandlerException, TranslationException;

}