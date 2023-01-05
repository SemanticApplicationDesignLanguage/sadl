package com.ge.research.sadl.jena.reasoner.builtin;

import org.apache.jena.ontology.OntModel;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;

public interface ITypedBaseBuiltin {
	/**
	 * Method to get the type signature of the built-in function
	 * @return
	 */
	abstract public String getFunctionSignatureString();
	
	/**
	 * Method to answer if the built-in functoin can accept a UnittedQuantity value as an
	 * input argument
	 * 
	 * @return true if the built-in can accept a UnittedQuantity value as an input
	 */
	abstract boolean canProcessUnittedQuantity();

	/**
	 * Method to answer if the built-in function can accept a List as an input argument and
	 * treat list members as if they were inputs to the built-in function.
	 * 
	 * @return true if the built-in can accept a List of values as an input
	 */
	abstract boolean canProcessListArgument();
	
	/**
	 * Method to validate the input argument types and return the output type. 
	 * @param model -- the OntModel providing the context for validation
	 * @param be -- the BuiltinElement, passed in so that fields can be set within it
	 * @param argTypes -- the type (identified by URI) of the argument(s)
	 * @return -- the type of the return value (identified by URI)
	 * @throws UnittedQuantityHandlerException, TranslationException
	 */
	abstract Node validateArgumentTypes(OntModel model, BuiltinElement be, java.util.List<Node> argTypes) throws TranslationException;

}