package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.reasoner.TranslationException;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;

public interface ITypedBaseBuiltin {
	
	/**
	 * Method to get the type signature of the built-in function
	 * @return
	 */
	abstract public String getFunctionSignatureString();
	
	/**
	 * Method to answer what, if any, UnittedQuantity processing the built-in function 
	 * can provide
	 * 
	 * @return true if the built-in can accept a UnittedQuantity value as an input
	 */
	abstract UnittedQuantityBuiltinHandlingType getUnittedQuantityProcessingConstraint();

	
	/**
	 * Method to answer if the built-in function can accept UnittedQuantity value(s) as
	 * input argument(s)
	 * 
	 * @return true if the built-in can accept UnittedQuantity value(s) as input
	 */
	abstract boolean canProcessUnittedQuantityArguments();

	/**
	 * Method to answer if the built-in function can accept a List as an input argument and
	 * treat list members as if they were inputs to the built-in function.
	 * 
	 * @return true if the built-in can accept a List of values as an input
	 */
	abstract boolean canProcessListArgument();
	
	/**
	 * Method to determine if a built-in function can take a set of arguments that define a graph 
	 * pattern, that is the 2nd and then every 3rd arg is a property
	 * @return
	 */
	abstract boolean canProcessGraphPatternArguments();
	
	/**
	 * Method to answer the number of arguments (VariableNodes) expected
	 * to have values bound to them as results of built-in evaluation.
	 * @return
	 */
	abstract int numOutputArgs();
	
	/**
	 * Method to validate the input argument types and return the output types. 
	 * @param model -- the OntModel providing the context for validation
	 * @param be -- the BuiltinElement, passed in so that fields can be set within it
	 * @param args -- the arguments (needed to determine if a graph pattern)
	 * @param argTypes -- the type (identified by URI) of the argument(s)
	 * @return -- the types of the return values (identified by URI encapsulated in a Node)
	 * @throws UnittedQuantityHandlerException, TranslationException
	 */
	abstract Node[] validateArgumentTypes(OntModel model, BuiltinElement be, java.util.List<Node> args, java.util.List<Node> argTypes) throws TranslationException;


	/**
	 * Method to determine if a set of built-in arguments might be a graph pattern.
	 * @param model
	 * @param args
	 * @return
	 * @throws TranslationException 
	 */
	public static boolean isGraphPattern(OntModel model, List<Node> args) throws TranslationException {
		if (args == null || args.size() < 2) {
			return false;
		}
    	for (Node n : args) {
    		if (!(n instanceof NamedNode) && !(n instanceof VariableNode)) {
    			return false;
    		}
    	}
    	// verify--the 2nd and then every 3rd arg is a property
    	for (int i = 1; i < args.size(); i = i + 3) {
    		Node n = args.get(i);
    		if (n.getURI() != null) {
	    		Resource nrsrc = model.getResource(n.getURI());
	    		StmtIterator itr = model.listStatements(nrsrc, RDF.type, (RDFNode)null);
	    		boolean isProp = false;
	    		while (itr.hasNext()) {
	    			RDFNode on = itr.next().getObject();
	    			if (!on.isURIResource()) {
	    				return false;
	    			}
	    			if (on.equals(OWL.ObjectProperty) ||
	    					on.equals(OWL.DatatypeProperty) ||
	    					on.equals(OWL.AnnotationProperty) ||
	    					on.equals(RDF.Property)) {
	    				itr.close();
	    				isProp = true;
	    				break;
	    			}
	    		}
	    		if (!isProp) {
	    			itr.close();
	    			return false;
	    		}
    		}
    	}
    	return true;
	}
}