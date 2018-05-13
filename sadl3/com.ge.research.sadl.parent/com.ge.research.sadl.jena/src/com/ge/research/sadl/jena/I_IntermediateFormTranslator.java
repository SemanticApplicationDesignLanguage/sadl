package com.ge.research.sadl.jena;

import java.util.List;

import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;

public interface I_IntermediateFormTranslator {

	/**
	 * Method to set the target of this translation, which can be a Test, a Query, or a Rule instance
	 * @param _target -- the target
	 */
	void setTarget(Object _target);

	/**
	 * Method to get a list of errors (IFTranslationError) generated, if any, during translation 
	 * @return -- list of errors
	 */
	List<IFTranslationError> getErrors();

	/**
	 * Method to obtain a model processor
	 * @return
	 */
	abstract JenaBasedSadlModelProcessor getModelProcessor();
	
	/**
	 * Method to transform an Object from the raw form to the "cooked" form, generally meaning that the graph patterns are flattened
	 * and connected with variables.
	 * @param toCookObj -- the Object to be operated upon
	 * @return -- the transformed Object
	 * @throws TranslationException
	 * @throws InvalidTypeException 
	 * @throws InvalidNameException 
	 */
	abstract Object cook(Object toCookObj) throws TranslationException, InvalidNameException, InvalidTypeException;

	/**
	 * MMethod to transform an Object from the raw form to the "cooked" form, generally meaning that the graph patterns are flattened
	 * and connected with variables.
	 * @param toCookObj -- the Object to be operated upon
	 * @return -- the transformed Object
	 * @throws TranslationException
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 */
	abstract Object cook(Object toCookObj, boolean treatAsConclusion)
			throws TranslationException, InvalidNameException, InvalidTypeException;

	/**
	 * Method to take a list of GraphPatternElements and convert them into a single conjunctive Junction, returned as the 
	 * single element of the returned list
	 * @param patterns -- input GraphPatternElement list
	 * @return -- a Junction as 1st and only element of returned List
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws TranslationException
	 */
	List<GraphPatternElement> listToAnd(List<GraphPatternElement> patterns) throws InvalidNameException, InvalidTypeException, TranslationException;

	/**
	 * Method to set the system-generated variable starting sequence number, which would have been set in the model processor
	 * @param vn -- starting system-generated variable sequence number
	 */
	void setStartingVariableNumber(int vn);

	/** 
	 * Method to return the current system-generated variable sequence number
	 * @return -- current system-generated variable sequence number
	 */
	int getVariableNumber();

	/**
	 * Method to convert linked lists into Junctions with ProxyNodes
	 * @param element -- junction to be flattened
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws TranslationException
	 */
	void flattenJunction(Junction element) throws InvalidNameException, InvalidTypeException, TranslationException;

	/**
	 * Method to set an encapsulating target, e.g., when a Query is inside a Test, the Test is the encapsulating target
	 * @param _encapsulatingTarget
	 */
	void setEncapsulatingTarget(Object _encapsulatingTarget);

	Object getTarget();
	
	/**
	 * Method to determine if a GraphPatternElement can only be in the conclusions of the target
	 * @param gpe
	 * @return
	 */
	boolean graphPatternElementMustBeInConclusions(GraphPatternElement gpe);
}