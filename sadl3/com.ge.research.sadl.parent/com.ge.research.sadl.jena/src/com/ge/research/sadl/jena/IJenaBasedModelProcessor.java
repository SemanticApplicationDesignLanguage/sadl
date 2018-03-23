package com.ge.research.sadl.jena;

import java.io.IOException;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.validation.CheckMode;

import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.hp.hpl.jena.rdf.model.RDFNode;

public interface IJenaBasedModelProcessor {

	void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context);

	void onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode, ProcessorContext context);

	/** Method to obtain the rules in a SADL model in IntermediateForm
	 * @return-- the Rules of the model
	 */
	public List<Rule> getRules();
	
	/**
	 * Method to obtain the equations in a SADL model in IntermediateForm
	 * @return
	 */
	public List<Equation> getEquations();
	
	/**
	 * Method to obtain the sadl commands in a SADL model
	 * @return -- the SadlCommands of the model
	 */
	public List<SadlCommand> getSadlCommands();
	
	/**
	 * Method to obtain results of model processing, e.g., for testing.
	 * @throws TranslationException 
	 * @throws InvalidTypeException 
	 * @throws InvalidNameException 
	 * @throws IOException 
	 * @throws ConfigurationException 
	 * @throws PrefixNotFoundException 
	 */
	List<Object> getIntermediateFormResults(boolean bRaw, boolean treatAsConclusion) throws InvalidNameException,
			InvalidTypeException, TranslationException, IOException, PrefixNotFoundException, ConfigurationException;

	/**
	 * Call this method to expand all of the ProxyNodes in a single GraphPatternElement or a List<GraphPatternElement>,
	 * convert any implicit conjunctions to explicit conjunctions, and return the result as a GraphPatternElement
	 * @param rawIntermediateForm
	 * @param treatAsConclusion
	 * @return
	 * @throws TranslationException 
	 * @throws InvalidTypeException 
	 * @throws InvalidNameException 
	 */
	GraphPatternElement expandNodesInIntermediateForm(Object rawIntermediateForm, boolean treatAsConclusion)
			throws InvalidNameException, InvalidTypeException, TranslationException;

	boolean compareTranslations(String result, String evalTo);
	
	/**
	 * Method to initialize preferences for model processor. Because this is called in onValidate, any extension classes should also call the super class method.
	 * @param context
	 */
	public void initializePreferences(ProcessorContext context);
	
	/**
	 * Method to obtain an IntermediateForm Translator appropriate for the given model processor
	 * @return
	 */
	public IntermediateFormTranslator getIfTranslator();

	/**
	 * Method to determine if an RDFNode is a typed list class
	 * @param node -- the typed list class
	 * @return type of the typed list
	 * @throws TranslationException
	 */
	NamedNode getTypedListType(RDFNode node) throws TranslationException;

	/**
	 * Method to determine the type of an RDFNode that is a typed list class
	 * @param node the node that might be a typed list class
	 * @return true if a typed list else false
	 */
	boolean isTypedListSubclass(RDFNode node);
	
}