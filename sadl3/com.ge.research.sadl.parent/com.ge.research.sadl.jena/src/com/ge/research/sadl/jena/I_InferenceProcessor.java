package com.ge.research.sadl.jena;

import org.eclipse.emf.ecore.resource.Resource;

import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Literal;

public interface I_InferenceProcessor {

	boolean validateModel() throws ConfigurationException, ReasonerNotFoundException;

	Resource getCurrentResource();
	ProcessorContext getCurrentContext();

	int testModel(Test cmd) throws TranslationException, InvalidNameException, ConfigurationException, ReasonerNotFoundException, JenaProcessorException, QueryParseException, QueryCancelledException, TripleNotFoundException, InvalidTypeException;

	ResultSet processQuery(Query sadlQuery) throws ConfigurationException, ReasonerNotFoundException, TranslationException, InvalidNameException, JenaProcessorException, QueryParseException, QueryCancelledException;

	ResultSet matchTriplePattern(String subject, String prediate, String objval) throws TripleNotFoundException, ConfigurationException, ReasonerNotFoundException;
	
	TestResult testTriple(TripleElement triple)
			throws TripleNotFoundException, InvalidTypeException, ConfigurationException, ReasonerNotFoundException;

	boolean compareObjects(Object lval, Object objVal, OntResource rngrsrc);
	
	String sadlLiteralToString(com.ge.research.sadl.model.gp.Literal sadlLiteral);
	
	Literal valueToJenaLiteral(Object val, String rangeUri); 
}