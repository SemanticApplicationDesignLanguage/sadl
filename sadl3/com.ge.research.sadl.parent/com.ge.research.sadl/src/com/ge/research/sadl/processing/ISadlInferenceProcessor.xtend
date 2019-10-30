/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
package com.ge.research.sadl.processing

import com.ge.research.sadl.model.gp.Query
import com.ge.research.sadl.model.gp.TripleElement
import com.ge.research.sadl.reasoner.ConfigurationException
import com.ge.research.sadl.reasoner.InvalidNameException
import com.ge.research.sadl.reasoner.QueryCancelledException
import com.ge.research.sadl.reasoner.QueryParseException
import com.ge.research.sadl.reasoner.ReasonerNotFoundException
import com.ge.research.sadl.reasoner.SadlCommandResult
import com.ge.research.sadl.reasoner.TranslationException
import java.util.Map
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import com.hp.hpl.jena.ontology.OntModel

/**
 * Generic hook for 3rd party processors to participate in the processing of SADL resources
 *  
 */
interface ISadlInferenceProcessor {
	
	/**
	 * Called to do an inference operation on a SADL Resource
	 * 0th element is a list of the SADL commands processed
	 * 1st element of returned Object array is a list of inference results (if any)
	 * 2nd element is a list of error Strings (if any)
	 * Note: all results are returned on call to runInference because separate calls go to different instances of the processor
	 */	
	def Object[] runInference(Resource resource, String owlModelPath, String modelFolderPath, Map<String,String> prefMap) throws SadlInferenceException;
		
	/**
	 * Called to run a named query
	 * 0th and only element is SadlCmdResults
	 */
	def Object[] runNamedQuery(Resource resource, String queryName) throws SadlInferenceException;
	
	/**
	 * Called to run an ad hoc query
	 */
	def SadlCommandResult processAdhocQuery(Resource resource, Query query) throws ConfigurationException, TranslationException, InvalidNameException, ReasonerNotFoundException, QueryParseException, QueryCancelledException;

	/**
	 * Call to insert complete triples into model and query using incomplete triples
	 * returns an array of ResultSet, one for each query triple pattern
	 */
	def Object[] insertTriplesAndQuery(Resource resource, TripleElement[] triples) throws SadlInferenceException;

	/**
	 * {@code true} if the resource can be processed with the current processor.
	 */
	def boolean isSupported(Resource resource) {
		return resource?.URI.supported;
	}

	/**
	 * {@code true} if a resource given as its URI can be processed with the current processor. Otherwise, {@code false}.
	 */
	def boolean isSupported(URI uri) {
		return uri?.fileExtension.supported;
	}

	/**
	 * {@code true} if the inference processor can be used to process files with the given file extension argument. Otherwise, {@code false}.
	 * Clients should expect a file extension <b>without</b> the leading dot ({@code .}). So for example, in case of SADL, the argument will be
	 * {@code "sadl"} and <b>not</b> {@code .sadl}.
	 */
	def boolean isSupported( /*nullable*/ String fileExtension);
	
	/**
	 * Method to set the OntModel to be used during inference.
	 * This enables inference over an in-memory model.
	 * @param theJenaModel
	 */
	def void setTheJenaModel(OntModel theJenaModel)
	
		/**
	 * Method to set the preferences to be used during inference
	 * @param preferenceMap
	 */
	def void setPreferences(Map<String, String> preferenceMap)
	
}
