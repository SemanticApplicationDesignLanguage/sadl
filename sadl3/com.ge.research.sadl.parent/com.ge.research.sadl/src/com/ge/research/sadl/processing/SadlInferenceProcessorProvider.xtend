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
import com.ge.research.sadl.reasoner.TranslationException
import com.google.common.base.Optional
import com.google.inject.Inject
import com.google.inject.Injector
import java.util.Map
import org.eclipse.emf.ecore.resource.Resource
import org.apache.jena.ontology.OntModel
import java.util.List
import com.ge.research.sadl.model.gp.Rule

/**
 * Provides {@code SADL} inferences.
 */
class SadlInferenceProcessorProvider extends AbstractSadlProcessorProvider<ISadlInferenceProcessor, Resource> {

	static val EXTENSION_ID = 'com.ge.research.sadl.sadl_inference_processor';

	static val ISadlInferenceProcessor NOOP_INFERENCER = new ISadlInferenceProcessor() {

		override runInference(Resource resource, String owlModelPath, String modelFolderPath, Map<String, String> prefMap) throws SadlInferenceException {
			return newArrayOfSize(0);
		}

		override runNamedQuery(Resource resource, String queryName) throws SadlInferenceException {
			return newArrayOfSize(0);
		}

		override processAdhocQuery(Resource resource, Query query) throws ConfigurationException, TranslationException, InvalidNameException, ReasonerNotFoundException, QueryParseException, QueryCancelledException {
			return null;
		}

		override insertTriplesAndQuery(Resource resource, TripleElement[] triples) throws SadlInferenceException {
			return newArrayOfSize(0);
		}

		override insertTriplesAndQuery(Resource resource, List<TripleElement[]> triples) throws SadlInferenceException {
			return newArrayOfSize(0);
		}

		override isSupported(String fileExtension) {
			return false;
		}
		
		override setTheJenaModel(OntModel theJenaModel) {
			throw new UnsupportedOperationException("TODO: auto-generated method stub")
		}
		
		override setPreferences(Map<String, String> preferenceMap) {
			throw new UnsupportedOperationException("TODO: auto-generated method stub")
		}
		
		override insertRulesAndQuery(Resource resource, List<Rule> rules) throws SadlInferenceException {
			throw new UnsupportedOperationException("TODO: auto-generated method stub")
		}
		
	}

	@Inject
	new(Injector injector) {
		super(ISadlInferenceProcessor, injector);
	}

	override getProcessor(Resource resource) {
		return doCreateProcessor(resource);
	}

	
	override protected getExtensionPointId() {
		return Optional.of(EXTENSION_ID);
	}

	protected def doCreateProcessor(Resource resource) {
		val processor = allProcessors.findFirst[isSupported(resource)];
		return if(processor === null) NOOP_INFERENCER else processor;
	}

}
