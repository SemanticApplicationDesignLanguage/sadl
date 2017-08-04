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

import java.util.Map
import org.eclipse.emf.ecore.resource.Resource

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
}
