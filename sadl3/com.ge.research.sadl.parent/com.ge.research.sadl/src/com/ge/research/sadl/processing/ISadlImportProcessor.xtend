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

import org.eclipse.emf.ecore.resource.Resource

/**
 * Generic hook for 3rd party processors to participate in the processing of SADL resources
 *  
 */
interface ISadlImportProcessor {
	
	/**
	 * Called to do an import operation of some information format to SADL
	 * 0th element of returned Object array is the InputStream with the results of the import
	 * 1st element is a list of error Strings (if any)
	 * Note: all results are returned on call to onImport because separate calls go to different instances of the processor
	 */	
	def Object[] onImport(Resource resource, String targetProjectPath);
	
	/**
	 * Same outputs as above but single OWL model content as a String input
	 */
	def Object[] onImport(String owlContent)
	
	/**
	 * Same outputs as above but single OWL model content as a String input
	 */
	def Object[] onImport(String owlContent, String modelURI)
	
}
