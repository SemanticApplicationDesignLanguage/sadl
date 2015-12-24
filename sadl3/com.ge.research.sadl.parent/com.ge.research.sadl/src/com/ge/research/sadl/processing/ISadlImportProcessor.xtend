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
	def Object[] onImport(Resource resource, String targetProjectOwlModelsFolder);
	
	/**
	 * Same outputs as above but single OWL model content as a String input
	 */
	def Object[] onImport(String owlContent)
	
}
