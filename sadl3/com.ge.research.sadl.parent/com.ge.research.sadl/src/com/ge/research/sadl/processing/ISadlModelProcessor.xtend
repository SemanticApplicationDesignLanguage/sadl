package com.ge.research.sadl.processing

import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.util.IAcceptor

/**
 * Generic hook for 3rd party processors to participate in the processing of SADL resources
 *  
 */
interface ISadlModelProcessor {
	
	/**
	 * Called in the validation phase
	 */
	def void onValidate(Resource resource, IAcceptor<Issue> issueAcceptor, CancelIndicator cancelIndicator);
	
	/**
	 * Called during code generation
	 */
	def void onGenerate(Resource resource, IFileSystemAccess2 fsa, CancelIndicator cancelIndicator);
}