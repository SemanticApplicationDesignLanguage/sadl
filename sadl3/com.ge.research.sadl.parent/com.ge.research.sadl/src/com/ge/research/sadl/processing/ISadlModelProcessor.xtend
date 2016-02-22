package com.ge.research.sadl.processing

import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.util.CancelIndicator
import java.util.List
import java.io.IOException
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.preferences.IPreferenceValues

/**
 * Generic hook for 3rd party processors to participate in the processing of SADL resources
 *  
 */
interface ISadlModelProcessor {
	
	/**
	 * Called in the validation phase
	 */
	def void onValidate(Resource resource, ValidationAcceptor issueAcceptor, ProcessorContext context);
	
	/**
	 * Called during code generation
	 */
	def void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context);
	
	/**
	 * Called when external models are downloaded
	 */
	def void processExternalModels(String mappingFileFolder, List<String> fileNames) throws IOException;
	
	@Data class ProcessorContext {
		CancelIndicator cancelIndicator
		IPreferenceValues preferenceValues
	}
	
}
