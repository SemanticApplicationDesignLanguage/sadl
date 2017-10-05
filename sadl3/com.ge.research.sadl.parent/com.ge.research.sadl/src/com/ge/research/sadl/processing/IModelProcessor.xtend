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

import com.ge.research.sadl.processing.ISadlOntologyHelper.Context
import com.ge.research.sadl.sADL.SadlResource
import java.io.IOException
import java.util.List
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.preferences.IPreferenceValues
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.CheckMode

/**
 * Generic hook for 3rd party processors to participate in the processing of SADL resources
 *  
 */
interface IModelProcessor {

	static val SYNTHETIC_FROM_TEST = "__synthetic";

	/**
	 * The NOOP model processor. Does nothing at all.
	 */
	static val NOOP = new IModelProcessor() {

		override onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode,
			ProcessorContext context) {
			// NOOP
		}

		override onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context) {
			// NOOP
		}

		override validate(Context context, SadlResource candidate) {
			// NOOP
		}

		override processExternalModels(String mappingFileFolder, List<String> fileNames) throws IOException {
			// NOOP
		}

		override isSupported(String fileExtension) {
			return false;
		}

	}

	/**
	 * Called in the validation phase
	 */
	def void onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode, ProcessorContext context);

	/**
	 * Called during code generation
	 */
	def void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context);

	/**
	 * Checks whether the candidate SADL resource is valid in the given context. 
	 */
	def void validate(Context context, SadlResource candidate);

	/**
	 * Called when external models are downloaded
	 */
	def void processExternalModels(String mappingFileFolder, List<String> fileNames) throws IOException;

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
	 * {@code true} if the model processor can be used to process files with the given file extension argument. Otherwise, {@code false}.
	 * Clients should expect a file extension <b>without</b> the leading dot ({@code .}). So for example, in case of SADL, the argument will be 
	 * {@code "sadl"} and <b>not</b> {@code .sadl}. 
	 */
	def boolean isSupported( /*nullable*/ String fileExtension);

	@Data class ProcessorContext {
		CancelIndicator cancelIndicator
		IPreferenceValues preferenceValues
	}

}
