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

import java.io.IOException
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.preferences.IPreferenceValues
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.CheckMode
import com.ge.research.sadl.reasoner.ResultSet
import org.eclipse.xtext.util.IAcceptor
import com.ge.research.sadl.builder.MessageManager.SadlMessage

/**
 * Generic hook for 3rd party processors to participate in the processing of SADL resources
 *  
 */
interface IModelProcessor {
	
	/**
	 * Called in the validation phase
	 */
	def void onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode, ProcessorContext context);
	
	/**
	 * Called during code generation
	 */
	def void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context);
	
	/**
	 * Called when external models are downloaded
	 */
	def void processExternalModels(String mappingFileFolder, List<String> fileNames) throws IOException;
	
	/**
	 * Called to process all of the Ask, Test, Explain, etc. commands in a .sadl file
	 */
	def void processCommands(Resource resource, ValidationAcceptor issueAcceptor, IAcceptor<SadlMessage> resultAcceptor, ProcessorContext context);
	
	/**
	 * Called to process an adhoc query against the model in a .sadl file
	 */
	def void processAdhocQuery(Resource resource, ValidationAcceptor issueAcceptor, ProcessorContext context, String query);
	
	@Data class ProcessorContext {
		CancelIndicator cancelIndicator
		IPreferenceValues preferenceValues
	}
	
}
