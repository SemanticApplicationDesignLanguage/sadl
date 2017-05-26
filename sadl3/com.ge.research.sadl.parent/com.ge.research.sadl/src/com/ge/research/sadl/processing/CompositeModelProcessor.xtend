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
import com.google.common.collect.ImmutableList
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.validation.CheckMode
import com.google.common.annotations.VisibleForTesting

/**
 * Composite {@link IModelProcessor model processor} implementation.
 * Delegates each calls to the wrapped model processors.
 */
@Data
class CompositeModelProcessor implements IModelProcessor {

	val Iterable<? extends IModelProcessor> processors;

	@Override
	override onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode, ProcessorContext context) {
		processors.forEach[onValidate(resource, issueAcceptor, mode, context)];
	}

	@Override
	override onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context) {
		processors.forEach[onGenerate(resource, fsa, context)];
	}

	@Override
	override processExternalModels(String mappingFileFolder, List<String> fileNames) {
		processors.forEach[processExternalModels(mappingFileFolder, fileNames)];
	}

	@Override
	override validate(Context context, SadlResource candidate) {
		processors.forEach[validate(context, candidate)];
	}

	@VisibleForTesting
	def Iterable<IModelProcessor> getProcessors() {
		return ImmutableList.copyOf(processors);
	}

}
