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

import com.google.inject.Inject
import com.google.inject.Provider
import java.util.List
import java.util.Set
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.util.OnChangeEvictingCache
import org.eclipse.xtext.validation.CheckMode
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import org.eclipse.xtext.util.IAcceptor
import com.ge.research.sadl.builder.MessageManager.SadlMessage

class SadlModelProcessorProvider implements IModelProcessorProvider {
	
	@Inject OnChangeEvictingCache cache
	
	public static val Set<Provider<? extends IModelProcessor>> SADL_PROCESSOR_Registry = newHashSet
	
	override IModelProcessor getProcessor(Resource resource) {
		return cache.get("modelprocessor", resource) [
			doCreateProcessor(resource)
		]
	}
	
	protected def IModelProcessor doCreateProcessor(Resource resource) {
		val processors = getAllProviders.map[get];
		return new CompositeModelProcessor(processors)
	}
	
	@Data static class CompositeModelProcessor implements IModelProcessor {
		
		val Iterable<? extends IModelProcessor> processors
		
		override onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode, ProcessorContext context) {
			processors.forEach[onValidate(resource, issueAcceptor, mode, context)]
		}
		override onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context) {
			processors.forEach[onGenerate(resource, fsa, context)]
		}
		
		override processExternalModels(String mappingFileFolder, List<String> fileNames) {
			processors.forEach[processExternalModels(mappingFileFolder, fileNames)]
		}
		
	}
	
	protected def Iterable<? extends Provider<? extends IModelProcessor>> getAllProviders() {
		SADL_PROCESSOR_Registry
	}
	
}