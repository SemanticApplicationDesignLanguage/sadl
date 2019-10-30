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
package com.ge.research.sadl.validation

import com.ge.research.sadl.processing.IModelProcessor
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContextPreferenceValuesProvider
import com.ge.research.sadl.processing.IModelProcessorProvider
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.processing.ValidationAcceptorImpl
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.service.OperationCanceledError
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.util.internal.EmfAdaptable
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.validation.ResourceValidatorImpl

class ResourceValidator extends ResourceValidatorImpl {

	/**
	 * Key for the cached validation issues on the resource scope cache.
	 */
	public static val CACHED_ISSUES_KEY = 'issues';

	@Inject IModelProcessorProvider processorProvider
	@Inject ProcessorContextPreferenceValuesProvider processorContextPreferenceValuesProvider

	override validate(Resource resource, CheckMode mode, CancelIndicator mon) throws OperationCanceledError {
		if (resource instanceof XtextResource) {
			return resource.cache.get(CACHED_ISSUES_KEY, resource) [
				super.validate(resource, mode, mon)
			];
		} else {
			return emptyList;
		}
	}

	override protected validate(Resource resource, CheckMode mode, CancelIndicator monitor, IAcceptor<Issue> acceptor) {
		ModelProcessorAdapter.removeFromEmfObject(resource);
		super.validate(resource, mode, monitor, acceptor);
		val processor = processorProvider.getProcessor(resource);
		val delegateAcceptor = new ValidationAcceptorImpl(acceptor);
		val preferenceValues = processorContextPreferenceValuesProvider.getPreferenceValues(resource);
		val context = new ProcessorContext(monitor, preferenceValues);
		processor.doValidate(resource, delegateAcceptor, mode, context);
	}
	
	protected def doValidate(IModelProcessor processor, Resource resource, ValidationAcceptor acceptor, CheckMode mode, ProcessorContext context) {
		processor.onValidate(resource, acceptor, mode, context);
		new ModelProcessorAdapter(processor).attachToEmfObject(resource);
	}

}

@Data
@EmfAdaptable
class ModelProcessorAdapter {
	val IModelProcessor processor;
}