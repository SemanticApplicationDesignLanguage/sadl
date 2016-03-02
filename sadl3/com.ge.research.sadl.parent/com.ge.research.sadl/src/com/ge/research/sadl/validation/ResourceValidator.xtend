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

import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.ge.research.sadl.processing.ValidationAcceptor
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.validation.ResourceValidatorImpl
import com.ge.research.sadl.processing.ISadlModelProcessor.ProcessorContext
import org.eclipse.xtext.preferences.IPreferenceValuesProvider

class ResourceValidator extends ResourceValidatorImpl {
	
	@Inject SadlModelProcessorProvider processorProvider 
	@Inject IPreferenceValuesProvider preferenceProvider
	
	
	override protected validate(Resource resource, CheckMode mode, CancelIndicator monitor, IAcceptor<Issue> acceptor) {
		super.validate(resource, mode, monitor, acceptor)
		val processor = processorProvider.getProcessor(resource.resourceSet)
		processor.onValidate(resource, new ValidationAcceptor(acceptor), new ProcessorContext(monitor,  preferenceProvider.getPreferenceValues(resource)))
	}
	
	
}