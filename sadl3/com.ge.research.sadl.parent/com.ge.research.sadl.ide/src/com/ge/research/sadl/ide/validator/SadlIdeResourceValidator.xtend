/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.validator

import com.ge.research.sadl.processing.IModelProcessor
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.utils.SadlProjectHelper
import com.ge.research.sadl.validation.ResourceValidator
import com.google.inject.Inject
import java.net.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.service.OperationCanceledError

/**
 * Resource validator for the generic IDE. Bails the validation process (and does not invoke the model processors)
 * if the resource is not in a SADL project. This is used to avoid automatic {@code OwlModels} folder creation. 
 * 
 * Accepts resources only with {@code file} URI scheme. Ignores everything else.
 * 
 * @author akos.kitta
 */
class SadlIdeResourceValidator extends ResourceValidator {

	@Inject SadlProjectHelper projectHelper;
	
	override validate(Resource resource, CheckMode mode, CancelIndicator mon) throws OperationCanceledError {
		return if (!resource.URI.file) emptyList else super.validate(resource, mode, mon);
	}

	override protected doValidate(IModelProcessor processor, Resource resource, ValidationAcceptor acceptor,
		CheckMode mode, ProcessorContext context) {
			

		// Terminate the model processor validation and generation if the model is not in a SADL project.
		if (checkInSadlProject(resource, acceptor)) {
			super.doValidate(processor, resource, acceptor, mode, context)
		}
	}

	protected def boolean checkInSadlProject(Resource it, ValidationAcceptor acceptor) {
		if (URI.file && !contents.nullOrEmpty) {
			val model = contents.head;
			if (model instanceof SadlModel) {
				val root = projectHelper.getRoot(new URI(URI.toString));
				if (root === null) {
					acceptor.addError('The SADL resource is not in a SADL project.', model);
					return false
				}
			}
		}
		return true;
	}

}
