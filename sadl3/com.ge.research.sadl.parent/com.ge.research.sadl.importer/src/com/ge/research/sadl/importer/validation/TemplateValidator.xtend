/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.importer.validation

import com.ge.research.sadl.importer.template.TemplateModel
import com.ge.research.sadl.importer.template.TemplatePackage
import org.eclipse.xtext.validation.Check

/**
 * This class contains custom validation rules. 
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class TemplateValidator extends AbstractTemplateValidator {
	
//	public static val INVALID_NAME = 'invalidName'
//
//	@Check
//	def checkGreetingStartsWithCapital(Greeting greeting) {
//		if (!Character.isUpperCase(greeting.name.charAt(0))) {
//			warning('Name should start with a capital', 
//					TEMPLATEPackage.Literals.GREETING__NAME,
//					INVALID_NAME)
//		}
//	}
	

	@Check
	def checkSadlModel(TemplateModel model) {
		val imps = model.imports
		var tstsFound = newArrayList
		for (imp : imps) {
			if (imp.importResource !== null) {
				val tsturi = imp.importResource.baseUri
				if (tstsFound.contains(tsturi)) {
					warning("Duplicate test", imp, TemplatePackage.Literals.IMPORT__IMPORT_RESOURCE)
				}
				else {
					tstsFound.add(tsturi);
				}
			}
		}
	}
}
