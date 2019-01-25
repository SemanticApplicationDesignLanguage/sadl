/** 
 * Copyright 2007-2019 General Electric Company, All Rights Reserved
 * Project: SADL
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 */
package com.ge.research.sadl.utils

import java.net.URI

import static com.ge.research.sadl.processing.SadlConstants.SADL_BASE_MODEL_FILENAME

/**
 * Helper for the {@code SadlBaseModel}. Should be used only with the SADL runtime module.
 */
class SadlBaseModelHelper {

	/**
	 * The reserved file name with the {@code .owl} extension for the SADL base model.
	 */
	public static val SADL_BASE_MODEL = '''«SADL_BASE_MODEL_FILENAME».owl''';

	def boolean is(URI uri) {
		return uri !== null && uri.toString.endsWith(SADL_BASE_MODEL);
	}

	def boolean is(org.eclipse.emf.common.util.URI uri) {
		return uri !== null && uri.toString.endsWith(SADL_BASE_MODEL);
	}

}
