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
package com.ge.research.sadl.ide.utils

import com.ge.research.sadl.utils.SadlBaseModelHelper
import java.net.URI

import static com.ge.research.sadl.utils.ResourceManager.OWLDIR

/**
 * SADL base model helper for the generic IDE.
 * 
 * <p>
 * Unlike its super class, it does not only checks the file name, but the container {@code OwlModels} folder.
 */
class SadIdelBaseModelHelper extends SadlBaseModelHelper {

	override is(URI uri) {
		return uri !== null && 'file'.equalsIgnoreCase(uri.scheme) &&
			uri.toString.endsWith('''«OWLDIR»/«SADL_BASE_MODEL»''');
	}

	override is(org.eclipse.emf.common.util.URI uri) {
		return uri !== null && uri.file && uri.toString.endsWith('''«OWLDIR»/«SADL_BASE_MODEL»''');
	}

}
