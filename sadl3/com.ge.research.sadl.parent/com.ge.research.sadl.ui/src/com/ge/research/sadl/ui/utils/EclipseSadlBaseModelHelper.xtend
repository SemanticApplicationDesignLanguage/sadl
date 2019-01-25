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
package com.ge.research.sadl.ui.utils

import com.ge.research.sadl.utils.SadlBaseModelHelper
import java.net.URI

import static com.ge.research.sadl.utils.ResourceManager.OWLDIR

/**
 * SADL base model helper for Eclipse.
 * 
 * It checks the file name, the container folder name, and that the container folder is a top level container in the project.
 */
class EclipseSadlBaseModelHelper extends SadlBaseModelHelper {

	override is(URI uri) {
		return uri !== null && 'platform'.equalsIgnoreCase(uri.scheme) &&
			uri.toString.endsWith('''«OWLDIR»/«SADL_BASE_MODEL»''');
	}

	override is(org.eclipse.emf.common.util.URI uri) {
		// Segment count `4` comes from: platform:/resource/PROJECT_NAME/OwlModels/SadlBaseModel
		if (uri !== null && uri.platform && uri.segmentCount === 4) {
			val segments = uri.segments;
			return 'resource' == segments.get(0) && OWLDIR == segments.get(2) && SADL_BASE_MODEL == segments.get(3);
		}
		return false;
	}

}
