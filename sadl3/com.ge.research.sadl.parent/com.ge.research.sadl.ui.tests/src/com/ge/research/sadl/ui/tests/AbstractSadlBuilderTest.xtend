/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.tests

import org.eclipse.core.resources.IResource

import static org.eclipse.core.resources.IMarker.*
import static org.eclipse.core.resources.IResource.*

/**
 * Base for all platform and or builder related tests.
 * 
 * @author akos.kitta
 */
abstract class AbstractSadlBuilderTest extends AbstractSadlPlatformTest {
	
	/**
	 * Asserts that the given resource has at least one associated problem marker with error severity. 
	 */
	protected def assertHasErrors(IResource resource) {
		assertEquals(SEVERITY_ERROR, resource.findMaxProblemSeverity(PROBLEM, true, DEPTH_INFINITE));
	}
	
	/**
	 * Asserts that the given resource has zero problems markers with either error or warning severity.
	 */
	protected def assertHasNoIssues(IResource resource) {
		assertEquals(/*No errors/warnings.*/ -1, resource.findMaxProblemSeverity(PROBLEM, true, DEPTH_INFINITE));
	}
	
}