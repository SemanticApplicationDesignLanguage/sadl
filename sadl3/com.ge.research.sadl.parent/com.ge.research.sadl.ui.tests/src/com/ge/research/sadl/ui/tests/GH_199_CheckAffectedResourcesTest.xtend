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

import org.junit.Test
import static org.eclipse.core.resources.IMarker.*
import static org.eclipse.core.resources.IResource.*

/**
 * Test for checking whether transitive downstream resources get rebuilt when a change
 * occurs in the in the upstream resource.
 * 
 * @author akos.kitta
 */
class GH_199_CheckAffectedResourcesTest extends AbstractSadlPlatformTest {
	
	@Test
	def void checkTransitiveDownstreamGetsRebuilt() {
		createFile('A.sadl', '''
			uri "http://sadl.org/A.sadl".
			// AAA is a class.
		''');
		createFile('B.sadl', '''
			uri "http://sadl.org/B.sadl".
			import "http://sadl.org/A.sadl".
		''');
		val downstreamFile = createFile('C.sadl', '''
			uri "http://sadl.org/C.sadl".
			import "http://sadl.org/B.sadl".
			MyAAA is a AAA.
		''');
		assertEquals(SEVERITY_ERROR, downstreamFile.findMaxProblemSeverity(PROBLEM, true, DEPTH_INFINITE));
		
		setFileContent('A.sadl', '''
			uri "http://sadl.org/A.sadl".
			AAA is a class.
		''');
		assertEquals(/*No errors/warnings.*/ -1, downstreamFile.findMaxProblemSeverity(PROBLEM, true, DEPTH_INFINITE));
		
	}
	
}