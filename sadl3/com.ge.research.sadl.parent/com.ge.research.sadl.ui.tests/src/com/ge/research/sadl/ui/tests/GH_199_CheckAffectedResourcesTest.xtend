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

/**
 * Test for checking whether transitive downstream resources get rebuilt when a change
 * occurs in the in the upstream resource.
 * 
 * @author akos.kitta
 */
class GH_199_CheckAffectedResourcesTest extends AbstractSadlBuilderTest {

	@Test
	def void checkTransitiveDownstreamGetsRebuilt_01() {
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
		downstreamFile.assertHasErrors;

		setFileContent('A.sadl', '''
			uri "http://sadl.org/A.sadl".
			AAA is a class.
		''');

		downstreamFile.assertHasNoIssues;
	}

	/**
	 * This test case was merged from SRL GH-28.
	 */
	@Test
	def void checkTransitiveDownstreamGetsRebuilt_02() {
		createFile('m.sadl', '''
			uri "http://sadl.org/m.sadl" alias m.
			System is a class described by p1 with values of type int.
		''');
		val test = createFile('t.sadl', '''
			uri "http://sadl.org/t.sadl" alias t.
			import "http://sadl.org/m.sadl".
			Test: p1 > 12.
		''');
		test.assertHasNoIssues;

		setFileContent('m.sadl', '''
			uri "http://sadl.org/m.sadl" alias m.
			System is a class described by p1 with values of type string.
		''');
		test.assertHasErrors;

		setFileContent('m.sadl', '''
			uri "http://sadl.org/m.sadl" alias m.
			System is a class described by p1 with values of type int.
		''');
		test.assertHasNoIssues;
	}

}
