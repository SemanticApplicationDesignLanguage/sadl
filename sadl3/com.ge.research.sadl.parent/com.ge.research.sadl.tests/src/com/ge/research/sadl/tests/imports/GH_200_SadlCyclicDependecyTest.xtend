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
package com.ge.research.sadl.tests.imports

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.AbstractLinkingTest
import com.google.inject.Inject
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Ignore
import org.junit.Test

import static org.junit.Assert.*

/**
 * Test for checking the cyclic dependency validation for SADL models. 
 * 
 * @author akos.kitta
 */
class GH_200_SadlCyclicDependecyTest extends AbstractLinkingTest {

	@Inject extension ValidationTestHelper;

	@Test
	def void checkNoCycle() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/B.sadl".'''.sadl;
		'''uri "http://sadl.org/B.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		'''uri "http://sadl.org/C.sadl".'''.sadl;
		val issues = validate(resource);
		assertEquals(0, issues.size);
	}
	
	@Test
	def void checkRedundantImportIsNoCycle() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/B.sadl". import "http://sadl.org/B.sadl".'''.sadl;
		'''uri "http://sadl.org/B.sadl". import "http://sadl.org/C.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		'''uri "http://sadl.org/C.sadl". import "http://sadl.org/D.sadl". import "http://sadl.org/D.sadl".'''.sadl;
		'''uri "http://sadl.org/D.sadl".'''.sadl;
		val issues = validate(resource);
		assertEquals(0, issues.size);
	}
	
	@Test
	def void checkExplicitReImportIsNoCycle() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/B.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		'''uri "http://sadl.org/B.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		'''uri "http://sadl.org/C.sadl".'''.sadl;
		val issues = validate(resource);
		assertEquals(0, issues.size);
	}

	@Test
	@Ignore("https://github.com/crapo/sadlos2/issues/158")
	def void checkSelfReference() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/A.sadl".'''.sadl.contents.head as SadlModel;
		val issues = validate(resource);
		assertEquals(1, issues.size);
		assertTrue(issues.head.message.contains('[http://sadl.org/A.sadl] -> [http://sadl.org/A.sadl]'));
	}

	@Test
	def void checkSimpleCycle() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/B.sadl".'''.sadl;
		'''uri "http://sadl.org/B.sadl". import "http://sadl.org/A.sadl".'''.sadl;
		val issues = validate(resource);
		assertEquals(1, issues.size);
		assertTrue(issues.head.message.contains('[http://sadl.org/A.sadl] -> http://sadl.org/B.sadl -> [http://sadl.org/A.sadl]'));
	}
	
	@Test
	def void checkTransitiveCycle() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/B.sadl".'''.sadl;
		'''uri "http://sadl.org/B.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		'''uri "http://sadl.org/C.sadl". import "http://sadl.org/A.sadl".'''.sadl;
		val issues = validate(resource);
		assertEquals(1, issues.size);
		assertTrue(issues.head.message.contains('[http://sadl.org/A.sadl] -> http://sadl.org/B.sadl -> http://sadl.org/C.sadl -> [http://sadl.org/A.sadl]'));
	}
	
	@Test
	def void checkHook() {
		val resource = '''uri "http://sadl.org/A.sadl". import "http://sadl.org/B.sadl".'''.sadl;
		'''uri "http://sadl.org/B.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		'''uri "http://sadl.org/C.sadl". import "http://sadl.org/D.sadl".'''.sadl;
		'''uri "http://sadl.org/D.sadl". import "http://sadl.org/C.sadl".'''.sadl;
		val issues = validate(resource);
		assertEquals(1, issues.size);
		assertTrue(issues.head.message.contains('http://sadl.org/A.sadl -> http://sadl.org/B.sadl -> [http://sadl.org/C.sadl] -> http://sadl.org/D.sadl -> [http://sadl.org/C.sadl]'));
	}

}
