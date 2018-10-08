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
package com.ge.research.sadl.tests.lsp

import org.junit.Test

import static extension com.ge.research.sadl.tests.helpers.XtendTemplateHelper.unifyEOL

/**
 * Test for checking the {@link ISemanticHighlightService semantic highlight
 * service} for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
class SadlColoringTest extends AbstractSadlLanguageServerTest {

	override setup() {
		super.setup();
		initialize();
	}
	
	override assertEquals(String expected, String actual) {
		super.assertEquals(expected.unifyEOL, actual.unifyEOL)
	}

	@Test
	def void checkColoring_01() {
        val uri = '''file:///MyModel.«fileExtension»''';
        open(uri, '''
		uri "http://sadl.imp/shapes_top".

		Shape is a top-level class.'
        ''');
		assertEquals('''
		«uri» ->
		 * [[0, 4] .. [0, 32]] -> [21]
		 * [[2, 0] .. [2, 5]] -> [9]''', coloringParams.toExpectation);
	}

	@Test
	def void checkColoring_02() {
		val uri = '''file:///MyModel.«fileExtension»'''
		
		uri.open('''
		uri "http://sadl.imp/shapes_specific".
		import "file://shapes-top.sadl" as shapes-top.

		Circle is a type of Shape,
		        described by radius with values of type float.

		Rectangle is a type of Shape,
		       described by height with values of type float,
		        described by width with values of type float.''');
		
		assertEquals('''
		«uri» ->
		 * [[1, 7] .. [1, 31]] -> [21]
		 * [[0, 4] .. [0, 37]] -> [21]
		 * [[3, 0] .. [3, 6]] -> [9]
		 * [[3, 20] .. [3, 25]] -> [20]
		 * [[4, 21] .. [4, 27]] -> [3]
		 * [[6, 0] .. [6, 9]] -> [9]
		 * [[6, 23] .. [6, 28]] -> [20]
		 * [[7, 20] .. [7, 26]] -> [3]
		 * [[8, 21] .. [8, 26]] -> [3]''', coloringParams.toExpectation);
	}

	@Test
	def void checkColoring_03() {
		val uri = '''file:///MyModel.«fileExtension»'''
		
		uri.open('''
		uri "http://sadl.imp/shapes_test" .
		import "file://shape-rules.sadl" as shape-rules.

		MyCircle is a Circle, has radius 3.5 .

		MyRect is a Rectangle, has height 3.5, has width 4.5.

		Test: MyCircle has area 38.48 .''');
		
		assertEquals('''
		«uri» ->
		 * [[1, 7] .. [1, 32]] -> [21]
		 * [[0, 4] .. [0, 33]] -> [21]
		 * [[3, 0] .. [3, 8]] -> [5]
		 * [[3, 14] .. [3, 20]] -> [20]
		 * [[3, 26] .. [3, 32]] -> [20]
		 * [[5, 0] .. [5, 6]] -> [5]
		 * [[5, 12] .. [5, 21]] -> [20]
		 * [[5, 27] .. [5, 33]] -> [20]
		 * [[5, 43] .. [5, 48]] -> [20]
		 * [[7, 6] .. [7, 14]] -> [5]
		 * [[7, 19] .. [7, 23]] -> [20]''', coloringParams.toExpectation);
	}

}
