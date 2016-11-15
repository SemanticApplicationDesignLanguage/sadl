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

import com.ge.research.sadl.ide.SadlProjectStructureInitializer
import com.ge.research.sadl.utils.PathToFileUriConverter
import com.google.inject.Inject
import org.junit.Test

/**
 * Test for checking the {@link ISemanticHighlightService semantic highlight
 * service} for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
class SadlColoringTest extends AbstractSadlLanguageServerTest {

	@Inject
	SadlProjectStructureInitializer projectStructureInitializer;

	@Inject
	PathToFileUriConverter uriConverter;

	@Override
	override setup() {
		super.setup();
		initialize();
	}

	@Test
	def void checkColoring_01() {
		val file = root.toPath.resolve('''MyModel.«fileExtension»''').toFile;
		val uri = file.toURI.toString;
		uri.open('''uri "http://sadl.imp/shapes_top".

		Shape is a top-level class.''');
		assertEquals('''«uriConverter.createFileUri(file)» -> 
 * [[0, 4] .. [0, 32]] -> [uri]
 * [[2, 2] .. [2, 7]] -> [class]''', coloringParams.toExpectation);
	}

	@Test
	def void checkColoring_02() {
		val file = root.toPath.resolve('''MyModel.«fileExtension»''').toFile;
		val uri = file.toURI.toString;
		uri.open('''uri "http://sadl.imp/shapes_specific".
		import "file://shapes-top.sadl" as shapes-top.

		Circle is a type of Shape,
		        described by radius with values of type float.

		Rectangle is a type of Shape,
		       described by height with values of type float,
		        described by width with values of type float.''');
		assertEquals('''«uriConverter.createFileUri(file)» -> 
 * [[1, 9] .. [1, 33]] -> [uri]
 * [[0, 4] .. [0, 37]] -> [uri]
 * [[3, 2] .. [3, 8]] -> [class]
 * [[3, 22] .. [3, 27]] -> [variable]
 * [[4, 23] .. [4, 29]] -> [dataProperty]
 * [[6, 2] .. [6, 11]] -> [class]
 * [[6, 25] .. [6, 30]] -> [variable]
 * [[7, 22] .. [7, 28]] -> [dataProperty]
 * [[8, 23] .. [8, 28]] -> [dataProperty]''', coloringParams.toExpectation);
	}

	@Test
	def void checkColoring_03() {
		val file = root.toPath.resolve('''MyModel.«fileExtension»''').toFile;
		val uri = file.toURI.toString;
		uri.open('''uri "http://sadl.imp/shapes_test" .
		import "file://shape-rules.sadl" as shape-rules.

		MyCircle is a Circle, has radius 3.5 .

		MyRect is a Rectangle, has height 3.5, has width 4.5.

		Test: MyCircle has area 38.48 .''');
		assertEquals('''«uriConverter.createFileUri(file)» -> 
 * [[1, 9] .. [1, 34]] -> [uri]
 * [[0, 4] .. [0, 33]] -> [uri]
 * [[3, 2] .. [3, 10]] -> [instance]
 * [[3, 16] .. [3, 22]] -> [variable]
 * [[3, 28] .. [3, 34]] -> [variable]
 * [[5, 2] .. [5, 8]] -> [instance]
 * [[5, 14] .. [5, 23]] -> [variable]
 * [[5, 29] .. [5, 35]] -> [variable]
 * [[5, 45] .. [5, 50]] -> [variable]
 * [[7, 8] .. [7, 16]] -> [instance]
 * [[7, 21] .. [7, 25]] -> [variable]''', coloringParams.toExpectation);
	}

	@Override
	override protected initialize() {
		val rootPath = root.absoluteFile.toPath;
		projectStructureInitializer.initialize(rootPath);
		super.initialize();
	}

}
