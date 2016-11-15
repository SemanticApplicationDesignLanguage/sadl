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
import com.google.inject.Inject
import org.junit.Test

/**
 * Test for checking the {@link ISemanticHighlightService semantic highlight
 * service} for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
class SadlColoringTest extends AbstractSadlLanguageServerTest {

	static val CONTENT = '''uri "http://sadl.imp/shapes_top".

	Shape is a top-level class.
	''';

	@Inject
	SadlProjectStructureInitializer projectStructureInitializer;

	@Test
	def void checkColoring() {
		'''MyModel.«fileExtension»'''.writeFile(CONTENT);
		initialize;
		assertEquals("", coloringParams.toExpectation);
//		testColoring[
//			model = CONTENT;
//			expectedColoredRangesWithStyles = '';
//		];
	}

	@Override
	override protected initialize() {
		val rootPath = root.absoluteFile.toPath;
		projectStructureInitializer.initialize(rootPath);
		super.initialize();
	}

}
