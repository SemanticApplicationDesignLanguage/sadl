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

/**
 * Test class for checking the behavior of the content assist via the language server.
 * 
 * @author akos.kitta
 */
class SadlCompletionTest extends AbstractSadlLanguageServerTest {

	static val BASE_URI = '"http://sadl.org/MyModel.sadl"';

	@Test
	def void emptyResource() {
		testCompletion[
			model = '';
			expectedCompletionItems = ''' uri  ->  uri  [[0, 0] .. [0, 0]]
			''';
		];
	}
	
	@Test
	def void baseUriAssignment() {
		testCompletion[
			model = 'uri ';
			column = model.length;
			expectedCompletionItems = '''
				«BASE_URI» -> «BASE_URI» [[0, 4] .. [0, 4]]
			''';
		];
	}
	
	@Test
	def void afterBaseUri() {
		testCompletion[
			model = '''uri «BASE_URI» ''';
			column = model.length;
			expectedCompletionItems = '''
				.
				 (. - End of Sentence) -> .
				 [[0, 35] .. [0, 35]]
				alias  -> alias  [[0, 35] .. [0, 35]]
				version  -> version  [[0, 35] .. [0, 35]]
				(  -> (  [[0, 35] .. [0, 35]]
				,  -> ,  [[0, 35] .. [0, 35]]
			''';
		];
	}
	
	@Test
	def void alias() {
		testCompletion[
			filesInScope = #{'other.sadl' -> '''uri "http://sadl.org/other.sadl".'''}
			model = '''uri «BASE_URI». import ''';
			column = model.length;
			expectedCompletionItems = '''
				"http://sadl.org/other.sadl" (SadlModel) -> "http://sadl.org/other.sadl" [[0, 43] .. [0, 43]]
			''';
		];
	}
	
	@Test
	def void aliasWithFiltering() {
		testCompletion[
			filesInScope = #{
				'other.sadl' -> '''uri "http://sadl.org/other.sadl".''',
				'another.sadl' -> '''uri "http://sadl.org/another.sadl".'''
			}
			model = '''uri «BASE_URI». import "http://sadl.org/another.sadl". import ''';
			column = model.length;
			expectedCompletionItems = '''
				"http://sadl.org/other.sadl" (SadlModel) -> "http://sadl.org/other.sadl" [[0, 82] .. [0, 82]]
			''';
		];
	}

}
