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

import com.ge.research.sadl.preferences.SadlPreferences
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

class SadlModelArticleUITest extends AbstractSadlPlatformTest {

	@Test
	def void testArticles_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.TRUE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of Circle is Y.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			assertEquals(2, issues.size)
		]

	}

	@Test
	def void testArticles_02() {

		updatePreferences(new PreferenceKey(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.id, Boolean.FALSE.toString));

		createFile('UseArticles.sadl', '''
			uri "http://sadl.org/TestArticles.sadl" alias TestArticles.
			
			Shape is a class described by area with values of type decimal.
			
			Rectangle is a type of Shape, described by height with values of type decimal, described by width with values of type decimal.
			
			Circle is a type of Shape, described by radius with values of type decimal.
			
			ShapeCalculator is a class.
			MyShapeCalculator is a ShapeCalculator.
			MyCircle is a Circle.
			
			Rule R1: 
			if X is radius of Circle and
				X > 0 and
				Y is X^2*PI
			then
				area of Circle is Y.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			assertEquals(0, issues.size);
		]

	}

}
