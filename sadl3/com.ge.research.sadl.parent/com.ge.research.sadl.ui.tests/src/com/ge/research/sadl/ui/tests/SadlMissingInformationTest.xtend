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
import org.junit.Ignore
import org.junit.Test
import org.eclipse.xtext.diagnostics.Severity

class SadlMissingInformationTest extends AbstractSadlPlatformTest {

	@Test
	def void testMissingPattern_01() {

		updatePreferences(new PreferenceKey(SadlPreferences.FIND_AND_EXPAND_MISSING_PATTERNS.id, Boolean.TRUE.toString));

		createFile('MissingInfo.sadl', '''
			  uri "http://sadl.org/SimplePathFindingCase.sadl" alias spfc.
			  Shape is a class described by area with values of type float.
			  
			  Circle is a type of Shape described by radius with values of type float.
			  
			  Rule R1: if c is a Circle then area is PI*radius^2.
		''').resource.assertValidatesTo [ jenaModel, rules, commands, issues, processor |
			assertNotNull(jenaModel)
			issues.map[message].forEach[println(it)];
			for (rule : rules) {
				println(rule.toString)
			}
			assertEquals(0, issues.size)
			assertEquals("Rule R1:  if rdf(c, rdf:type, spfc:Circle) and rdf(c, spfc:radius, v0) and ^(v0,2,v1) and *(PI,v1,v2) then rdf(c, spfc:area, v2).", 
				rules.get(0).toString
			)
		]

	}

}
