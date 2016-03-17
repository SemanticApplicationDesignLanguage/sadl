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
package com.ge.research.sadl.tests

import org.junit.Test

import static org.junit.Assert.*

class OWLDeclarationsAstTest extends AbstractSADLParsingTest {
	
	@Test def void testModelName_01() {
		'''
		uri "http://sadl.org/Tests/ModelName" alias ^class.
		'''.assertAST[
			assertEquals("http://sadl.org/Tests/ModelName", baseUri)
			assertEquals("class", alias)
		]
	}
	
	
	@Test def void testModelName_02() {
		'''
		uri "http://sadl.org/Tests/ModelName" alias mn version "1".		
		'''.assertAST [
			assertEquals("1",version)
		]
	}
	
	@Test def void testModelName_03() {
		// this is fairly new and is probably inconsistent with annotations for classes, properties, and instances in that
		// the annotation is not right after the name but is after the prefix and version.
		'''
		uri "http://sadl.org/Tests/ModelName" alias mn version "1" (note "This is an rdfs:label") (alias "Model Name").		
		'''.assertAST [
			assertEquals("note",annotations.head.type)
			assertEquals("This is an rdfs:label",annotations.head.contents.head)
			
			assertEquals("alias",annotations.get(1).type)
			assertEquals("Model Name",annotations.get(1).contents.head)
		]
	}
	
}