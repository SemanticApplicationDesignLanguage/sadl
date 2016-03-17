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
import org.junit.Ignore

class SadlRuleParserTest extends AbstractSADLParsingTest {

	/* These tests check for errors that should be found and reported */
	/* These test check that no errors occur in valid constructs */
	@Test def void testInstanceDeclaration_02() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy.
			
			Rule AllThingsConnect: if x is a Thingy and y is a Thingy and x != y then x has connectedTo y .
		'''.assertNoErrors

	}
	
	@Test def void testPolyRule() {
		'''
			uri "http://sadl.impl/polyrule".
			
			AnyThingWillDo is a class.
			
			Rule Poly_Rule
				if x is a childClass
					and childClass is a type of superClass
				then x is a superClass.
		'''.assertNoErrors
	}

	@Test def void testInstanceDeclaration_04() {
		'''
			uri "http://sadl.imp/negnumber".
			
			 Freezer is a class, described by expectedTemperature with values of type float.
			
			 MyFreezer is a Freezer with expectedTemperature -20.
			 YourFreezer is a Freezer with expectedTemperature -20.5.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceDeclaration_05() {
		'''
			uri "http://sadl.imp/negnumber".
			
			ASSERT_DM-17 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_3,
			    with functional_max 750,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min -100,
			    with tolerance 0.5,
			    with resolution 0.5.
		'''.assertNoErrors
	}
	
	@Test def void testInstanceDeclaration_06() {
		'''
			uri "http://sadl.imp/negnumber".
			
			ASSERT_DM-17 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_3,
			    with functional_max 750,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min -100,
			    with tolerance .5,
			    with resolution .5.
		'''.assertNoErrors
	}
	
	/**
	 * see https://github.com/crapo/sadlos2/issues/46
	 */
	@Test def void testNewlineSparatesAndedPredicates() {
		'''
			uri "http://sadl.imp/negnumber".
			
			Rule Rule_Name
			given
			x is a Thingy
			
			then
			intVal of x is abs(-1) and
			flVal of x is abs(-1.0) and
			dblVal of x is abs(-1.0).
		'''.assertNoErrors
	}
	
}
