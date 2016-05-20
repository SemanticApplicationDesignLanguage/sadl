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
package com.ge.research.sadl.tests.scoping

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class ScopingTest {
	@Inject extension ParseHelper<SadlModel>
	@Inject extension DeclarationExtensions
	@Inject ValidationTestHelper validationTestHelper

	@Test def void testLocalVariable_02() {
		val model = '''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy, described by color with values of type string.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy.
			
			Rule AllThingysConnect: 
				if x != y and x is a Thingy and y is a Thingy 
				   then x has connectedTo y .
		'''.parse
		model.elements.get(4) as RuleStatement => [
			val names = EcoreUtil2.getAllContents(ifs).filter(Name).filter[concreteName == 'x'].toList
			val reference = names.get(0)
			val declaration = names.get(1)
			assertSame(declaration, reference.name)
		]
	}
	
	@Test def void testLocalVariable_01() {
		val model = '''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy, described by color with values of type string.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy.
			
			Rule AllThingysConnect: if x is a Thingy and y is a Thingy and x != y then x has connectedTo y .
			Rule AllThingysAreBlue: if x is a Thingy then color of x is "blue".
		'''.parse
		
		validationTestHelper.assertNoErrors(model.eResource)
		
		model.elements.get(4) as RuleStatement => [
			val names = EcoreUtil2.getAllContents(ifs).filter(Name).toList
			val x = names.get(0)
			assertSame(x, x.name)
			val y = names.get(1)
			assertSame(y, y.name)
			for (other : names.tail) {
				if (other.concreteName == 'x') {
					assertSame(x, other.name)
				} else {
					assertSame(y, other.name)
				}
			}
		]
		
		model.elements.get(5) as RuleStatement => [
			val names = EcoreUtil2.getAllContents(it, false).filter(Name).toList
			assertEquals(3, names.size)
			val x = names.get(0)
			assertSame(x, x.name)
			assertFalse(names.get(1).eIsProxy)
			assertEquals("color", names.get(1).concreteName)
			assertSame(x, names.get(2).name)
		]
	}
		
	@Test def void testScopingSlowdown() {
		for (i:1 ..< 10) {
			var modeltext = "uri \"http://com.ge.research.sadl/scopingscaletest\".\n"
			for (j:0 ..< i*100) {
				modeltext += "Pr" + j + " is a property.\n"
			}
//			print(modeltext)
			var t1 = System.currentTimeMillis
			var model = modeltext.parse
			validationTestHelper.assertNoErrors(model.eResource)
			var t2 = System.currentTimeMillis;
			print(i*100 + "," + (t2-t1) + "ms\n")
		}
	}
}