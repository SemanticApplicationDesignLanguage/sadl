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
}