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
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.SADLNoopModelProcessorsInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLNoopModelProcessorsInjectorProvider)
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
	
//	@Ignore
	@Test def void testLocalVariable_03() {
		val model = '''
			uri "http://com.ge.research.sadl/Bug3434542" alias Bug3434542. 
			
			Aa is a class.
			
			Ask: x is a Aa .
		'''.parse
		
		validationTestHelper.assertNoErrors(model.eResource)
		
		model.elements.get(1) as QueryStatement => [
			val names = EcoreUtil2.getAllContents(it, false).filter(Name).toList
			assertEquals(1, names.size)
			val x = names.get(0)
			assertSame(x, x.name)
		]
			
		
		
	}
		
	@Ignore
	@Test def void testLocalVariable_04() {
		val model = '''
			uri "http://com.ge.research.sadl/Bug3434542" alias Bug3434542. 
			
			
			Aa is a class.
			
			Ab is a type of Aa.
			
			Ac is a type of Aa.
			
			_one_ is a Ab.
			_two_ is a Ac.
			
			
			Ask: x is a Ab .
			Ask: x is not a Ab .
			Ask: x is a Aa and x is not a Ab .
			Ask: select x where x is a Aa and x is not a Ab .
			
			Ask: "select ?x where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?t . FILTER (!EXISTS { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://com.ge.research.sadl/Bug3434542#Ab> })}".
			
			Test: x is an Ab is _one_.
			Test: (select x where x is an Aa and x is not an Ab) is _two_.
		'''.parse
		
		validationTestHelper.assertNoErrors(model.eResource)
		
		model.elements.get(5) as QueryStatement => [
			val names = EcoreUtil2.getAllContents(it, false).filter(Name).toList
			assertEquals(1, names.size)
			val x = names.get(0)
			assertSame(x, x.name)
		]
			
	}
	
	@Ignore
	@Test def void testLocalVariableRule1() {
		val model = '''
			uri "http://sadl.org/TestSadlIde/DateMinMax" alias dtminmax version "$Revision: 1.1 $ Last modified on   $Date: 2015/07/15 12:51:12 $". 
			
			Event is a class, 
				described by when with values of type dateTime,
				described by openingEvent with a single value of type Event,			
				described by closingEvent with a single value of type Event.
	
			Circus is a class,
				described by event with values of type Event.
				
			Rule OpeningEvent:
				if 	c is a Circus
					eventList is list(c, event, e, e, when)
					oet is min(eventList)
					oe has when oet
				then c has openingEvent oe.
		'''.parse
		
		validationTestHelper.assertNoErrors(model.eResource)
		
		model.elements.get(5) as QueryStatement => [
			val names = EcoreUtil2.getAllContents(it, false).filter(Name).toList
			assertEquals(1, names.size)
			val x = names.get(0)
			assertSame(x, x.name)
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