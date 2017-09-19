
/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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

import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SelectExpression
import com.ge.research.sadl.sADL.TestStatement
import org.eclipse.xtext.util.EmfFormatter
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test
import com.google.inject.Inject
import com.ge.research.sadl.model.DeclarationExtensions

class SADLParsingTest extends AbstractSADLParsingTest {

	@Test 
	def void testSublist_01() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching var1 is true and var2 is true and var3 is false).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_02() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching value >= 42).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_03() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching index >= 42).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_04() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching type is Person).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_05() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching OtherList does not contain value).
		'''.assertNoErrors
	}
	
	@Test 
	def void testSublist_06() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: Items is (the sublist of list_of_items matching index >= 42 and type is Person and OtherList does not contain value).
		'''.assertNoErrors
	}
	
	@Test 
	def void testListTypes_01() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			MyPets is a Pet List.
		'''.assertNoErrors
	}
	
	@Test 
	def void testListTypes_02() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			GennysPriorizedPets is the Pet List [Spot, Lassie].
		'''.assertNoErrors
	}
	
	@Test 
	def void testListTypes_03() {
		'''
			uri "http://com.ge.research.sadl/sublists". 
			
			Test: MyPets is a Pet List.
		'''.assertNoErrors
	}

	@Test 
	def void testPropertyTypeOnly() {
		'''
			uri "http://com.ge.research.sadl/proptypeonly". 
			
			dtprop is a property with values of type data.
			objprop is a property with values of type class.
		'''.assertNoErrors
	}

	@Test 
	def void testSubjHasPropCompareExpr() {
		'''
			 uri "http://sadl.org/testrule.sadl" alias testrule.
			 
			 Person is a class described by age with values of type int.
			 
			 Rule R1: if x is a Person and x has age > 30 then print("hi").
			 
			 Rule R2: if x is a Person and age of x > 30 then print("hi").
 		'''.assertNoErrors
	}
	
	
	@Test
	def void testSubjectHasPropSomeMore() {
		'''
			 uri "http://sadl.org/HasTests.sadl" alias ht.
			 
			 Person is a class.
			 age describes Person with values of type int.		// a "passive" property--a characteristic that a Person has; also a DatatypeProperty
			 child describes Person with values of type Person.	// another "passive" property, this time an ObjectProperty
			 
			 
			 teaches describes Person with values of type Person. // an "action" property--something that a Person does
			 
			 // Instance declarations
			 George is a Person.
			 John is a Person, has age 23, has teaches George.	// this is currently valid SADL grammar but not good English
			 Julia is a Person, teaches George.	// this does not work but is desired
			 
			 Sue is a Person. 
			 Sue teaches George.	// this works currently, which is good
			 
			 
			 knows describes Person with values of type Person.		// an "active" property
			 The relationship of Person to Person is acquaintance.	// a "passive" property
			 
			 Rule R1 if x is a Person and x has teaches y then x has acquaintance y.	// this works
			 
			 Rule R2 if x is a Person and x teaches y then x knows y.	// this doesn't but is desired
		'''.assertNoErrors
	}
	
	@Test
	def void testGrammarCoverage() {
		'''
			 uri "http://sadl.org/GrammarCoverage.sadl" alias GrammarCoverage.
			 
			 Person is a class described by age with values of type int, described by friend with values of type Person.
			 weight describes Person with values of type float.
			 child describes Person with values of type Person.
			 son is a type of child.
			 
			 Gender is a class, must be one of {Male, Female}.
			 gender describes Person with values of type Gender.
			 son of Person only has values of type Male.
			 
			 Pet is a class.
			 The relationship of Person to Pet is owns.
			 
			 {Professor, Teacher, Student, Pupil} are types of Person.
			 relationship of {Professor or Teacher} to {Student or Pupil} is teaches.
			 
			 Woman is a type of Person.
			 
			 gender of Woman always has value Female.
			 
			 Woman is the same as {Person and (gender always has value Female)}.	// this 'has' is part of 'always has value' and is not optional
			 
			 Plato is a Student.
			 Socrates is a Professor, has teaches Plato.  // *** this 'has' should be optional
			 Socrates is a Professor, teaches Plato.  // *** this 'has' should be optional
			 Socrates has teaches Plato.				  // this 'has' is already optional, as shown in next statement
			 	Socrates teaches Plato.
			 	
			// A friend of Socrates is Plato.		// OK that this isn't supported, can be said more plainly as:
			 	Plato has friend Socrates.
			// Plato is a friend of Socrates. 		// OK that this isn't supported, can be said more plainly as:
			 	Socrates has friend Plato.
			// Plato is a friend of a friend of Socrates.	// as a graph pattern (see below), this is OK but as a declarative statement it is ambiguous
			// The age of a friend of Socrates is 23.	// as a graph bappern (see below), this is OK but as a declarative statement it is ambiguous
			 
			 Expr: a friend of a friend of Plato.
			 Expr: the age of a friend of Plato.
			 Expr: the age of a friend of Plato is 23.
			 
			 Sue is a Professor with friend (a Person with teaches Plato).		// 'with' here is optional, although in English we would say 'who'
			 	Sue is a Professor, friend (a Person teaches Plato).
			 
			 Rule R1: if x is a friend of a friend of Plato then Plato has friend x.	// *** 'has' should be optional
			 Rule R11: if x is a friend of a friend of Plato then Plato friend x.	// *** 'has' should be optional
			 Rule R1a: if Plato has friend x and x has friend y then Plato has friend y.	// *** all 3 'has' words should be optional
			 Rule R1a2: if Plato friend x and x friend y then Plato friend y.	// *** all 3 'has' words should be optional
			 
			 Ask: Plato has age x and x > 30 and x < 40.	// 'has' should be optional
			 Ask: Plato age x and x > 30 and x < 40.	// 'has' should be optional
			 Ask: x has age y and y > 30 and y < 40.		// 'has' should be optional
			 Ask: x age y and y > 30 and y < 40.		// 'has' should be optional
			 
			 Test: Socrates has teaches Plato.		// 'has' should be optional
			 Test: Socrates teaches Plato.		// 'has' should be optional
			 Test: not Socrates has teaches Plato.	// 'has' should be optional
			 Test: not Socrates teaches Plato.	// 'has' should be optional
			 Test: teaches of Socrates is not Plato.
		'''.assertNoErrors
	}
	
	@Ignore
	@Test
	def void testNegativeNumericConstants() {
		'''
			 uri "http://sadl.org/UsingNumericConstants.sadl" alias UsingNumericConstants.
			 
			 Foo is a class described by fprop with values of type decimal.
			 
			 MyFoo is a Foo with fprop PI.
			 MyFoo4 is a Foo with fprop 3.14.
			 
			 MyFoo3 is a Foo with fprop -PI.	// grammar error: this should work just like the negative number below
			 MyFoo5 is a Foo with fprop -3.14.

			 MyFoo5 has fprop -e.				// this should work also
		'''.assertNoErrors
	}
	
	@Test
	def void testNegatedConstantsPropOfSubject() {
		'''
			 uri "http://sadl.org/UsingNumericConstants.sadl" alias UsingNumericConstants.
			 
			 Foo is a class described by fprop with values of type decimal.
			 
			 // the following all work as PropOfSubject expressions
			 Test: fprop of MyFoo3 is PI.
			 Test: fprop of MyFoo3 is -PI.
			 Test: fprop of MyFoo3 is not PI.
			 Test: fprop of MyFoo3 is known.
			 Test: fprop of MyFoo3 is not known.
			 Test: fprop of MyFoo3 is e.
			 Test: fprop of MyFoo3 is -e.
		'''.assertNoErrors
	}
	
	@Ignore
	@Test
	def void testNegatedConstantsSubjHasProp() {
		'''
			 uri "http://sadl.org/UsingNumericConstants.sadl" alias UsingNumericConstants.
			 
			 Foo is a class described by fprop with values of type decimal.
			 
			 // some of the following do not work as SubjHasPop expressions
			 Test: MyFoo3 has fprop PI.
			 Test: MyFoo3 has fprop -PI.
			 Test: MyFoo3 has fprop not PI.		// grammar error
			 Test: MyFoo3 has fprop known.
			 Test: MyFoo3 has fprop not known.	// grammar error
			 Test: MyFoo3 has fprop e.
			 Test: MyFoo3 has fprop -e.
			 Test: MyFoo3 has fprop not e.		// grammar error
			 Test: MyFoo3 has fprop not -e.		// grammar error		
		'''.assertNoErrors
	}
	
	@Test
	def void testInvalidUseOfConstants() {
		var errs = newArrayList("mismatched input '*' expecting '.'")
		assertErrors('''
			 uri "http://sadl.org/UsingNumericConstants.sadl" alias UsingNumericConstants.
			 
			 Foo is a class described by fprop with values of type decimal.
			 
			 MyFoo2 is a Foo with fprop PI * -1 .	// this should not work--it involves a computation 
		''', errs)
	}
	
	@Test
	def void testUnitsOnNumbers() {
		''' uri "http://sadl.org/OntologyWithUnittedQuantity.sadl" alias OntologyWithUnittedQuantity.
		 
		 	George has height 70 inches.
		'''.assertNoErrors
	}
	
	@Test
	def void testConstants() {
		'''
			 uri "http://sadl.org/OntologyWithoutUnittedQuantity.sadl" alias OntologyWithoutUnittedQuantity.
			 
			 Person is a class described by gender with a single value of type Gender,
			 	described by age with values of type decimal,
			 	described by height with values of type decimal,
			 	described by weight with values of type decimal.
			 Gender is a class, can only be one of {Male, Female}. 	
			 Obese is a type of Person.
			 	
			 George is a Person with age 23, with height 70, with weight 165.	
			 
			 Mary is a Person with weight PI.
			// Emily is a Person with weight e .		// why does this statement fail?
			 
			 Rule DumbRule:
			 	if p is a Person
			 	then weight of p is age of p * PI * e^3.
		'''.assertNoErrors
	}
	
	@Test
	def void testTestWithCommas() {
		'''
			uri "http://sadl.imp/abs".
			
			Thingy is a top-level class.
			intVal describes Thingy has values of type int.
			flVal describes Thingy has values of type float.
			dblVal describes Thingy has values of type double.
			
			Rule Rule_Name
				given
					x is a Thingy
			//	if
					
				then
					intVal of x is abs(-1) and
					flVal of x is abs(-1.0) and
					dblVal of x is abs(-1.0).
					
			MyThingy is a Thingy.
			
			Test: MyThingy has intVal 1, has flVal 1.0, has dblVal 1.0 .
		'''.assertNoErrors
	}
	
	@Test
	def void testNewlineSeparation() {
		val model = '''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy, described by color with values of type string.
			
			Rule TwoThingiesNotEqual:
			given x1 is a Thingy
			x2 is a Thingy
			if x1 != x2
			then print(x1, " != ", x2).
		'''.sadl.contents.head as SadlModel
		
		Assert.assertEquals(2, model.elements.size)
		val rule = model.elements.get(1) as RuleStatement
		println(EmfFormatter.listToStr(rule.ifs))
	}
	
	@Test
	def void testQueryAsExpression() {
		val model = '''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy, described by color with values of type string.
			
			Test: (select x where x is a Thingy) is 2.
		'''.sadl.contents.head as SadlModel
		
		Assert.assertEquals(2, model.elements.size)
		val test = model.elements.get(1) as TestStatement
		Assert.assertTrue((test.tests.head as BinaryOperation).left instanceof SelectExpression)
	}
	
	@Test
	def void testRdfAndOwlNamespace() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			x has comment "set by Rule1".
			Test: select x,y where x has owl:equivalentClass y.
			Rule testrule
			if x is a Thingy
			then print(x, rdf:^type, "This works!").
		'''.assertNoErrors
	}
	
	@Ignore	// this grammar change was backed out because it changed precedence and broke things of the form "p1 of s1 is not p2 of s2"
	@Test
	def void testNegationOfObjectTriple() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Test: x has color not Red.
			Test: color of x is not Red.
		'''.assertNoErrors
	}
	
	@Test
	def void testConstantKnown() {
		'''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Test: dps of MyThingy2 is known.
			Test: dpf of MyThingy1 is not known.
		'''.assertNoErrors
	}
	
	@Inject extension DeclarationExtensions
	
	@Test
	def void testQNameWithEscape() {
		val model = '''
		 uri "http://sadl.org/LatticeToTree.sadl" alias LatticeToTree.
		 
		 Resource is a class described by LatticeToTree:value with values of type float.
		 Person is a class.
		 ^uses describes {Person or Resource} with values of type Resource.
		'''.sadl.contents.head as SadlModel
		
		Assert.assertEquals(3, model.elements.size)
		val test = model.elements.get(0) as SadlClassOrPropertyDeclaration
		val psr = test.describedBy.get(0) as SadlProperty
		Assert.assertEquals("value", psr.nameDeclarations.head.concreteName)
		Assert.assertEquals("LatticeToTree:value", psr.nameDeclarations.head.conceptQualifiedName)
	}

}
