/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.tests.AbstractSADLModelProcessorTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith
import static org.junit.Assert.assertEquals
import static org.junit.Assert.assertNotNull
import static org.junit.Assert.assertTrue
import com.google.common.collect.Iterables

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlModelProcessorTypeCheckingTest extends AbstractSADLModelProcessorTest {
	
	@Inject ValidationTestHelper validationTestHelper
	
	@Test
	def void testRuleVariableAsDomain() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 
			 Rule R1: if p1 is a Person and p2 is a Pet and p2 has owns p1 then p1 has caredFor true.
 		'''.sadl
		val issues = validationTestHelper.validate(sadlModel)
		for (issue:issues) {
			println(issue.toString)
		}
		Assert.assertNotNull(issues)
		Assert.assertEquals(3, issues.size)
		Assert.assertEquals(issues.get(0).message, "http://sadl.org/Test1.sadl#Pet is not in domain of property http://sadl.org/Test1.sadl#owns")
		Assert.assertEquals(issues.get(1).message, "Class 'http://sadl.org/Test1.sadl#Person' is not in the range of property 'http://sadl.org/Test1.sadl#owns'")
		Assert.assertEquals(issues.get(2).message, "http://sadl.org/Test1.sadl#Person is not in domain of property http://sadl.org/Test1.sadl#caredFor")
	}
	
	@Test
	def void testUserDefinedEquation1() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime,
			 	described by age with values of type float.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 
			 External subtractDates(dateTime t1, dateTime t2, string u) returns float : "http://sadl.org/builtins/subtractDates".
			 
			 // to use the locally defined equation, it must be prefixed with the local model alias
			 Rule AgeRule: if p is a LivingThing then age of p is Test1:subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testSimpleRule() {
		val sadlModel = '''
			 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
			 
			 System is a class,
			    described by var1 with a single value of type int,
			    described by var2 with a single value of type int,
			    described by var3 with a single value of type int,
			    described by var4 with a single value of type int.
			     	
			 Rule example-1:
			 	if var2 of System is not var3 of System
			 	then var1 of System is  var2 of System + var3 of System.
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testSimpleRule2() {
		val sadlModel = '''
			 uri "http://sadl.org/rulevars2.sadl" alias rulevars2.
			 
			 System is a class,
			    described by var1 with a single value of type int,
			    described by var2 with a single value of type int,
			    described by var3 with a single value of type int,
			    described by var4 with a single value of type int.
			     	
			 Rule example-1:
			 	if var2 of System is not var3 of System and
			 	x is var2 of System + var3 of System
			 	then var1 of System is  x.
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testRuleMultipleClassMembership() {
		val sadlModel = '''
		uri "http://sadl.org/gh633.sadl" alias gh633.
		 
		B is a class.
		C is a class.
		D is a class.
		
		NotC is the same as not C.
		
		Rule r:
		if x is a B and x is a NotC
		then x is a D.
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testUserDefinedEquation2() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime,
			 	described by age with values of type float.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 
			 External subtractDates(dateTime t1, dateTime t2, string u) returns float : "http://sadl.org/builtins/subtractDates".
			 
			 Rule AgeRule: if p is a LivingThing then age of p is Test1:subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testUserDefinedEquation3() {
		val sadlModel = '''
			 uri "http://sadl.org/SpeedOfSound.sadl" alias sos.
			 Gas is a class.
			 Air is a type of Gas.
			 speedOfSound describes Gas with values of type decimal.
			 altitude describes Air with values of type decimal.
			 
			 primitiveData is a type of {decimal or boolean or string}.
			 
			 External cg(ExternalEquation eq, primitiveData arg, ...) returns decimal: "http://com.ge.research.darpa.aske.sos".
			 
			 External sosfromalt(decimal alt) returns decimal: "http://com.ge.research.darpa.aske.kchain.sosairfromalt".
			 
			 Rule SpeedOfSoundInAirGivenAltitude:
			   if air is some Air and alt is altitude of air and
			   sosair is  cg(sosfromalt, alt)
			   then speedOfSound of air is sosair.
   		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testUserDefinedEquation4() {
		val sadlModel = '''
			 uri "http://sadl.org/SpeedOfSound.sadl" alias sos.
			 Gas is a class.
			 Air is a type of Gas.
			 speedOfSound describes Gas with values of type decimal.
			 altitude describes Air with values of type decimal.
			 
			 External sosfromalt(decimal alt, string unt) returns decimal,string: "http://com.ge.research.darpa.aske.kchain.sosairfromalt".
			 
			 Rule SpeedOfSoundInAirGivenAltitude:
			   if air is some Air and alt is altitude of air and
			   [sosair,unt] is  sosfromalt(alt, "ft")
			   then speedOfSound of air is sosair.
   		'''.sadl
		sadlModel.assertError('''Object of conclusion triple 'rdf(air, sos:speedOfSound, sosair)' is not bound in rule premises''')
	}
	
	@Test
	def void testNestedInstanceDomain() {
		val sadlModel = '''
		 uri "http://sadl.org/GrammarCoverage.sadl" alias GrammarCoverage.
		 
		 Person is a class, described by friend with values of type Person .
		 
		 {Professor, Teacher, Student, Pupil} are types of Person.
		 relationship of {Professor or Teacher} to {Student or Pupil} is teaches.
		 
		 Plato is a Student.
		 Sue is a Professor with friend (a Person with teaches Plato).
		 Sue is a Professor, friend (a Person teaches Plato).
 	 	'''.sadl
 	 	val issues = validate(sadlModel)
 	 	for (issue : issues) {
 	 		println(issue.toString)
 	 	}
		val String[] errors = #["The subject  is not in domain of property http://sadl.org/GrammarCoverage.sadl#teaches",
			"The subject  is not in domain of property http://sadl.org/GrammarCoverage.sadl#teaches"
		] 	
 	 	sadlModel.assertErrors(errors)
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_15() {
		'''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
			Foo is a class.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl" as i1.
			import "http://sadl.org/I2.sadl" as i2.
			
			current_1:Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_16() {
		'''
			uri "http://sadl.org/I1.sadl" alias I1.
			Foo is a class.
		'''.sadl;

		'''
			uri "http://sadl.org/I2.sadl" alias I2.
			Foo is a class.
		'''.sadl;

		val model_1 = '''
			uri "http://sadl.org/Current_1.sadl" alias current_1.
			import "http://sadl.org/I1.sadl".
			import "http://sadl.org/I2.sadl".
			
			I1:Foo is a class.
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		for (issue : issues_1) {
			println(issue.toString)
		}
		model_1.assertError("Declaration of concepts in another namespace not supported")
//		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}
	
	@Test
	def void testLocalVsImportedNames_GH_226_x() {

		val model_1 = '''
			uri "http://sadl.org/ExternalFooBar.sadl" alias efb.
			
			External efb:addOne(decimal X) returns decimal: "http://some/other/ns#addOne".
			
			Rule R1:
				if x = 1 and y = builtinfunctions:addOne(x) and z = efb:addOne(x)
				then print("builtinfunctions:addOne returned ",y," , efb:addOne returned ", z).
		'''.sadl.enableAmbiguousNameDetection;

		val issues_1 = validate(model_1);
		assertEquals(Iterables.toString(issues_1), 4, issues_1.size);
	}
	
	@Test
	def void testIncompletelyDefinedEquation() {
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 
			 PhysicalThing is a class,
			 	described by weight with values of type UnittedQuantity,
			 	described by density with values of type float.
			 	
			 LivingThing is a type of PhysicalThing,
			 	described by dateOfBirth with values of type dateTime,
			 	described by age with values of type float.
			 	
			 Mammal is a type of LivingThing,
			 	described by child with values of type Mammal.
			 	
			 Person is a type of Mammal.
			 child of Person only has values of type Person.
			 
			 Pet is a class, described by caredFor with a single value of type boolean.
			 owns describes Person with values of type Pet.
			 		 
			 Rule AgeRule: if p is a LivingThing then age of p is subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
//		val String[] errors = #["Number of arguments does not match function declaration",
//			"xsd:dateTime, an RDF datatype  xsd:dateTime, cannot operate (argument) with No type check info generated.",
//			"age, a datatype property with range  xsd:float, cannot be compared (is) with No type check info generated.",
//			"Type comparison not possible"
//		]
//		sadlModel.assertErrors(errors)
		sadlModel.assertError("age, a datatype property with range  xsd:float, cannot be compared (is) with No type check info generated.")
	}
	
	@Test
	def void testTypedList_00() {
		'''
		 uri "http://sadl.org/UsingListExpression.sadl" alias UsingListExpression.
		 YooHoo is a class.
		 l1 is the YooHoo List [yh1, yh2, yh3, yh4].
		 '''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			assertTrue(issues.size == 0)
		]
	}
	
	@Test
	def void testTypedList_01() {
		'''
		 uri "http://sadl.org/UsingListExpression.sadl" alias UsingListExpression.
		 YooHoo is a class.
		 YooHooList is a type of YooHoo List, described by selectedElement with values of type YooHoo.
		 l1 is the YooHooList [yh1, yh2, yh3, yh4].
		 '''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			assertTrue(issues.size == 0)	// content requires articles to be enabled, 2 var not bound in premises
		]
	}
	
	@Test
	def void testTypedList_06() {
		val sadlModel = '''
		uri "http://sadl.org/list.sadl3" alias lst3.
		
		B is a class.
		C is a class.
		D is a class described by p with values of type BC List .
		
		BC is the same as {B or C}.
		
		i1 is a B.
		i2 is a C.
		
		i3 is a D has p [i1,i2].
		'''.sadl
		val issues_1 = validate(sadlModel);
		assertEquals(Iterables.toString(issues_1), 0, issues_1.size);
	}
	
	@Test
	def void testTypedList_07() {
		val sadlModel = '''
			 uri "http://sadl.org/list.sadl" alias lst.
			 
			 B is a class.
			 C is a class.
			 D is a class described by p with values of type B List.
			 
			 i1 is a {B and C}.
			 
			 i2 is a D with p [i1].
	 		'''.sadl
	 		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.message)
		}
		assertEquals(Iterables.toString(issues), 0, issues.size);
	}
	
	@Test
	def void testTypedList_08() {
		val sadlModel = '''
			 uri "http://sadl.org/list.sadl" alias lst.
			 
			 B is a class.
			 C is a class.
			 D is a class described by p with values of type B List.
			 
			 i1 is a {B and C}.
			 i2 is a B.
			 
			 i3 is a D with p [i1, i2].
	 		'''.sadl
	 		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.message)
		}
		assertEquals(Iterables.toString(issues), 0, issues.size);
	}
	
	@Test
	def void testTypedList_09() {
		val sadlModel = '''
			 uri "http://sadl.org/list.sadl" alias lst.
			 
			 B is a class.
			 C is a class.
			 D is a class described by p with values of type B List.
			 
			 i1 is a {B and C}.
			 i2 is a B.
			 i3 is a C.
			 
			 i4 is a D with p [i1, i2, i3].
	 		'''.sadl
	 		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.message)
		}
		// expect error on last line as i5 could be a C
		sadlModel.assertError("TypeCheckInfo(B (List), type of an unnamed typed list class, range of property p), cannot be compared (is) with TypeCheckInfo(the List [TypeCheckInfo(i1 (type B and C),TypeCheckInfo(http://sadl.org/list.sadl#i2, range, B),TypeCheckInfo(http://sadl.org/list.sadl#i3, range, C)].")
	}
	
	@Test
	def void testTypedList_10() {
		val sadlModel = '''
		 uri "http://sadl.org/list.sadl" alias lst.
		 
		 B is a class.
		 C is a class.
		 D is a class described by p with values of type B List.
		 
		 i1 is a {B and C}.
		 i2 is a D with p [i1].
		 
		 i3 is a B.
		 i3 is a C.
		 i4 is a D with p [i3].
		 
		 i5 is a {B or C}.
		 i6 is a D with p [i5].
		'''.sadl
		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.message)
		}
		// expect error on last line as i5 could be a C
		sadlModel.assertError("TypeCheckInfo(B (List), type of an unnamed typed list class, range of property p), cannot be compared (is) with TypeCheckInfo(the List [TypeCheckInfo(i5 (type B or C)].")
	}

	@Test
	def void testFirstElement_01() {
		'''
		 uri "http://sadl.org/UsingListExpression.sadl" alias UsingListExpression.
		 YooHoo is a class.
		 l1 is the YooHoo List [yh1, yh2, yh3, yh4].
		 Rule ListFirst:
		   if l1 is a YooHoo List and 
		   		first element of l1 is le1 then print(le1).
		 '''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(issues.size == 1)	
			issues.get(0).equals("Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information")
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule ListFirst:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHoo (List)) and firstElement(UsingListExpression:l1,le1) then print(le1)."))
		]
	}
	
	@Test
	def void testFirstElement_01Compare() {
		'''
		 uri "http://sadl.org/UsingListExpression.sadl" alias UsingListExpression.
		 YooHoo is a class.
		 l1 is the YooHoo.
		 Rule ListFirst:
		   if l1 is a YooHoo then print(l1).
		 '''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(issues.size == 1)	
			issues.get(0).equals("Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information")
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule ListFirst:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHoo) then print(UsingListExpression:l1)."))
		]
	}

	@Test
	def void testFirstElement_02() {
		'''
		 uri "http://sadl.org/UsingListExpression.sadl" alias UsingListExpression.
		 YooHoo is a class.
		 YooHooList is a type of YooHoo List, described by selectedElement with values of type YooHoo.
		 l1 is the YooHooList [yh1, yh2, yh3, yh4].
		 Rule ListFirst:
		   if l1 is a YooHooList and 
		   		first element of l1 is le1 then l1 has selectedElement le1 and print(le1).
		 '''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(issues.size == 1)	
			issues.get(0).equals("Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information")
			assertTrue(rules.size == 1)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					"Rule ListFirst:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and firstElement(UsingListExpression:l1,le1) then rdf(UsingListExpression:l1, UsingListExpression:selectedElement, le1) and print(le1)."))
		]
	}
	
	@Test
	def void testListBuiltins_01() {
		val issueList = newArrayList(
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 1, was found, but the reasoner and translator pair does not provide further type-checking information",
			"Built-in function, parameter 2, was found, but the reasoner and translator pair does not provide further type-checking information"
			
		)
		'''
		  uri "http://sadl.org/UsingListExpression.sadl" alias UsingListExpression.
		  
		  YooHoo is a class.
		  YooHooList is a type of YooHoo List, described by selectedElement with values of type YooHoo.
		  l1 is the YooHooList [yh1, yh2, yh3, yh4].
		  
		  Rule ListFirst:
		    if l1 is a YooHooList and 
		    		first element of l1 is le1 then l1 has selectedElement le1 and print("ListFirst: ", le1).
		    		
		  Rule ListLast:
		  	if l1 is a YooHooList and last element of l1 is lel then print("Last element of l1: ", lel).   	
		  	
		  Rule ListAfter:
		  	if l1 is a YooHooList and element after yh2 in l1 is lea then print("Element after yh2: ", lea). 
		    
		  Rule ListBefore:
		  	if l1 is a YooHooList and element before yh3 in l1 is leb then print("Element before yh3: ", leb). 
		 
		  Rule ListElementAt:
		  	if l1 is a YooHooList and element 3 of l1 is le3 then print("Element 3: ", le3). 
		 
		  Rule ListIndex:
		  	if l1 is a YooHooList and index of yh2 in l1 is idx then print("Index of yh2: ", idx). 
		  	
		  Rule ListLength:
		  	if l1 is a YooHooList and len is length of l1 then print("Length of l1: ", len).
		 '''.assertValidatesTo[ jenaModel, rules, cmds, issues, processor |
			assertNotNull(jenaModel)
			if (issues !== null) {
				for (issue : issues) {
					System.out.println(issue.message)
				}
			}
			if (rules !== null) {
				for (rule : rules) {
					System.out.println(rule.toString)
				}
			}
			assertTrue(issues.size == 14)
			var idx = 0
			for (iss : issues) {
				assertEquals(issueList.get(idx), iss.message)
				idx++
			}
			assertTrue(rules.size == 7)
			assertTrue(
				processor.compareTranslations(rules.get(0).toString(),
					'Rule ListFirst:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and firstElement(UsingListExpression:l1,le1) then rdf(UsingListExpression:l1, UsingListExpression:selectedElement, le1) and print("ListFirst: ",le1).'))
			assertTrue(
				processor.compareTranslations(rules.get(1).toString(),
					'Rule ListLast:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and lastElement(UsingListExpression:l1,lel) then print("Last element of l1: ",lel).'))
			assertTrue(
				processor.compareTranslations(rules.get(2).toString(),
					'Rule ListAfter:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and elementAfter(UsingListExpression:l1,UsingListExpression:yh2,lea) then print("Element after yh2: ",lea).'))
			assertTrue(
				processor.compareTranslations(rules.get(3).toString(),
					'Rule ListBefore:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and elementBefore(UsingListExpression:l1,UsingListExpression:yh3,leb) then print("Element before yh3: ",leb).'))
			assertTrue(
				processor.compareTranslations(rules.get(4).toString(),
					'Rule ListElementAt:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and elementInList(UsingListExpression:l1,3,le3) then print("Element 3: ",le3).'))
			assertTrue(
				processor.compareTranslations(rules.get(5).toString(),
					'Rule ListIndex:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and index(UsingListExpression:l1,UsingListExpression:yh2,idx) then print("Index of yh2: ",idx).'))
			assertTrue(
				processor.compareTranslations(rules.get(6).toString(),
					'Rule ListLength:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and length(UsingListExpression:l1,len) then print("Length of l1: ",len).'))
		]
/*
Rule ListFirst:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and firstElement(UsingListExpression:l1,le1) then rdf(UsingListExpression:l1, UsingListExpression:selectedElement, le1) and print("ListFirst: ",le1).
Rule ListLast:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and lastElement(UsingListExpression:l1,lel) then print("Last element of l1: ",lel).
Rule ListAfter:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and elementAfter(UsingListExpression:l1,UsingListExpression:yh2,lea) then print("Element after yh2: ",lea).
Rule ListBefore:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and elementBefore(UsingListExpression:l1,UsingListExpression:yh3,leb) then print("Element before yh3: ",leb).
Rule ListElementAt:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and elementInList(UsingListExpression:l1,3,le3) then print("Element 3: ",le3).
Rule ListIndex:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and index(UsingListExpression:l1,UsingListExpression:yh2,idx) then print("Index of yh2: ",idx).
Rule ListLength:  if rdf(UsingListExpression:l1, rdf:type, UsingListExpression:YooHooList (List)) and length(UsingListExpression:l1,len) then print("Length of l1: ",len).
		*/
	}
	
	@Test
	def void testIntersectionClass_01() {
		val sadlModel = '''
		 uri "http://sadl.org/NoList.sadl" alias nolist.
		 
		 B is a class.
		 C is a class.
		 BnC is the same as {B and C}.
		 BoC is the same as {B or C}.
		 
		 D is a class described by p1 with values of type BnC,
		 	described by p2 with values of type BoC.
		 	
		 B1 is a B.
		 C1 is a C. 	
		 BnC1 is a BnC.
		 BoC1 is a BoC.
		 
		 D1 is a D with p1 B1, with p2 C1.	// this is worthy of a warning
		 
		 D2 is a D with p1 C1, with p2 B1.	// likewise
		'''.sadl
		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.message)
		}
		// expect warning that p1 values may not be in range (is B or C but can't know if in BnC)
		val String[] errors = #[
			"p1, an object property with range  http://sadl.org/NoList.sadl#BnC, cannot be compared (is) with B1, an instance of type  http://sadl.org/NoList.sadl#B.",
			"p1, an object property with range  http://sadl.org/NoList.sadl#BnC, cannot be compared (is) with C1, an instance of type  http://sadl.org/NoList.sadl#C."
		]
		sadlModel.assertErrors(errors)
	}
	
	@Test
	def void testIntersectionClass_02() {
		val sadlModel = '''
		 uri "http://sadl.org/NoList2.sadl" alias nolist2.
		 
		 B is a class.
		 C is a class.
		 
		 D is a class described by p1 with values of type {B and C},
		 	described by p2 with values of type {B or C}.
		  
		 B1 is a B.
		 C1 is a C. 	
		 BnC1 is a {B and C}.
		 BoC1 is a {B or C}.
		 
		 D1 is a D with p1 B1, with p2 C1.	// this is worthy of a warning
		 
		 D2 is a D with p1 C1, with p2 B1.	// likewise
		'''.sadl
		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.message)
		}
		// expect warning that p1 values may not be in range (is B or C but can't know if in BnC)
		val String[] errors = #[
			"p1, an object property with range  http://sadl.org/NoList2.sadl#B and http://sadl.org/NoList2.sadl#C, cannot be compared (is) with B1, an instance of type  http://sadl.org/NoList2.sadl#B.",
			"p1, an object property with range  http://sadl.org/NoList2.sadl#B and http://sadl.org/NoList2.sadl#C, cannot be compared (is) with C1, an instance of type  http://sadl.org/NoList2.sadl#C."
		]
		sadlModel.assertErrors(errors)
	}

	@Ignore
	@Test
	def void testAgeOfMarthaIsAnInt() {
		val sadlModel = '''
		 uri "http://sadl.org/AgeOfMarthaIsAnInt.sadl" alias AgeOfMarthaIsAnInt.
		 
		 Person is a class described by age with values of type data.
		 Martha is a Person.
		 
		 Rule R1: if age of Martha is an int then Martha has age 3.
		'''.sadl
		val issues = validate(sadlModel)
		for (issue : issues) {
			println(issue.toString)
		}
		
	}
}
