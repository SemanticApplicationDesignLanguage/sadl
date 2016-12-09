/*
 * © 2014-2016 General Electric Company – All Rights Reserved
 *
 * This software and any accompanying data and documentation are CONFIDENTIAL 
 * INFORMATION of the General Electric Company (“GE”) and may contain trade secrets 
 * and other proprietary information.  It is intended for use solely by GE and authorized 
 * personnel.
 */
package com.ge.research.sadl.tests.model

import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert

@RunWith(XtextRunner)
//@InjectWith(RequirementsInjectorProvider)
class SadlModelProcessorTestTypeChecking extends AbstractProcessorTest {
	
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> sadlProcessorProvider
	@Inject IPreferenceValuesProvider preferenceProvider
	
	@Test
	def void testRuleVariableAsDomain() {
		val sadlImplicit = '''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			Event is a class.
			impliedProperty is a type of annotation.
			UnittedQuantity is a class,
			 	described by ^value with values of type decimal,
			 	described by unit with values of type string.
		 '''.sadl
		val sadlModel = '''
			 uri "http://sadl.org/Test1.sadl" alias Test1.
			 import "http://sadl.org/sadlimplicitmodel".		// this shouldn't be needed because it's implicit...	 
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
		sadlModel.assertOnlyWarningsOrInfo
		val issues1 = validationTestHelper.validate(sadlModel)
//		val List<Issue> issues1= newArrayList
//		sprocessor1.onValidate(sadlModel1, new ValidationAcceptor([issues1 += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(sadlModel1)))
		Assert.assertNotNull(issues1)
		Assert.assertTrue(issues1.size == 2)
		Assert.assertEquals(issues1.get(0).message, "Variable p2 is of type http://sadl.org/Test1.sadl#Pet which is not in domain of property http://sadl.org/Test1.sadl#owns")
		Assert.assertEquals(issues1.get(1).message, "Variable p1 is of type http://sadl.org/Test1.sadl#Person which is not in domain of property http://sadl.org/Test1.sadl#caredFor")
	}
	
	@Test
	def void testUserDefinedEquation() {
		val sadlImplicit = '''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			Event is a class.
			impliedProperty is a type of annotation.
			UnittedQuantity is a class,
			 	described by ^value with values of type decimal,
			 	described by unit with values of type string.
		 '''.sadl
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
			 
			 Rule AgeRule: if p is a LivingThing then age of p is subtractDates(now(), dateOfBirth of p, "y"). 		
		'''.sadl
		sadlModel.assertNoErrors
	}
	
	@Test
	def void testUndefinedEquation() {
		val sadlImplicit = '''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			Event is a class.
			impliedProperty is a type of annotation.
			UnittedQuantity is a class,
			 	described by ^value with values of type decimal,
			 	described by unit with values of type string.
		 '''.sadl
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
		sadlModel.assertOnlyWarningsOrInfo
		val issues1 = validationTestHelper.validate(sadlModel)
//		val List<Issue> issues1= newArrayList
//		sprocessor1.onValidate(sadlModel1, new ValidationAcceptor([issues1 += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(sadlModel1)))
		Assert.assertNotNull(issues1)
		Assert.assertTrue(issues1.size == 1)
		Assert.assertEquals(issues1.get(0).message, "Variable p2 is of type http://sadl.org/Test1.sadl#Pet which is not in domain of property http://sadl.org/Test1.sadl#owns")
	}
}
