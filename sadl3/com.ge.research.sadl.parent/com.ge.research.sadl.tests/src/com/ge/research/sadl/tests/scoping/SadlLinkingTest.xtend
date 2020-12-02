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

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.common.base.Stopwatch
import java.util.concurrent.TimeUnit
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlLinkingTest extends AbstractLinkingTest {

	@Test
	def void testLinkingQNames() {
		'''
			uri "http://sadl.org/allqnames.sadl" alias aqn.
			
			[aqn:Shape] is a class.
			[aqn:area] describes <aqn:Shape> with values of type float.
			
			[aqn:MyShape] is a <aqn:Shape> with <aqn:area> 23 .
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingSimpleNames() {
		'''
			uri "http://sadl.org/allqnames.sadl" alias aqn.
			
			[Shape] is a class.
			[area] describes <Shape> with values of type float.
			
			[MyShape] is a <Shape> with <area> 23 .
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinking2Files() {
		'''
			uri "http://sadl.org/allqnames.sadl" alias aqn.
			
			[Shape] is a class.
			[area] describes <Shape> with values of type float.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/allqnames2.sadl" alias aqn2.
			import "http://sadl.org/allqnames.sadl".
			
			[MyShape] is a <Shape> with <area> 23 .
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinking3Files() {
		'''
			uri "http://sadl.org/allqnames.sadl" alias aqn.
			
			[Shape] is a class.
			[area] describes <Shape> with values of type float.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/allqnames2.sadl" alias aqn2.
			import "http://sadl.org/allqnames.sadl".
			
			[MyShape] is a <Shape> with <area> 23 .
			[Rectangle] is a type of <Shape>, described by [height] with values of type float, described by [width] with values of type float.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org.allqames3.sadl" alias aqn3.
			import "http://sadl.org/allqnames2.sadl".
			
			[MyRect] is a <Rectangle> with <height> 3, with <width> 4, with <area> 12 .
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingEquations() {
		'''
			uri "http://sadl.org/equations".
			
			Equation [foo](int [b], int [c]) returns int : <b> + <c>.
			Equation [bar](int [b], int [c]) returns int: <b> + <c>.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingEquations_02() {
		'''
			uri "http://sadl.org/equations".
			
			Equation [foo](int [b], int [c]) returns int : <b> + <c>.
			Equation [bar](int [b], int [c]) returns int: <foo>(<b>, <c>).
		'''.assertLinking[sadl]
	}

	// https://github.com/crapo/sadlos2/issues/376
	@Test
	def void testExternalEquationStatement_GH_376() {
		'''
		uri "http://sadl.org/SemanticConstraintsExample.sadl" alias SemanticConstraintsExample.
		
		Circle is a class described by radius with values of type float.
		
		Equation areaBetweenConcentricCircles(float [largerRadius], float [smallerRadius] (<largerRadius> > <smallerRadius>)) returns float :
			return PI*(<largerRadius>^2 - <smallerRadius>^2).
		
		External areaBetweenConcentricCircles2(float [largerRadius], float [smallerRadius] (<largerRadius> > <smallerRadius>)) returns float :
			"uri".
		
		Air is a class, described by altitude with values of type UnittedQuantity,
			described by temperature with values of type UnittedQuantity.
		
		External troposphereTemperature(UnittedQuantity [alt] (altitude of some Air and unit of <alt> is "ft" and ^value of <alt> <= 36152))
			returns UnittedQuantity (temperature of the Air): "uri".
		
		Equation stratosphereTemperature(UnittedQuantity [alt] (altitude of some Air and unit of <alt> is "ft" and
			^value of <alt> > 36152 and ^value of <alt> <= 82345))
				returns UnittedQuantity (temperature of the Air) : return (a UnittedQuantity with ^value 389.98, with unit "F").
		'''.assertLinking[sadl]
	}

	// https://github.com/crapo/sadlos2/issues/67
	@Test
	def void testLinkingPrecedence_01() {
		'''
			uri "http://sadl.org/equations".
			
			[area] describes <Shape> with values of type float.
			[Shape] is a class.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_02() {
		'''
			uri "http://sadl.org/equations".
			
			[area] describes <Rectangle> with values of type float.
			[Rectangle] is a type of <Shape>.
			[Shape] is a class.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_03() {
		'''
			uri "http://sadl.org/equations".
			
			[area] describes <Rectangle> with values of type float.
			A Foo is a [Rectangle] only if <area> has one value of type float.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_04() {
		'''
			uri "http://sadl.org/equations".
			
			[area] describes <Rectangle> with values of type float.
			A Foo is a [Rectangle] only if <area> has one value of type float.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_05() {
		'''
			uri "http://sadl.org/classes.sadl" alias clsses.
			
			[LivingThing] is a class.
			[Mammal] is a type of <LivingThing> described by [friend] with values of type <Mammal>.
			{[Earth], [Water], [Air], [Fire], [Aether]} are classes.
			
			{[Cow], [Horse], [Dog], [Cat], [Human]} are types of <Mammal>.
			
			A <Dog> is a [Pet] only if <friend> has at least 1 value of type <Human>.
			
			[NonLivingThing] is the same as not <LivingThing>.
			
			[Consumable] is a class.
			[Liquid] is a class.
			[PotableLiquid] is the same as {<Consumable> and <Liquid>}.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_06() {
		'''
			 uri "http://sadl.org/instances.sadl" alias nsinstances.
			 
			 import "http://sadl.org/classes.sadl".
			 
			[Gregory] is a Human with friend <Lassie>. 
			 
			[Lassie] is a Dog.	// this is the declaration of Lassie
			{[MrEd], [SeaBuiscu]t, [Flicka]} are instances of Horse.
			 
			[Winter] is a <Season>. 
			 
			[Season] is a class, must be one of {<Spring>, [Summer], [Fall], <Winter>}.
			
			[Spring] is a Season.	
			 
			[Book] is a class.
			[HarperLeeBook] is a type of <Book>, 
				can only be one of {[ToKillaMockingbird], <GoSetaWatchman>}.	
			[GoSetaWatchman] is a <Book>.	
			
			[Holiday] is a class described by [when] with values of type <Season>.
			[NewYear] is a Holiday with when <Winter>.
			[BastilleDay] is a Holiday with when <Summer>.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_07() {
		'''
			uri "http://sadl.org/properties.sadl" alias properties.
			
			[Shape] is a class described by <area>.
			
			[area] is a property.	
			
			[displayColor] is a type of annotation.
			[surfaceArea] is a type of <area>.		
			
			[Circle] is a type of <Shape>.
			[radius] describes <Circle> with values of type float.	
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_08() {
		'''
			uri "http://sadl.org/TestRequrements/ontology2" alias ont2 version "$Revision:$ Last modified on   $Date:$". 
			
			[Vehicle] is a class, 
				described by [number_of_wheels] with values of type int,
				described by [number_of_seats] with values of type int,
				described by [color] with values of type string.
				
			[Unicycle] is a type of <Vehicle>.
			<number_of_wheels> of <Unicycle> always has value 1 .
			<number_of_seats> of <Unicycle> always has value 1 .
			
				
			[Bicycle] is a type of <Vehicle>.
			<number_of_wheels> of <Bicycle> always has value 2 .
			<number_of_seats> of <Bicycle> always has value 1 .
			
			[Car] is a type of <Vehicle>.
			<number_of_wheels> of <Car> always has value 4 .
			
			[MyCar] is a <Car> with <color> "blue", with <number_of_seats> 4 .
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_09() {
		'''
			 uri "http://sadl.org/ListDecls.sadl" alias ListDecls.
			 
			Person is a class described by gender with values of type Gender,
				described by age with values of type int, 
				described by height with values of type float,
				described by child with values of type Person,
				described by orderedChildren with values of type Person List,
				described by orderedFemaleChildren with values of type Person List.
				
			Gender is a class, must be one of {Male, Female}.
			
			GeorgesChildren is the Person List [Sue, Wayne, Fred].
			AllKnownChildren is the Person List [Sue, Wayne, Fred, John, Francis, Boyd].
			George is a Person with orderedChildren GeorgesChildren.
			
			NotGeorgesChildren is a Person List.
			
			Rule R1:
			if  p is a Person
				p has orderedChildren oc
			then 
				p has orderedChildren (the sublist of oc matching gender is Female).
			
			Rule R2:
			then 
				NotGeorgesChildren is (the sublist of AllKnownChildren matching GeorgesChildren does not contain value).
				
			Rule R3:
			if  [p] is a Person and
				<p> has orderedChildren [oc] and
				<oc> contains [c]
			then
				<p> has child <c>.
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingPrecedence_10() {
		'''
			uri "http://assert/Properties" alias Properties.
			reference_class is a type of annotation.
			reference_property is a type of annotation.
			reference_range is a type of annotation.
			reference_instance is a type of annotation.
			INTERFACE_DEFINITION is a class.
			  functional_max describes INTERFACE_DEFINITION with values of type decimal.
			  functional_min describes INTERFACE_DEFINITION with values of type decimal.
			  physical_max describes INTERFACE_DEFINITION with values of type decimal.
			  physical_min describes INTERFACE_DEFINITION with values of type decimal.
			  tolerance describes INTERFACE_DEFINITION with values of type decimal.
			  resolution describes INTERFACE_DEFINITION with values of type decimal.
			  physical_mapping describes INTERFACE_DEFINITION with values of type PHYSICAL_MAPPING.
			  PHYSICAL_MAPPING is a class.
			    physical_value describes PHYSICAL_MAPPING with values of type decimal.
			//SectionObjectId=ASSERT_DM-2
			example_1 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-3
			example_2 is a property.
			//SectionObjectId=ASSERT_DM-4
			example_3 is a property.
			//SectionObjectId=ASSERT_DM-5
			example_4 is a property.
			//SectionObjectId=ASSERT_DM-6
			example_5 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-7
			sent is a property.
			//SectionObjectId=ASSERT_DM-8
			received is a property.
			//SectionObjectId=ASSERT_DM-9
			example_7_1 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-50
			example_7_2 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-10
			example_7_6 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-11
			example_8 is a property.
			//SectionObjectId=ASSERT_DM-12
			example_9 is a property.
			//SectionObjectId=ASSERT_DM-13
			example_10 is a property with values of type int.
			//SectionObjectId=ASSERT_DM-14
			example_11 is a property.
			//SectionObjectId=ASSERT_DM-51
			example_12 is a property.
			//SectionObjectId=ASSERT_DM-52
			example_13 is a property.
			//SectionObjectId=ASSERT_DM-53
			example_14 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-54
			example_15 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-69
			example_16 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-70
			example_17 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-71
			example_18 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-77
			example_19 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-81
			example_20 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-82
			example_21 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-83
			example_22 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-84
			example_23 is a property.
			//SectionObjectId=ASSERT_DM-15
			input_1 is a property.
			//SectionObjectId=ASSERT_DM-16
			input_2 is a property.
			//SectionObjectId=ASSERT_DM-17
			input_3 is a property.
			//SectionObjectId=ASSERT_DM-18
			input_4 is a property.
			//SectionObjectId=ASSERT_DM-19
			input_5 is a property.
			//SectionObjectId=ASSERT_DM-20
			input_6 is a property.
			//SectionObjectId=ASSERT_DM-21
			input_7 is a property.
			//SectionObjectId=ASSERT_DM-22
			input_8 is a property.
			//SectionObjectId=ASSERT_DM-23
			input_9 is a property.
			//SectionObjectId=ASSERT_DM-24
			input_10 is a property.
			//SectionObjectId=ASSERT_DM-25
			input_11 is a property.
			//SectionObjectId=ASSERT_DM-27
			event_data_1 is a property.
			//SectionObjectId=ASSERT_DM-30
			event_data_1 is a property.
			//SectionObjectId=ASSERT_DM-33
			event_data_2 is a property.
			//SectionObjectId=ASSERT_DM-34
			event_data_3 is a property.
			//SectionObjectId=ASSERT_DM-32
			event_data_1 is a property.
			//SectionObjectId=ASSERT_DM-78
			result is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-73
			event_data_1 is a property.
			//SectionObjectId=ASSERT_DM-79
			result is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-75
			event_data_1 is a property.
			//SectionObjectId=ASSERT_DM-76
			event_data_2 is a property.
			//SectionObjectId=ASSERT_DM-40
			property_1 is a property.
			//SectionObjectId=ASSERT_DM-42
			property_2 is a property.
			//SectionObjectId=ASSERT_DM-44
			property_3 is a property with values of type boolean.
			//SectionObjectId=ASSERT_DM-46
			_value is a property.
			//SectionObjectId=ASSERT_DM-80
			input is a property.
		'''.assertLinking[sadl]
		'''
			uri "http://ont.sample/SYSTEM" alias SYSTEM.
			//SectionObjectId=ASSERT_DM-1
			import "http://assert/Properties".
			
			//ObjectId=ASSERT_DM-1
			SYSTEM is a class, must be one of {SYS_1,SYS_2}.
			  //ObjectId=ASSERT_DM-2
			  example_1 describes SYSTEM with values of type boolean.
			  example_1 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-2 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_1 .
			
			  //ObjectId=ASSERT_DM-3
			  example_2 describes SYSTEM with values of type DATA.
			  example_2 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-3 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_2,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-4
			  example_3 describes SYSTEM with values of type ENUMERATION.
			  example_3 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-4 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_3,
			    with functional_max 3,
			    with functional_min 0,
			    with physical_max 0,
			    with physical_min 2,
			    with tolerance 1,
			    with resolution 1,
			    with physical_mapping (a PHYSICAL_MAPPING, with reference_instance <Enum_1>, with physical_value 0),
			    with physical_mapping (a PHYSICAL_MAPPING, with reference_instance Enum_2, with physical_value 1),
			    with physical_mapping (a PHYSICAL_MAPPING, with reference_instance Enum_3, with physical_value 2) .
			
			  //ObjectId=ASSERT_DM-5
			  example_4 describes SYSTEM with values of type DATA.
			  example_4 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-5 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_4,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-6
			  example_5 describes SYSTEM with values of type boolean.
			  example_5 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-6 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_5 .
			
			  //ObjectId=ASSERT_DM-7
			  sent describes SYSTEM with values of type SYSTEM_EVENT.
			
			  ASSERT_DM-7 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property sent .
			
			  //ObjectId=ASSERT_DM-8
			  received describes SYSTEM with values of type SYSTEM_EVENT.
			
			  ASSERT_DM-8 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property received .
			
			  //ObjectId=ASSERT_DM-9
			  example_7_1 describes SYSTEM with values of type boolean.
			  example_7_1 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-9 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_7_1 .
			
			  //ObjectId=ASSERT_DM-50
			  example_7_2 describes SYSTEM with values of type boolean.
			  example_7_2 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-50 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_7_2 .
			
			  //ObjectId=ASSERT_DM-10
			  example_7_6 describes SYSTEM with values of type boolean.
			  example_7_6 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-10 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_7_6 .
			
			  //ObjectId=ASSERT_DM-11
			  example_8 describes SYSTEM with a single value of type DATA List.
			  example_8 of SYSTEM has at most 20 values.
			  example_8 of SYSTEM has at least 3 values.
			
			  ASSERT_DM-11 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_8,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-12
			  example_9 describes SYSTEM with a single value of type DATA List.
			
			  ASSERT_DM-12 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_9,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-13
			  example_10 describes SYSTEM with values of type int.
			  example_10 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-13 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_10 .
			
			  //ObjectId=ASSERT_DM-14
			  example_11 describes SYSTEM with a single value of type DATA List.
			  example_11 of SYSTEM has exactly 3 values.
			
			  ASSERT_DM-14 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_11,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-51
			  example_12 describes SYSTEM with values of type DATA.
			  example_12 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-51 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_12,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-52
			  example_13 describes SYSTEM with values of type DATA.
			  example_13 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-52 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_13,
			    with functional_max 2.5,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance .1,
			    with resolution .1 .
			
			  //ObjectId=ASSERT_DM-53
			  example_14 describes SYSTEM with values of type boolean.
			  example_14 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-53 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_14 .
			
			  //ObjectId=ASSERT_DM-54
			  example_15 describes SYSTEM with values of type boolean.
			  example_15 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-54 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_15 .
			
			  //ObjectId=ASSERT_DM-69
			  example_16 describes SYSTEM with values of type boolean.
			  example_16 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-69 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_16 .
			
			  //ObjectId=ASSERT_DM-70
			  example_17 describes SYSTEM with values of type boolean.
			  example_17 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-70 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_17 .
			
			  //ObjectId=ASSERT_DM-71
			  example_18 describes SYSTEM with values of type boolean.
			  example_18 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-71 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_18 .
			
			  //ObjectId=ASSERT_DM-77
			  example_19 describes SYSTEM with values of type boolean.
			  example_19 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-77 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_19 .
			
			  //ObjectId=ASSERT_DM-81
			  example_20 describes SYSTEM with values of type boolean.
			  example_20 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-81 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_20 .
			
			  //ObjectId=ASSERT_DM-82
			  example_21 describes SYSTEM with values of type boolean.
			  example_21 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-82 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_21 .
			
			  //ObjectId=ASSERT_DM-83
			  example_22 describes SYSTEM with values of type boolean.
			  example_22 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-83 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_22 .
			
			  //ObjectId=ASSERT_DM-84
			  example_23 describes SYSTEM with values of type DATA.
			  example_23 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-84 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property example_23,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-15
			  input_1 describes SYSTEM with values of type DATA.
			  input_1 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-15 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_1,
			    with functional_max 500,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-16
			  input_2 describes SYSTEM with values of type DATA.
			  input_2 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-16 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_2,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-17
			  input_3 describes SYSTEM with values of type DATA.
			  input_3 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-17 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_3,
			    with functional_max 750,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min -100,
			    with tolerance 0.5,
			    with resolution 0.5 .
			
			  //ObjectId=ASSERT_DM-18
			  input_4 describes SYSTEM with values of type DATA.
			  input_4 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-18 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_4,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-19
			  input_5 describes SYSTEM with values of type DATA.
			  input_5 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-19 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_5,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-20
			  input_6 describes SYSTEM with values of type COMMON_CLASS.
			  input_6 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-20 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_6 .
			
			  //ObjectId=ASSERT_DM-21
			  input_7 describes SYSTEM with values of type ENUMERATION.
			  input_7 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-21 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_7,
			    with functional_max 3,
			    with functional_min 1,
			    with physical_max 0,
			    with physical_min 3,
			    with tolerance 1,
			    with resolution 1,
			    with physical_mapping (a PHYSICAL_MAPPING, with reference_instance <Enum_1>, with physical_value 1),
			    with physical_mapping (a PHYSICAL_MAPPING, with reference_instance Enum_2, with physical_value 2),
			    with physical_mapping (a PHYSICAL_MAPPING, with reference_instance Enum_3, with physical_value 3) .
			
			  //ObjectId=ASSERT_DM-22
			  input_8 describes SYSTEM with values of type DATA.
			  input_8 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-22 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_8,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-23
			  input_9 describes SYSTEM with values of type DATA.
			  input_9 of SYSTEM has exactly 1 values.
			
			  ASSERT_DM-23 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_9,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-24
			  input_10 describes SYSTEM with a single value of type DATA List.
			  input_10 of SYSTEM has at most 20 values.
			  input_10 of SYSTEM has at least 1 values.
			
			  ASSERT_DM-24 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_10,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-25
			  input_11 describes SYSTEM with a single value of type DATA List.
			  input_11 of SYSTEM has at most 20 values.
			  input_11 of SYSTEM has at least 1 values.
			
			  ASSERT_DM-25 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input_11,
			    with functional_max 1000,
			    with functional_min 0,
			    with physical_max 1000,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-26
			  SYSTEM_EVENT is a type of EVENT.
			    //ObjectId=ASSERT_DM-28
			    SYSTEM_EVENT_1 is a type of SYSTEM_EVENT.
			      //ObjectId=ASSERT_DM-27
			      event_data_1 describes SYSTEM_EVENT_1 with values of type DATA.
			      event_data_1 of SYSTEM_EVENT_1 has exactly 1 values.
			
			      ASSERT_DM-27 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_1,
			        with reference_property event_data_1,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			    
			    //ObjectId=ASSERT_DM-29
			    SYSTEM_EVENT_2 is a type of SYSTEM_EVENT.
			      //ObjectId=ASSERT_DM-30
			      event_data_1 describes SYSTEM_EVENT_2 with values of type DATA.
			      event_data_1 of SYSTEM_EVENT_2 has exactly 1 values.
			
			      ASSERT_DM-30 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_2,
			        with reference_property event_data_1,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			      //ObjectId=ASSERT_DM-33
			      event_data_2 describes SYSTEM_EVENT_2 with values of type DATA.
			      event_data_2 of SYSTEM_EVENT_2 has exactly 1 values.
			
			      ASSERT_DM-33 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_2,
			        with reference_property event_data_2,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			      //ObjectId=ASSERT_DM-34
			      event_data_3 describes SYSTEM_EVENT_2 with values of type DATA.
			      event_data_3 of SYSTEM_EVENT_2 has exactly 1 values.
			
			      ASSERT_DM-34 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_2,
			        with reference_property event_data_3,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			    
			    //ObjectId=ASSERT_DM-31
			    SYSTEM_EVENT_3 is a type of SYSTEM_EVENT.
			      //ObjectId=ASSERT_DM-32
			      event_data_1 describes SYSTEM_EVENT_3 with values of type DATA.
			      event_data_1 of SYSTEM_EVENT_3 has exactly 1 values.
			
			      ASSERT_DM-32 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_3,
			        with reference_property event_data_1,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			    
			    //ObjectId=ASSERT_DM-35
			    SYSTEM_EVENT_4 is a type of SYSTEM_EVENT.
			    
			    //ObjectId=ASSERT_DM-36
			    SYSTEM_EVENT_5 is a type of SYSTEM_EVENT.
			    
			    //ObjectId=ASSERT_DM-72
			    SYSTEM_EVENT_6 is a type of SYSTEM_EVENT.
			      //ObjectId=ASSERT_DM-78
			      result describes SYSTEM_EVENT_6 with values of type boolean.
			      result of SYSTEM_EVENT_6 has exactly 1 values.
			
			      ASSERT_DM-78 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_6,
			        with reference_property result .
			
			      //ObjectId=ASSERT_DM-73
			      event_data_1 describes SYSTEM_EVENT_6 with values of type DATA.
			      event_data_1 of SYSTEM_EVENT_6 has exactly 1 values.
			
			      ASSERT_DM-73 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_6,
			        with reference_property event_data_1,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			    
			    //ObjectId=ASSERT_DM-74
			    SYSTEM_EVENT_7 is a type of SYSTEM_EVENT.
			      //ObjectId=ASSERT_DM-79
			      result describes SYSTEM_EVENT_7 with values of type boolean.
			      result of SYSTEM_EVENT_7 has exactly 1 values.
			
			      ASSERT_DM-79 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_7,
			        with reference_property result .
			
			      //ObjectId=ASSERT_DM-75
			      event_data_1 describes SYSTEM_EVENT_7 with values of type DATA.
			      event_data_1 of SYSTEM_EVENT_7 has exactly 1 values.
			
			      ASSERT_DM-75 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_7,
			        with reference_property event_data_1,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			      //ObjectId=ASSERT_DM-76
			      event_data_2 describes SYSTEM_EVENT_7 with values of type DATA.
			      event_data_2 of SYSTEM_EVENT_7 has exactly 1 values.
			
			      ASSERT_DM-76 is a INTERFACE_DEFINITION,
			        with reference_class SYSTEM_EVENT_7,
			        with reference_property event_data_2,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			    
			  
			  //ObjectId=ASSERT_DM-38
			  COMMON_CLASS is a class.
			    //ObjectId=ASSERT_DM-39
			    CLASS_1 is a type of COMMON_CLASS.
			      //ObjectId=ASSERT_DM-40
			      property_1 describes CLASS_1 with values of type DATA.
			      property_1 of CLASS_1 has exactly 1 values.
			
			      ASSERT_DM-40 is a INTERFACE_DEFINITION,
			        with reference_class CLASS_1,
			        with reference_property property_1,
			        with functional_max 1000,
			        with functional_min 0,
			        with physical_max 1000,
			        with physical_min 0,
			        with tolerance 1,
			        with resolution 1 .
			
			    
			    //ObjectId=ASSERT_DM-41
			    CLASS_2 is a type of COMMON_CLASS.
			      //ObjectId=ASSERT_DM-42
			      property_2 describes CLASS_2 with values of type ENUMERATION.
			      property_2 of CLASS_2 has exactly 1 values.
			
			      ASSERT_DM-42 is a INTERFACE_DEFINITION,
			        with reference_class CLASS_2,
			        with reference_property property_2 .
			
			    
			    //ObjectId=ASSERT_DM-43
			    CLASS_3 is a type of COMMON_CLASS.
			      //ObjectId=ASSERT_DM-44
			      property_3 describes CLASS_3 with values of type boolean.
			      property_3 of CLASS_3 has exactly 1 values.
			
			      ASSERT_DM-44 is a INTERFACE_DEFINITION,
			        with reference_class CLASS_3,
			        with reference_property property_3 .
			
			  //ObjectId=ASSERT_DM-37
			  ENUMERATION is a class, must be one of {[Enum_1],Enum_2,Enum_3}.
			    
			  
			  //ObjectId=ASSERT_DM-45
			  COMPUTATION is a class.
			    //ObjectId=ASSERT_DM-46
			    _value describes COMPUTATION with values of type DATA.
			
			    ASSERT_DM-46 is a INTERFACE_DEFINITION,
			      with reference_class COMPUTATION,
			      with reference_property _value .
			
			  
			  //ObjectId=ASSERT_DM-80
			  input describes SYSTEM with values of type DATA.
			
			  ASSERT_DM-80 is a INTERFACE_DEFINITION,
			    with reference_class SYSTEM,
			    with reference_property input .
			
			  //ObjectId=ASSERT_DM-47
			  DATA is a class.
			  
			  //ObjectId=ASSERT_DM-48
			  EVENT is a class.
			  
			  //ObjectId=ASSERT_DM-49
			  description is a type of annotation.
			  
			
			//EndSectionObject
		'''.assertLinking[sadl]
	}

	@Test
	def void testLinkingQnamesNeeded() {
		'''
			uri "http://sadl.org/NS1.sadl" alias ns1.
			
			[Car] is a class described by [position] with values of type <Location>.
			[Location] is a class, described by [longitude] with values of type float, described by [latitude] with values of type float.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/NS2.sadl" alias ns2.
			
			[Airplane] is a class described by [position] with values of type <Location>.
			
			[Location] is a class described by [longitude] with values of type float, 
			    described by [latitude] with values of type float,
			    described by [altitude] with values of type float.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/NS3.sadl" alias ns3.
			
			import "http://sadl.org/NS1.sadl".
			import "http://sadl.org/NS2.sadl".
			
			[MyCar] is an <Car> with <ns1:position> (a <ns1:Location> with <ns1:longitude> -72.025, with <ns1:latitude> 43.654).
			[MyPlane] is an <Airplane> with <ns2:position> (a <ns2:Location> with <ns2:longitude> -72.025, with <ns2:latitude> 43.654, with <altitude> 1000).
			
		'''.assertLinking[sadl]
	}

	/**
	 * https://github.com/crapo/sadlos2/issues/97
	 */
	@Test
	def void testLinkingImplicitPackage_01() {
		'''
			uri "http://sadl.org/baseModel".
			
			[Car] is a class described by [position] with values of type <Location>.
			[Location] is a class
				, described by [longitude] with values of type float
				, described by [latitude] with values of type float.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/NS3.sadl" alias ns3.

			import "http://sadl.org/baseModel".
			
			[MyCar] is an <Car> with <position> (a <Location> with <longitude> -72.025, with <latitude> 43.654).
			
		'''.assertLinking[sadl]
	}

	@Test
	def void testRecursion() {
		'''
			uri "http://sadl.org/NS1.sadl" alias ns1.
			
			[Foo2] is a type of <Foo>.
			[Foo3] is a type of <Foo2>.
			[Foo] is a type of <Foo2>.
		'''.assertLinking[sadl]
	}

	@Test
	def void testRecursion_01() {
		'''
			uri "http://sadl.org/NS1.sadl" alias ns1.
			
			[Foo] is a type of <Foo>.
		'''.assertLinking[sadl]
	}

	@Test
	def void testImportPerformance() {
		for (i : 1 .. 19) {
			val started = Stopwatch.createStarted
			val resource = '''
				uri "http://sadl.org/NS1.sadl«i»" alias ns«i».
				«FOR j : 1 ..< i»
					import "http://sadl.org/NS1.sadl«j»" as ns«j»foo.
				«ENDFOR»
				
				«FOR k : 1..100»
					Car«k» is a class described by position with values of type Location«k».
					Location«k» is a class, described by longitude with values of type float, described by latitude with values of type float.
				«ENDFOR»
			'''.sadl
			EcoreUtil.resolveAll(resource)
			println("Iteration " + i + " Took : " + started.elapsed(TimeUnit.MILLISECONDS))
		}
	}
	
	@Test
	def void testQueryVariable_1() {
		'''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			
			impliedProperty is a type of annotation.
			expandedProperty is a type of annotation.
			UnittedQuantity is a class,
				described by ^value with values of type decimal,
				described by unit with values of type string.
			UnittedQuantity has expandedProperty ^value, has expandedProperty unit.
		'''.assertLinking[sadl]
		
		'''
			 uri "http://sadl.org/TestExpandedInQuery.sadl" alias TestExpandedInQuery.
			 
			 [PhysicalObject] is a class described by [weight] with values of type <UnittedQuantity>.
			 
			 [MonaLisa] is a <PhysicalObject> with <weight> 25 lbs.
			 [AngelOak] is a <PhysicalObject> with <weight> 10000 lbs.
			 
			 Ask: <weight>.
			 Ask: <weight> of [x].
			 Ask: [x] has <weight>.
			 Ask Q1: [x] has <weight>.
			 Ask: <weight> of <AngelOak>.
			 Ask: <AngelOak> has <weight>.
			 Ask: select <po> where [po] has <weight>.
			 Ask: select <po>, <w> where [po] has weight [w].
			 Ask: select <po>, <v>, <u> where [po] has <weight> [w] and <w> has ^value [v] and <w> has unit [u].
			 Ask: select <po>, <v>, <u> where [po] has weight [w] and <w> has ^value [v] and <w> has unit [u] and <v> > 1000.
			 Ask: select <p> where [p] is a <PhysicalObject>.
			 Ask Named: select <p> where [p] is a <PhysicalObject>.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testQueryVariable_2() {
		'''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 Foo is a class described by bar with values of type Whim.
			 Whim is a class described by wham with values of type string.
			 Ask: select <i> where [i] is a Foo.
			 Ask: select <i> where [i] is a Foo with bar (a Whim with wham "hi").
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testRuleVariable_1() {
		'''
			uri "http://sadl.org/rulevars.sadl" alias rulevars.
			Person is a class.
			teaches describes Person with values of type Person.
			knows describes Person with values of type Person.
			A relationship of Person to Person is acquaintance. 
			
			Rule R1 if [x] is a Person and <x> has teaches [y] then <x> has knows <y>.
			 
			Rule R2 if [x] is a Person and <x> has teaches [y] then <x> has acquaintance <y>.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testRuleVariable_2() {
		'''
			uri "http://sadl.org/RuleDeclSHPvsPofS.sadl" alias RuleDeclSHPvsPofS.
			 
			PhysicalThing is a class.
			
			Person is a type of PhysicalThing, described by favoriteColor with values of type string, 
				described by owns with values of type PhysicalThing
				described by age with values of type float.
			 
			Vehicle is a type of PhysicalThing, 
				described by number_of_wheels with values of type int,
				described by number_of_seats with values of type int,
				described by color with values of type string.
				
			Unicycle is a type of Vehicle.
			number_of_wheels of Unicycle always has value 1 .
			number_of_seats of Unicycle always has value 1 .
			
				
			Bicycle is a type of Vehicle.
			number_of_wheels of Bicycle always has value 2 .
			number_of_seats of Bicycle always has value 1 .
			
			Car is a type of Vehicle.
			number_of_wheels of Car always has value 4 .
			 
			Rule CarIsFavoriteColor
			  given [p] is a Person
			  if <p> has owns [t] and
			  	 <t> has color [c]
			  then <p> has favoriteColor <c>.
			
			Rule CarIsFavoriteColorAlt
			  given p is a Person
			  if p has owns t and
			  	 t has color [c]
			  then favoriteColor of p is <c>.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testOnlyIf_01() {
		'''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 IRS is a class
			 	described by ground_speed with values of type DATA.
			 ground_speed of IRS only has values of type DATA.
			 DATA is a class described by _value with values of type decimal.
			 
			 is_ground_speed_of describes DATA with values of type IRS.
			 is_ground_speed_of is the inverse of ground_speed.
			 
			 A <DATA> is a [DATA2] only if is_ground_speed_of only has values of type IRS.
			 <DATA2> has expandedProperty _value.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testOnlyIf_02() {
		'''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 IRS is a class
			 	described by ground_speed with values of type DATA.
			 ground_speed of IRS only has values of type DATA.
			 DATA is a class described by _value with values of type decimal.
			 
			 is_ground_speed_of describes DATA with values of type IRS.
			 is_ground_speed_of is the inverse of ground_speed.
			 
			 [DATA2] is a type of DATA.
			 A <DATA> is a <DATA2> only if is_ground_speed_of only has values of type IRS.
			 <DATA2> has expandedProperty _value.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testEmbeddedInstanceDeclaration() {
		'''
			 uri "http://sadl.org/test.sadl" alias test.
			 
			 [Person] is a class described by [child] with values of type <Person>.
			 
			 [Clyde] is a Person with child (a Person [Nancy]).
			 <Nancy> has child (a Person [Peter]).
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testRuleVariables_01() {
		'''
			uri "http://sadl.org/rulevars.sadl" alias rulevars.
						
			Person is a class.
			teaches describes Person with values of type Person.
			knows describes Person with values of type Person.
			A relationship of Person to Person is acquaintance. 
			
			Rule R1 if x is a Person and x has teaches y then x has knows y.
			 
			Rule R2 if x is a Person and x has teaches y then x has acquaintance y. 
			
			Rule R3 if [x] is a Person and <x> teaches [y] then <x> knows <y>.
			
«««			Rule R4: if a Person knows a second Person then the second Person knows the first Person.
			 Rule R4b: if a Person has knows a second Person then the second Person has knows the first Person.
			
			Rule R5: if [x] is a Person and knows of <x> is [y] then knows of <y> is <x>.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testNoLinkingForUnits() {
		'''
		uri "http://sadl.org/testunits" alias tu.
		Expr: (2 + 3) seconds.
		[seconds] is a class.
		
		B is a type of <seconds>.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testSameAsDefinition() {
		'''
		uri "http://sadl.org/testsameasdefn" alias tsd.
		Man is a class.
		Parent is a class.
		[Father] is the same as {<Man> and <Parent>}.
		'''.assertLinking[sadl]
	}

	// https://github.com/crapo/sadlos2/issues/423
	@Test
	def void testCanLinkToFQNWithMultipleSegments() {
		'''
			uri "http://sadl.org/STEM/BaseModel" alias base.
			[Thing] (alias "super class of everything") is a class.
			[System] is a type of <Thing>. 
			[Subsystem] is a type of <System>.
			[Bus] is a type of <System>. 
			// For reading in the tabular data; some properties will be from the 3rd csv file
			[Connection] is a type of <Subsystem>
				described by [binding] with a single value of type <Bus>.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/STEM/Scenario" alias scn (note "This ontology was created from a CSV data source.").
			import "http://sadl.org/STEM/BaseModel" as base.
			[c16DeliveryDroneSystem.ImplDeliveryDroneSystem] is a <base:Connection>.
			[c1DeliveryDroneSystem.ImplDeliveryDroneSystem] is a <base:Connection>.
		'''.assertLinking[sadl]
		'''
			uri "http://sadl.org/STEM/Scenario3" alias scn3 (note "This ontology was created from a CSV data source.").
			import "http://sadl.org/STEM/Scenario" as scn.
			import "http://sadl.org/STEM/BaseModel" as base.
			// Individuals:
			[ethernet] is a <base:Bus>.
			// Other statements:
			<scn:c1DeliveryDroneSystem.ImplDeliveryDroneSystem> has <base:binding> <ethernet>.
			<scn:c16DeliveryDroneSystem.ImplDeliveryDroneSystem> has <base:binding> <ethernet>.
		'''.assertLinking[sadl]
	}

	// https://github.com/crapo/sadlos2/issues/447
	@Test
	def void testCanLinkToAllDifferentStatement() {
		'''
			uri "http://glguy.net/sadl/alldifferentexample".
			[C] is a class.
			[I] is a <C>.
			[J] is a <C>.
			{<I>,<J>} are not the same.
		'''.assertLinking[sadl]
	}
	
	// https://github.com/crapo/sadlos2/issues/511
	@Test
	def void testTripleBeforeTripleSubjectDecl() {
		'''
		uri "https://github.com/crapo/sadlos2/issues/511".
		[Cls] is a class described by [objProp] with values of type Cls.
		[i1] is a <Cls>.
		<i2> has <objProp> <i1>.
		[i2] is a <Cls>.
		'''.assertLinking[sadl]
	}

}
