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
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Ignore

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
			A Foo is a [Rectangle] only if area has one value of type float.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testLinkingPrecedence_04() {
		'''
			uri "http://sadl.org/equations".
			
			[area] describes <Rectangle> with values of type float.
			A Foo is a [Rectangle] only if area has one value of type float.
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
			 uri "http://sadl.org/instances.sadl" alias instances.
			 
			 import "http://sadl.org/classes.sadl".
			 
			[Gregory] is a Human with friend <Lassie>. 
			 
			[Lassie] is a Dog.	// this is the declaration of Lassie
			// {MrEd, SeaBuiscut, Flicka} are instances of Horse.
			 
			[Winter] is a <Season>. 
			 
			[Season] is a class, must be one of {<Spring>, [Summer], [Fall], <Winter>}.
			
			[Spring] is a Season.	
			 
			[Book] is a class.
			[HarperLeeBook] is a type of <Book>, 
				can only be one of {[ToKillaMockingbird], <GoSetaWatchman>}.	
			[GoSetaWatchman] is a <Book>.	
			
			[Event] is a class described by [when] with values of type <Season>.
			[NewYear] is an Event with when <Winter>.
			[BastilleDay] is an Event with when <Summer>.
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
	
	@Ignore
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
	
	@Ignore
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
}