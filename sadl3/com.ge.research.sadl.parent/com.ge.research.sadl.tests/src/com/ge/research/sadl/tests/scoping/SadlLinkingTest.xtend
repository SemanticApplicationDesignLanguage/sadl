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
			import "http://sadl.org/allqnames.sadl"
			
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
			import "http://sadl.org/allqnames.sadl"
			
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
			
			Equation [foo](int [a], int [b]) returns int : <a> + <b>.
			Equation [bar](int [a], int [b]) returns int: <a> + <b>.
		'''.assertLinking[sadl]
	}
	
	@Test
	def void testLinkingEquations_02() {
		'''
			uri "http://sadl.org/equations".
			
			Equation [foo](int [a], int [b]) returns int : <a> + <b>.
			Equation [bar](int [a], int [b]) returns int: <foo>(<a>, <b>).
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
	
}