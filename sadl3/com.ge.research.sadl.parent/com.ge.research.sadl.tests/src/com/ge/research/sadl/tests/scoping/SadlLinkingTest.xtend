package com.ge.research.sadl.tests.scoping

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.ge.research.sadl.tests.SADLInjectorProvider
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
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
			import "http://sadl.org/allqnames.sadl"
			
			[MyShape] is a <Shape> with <area> 23 .
		'''.assertLinking[sadl]
	}
}