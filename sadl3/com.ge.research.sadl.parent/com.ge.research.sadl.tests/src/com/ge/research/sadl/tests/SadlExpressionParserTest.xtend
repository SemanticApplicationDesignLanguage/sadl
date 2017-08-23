package com.ge.research.sadl.tests

import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.EquationStatement
import com.ge.research.sadl.sADL.Expression
import com.ge.research.sadl.sADL.NumberLiteral
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.Unit
import org.junit.Test

import static org.junit.Assert.*

class SadlExpressionParserTest extends AbstractSADLParsingTest {
	
	
	private def <T extends Expression> T expression(CharSequence expression) {
		val resource = '''
			uri "http://sadl.org/equations".
						
			Equation foo(int ^a, int b) returns int : «expression».
		'''.sadl
		assertTrue(resource.errors.join(',')[toString], resource.errors.empty)
		return ((resource.contents.head as SadlModel).elements.head as EquationStatement).body as T
	}
	
	@Test def void testUnits_01() {
		'''
			42 seconds
		'''.<Unit>expression => [ 
			assertEquals(42, (value as NumberLiteral).value.toBigInteger.intValue)
		]
	}
	
	@Test def void testUnits_02() {
		'''
			3 + 42 seconds
		'''.<BinaryOperation>expression => [ op |
			 val it = op.right as Unit
			assertEquals(42, (value as NumberLiteral).value.toBigInteger.intValue)
		]
	}
	
	@Test def void testUnits_03() {
		'''
			(3 + 42) seconds
		'''.<Unit>expression => [
			assertEquals(42, ((value as BinaryOperation).right as NumberLiteral).value.toBigInteger.intValue)
		]
	}
}