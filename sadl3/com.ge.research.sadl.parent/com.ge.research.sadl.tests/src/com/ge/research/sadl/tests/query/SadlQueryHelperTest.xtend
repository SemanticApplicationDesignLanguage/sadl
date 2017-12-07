/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.query

import com.ge.research.sadl.query.SadlQueryHelper
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.tests.AbstractSadlTest
import com.google.inject.Inject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.junit.Test

import static extension org.junit.Assert.*

/**
 * Tests the SADL query helper which tries to locate SADL queries by their names.
 * 
 * @author akos.kitta
 */
class SadlQueryHelperTest extends AbstractSadlTest {

	@Inject
	extension SadlQueryHelper;

	@Test
	def void test_FindByName_01() {
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			AA is a class.
						
			Ask foo: x is an AA.
		'''.sadl;
		val result = findQueryByName('foo');
		result.assertNotNull;
	}

	@Test
	def void test_FindByName_02() {
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			AA is a class.
						
			Ask foo: x is an AA.
		'''.sadl;
		val result = findQueryByName('missing');
		result.assertNull;
	}

	@Test
	def void test_FindByName_03() {
		val it = '''
			uri "http://sadl.org/Foo.sadl" alias f.
			AA is a class.
						
			Ask foo: x is an AA.
		'''.sadl;
		val result = findQueryByName('f:foo');
		result.assertNotNull;
	}

	@Test
	def void test_FindByName_04() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask bar: y is a BB.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			import "http://sadl.org/Bar.sadl".
		'''.sadl;
		val result = findQueryByName('bar');
		result.assertNotNull;
	}

	@Test
	def void test_FindByName_05() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask bar: y is a BB.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			import "http://sadl.org/Bar.sadl" as b.
		'''.sadl;
		val result = findQueryByName('b:bar');
		result.assertNotNull;
	}

	@Test
	def void test_FindByName_06() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask queryName: y is a BB.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			import "http://sadl.org/Bar.sadl" as b.
			
			AA is a class.
						
			Ask queryName: x is an AA.
		'''.sadl;
		val result = findQueryByName('queryName');
		result.assertNotNull;
		assertEquals('x is an AA', result.expressionAsString);
	}

	@Test
	def void test_FindByName_07() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask queryName: y is a BB.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl" alias f.
			import "http://sadl.org/Bar.sadl" as b.
			
			AA is a class.
						
			Ask queryName: x is an AA.
		'''.sadl;
		val result = findQueryByName('f:queryName');
		result.assertNotNull;
		assertEquals('x is an AA', result.expressionAsString);
	}

	@Test
	def void test_FindByName_08() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask queryName: y is a BB.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl" alias f.
			import "http://sadl.org/Bar.sadl" as b.
			
			AA is a class.
						
			Ask queryName: x is an AA.
		'''.sadl;
		val result = findQueryByName('b:queryName');
		result.assertNotNull;
		assertEquals('y is a BB', result.expressionAsString);
	}

	@Test
	def void test_FindByName_09() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask queryName: y is a BB.
		'''.sadl;
		'''
			uri "http://sadl.org/Baz.sadl".
			CC is a class.
			
			Ask queryName: z is a CC.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			import "http://sadl.org/Bar.sadl" as b.
			import "http://sadl.org/Baz.sadl" as c.
		'''.sadl;
		val result = findQueryByName('queryName');
		result.assertNull;
	}

	@Test
	def void test_FindByName_10() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask queryName: y is a BB.
		'''.sadl;
		'''
			uri "http://sadl.org/Baz.sadl".
			CC is a class.
			
			Ask queryName: z is a CC.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl" alias f.
			import "http://sadl.org/Bar.sadl" as b.
			import "http://sadl.org/Baz.sadl" as c.
			AA is a class.
						
			Ask queryName: x is an AA.
		'''.sadl;
		val result = findQueryByName('f:queryName');
		result.assertNotNull;
		assertEquals('x is an AA', result.expressionAsString);
	}

	@Test
	def void test_FindByName_11() {
		'''
			uri "http://sadl.org/Bar.sadl".
			BB is a class.
			
			Ask queryName: y is a BB.
		'''.sadl;
		'''
			uri "http://sadl.org/Baz.sadl".
			CC is a class.
			
			Ask queryName: z is a CC.
		'''.sadl;
		val it = '''
			uri "http://sadl.org/Foo.sadl".
			import "http://sadl.org/Bar.sadl" as b.
			import "http://sadl.org/Baz.sadl" as c.
			AA is a class.
						
			Ask queryName: x is an AA.
		'''.sadl;
		val result = findQueryByName('c:queryName');
		result.assertNotNull;
		assertEquals('z is a CC', result.expressionAsString);
	}

	private def getExpressionAsString(QueryStatement it) {
		return NodeModelUtils.findActualNodeFor(expr).text.trim;
	}

}
