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
package com.ge.research.sadl.tests

import com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds
import com.ge.research.sadl.services.SADLGrammarAccess
import com.google.common.collect.Sets
import com.google.inject.Inject
import java.util.Collection
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.AfterClass
import org.junit.Assert
import org.junit.BeforeClass
import org.junit.Test
import org.junit.runner.RunWith

import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*

import static extension java.lang.reflect.Modifier.*

/**
 * Test for checking that the {@link ISadlOntologyHelper.GrammarContextIds} are in sync with the actual
 * SADL grammar. Basically, this test is used to make sure we have a compiler errors when any grammar
 * changes break the grammar context IDs.
 * 
 * @author akos.kittas
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class GrammarContextIDsTest extends Assert {

	@Inject
	extension SADLGrammarAccess

	static val Collection<String> IDS_TO_TEST = newHashSet();

	@BeforeClass
	static def void beforeClass() {
		val grammarContextIds = GrammarContextIds.declaredFields.filter [
			modifiers.static && modifiers.public && type === String
		];
		grammarContextIds.forEach [
			val name = name;
			val value = get(null) as String;
			assertEquals('''Field name and value should be the same. Name: «name». Value: «value»''', name, value);
		];
		assertEquals(grammarContextIds.size,
			ONTOLOGY_DEPENDENT_CONTEXT_IDS.size + ONTOLOGY_INDEPENDENT_CONTEXT_IDS.size);

		assertTrue(Sets.intersection(ONTOLOGY_DEPENDENT_CONTEXT_IDS, ONTOLOGY_INDEPENDENT_CONTEXT_IDS).empty);

		IDS_TO_TEST.addAll(grammarContextIds.map[name]);
	}

	@AfterClass
	static def void afterClass() {
		assertTrue('''The following grammar context IDs do not have corresponding test case: «IDS_TO_TEST».''',
			IDS_TO_TEST.empty);
	}

	@Test
	def void check_01() {
		assertEquals(
			SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE,
			sadlPrimaryTypeReferenceAccess.primitiveTypeAssignment_1_1
		);
	}

	@Test
	def void check_02() {
		assertEquals(
			SADLPRIMARYTYPEREFERENCE_TYPE,
			sadlPrimaryTypeReferenceAccess.typeAssignment_0_1
		);
	}

	@Test
	def void check_03() {
		assertEquals(
			SADLPROPERTYINITIALIZER_PROPERTY,
			sadlPropertyInitializerAccess.propertyAssignment_0_1
		);
	}

	@Test
	def void check_04() {
		assertEquals(
			SADLSTATEMENT_SUPERELEMENT,
			sadlStatementAccess.superElementAssignment_0_1_0_3_1_2
		);
	}

	@Test
	def void check_05() {
		assertEquals(
			SADLRESOURCE_NAME,
			sadlResourceAccess.nameAssignment_0
		);
	}

	@Test
	def void check_06() {
		assertEquals(
			PRIMARYEXPRESSION_VALUE,
			primaryExpressionAccess.valueAssignment_2_1
		);
	}

	@Test
	def void check_07() {
		assertEquals(
			SADLPRIMARYTYPEREFERENCE_TYPE,
			sadlPrimaryTypeReferenceAccess.typeAssignment_0_1
		);
	}

	@Test
	def void check_08() {
		assertEquals(
			SADLPROPERTYCONDITION_PROPERTY,
			sadlPropertyConditionAccess.propertyAssignment_0
		);
	}

	@Test
	def void check_09() {
		assertEquals(
			SADLPROPERTYINITIALIZER_VALUE,
			sadlPropertyInitializerAccess.valueAssignment_0_2_0
		);
	}

	@Test
	def void check_10() {
		assertEquals(
			PROPOFSUBJECT_RIGHT,
			propOfSubjectAccess.rightAssignment_0_1_0_1_1
		);
	}

	@Test
	def void check_11() {
		assertEquals(
			PROPOFSUBJECT_PROP,
			propOfSubjectAccess.propAssignment_0_1_0_1_0_0_2
		);
	}

	@Test
	def void check_12() {
		assertEquals(
			SADLPROPERTYRESTRICTION_TYPEONLY,
			sadlPropertyRestrictionAccess.typeonlyAssignment_2_5_0
		);
	}
	
	@Test
	def void check_13() {
		assertEquals(SADLPRIMARYTYPEREFERENCE_LIST, 
			sadlPrimaryTypeReferenceAccess.getListAssignment_1_2
		);
	}

	private def void assertEquals(String expected, Assignment actual) {
		try {
			assertEquals(expected, GrammarContextIds.TO_STRING.apply(actual));
			IDS_TO_TEST.remove(expected);
		} catch (AssertionError e) {
			// Do not throw at `@AfterClass`. It is already broken.
			IDS_TO_TEST.clear;
			throw e;
		}
	}

}
