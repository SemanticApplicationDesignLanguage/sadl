/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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

import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.utils.SadlResourceCommentProvider
import com.google.common.collect.Iterables
import com.google.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.XtextResource
import org.junit.Assert
import org.junit.Test

import static extension com.ge.research.sadl.tests.helpers.XtendTemplateHelper.unifyEOL

class SadlResourceCommentProviderTest extends AbstractSadlTest {

	@Inject
	private SadlResourceCommentProvider commentProvider;

	@Test
	def void testComments_noAnnotations() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood is a class.
		'''.assertComments(#[]);
	}

	@Test
	def void testComments_singleAlias() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (alias "first alias") is a class.
		'''.assertComments(#[]);
	}

	@Test
	def void testComments_multipleAliases() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (alias "first alias", "second alias", "third alias") is a class.
		'''.assertComments(#[]);
	}

	@Test
	def void testComments_singleComment() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (note "first comment") is a class.
		'''.assertComments(#['first comment']);
	}

	@Test
	def void testComments_multipleComments() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (note "first comment", "second comment", "third comment") is a class.
		'''.assertComments(#['first comment', 'second comment', 'third comment']);
	}

	@Test
	def void testComments_singleComment_LB() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (note "first
comment") is a class.
		'''.assertComments(#['first
comment']);
	}

	@Test
	def void testComments_multipleComments_LB() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood (note "first 
comment", "second
comment", "third

comment") is a class.
		'''.assertComments(#['first 
comment', 'second
comment', 'third

comment']);
	}
	
	@Test
	def void testCommentNotOnDefinition() {
		'''
			uri "http://sadl.org/a.sadl".
			AllThingsGood is a class.
			AllThingsGood (note "comment") is described by age with values of type float.
		'''.assertComments(#[]);
	}

	protected def dispatch void assertComments(CharSequence model, Iterable<String> expected) {
		assertComments(model.sadl, expected);
	}

	protected def dispatch void assertComments(XtextResource model, Iterable<String> expected) {
		EcoreUtil2.getAllContentsOfType(model.sadlModel, SadlResource).head.assertComments(expected);
	}

	protected def dispatch void assertComments(SadlResource it, Iterable<String> expected) {
		val actual = commentProvider.getComment(it).map[unifyEOL].toList;
		Assert.assertEquals(
			'''Expected: «Iterables.toString(expected)». Got instead: «Iterables.toString(actual)»''',
			expected.map[unifyEOL].toList,
			actual
		);
	}

}
