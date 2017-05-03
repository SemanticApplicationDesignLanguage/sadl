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
package com.ge.research.sadl.ui.tests.contentassist

import com.ge.research.sadl.model.DeclarationExtensions
import com.google.common.collect.Iterables
import org.junit.Test

import static com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder.*
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import static org.junit.Assert.*

/**
 * Test for transforming Eclipse-based content assist contexts into ontology helper contexts.
 * 
 * @author akos.kitta
 */
class SadlOntologyContextTest extends AbstractSadlContentAssistTest {

	@Test
	def void checkPropertyInitializerValueWithRestrictions() {
		val context = newBuilder('''
		uri "http://myUri". 
		Foo is a class described by p1 with values of type Foo. 
		myFoo is a Foo with p1 ''').ontologyContext;

		assertNotNull(context);
		assertNotEquals(MISSING_SUBJECT, context.subject);
		assertTrue(context.grammarContextId.present);
		assertEquals(SADLPROPERTYINITIALIZER_VALUE, context.grammarContextId.get);
		assertEquals(1, Iterables.size(context.restrictions));
		assertEquals('p1', declarationExtensions.getConcreteName(Iterables.get(context.restrictions, 0)));
	}

	@Test
	def void checkPropertyInitializerValue() {
		val context = newBuilder('''
		uri "http://myUri".
		Foo is a class described by p1 with values of type Foo.
		myFoo is a ''').ontologyContext;

		assertNotNull(context);
		assertEquals(MISSING_SUBJECT, context.subject);
		assertTrue(context.grammarContextId.present);
		assertEquals(SADLSTATEMENT_TYPE, context.grammarContextId.get);
		assertTrue(Iterables.isEmpty(context.restrictions));
	}

	private def getDeclarationExtensions() {
		return get(DeclarationExtensions);
	}

}
