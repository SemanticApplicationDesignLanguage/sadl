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

import com.ge.research.sadl.jena.JenaBasedSadlModelValidator
import com.ge.research.sadl.model.ConceptIdentifier
import com.ge.research.sadl.sADL.SADLFactory
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import com.ge.research.sadl.model.gp.NamedNode
import com.ge.research.sadl.jena.TypeCheckInfo

/**
 * Test to make sure that type check info has a corresponding #hashCode implementation 
 * for the overridden #equals.
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class GH_164_TypeCheckInfoTest extends Assert {

	var TypeCheckInfo left;
	var TypeCheckInfo right;

	@Before
	def void before() {
		val id_1 = new ConceptIdentifier;
		val id_2 = new NamedNode;
		val context = SADLFactory.eINSTANCE.createSadlModel => [
			alias = "foo";
			baseUri = "http://sadl.org/Foo.sadl";
		];

		// Type check info is not static, hence we need to initialize it in an enclosing validator.
		new JenaBasedSadlModelValidator(null, null, null, null, null) {
			// Poor man's initializer block in Xtend. (https://bugs.eclipse.org/bugs/show_bug.cgi?id=429141)
			package val FAKE = {
				val validator = new JenaBasedSadlModelValidator(null, null, null, null, null);
				left = new TypeCheckInfo(id_1, id_2, validator, context);
				right = new TypeCheckInfo(id_1, id_2, validator, context);
			}
		}
	}

	@After
	def void after() {
		left = null;
		right = null;
	}

	@Test
	def void checkEquals() {
		assertEquals(left, right);
	}

	@Test
	def void checkHashCode() {
		assertEquals(left.hashCode, right.hashCode);
	}

	@Test
	def void checkInHashSet() {
		assertEquals(1, newHashSet(left, right).size);
	}

}
