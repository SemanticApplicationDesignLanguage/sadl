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
package com.ge.research.sadl.tests.naming

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.google.inject.Inject
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.util.IAcceptor
import org.junit.Assert
import org.junit.Test

import static extension org.eclipse.emf.ecore.util.EcoreUtil.getAllProperContents

/**
 * Resource description strategy must not throw exception but return with {@code false}
 * in case of broken AST to interrupt the processing of the sub-tree.
 * 
 * The exception was due to a bug in the qualified name provider, when the concrete
 * name of a SADL resource is not available, the fully qualified name should be {@code null}
 * as well.
 * 
 * @author akos.kitta
 */
class GH_150_CannotCreateEObjectDescriptionWithBrokenAST extends AbstractLinkingTest {

	static val IAcceptor<IEObjectDescription> NOOP_ACCEPTOR = [];

	@Inject
	IDefaultResourceDescriptionStrategy strategy;

	@Test
	def void checkValidAST() {
		val itr = '''
			uri "http://sadl.org/Foo.sadl".
			
			foo is a class described by x a single value of type int.
		'''.sadl.getAllProperContents(false);
		while (itr.hasNext) {
			val next = itr.next;
			Assert.assertTrue('''Expected true when indexing EObject: «next».''',
				strategy.createEObjectDescriptions(next, NOOP_ACCEPTOR));
		}
	}

	@Test
	def void checkBrokenAST() {
		val itr = '''
			uri "http://sadl.org/Foo.sadl".
			
			foo is a class described by .
		'''.sadl.getAllProperContents(false);
		while (itr.hasNext) {
			val next = itr.next;
			Assert.assertTrue('''Expected true when indexing EObject: «next».''',
				strategy.createEObjectDescriptions(next, NOOP_ACCEPTOR));
		}
	}
	
}
