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
package com.ge.research.sadl.tests.imports

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.google.inject.Inject
import org.eclipse.xtext.scoping.IScopeProvider
import org.junit.Test
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SADLPackage
import org.junit.Assert

/**
 * No exceptions should be thrown when creating import scope with broken AST. 
 * 
 * @author akos.kitta
 */
class GH_147_ImportScopeOnBrokenASTTest extends AbstractLinkingTest {
	
	@Inject
	IScopeProvider scopeProvider;
	
	@Test
	def void checkNoNPE() {
		val model = '''
		uri "http://sadl.org/Foo.sadl".
		
		import
		'''.sadl;
		
		val context = model.contents.head as SadlModel;
		val ref = SADLPackage.Literals.SADL_RESOURCE__NAME;
		
		val scope = scopeProvider.getScope(context, ref);
		Assert.assertNotNull(scope);
		Assert.assertNotNull(scope.allElements);
	}
	
}