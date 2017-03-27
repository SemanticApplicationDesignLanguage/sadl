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

import com.ge.research.sadl.processing.ISadlImplicitModelContentProvider
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

import static extension com.ge.research.sadl.tests.helpers.XtendTemplateHelper.unifyEOL

/**
 * Test for checking the content of the SADL implicit model in the headless case.
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class GH_162_ImplicitModelContentProviderTest extends Assert {

	@Inject
	extension ISadlImplicitModelContentProvider;
	
	@Test
	def void checkContent() {
		assertEquals(ISadlImplicitModelContentProvider.Default.DEFAULT_CONTENT.unifyEOL, content.unifyEOL);
	}
	

}
