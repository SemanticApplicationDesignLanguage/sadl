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
package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.processing.ISadlImplicitModelContentProvider
import com.google.inject.Inject
import org.eclipse.core.runtime.Platform
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Assert
import org.junit.BeforeClass
import org.junit.Test
import org.junit.runner.RunWith

import static extension com.ge.research.sadl.tests.helpers.XtendTemplateHelper.unifyEOL

/**
 * Test for checking the content of the SADL implicit model with running Eclipse platform.
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLUiInjectorProvider)
class GH_162_ImplicitModelContentProviderPluginTest extends Assert {

	@BeforeClass
	static def void assertRunningPlatform() {
		assertTrue('These tests require a running Eclipse platform.
			Execute them as a JUnit Plug-in Test.
			If you see this error from Maven, then please configure your POM to use Tycho Surefire correctly for test execution.',
			Platform.isRunning);
	}

	@Inject
	extension ISadlImplicitModelContentProvider;
	
	@Test
	def void checkContent() {
		assertEquals(ISadlImplicitModelContentProvider.Default.DEFAULT_CONTENT.unifyEOL, content.unifyEOL);
	}
	

}
