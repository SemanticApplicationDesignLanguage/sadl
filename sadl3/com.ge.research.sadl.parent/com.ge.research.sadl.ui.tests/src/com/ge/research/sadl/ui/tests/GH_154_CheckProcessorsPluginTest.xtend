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

import com.ge.research.sadl.jena.JenaBasedSadlImportProcessor
import com.ge.research.sadl.jena.JenaBasedSadlInferenceProcessor
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.SadlImportProcessorProvider
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider
import com.ge.research.sadl.processing.SadlModelProcessorProvider
import com.google.inject.Inject
import org.eclipse.core.runtime.Platform
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Assert
import org.junit.BeforeClass
import org.junit.Test
import org.junit.runner.RunWith

/**
 * Test for checking whether any SADL model, inference, and import 
 * processors are available in the Eclipse-based case.
 * 
 * <p>
 * In this test we check the existence of the Jena-based ones.
 * The processors are registered via Eclipse-based extension points.
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLUiInjectorProvider)
class GH_154_CheckProcessorsPluginTest extends Assert {

	@Inject
	SadlModelProcessorProvider modelProcessorProvider;

	@Inject
	SadlInferenceProcessorProvider inferenceProcessorProvider

	@Inject
	SadlImportProcessorProvider importProcessorProvider;

	@BeforeClass
	static def void assertRunningPlatform() {
		assertTrue('These tests require a running Eclipse platform.
			Execute them as a JUnit Plug-in Test.
			If you see this error from Maven, then please configure your POM to use Tycho Surefire correctly for test execution.',
			Platform.isRunning);
	}

	@Test
	def void checkJenaModelProcessor() {
		modelProcessorProvider.allProcessors.filter(JenaBasedSadlModelProcessor).empty.assertFalse;
	}

	@Test
	def void checkJenaInferenceProcessor() {
		inferenceProcessorProvider.allProcessors.filter(JenaBasedSadlInferenceProcessor).empty.assertFalse;
	}

	@Test
	def void checkNotNullJenaImportProcessor() {
		importProcessorProvider.allProcessors.filter(JenaBasedSadlImportProcessor).empty.assertFalse;
	}

}
