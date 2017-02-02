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
import com.ge.research.sadl.utils.ImportHelper
import com.google.inject.Inject
import org.junit.Test
import org.eclipse.emf.ecore.resource.Resource
import com.ge.research.sadl.sADL.SadlModel
import java.util.Arrays
import org.junit.Assert

import static com.ge.research.sadl.processing.SadlConstants.SADL_IMPLICIT_MODEL_URI

/**
 * Test for checking the {@link ImportHelper SADL import helper}.
 * 
 * @author akos.kitta
 */
class ImportHelperTest extends AbstractLinkingTest {

	@Inject
	extension ImportHelper;

	@Test
	def void checkNoExplicitImport() {
		'''uri "myUri".'''.sadl.model.assertEquals('myUri', SADL_IMPLICIT_MODEL_URI);
	}

	@Test
	def void checkDirectImport() {
		'''uri "parentUri".'''.sadl;
		'''uri "myUri". import "parentUri".'''.sadl.model.assertEquals('myUri', 'parentUri', SADL_IMPLICIT_MODEL_URI);
	}

	@Test
	def void checkTransitiveImport() {
		'''uri "parentUri".'''.sadl;
		'''uri "middleUri". import "parentUri".'''.sadl;
		'''uri "myUri". import "middleUri".'''.sadl.model.assertEquals('myUri', 'middleUri', 'parentUri',
			SADL_IMPLICIT_MODEL_URI);
	}

	@Test
	def void checkDiamondImport() {
		'''uri "parentUri".'''.sadl;
		'''uri "middleUri1". import "parentUri".'''.sadl;
		'''uri "middleUri2". import "parentUri".'''.sadl;
		'''uri "myUri". import "middleUri1". import "middleUri2".'''.sadl.model.assertEquals('myUri', 'middleUri1',
			'middleUri2', 'parentUri', SADL_IMPLICIT_MODEL_URI);
	}

	@Test
	def void checkTransitiveDuplicateImport() {
		'''uri "parentUri".'''.sadl;
		'''uri "middleUri". import "parentUri".'''.sadl;
		'''uri "myUri". import "middleUri". import "middleUri".'''.sadl.model.assertEquals('myUri', 'middleUri',
			'parentUri', SADL_IMPLICIT_MODEL_URI);
	}

	private def getModel(Resource it) {
		return contents.filter(SadlModel).head;
	}

	private def assertEquals(SadlModel model, String... expected) {
		assertEquals(model.allImportedResourceUris, Arrays.asList(expected));
	}

	private def assertEquals(Iterable<String> expected, Iterable<String> actual) {
		Assert.assertEquals(expected.sort.join('\n'), actual.sort.join('\n'));
	}

}
