/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.validation

import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.ge.research.sadl.validation.SADLValidator
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class ValidationTest {
	
	@Test def void testDuplicateAlias() {
		'''
			uri "http://my/uri" alias foo.
		'''.sadl
		'''
		    uri "http://other/uri" alias foo.
		'''.sadl => [
			validationTestHelper.assertError(it, SADLPackage.Literals.SADL_MODEL, SADLValidator.INVALID_MODEL_ALIAS)
		]
	}
	
	@Test def void testDuplicateSimpleFileName() {
		val uri = '''
			uri "http://my/uri1" alias foo.
		'''.sadl.URI
		parseHelper.parse('''
		    uri "http://my/uri2" alias bar.
		''', uri.trimSegments(1).appendSegment("subpath").appendSegment(uri.lastSegment), currentResourceSet) => [
			validationTestHelper.assertError(it, SADLPackage.Literals.SADL_MODEL, SADLValidator.INVALID_MODEL_FILENAME)
		]
	}

	@Inject ValidationTestHelper validationTestHelper
	@Inject ParseHelper<SadlModel> parseHelper
	XtextResourceSet currentResourceSet

	protected def XtextResource sadl(CharSequence contents) {
		val resource = if (currentResourceSet === null) {
				parseHelper.parse(contents).eResource as XtextResource
			} else {
				parseHelper.parse(contents, currentResourceSet).eResource as XtextResource
			}
		currentResourceSet = resource.resourceSet as XtextResourceSet
		return resource
	}
}
