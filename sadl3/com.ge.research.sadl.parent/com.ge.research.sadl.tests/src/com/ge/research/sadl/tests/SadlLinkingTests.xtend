package com.ge.research.sadl.tests

import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlLinkingTests {
	
	@Inject ParseHelper<SadlModel> parseHelper
	
	@Test def void testImportsLink() {
		val first = parseHelper.parse('''
			uri "http://sadl.org.Tests/ModelName" alias foo.
		''')
		val second = parseHelper.parse('''
			uri "http://sadl.org/Tests/Import" alias imp.
			import "http://sadl.org.Tests/ModelName".
		''', first.eResource.resourceSet)
		assertSame(first, second.imports.head.importedResource)
	}
}