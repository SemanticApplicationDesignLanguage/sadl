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
package com.ge.research.sadl.tests

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.google.inject.Inject
import java.util.ArrayList
import java.util.List
import java.util.regex.Pattern
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.ToString
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.util.TextRegion
import org.junit.Assert
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
abstract class AbstractLinkingTest {
	
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
	
	protected def void assertLinking(CharSequence contents, (CharSequence)=>XtextResource parser) {
		val markerFile = parseReferenceMarker(contents)
		val model = parser.apply(markerFile.parseableContents)
		val allranges = new ArrayList(markerFile.declarations)
		allranges.addAll(markerFile.references)
		for (decl : allranges) {
			val node = NodeModelUtils.findLeafNodeAtOffset(model.parseResult.rootNode, decl.value.offset)
			val obj = NodeModelUtils.findActualSemanticObjectFor(node)
			if (obj instanceof SadlResource) {
				updateActual(markerFile, decl.key, decl.value, obj, obj.name)
			} else if (obj instanceof SadlSimpleTypeReference) {
				updateActual(markerFile, decl.key, decl.value, obj, obj.type)
			} else if (obj instanceof SadlPropertyInitializer) {
				updateActual(markerFile, decl.key, decl.value, obj, obj.property)
			} else {
				Assert.fail("unexpected node "+obj)
			}
		}
		Assert.assertEquals(markerFile.actualContents, markerFile.originalContents)
	}
	
	private def void updateActual(TestFile markerFile, String name, TextRegion region, EObject callSite, EObject reference) {
		if (callSite === reference) {
			markerFile.actualDeclarations.add(name -> region)
		} else if (!reference.eIsProxy) {
			markerFile.actualReferences.add(name -> region)
		} else {
			markerFile.actualUnresolved.add(name -> region)
		}
	}
	
	@Accessors @ToString private static class TestFile {
		String originalContents
		String parseableContents
		List<Pair<String, TextRegion>> declarations = newArrayList()
		List<Pair<String, TextRegion>> references = newArrayList()
		List<Pair<String, TextRegion>> actualDeclarations = newArrayList()
		List<Pair<String, TextRegion>> actualReferences = newArrayList()
		List<Pair<String, TextRegion>> actualUnresolved = newArrayList()
		
		def String getActualContents() {
			var result = originalContents
			for (decl : actualDeclarations) {
				result = result.substring(0,decl.value.offset) + "["+ decl.key + "]" + result.substring(decl.value.offset + decl.value.length)
			}
			for (decl : actualReferences) {
				result = result.substring(0,decl.value.offset) + "<" + decl.key + ">" + result.substring(decl.value.offset + decl.value.length)
			}
			for (decl : actualUnresolved) {
				result = result.substring(0,decl.value.offset) + "?" + decl.key + "?" + result.substring(decl.value.offset + decl.value.length)
			}
			return result
		}
	}
	
	private static val NAME = "[a-zA-Z0-9\\.:]+"
	private static val declarationMarker = Pattern.compile("\\[("+NAME+")\\]")
	private static val referenceMarker = Pattern.compile("\\<("+NAME+")\\>")
	
	private def TestFile parseReferenceMarker(CharSequence txt) {
		val result = new TestFile() => [
			originalContents = txt.toString
		]
		val declarationMatcher = declarationMarker.matcher(txt)
		while (declarationMatcher.find) {
			val region = new TextRegion(declarationMatcher.start, declarationMatcher.end - declarationMatcher.start)
			val name = declarationMatcher.group(1)
			result.declarations.add(name -> region)
		}
		val withoutDeclarations = declarationMatcher.replaceAll("_$1_")
		val referenceMatcher = referenceMarker.matcher(withoutDeclarations)
		while (referenceMatcher.find) {
			val region = new TextRegion(referenceMatcher.start, referenceMatcher.end - referenceMatcher.start)
			val name = referenceMatcher.group(1)
			result.references.add(name -> region)
		}
		result.parseableContents = referenceMatcher.replaceAll("_$1_")
		return result
	}
}