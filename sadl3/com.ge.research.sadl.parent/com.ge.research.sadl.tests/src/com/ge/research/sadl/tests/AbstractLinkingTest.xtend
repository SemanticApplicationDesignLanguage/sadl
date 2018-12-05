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

import com.ge.research.sadl.sADL.SadlPropertyCondition
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import java.util.List
import java.util.regex.Pattern
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtend.lib.annotations.ToString
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.util.TextRegion
import org.junit.Assert
import org.junit.runner.RunWith
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.tests.helpers.XtendTemplateHelper
import com.ge.research.sadl.sADL.SadlRangeRestriction

/**
 * Notes on writing linking tests:
 * <p>
 * <ul>
 * <li>When defining the SADL model, <b>[RESOURCE]</b> is the <b>definition</b> of a SADL resource and <i>&lt;RESOURCE&gt;</i> is the <i>reference</i>.</li>
 * <li>When CTRL/CMD + Clicking on an item, <b>[RESORUCE]</b> stays, as it is the <b>definition</b> and <i>&lt;RESOURCE&gt;</i> jumps to <b>[RESORUCE]</b>.</li>
 * </ul>
 * </p>
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
abstract class AbstractLinkingTest extends AbstractSadlTest {
	
	protected def void assertLinking(CharSequence contents, (CharSequence)=>XtextResource parser) {
		val escapedContents = XtendTemplateHelper.unifyEOL(contents);
		val markerFile = parseReferenceMarker(escapedContents)
		val model = parser.apply(markerFile.parseableContents)
		Assert.assertTrue(model.errors.map[message].join('\n'), model.errors.isEmpty)
		for (decl : markerFile.allNames) {
			val node = NodeModelUtils.findLeafNodeAtOffset(model.parseResult.rootNode, decl.region.offset)
			val obj = NodeModelUtils.findActualSemanticObjectFor(node)
			if (obj instanceof SadlResource) {
				updateActual(markerFile, decl.value, decl.region, obj, obj.name)
			} else if (obj instanceof SadlSimpleTypeReference) {
				updateActual(markerFile, decl.value, decl.region, obj, obj.type)
			} else if (obj instanceof SadlPropertyInitializer) {
				updateActual(markerFile, decl.value, decl.region, obj, obj.property)
			} else if (obj instanceof SadlPropertyCondition) {
				updateActual(markerFile, decl.value, decl.region, obj, obj.property)
			} else if (obj instanceof QueryStatement) {
//				updateActual(markerFile, decl.value, decl.region, obj, obj.expr)
			} else if (obj instanceof SadlRangeRestriction) {
			} else {
				Assert.fail("unexpected node "+obj)
			}
		}
		Assert.assertEquals(markerFile.originalContents, markerFile.actualContents)
	}
	
	private def void updateActual(TestFile markerFile, String name, TextRegion region, EObject callSite, EObject reference) {
		val Boolean isDeclaration = if (callSite === reference) {
			true
		} else if (!reference.eIsProxy) {
			false
		} else {
			null
		}
		markerFile.actualNames.add(new Name(name, isDeclaration, region))
	}
	
	@Accessors @ToString private static class TestFile {
		String originalContents
		String parseableContents
		List<Name> allNames = newArrayList()
		List<Name> actualNames = newArrayList()
		
		def String getActualContents() {
			var result = parseableContents
			var addedChars = 0
			for (decl : actualNames.sortBy[region.offset]) {
				val brackets = if (decl.isDeclaration === null) {
					"?"->"?" 
				} else if (decl.isDeclaration) {
					"["->"]"
				} else {
					"<"->">"
				}
				result = result.substring(0, decl.region.offset + addedChars) + brackets.key + decl.value + brackets.value + result.substring(decl.region.offset + decl.region.length + addedChars)
				addedChars += 2
			}
			return result
		}
	}
	
	@Data private static class Name {
		String value
		Boolean isDeclaration
		TextRegion region
	}
	
	static val NAME = "[a-zA-Z0-9_\\.:]+"
	static val markerPattern = Pattern.compile("[\\[\\<]("+NAME+")[\\]|\\>]")
	
	private def TestFile parseReferenceMarker(CharSequence txt) {
		val result = new TestFile() => [
			originalContents = txt.toString
		]
		val matcher = markerPattern.matcher(txt)
		var int charactersRemoved = 0
		while (matcher.find) {
			val region = new TextRegion(matcher.start - charactersRemoved, matcher.end - matcher.start - 2)
			charactersRemoved += 2
			val name = matcher.group(1)
			result.allNames.add(new Name(name, matcher.group.startsWith("["), region))
		}
		result.parseableContents = matcher.replaceAll("$1")
		result.allNames.forall[result.parseableContents.substring(region.offset, region.offset + region.length) == value]
		return result
	}
}