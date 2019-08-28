package com.ge.research.sadl.tests

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlStatement
import com.ge.research.sadl.services.SADLGrammarAccess
import com.google.inject.Inject
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextSyntaxDiagnostic
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.hamcrest.CoreMatchers.*
import static org.junit.Assert.assertEquals
import static org.junit.Assert.assertThat

import static extension org.eclipse.xtext.util.Strings.toUnixLineSeparator

@RunWith(XtextRunner)
@InjectWith(SADLNoopModelProcessorsInjectorProvider)
class StatementIsCompleteTest {

	@Inject
	extension ParseHelper<SadlModel> parseHelper

	@Inject
	SADLGrammarAccess grammarAccess

	@Test
	def void test() {
		val model = '''
			uri  "http://sadl.org/Test.sadl" alias Test.
			
			Rock is
			
			Person is a class described by age with values of type int.
			ChildrenList is a type of Person List length 1-*.
			children describes Person with values of type ChildrenList.
			children of Person has at most 1 value.
			
			PersonList is a type of Person List.
			PersonListList is a type of PersonList List.
			
			Rock2 is
			
			foo describes Person with values of type PersonListList.
			bar describes Person with values of type Person List length 1-4.
			bar of Person only has values of type Person List.
			bar of Person only has values of type Person List length 1-4.
			bar of Person has at least one value of type Person List length 1-4.
			bar of Person has at least 1 value of type Person List length 1-4.
			bar of Person has at most 2 value of type Person List length 1-4.
			
			
			Rule R1: if x is a Person and
					x has bar y and 
					y is a Person List //length 1-4
					z is a Person List length 1-4
					then print("Hurray!"). //x has age 50.
		'''.parse as SadlModel
		val EOS = grammarAccess.EOSRule
		val syntaxErrors = model.eResource.errors.filter(XtextSyntaxDiagnostic)
		val statements = newHashMap
		model.elements.filter(SadlStatement).forEach [
			var complete = false
			val node = NodeModelUtils.findActualNodeFor(it)
			val lastChild = node?.lastChild
			if (lastChild !== null && !node.text.nullOrEmpty) {
				val grammarElement = lastChild.grammarElement;
				if (grammarElement instanceof RuleCall) {
					if (grammarElement.rule === EOS) {
						if (!syntaxErrors.exists[node.contains(it)]) {
							complete = true
						}
					}
				}
				statements.put(node.text.trim.toUnixLineSeparator, complete)
			}
		]

		val completeStatements = statements.entrySet.filter[value].map[key]
		val incompleteStatements = statements.entrySet.filter[!value].map[key]
		assertEquals(13, statements.size)
		assertEquals(2, incompleteStatements.size)
		assertThat(incompleteStatements, hasItems(
			'Rock is\n\nPerson is a class described by age with values of type int.'.toUnixLineSeparator,
			'Rock2 is\n\nfoo describes Person with values of type PersonListList.'.toUnixLineSeparator
		))
		assertThat(completeStatements, not(hasItems(
			'Person is a class described by age with values of type int.'.toUnixLineSeparator,
			'foo describes Person with values of type PersonListList.'.toUnixLineSeparator
		)))
	}

	private def boolean contains(INode node, Diagnostic diagnostic) {
		val offset = node.offset
		val length = node.length
		return offset <= diagnostic.offset && (offset + length) >= (diagnostic.offset + diagnostic.length)
	}

}
