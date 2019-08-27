package com.ge.research.sadl.tests

import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.junit.Test
import org.junit.runner.RunWith
import org.eclipse.xtext.EcoreUtil2
import com.ge.research.sadl.sADL.SadlStatement
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import com.ge.research.sadl.services.SADLGrammarAccess
import org.eclipse.xtext.RuleCall

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
		'''.parse
		// Get the EOS rule
		val EOS = grammarAccess.EOSRule

		EcoreUtil2.getAllContentsOfType(model, SadlStatement).forEach [
			// Here, `it` is any kind of `SadlStatement`.
			// We locate the parse tree node from an AST node.
			val node = NodeModelUtils.findActualNodeFor(it)
			if (node !== null) {
				// We get the last child of the current node, to check if it ends with a `.`
				val lastChild = node.lastChild
				if (lastChild !== null) {
					val grammarElement = lastChild.grammarElement;
					if (grammarElement instanceof RuleCall) {
						if (grammarElement.rule === EOS) {
							println('SADL statement is complete: ' + node.text)
						}
					}
				}
			}
		]

	}

}