package com.ge.research.sadl.tests

import com.ge.research.sadl.owl2sadl.OwlToSadl
import com.ge.research.sadl.parser.antlr.SADLParser
import com.google.common.collect.ImmutableList
import com.google.common.collect.Iterables
import com.google.inject.Inject
import org.eclipse.xtext.GrammarUtil
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

import static java.lang.String.CASE_INSENSITIVE_ORDER
import static org.junit.Assert.assertEquals

/**
 * Test to make sure our static list of SADL keywords are in sync with the grammar.
 * 
 * Background: We cannot use the `SADLStandaloneSetup` when Eclipse is running. To make sure,
 * we can access the keywords without an `EObject`, `XtextResource`, `Guice`, or the UI code
 * we store the keywords in a static list. This test makes sure, the keyword list is in sync with the grammar.
 * If this test fails, copy the keywords list to {@link OwlToSadl#getSadlKeywords()}.
 * 
 * See https://github.com/crapo/sadlos2/issues/369
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlKeywordsTest {

	@Inject
	extension SADLParser;

	@Test
	def void checkKeywordsInSync() {
		val actual = ImmutableList.copyOf(GrammarUtil.getAllKeywords(grammarAccess.grammar))
		val expected = OwlToSadl.sadlKeywords
		try {
			assertEquals(Iterables.toString(expected.sortWith(CASE_INSENSITIVE_ORDER).toList),
				Iterables.toString(actual.sortWith(CASE_INSENSITIVE_ORDER).toList));
		} catch (AssertionError e) {
			println('Your SADL keywords are out of sync.')
			println('Copy the new keywords to OwlToSadl#getSadlKeywords')
			println('---------- KEYWORDS BEBGIN | COPY STARTS BELOW THIS LINE -----------')
			print('return Arrays.asList(')
			println(actual.map['''"«it»"'''].join(',\n') + ');')
			println('---------- KEYWORDS END | COPY ENDS ABOVE THIS LINE -----------')
			throw e;
		}
	}

}
