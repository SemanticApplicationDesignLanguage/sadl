package com.ge.research.sadl.tests

import com.ge.research.sadl.parser.antlr.lexer.InternalSADLLexer
import com.google.inject.Inject
import com.google.inject.Provider
import org.antlr.runtime.ANTLRStringStream
import org.antlr.runtime.Token
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

import static com.ge.research.sadl.parser.antlr.lexer.InternalSADLLexer.*

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class LexerTest {
	
	static val _ = 9999
	
	@Inject Provider<InternalSADLLexer> lexerProvider
	
	
	@Test def void testLexer1() {
		'''
			uri "http://sadl.org/model1" alias m1.
						Food is a class.
						Pizza is a type of Food.
		'''.assertTokens(
			Uri, RULE_STRING, Alias, RULE_ID, FullStop,
			RULE_ID, Is, _, Class, FullStop,
			RULE_ID, Is, _, Type, Of, RULE_ID, FullStop
		)
	}
	
	@Test def void testLexer() {
		val lexer = lexerProvider.get()
		lexer.charStream = new ANTLRStringStream('''
			324234.454
			.
			test.foo
			test.
			2342.
		''')
		lexer.nextToken => RULE_NUMBER
		lexer.nextToken => RULE_WS
		
		lexer.nextToken => FullStop
		lexer.nextToken => RULE_WS
		
		lexer.nextToken => RULE_ID
		lexer.nextToken => RULE_WS
		
		lexer.nextToken => RULE_ID
		lexer.nextToken => FullStop
		lexer.nextToken => RULE_WS
		
		lexer.nextToken => RULE_NUMBER
		lexer.nextToken => FullStop
		lexer.nextToken => RULE_WS
		
		lexer.nextToken => EOF
				
	}
	
	def assertTokens(CharSequence txt, int... types) {
		val lexer = lexerProvider.get()
		lexer.charStream = new ANTLRStringStream(txt.toString)
		for (t : types) {
			var token = lexer.nextToken
			println(token)
			if (token.type === RULE_WS) {
				token = lexer.nextToken
				println(token)
			}
			if (t !== _) {
				 token => t
			}
		}
	}
	
	def =>(Token token, int type) {
		Assert.assertEquals(token.toString, type, token.type)
	}
}