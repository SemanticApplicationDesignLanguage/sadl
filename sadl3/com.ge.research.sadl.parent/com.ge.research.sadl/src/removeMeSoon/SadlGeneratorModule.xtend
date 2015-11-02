package removeMeSoon

import org.eclipse.xtext.Grammar
import org.eclipse.xtext.xtext.generator.DefaultGeneratorModule
import org.eclipse.xtext.xtext.generator.parser.antlr.AntlrContentAssistGrammarGenerator
import org.eclipse.xtext.xtext.generator.parser.antlr.AntlrOptions

class SadlGeneratorModule extends DefaultGeneratorModule {
	
	def Class<? extends AntlrContentAssistGrammarGenerator> bindAntlrContentAssistGrammarGenerator() {
		return FixedAntlrContentAssistGenerator
	}
	
	static class FixedAntlrContentAssistGenerator extends AntlrContentAssistGrammarGenerator {
		
		override protected isParserBackTracking(Grammar it, AntlrOptions options) {
			return true
		}
		
	}
}