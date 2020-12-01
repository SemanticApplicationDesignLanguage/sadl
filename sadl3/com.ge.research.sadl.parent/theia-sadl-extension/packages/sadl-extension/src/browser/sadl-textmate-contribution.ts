import { injectable } from 'inversify';
import { LanguageGrammarDefinitionContribution, TextmateRegistry } from '@theia/monaco/lib/browser/textmate';

@injectable()
export class SadlTextmateContribution implements LanguageGrammarDefinitionContribution {

    registerTextmateLanguage(registry: TextmateRegistry) {
        const scope = 'source.sadl';
        const content = require('../../data/grammars/sadl.tmlanguage.json');
        registry.registerTextmateGrammarScope(scope, {
            async getGrammarDefinition() {
                return {
                    format: 'json',
                    content
                };
            }
        });
        registry.registerGrammarConfiguration('sadl', {
            tokenizerOption: {
                // no max char per line restriction
            }
        });
        registry.mapLanguageIdToTextmateGrammar('sadl', scope);
    }

}
