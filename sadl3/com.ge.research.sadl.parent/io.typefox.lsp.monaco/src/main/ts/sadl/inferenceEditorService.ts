import { MessageConnection } from 'vscode-jsonrpc';
import { LanguageClient } from '../client';
import { toEvaluationResultData } from './EvaluationResultData'
import { EvaluationResults } from './EvaluationResults'
import { InferenceResultProvider } from './InferenceResultProvider'
import { InferenceResult } from './protocol';

export class InferenceEditorService {

    provider: InferenceResultProvider | null;
    private _editor: monaco.editor.IStandaloneCodeEditor | null;
    private results: EvaluationResults;

    get editor() {
        return this._editor!;
    }

    set editor(editor: monaco.editor.IStandaloneCodeEditor)  {
        this._editor = editor;
        this.results = new EvaluationResults(editor);
        editor.addAction({
            id: 'sadl-run-tests',
            label: 'Run Tests',
            contextMenuGroupId: 'sadl-group',
            run: () => {
                this.runTests();
                return null;
            }
        });
        const updateResults = () => this.results.update();
        editor.onDidChangeModel(() => updateResults());
        editor.onDidChangeModelContent(() => updateResults());
    }

    protected runTests(): void {
        if (this.provider) {
            const uri = this.editor.getModel().uri;
            this.provider.provideInferenceResults(uri.toString()).then((result) => this.showResults(result));
        }
    }

    protected showResults(results: InferenceResult[]) {
        toEvaluationResultData(results).forEach((r) => {
            this.results.push(r);
        });
        this.results.update();
    }

}


