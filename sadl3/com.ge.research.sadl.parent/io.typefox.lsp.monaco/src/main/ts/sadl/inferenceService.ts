import { MessageConnection } from 'vscode-jsonrpc';
import { Disposable } from 'vscode-jsonrpc/lib/events';
import { LanguageClient } from '../client';
import { InferenceResultsRequest, InferenceResult, InferenceParams } from './protocol';

export class InferenceEditorService {

    provider: InferenceResultProvider | null
    private _editor: monaco.editor.IStandaloneCodeEditor | null;

    get editor() {
        return this._editor!;
    }

    set editor(editor: monaco.editor.IStandaloneCodeEditor)  {
        this._editor = editor;
        editor.addAction({
            id: 'sadl-run-tests',
            label: 'Run Tests',
            contextMenuGroupId: 'sadl-group',
            run: () => {
                this.runTests();
                return null;
            }
        });
    }

    protected runTests(): void {
        if (this.provider) {
            const uri = this.editor.getModel().uri;
            this.provider.provideInferenceResults(uri.toString()).then(result => this.showResults(result));
        }
    }

    protected showResults(result: InferenceResult[]) {
        alert(result);
    }

}

export class InferenceResultProvider {

    constructor(protected connection: MessageConnection) {
    }

    provideInferenceResults(uri: string): Thenable<InferenceResult[]> {
        const params = { uri };
        return this.connection.sendRequest(InferenceResultsRequest.type, params);
    }

}
