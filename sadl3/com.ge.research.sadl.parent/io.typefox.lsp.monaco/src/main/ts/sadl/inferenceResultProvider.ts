import { MessageConnection } from 'vscode-jsonrpc';
import { InferenceResult, InferenceResultsRequest } from './protocol';

export class InferenceResultProvider {

    constructor(protected connection: MessageConnection) {
    }

    provideInferenceResults(uri: string): Thenable<InferenceResult[]> {
        const params = { uri };
        return this.connection.sendRequest(InferenceResultsRequest.type, params);
    }

}