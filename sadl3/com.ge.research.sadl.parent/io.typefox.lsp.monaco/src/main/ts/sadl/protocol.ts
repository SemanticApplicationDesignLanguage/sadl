import { RequestType } from 'vscode-jsonrpc';
import * as lstypes from 'vscode-languageserver-types'

export const FAILED = 0;
export const PASSED = 1;

export interface InferenceResult {
    range: lstypes.Range;
    status: number;
    value: string;
}

export interface InferenceParams {
    uri: string;
    properties?: {
        [name: string]: string
    }
}

export namespace InferenceResultsRequest {
    export const type: RequestType<InferenceParams, InferenceResult[], void, void> = {
        get method() {
            return 'sadl/inferenceResults'
        },
        _: undefined
    }
}