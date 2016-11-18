import {
    RequestType
} from 'vscode-jsonrpc';

import {
    File
} from './types';

export interface ResolveFileParams {
    uri: string;
    maxDepth?: number;
}

export namespace ResolveFileRequest {
    export const type: RequestType<ResolveFileParams, File, void, void> = {
        get method() {
            return 'workspace/resolveFile'
        },
        _: undefined
    }
}
