import {
    RequestType
} from 'vscode-jsonrpc';

import {
    File, FileContent
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

export interface ResolveFileContentParams {
    uri: string;
}

export namespace ResolveFileContentRequest {
    export const type: RequestType<ResolveFileContentParams, FileContent, void, void> = {
        get method() {
            return 'workspace/resolveFileContent'
        },
        _: undefined
    }
}
