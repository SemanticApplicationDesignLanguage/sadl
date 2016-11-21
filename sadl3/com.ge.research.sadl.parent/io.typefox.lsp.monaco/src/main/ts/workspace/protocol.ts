import {
    RequestType
} from 'vscode-jsonrpc';

import {
    File, FileContent
} from './types';

export interface CreateFileParams {
    uri: string;
    content?: FileContent;
}

export namespace CreateFileRequest {
    export const type: RequestType<CreateFileParams, void, void, void> = {
        get method() {
            return 'workspace/createFile'
        },
        _: undefined
    }
}

export interface CreateDirectoryParams {
    uri: string;
}

export namespace CreateDirectoryRequest {
    export const type: RequestType<CreateDirectoryParams, void, void, void> = {
        get method() {
            return 'workspace/createDirectory'
        },
        _: undefined
    }
}

export interface DeleteFileParams {
    uri: string;
}

export namespace DeleteFileRequest {
    export const type: RequestType<DeleteFileParams, void, void, void> = {
        get method() {
            return 'workspace/deleteFile'
        },
        _: undefined
    }
}

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

export interface UpdateFileContentParams {
    uri: string;
    content: FileContent;
}

export namespace UpdateFileContentRequest {
    export const type: RequestType<UpdateFileContentParams, void, void, void> = {
        get method() {
            return 'workspace/updateFileContent'
        },
        _: undefined
    }
}
