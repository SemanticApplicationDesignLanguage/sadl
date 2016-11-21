import {
    File, FileContent
} from './types';

import {
    CreateFileRequest, CreateDirectoryRequest, DeleteFileRequest,
    ResolveFileRequest, ResolveFileContentRequest, UpdateFileContentRequest
} from './protocol';

import {
    MessageConnection
} from 'vscode-jsonrpc';

export interface IWorkspace {
    readonly rootPath: string;
    createFile(uri: string, content?: FileContent): PromiseLike<void>;
    createDirectory(uri: string): PromiseLike<void>;
    deleteFile(uri: string): PromiseLike<void>;
    resolveFile(uri: string, maxDepth?: number): PromiseLike<File | null>;
    resolveFileContent(uri: string): PromiseLike<FileContent | null>;
    updateFileContent(uri: string, content: FileContent): PromiseLike<void>;
}

export class RemoteWorkspace implements IWorkspace {

    readonly rootPath: string;

    constructor(protected props: RemoteWorkspace.Props) {
        this.rootPath = props.rootPath;
    }

    createFile(uri: string, content?: FileContent): PromiseLike<void> {
        const params = { uri, content };
        return this.props.connection.sendRequest(CreateFileRequest.type, params);
    }

    createDirectory(uri: string): PromiseLike<void> {
        const params = { uri };
        return this.props.connection.sendRequest(CreateDirectoryRequest.type, params);
    }

    deleteFile(uri: string): PromiseLike<void> {
        const params = { uri };
        return this.props.connection.sendRequest(DeleteFileRequest.type, params);
    }

    resolveFile(uri: string, maxDepth?: number): PromiseLike<File | null> {
        const params = { uri, maxDepth };
        return this.props.connection.sendRequest(ResolveFileRequest.type, params);
    }

    resolveFileContent(uri: string): PromiseLike<FileContent | null> {
        const params = { uri };
        return this.props.connection.sendRequest(ResolveFileContentRequest.type, params);
    }

    updateFileContent(uri: string, content: FileContent): PromiseLike<void> {
        const params = { uri, content };
        return this.props.connection.sendRequest(UpdateFileContentRequest.type, params);
    }

}

export namespace RemoteWorkspace {
    export interface Props {
        readonly rootPath: string;
        readonly connection: MessageConnection;
    }
}
