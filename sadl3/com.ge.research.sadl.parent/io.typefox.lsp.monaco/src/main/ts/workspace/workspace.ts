import {
    File, FileContent
} from './types';

import {
    CreateFileRequest, CreateDirectoryRequest, DeleteFileRequest,
    RenameFileRequest,
    ResolveFileRequest, ResolveFileContentRequest, UpdateFileContentRequest
} from './protocol';

import {
    MessageConnection
} from 'vscode-jsonrpc';

import * as protocol from 'vscode-languageclient/lib/protocol';

export interface IWorkspace {
    readonly rootPath: string;
    createFile(uri: string, content?: FileContent): PromiseLike<void>;
    createDirectory(uri: string): PromiseLike<void>;
    deleteFile(uri: string): PromiseLike<void>;
    renameFile(oldUri: string, newUri: string): PromiseLike<void>;
    resolveFile(uri: string, maxDepth?: number): PromiseLike<File | null>;
    resolveFileContent(uri: string): PromiseLike<FileContent | null>;
    updateFileContent(uri: string, content: FileContent): PromiseLike<void>;
}


export class RemoteWorkspace implements IWorkspace {
    
    readonly rootPath: string;

    constructor(protected props: RemoteWorkspace.Props) {
        this.rootPath = props.rootPath;
        props.connection.onNotification(protocol.DidChangeWatchedFilesNotification.type, (p) => {
            if (this.handler) {
                this.handler(p.changes);
            }
        });
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

    renameFile(oldUri: string, newUri: string): PromiseLike<void> {
        const params = { oldUri, newUri };
        return this.props.connection.sendRequest(RenameFileRequest.type, params);
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

    handler :  RemoteWorkspace.FileChangeHandler;

    onFileChange(handler :  RemoteWorkspace.FileChangeHandler) {
        this.handler = handler;
    }

}

export namespace RemoteWorkspace {
    
    export type FileChangeHandler = (events:protocol.FileEvent[]) => void;

    export interface Props {
        readonly rootPath: string;
        readonly connection: MessageConnection;
    }
}
