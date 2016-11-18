import {
    File, FileContent
} from './types';

import {
    ResolveFileRequest, ResolveFileContentRequest, UpdateFileContentRequest
} from './protocol';

import {
    MessageConnection
} from 'vscode-jsonrpc';

export interface IWorkspace {
    readonly rootPath: string;
    resolveFile(uri: string, maxDepth?: number): PromiseLike<File | null>;
    resolveFileContent(uri: string): PromiseLike<FileContent | null>;
    updateFileContent(uri: string, content: FileContent): PromiseLike<void>;
}

export class RemoteWorkspace implements IWorkspace {

    readonly rootPath: string;

    constructor(protected props: RemoteWorkspace.Props) {
        this.rootPath = props.rootPath;
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
