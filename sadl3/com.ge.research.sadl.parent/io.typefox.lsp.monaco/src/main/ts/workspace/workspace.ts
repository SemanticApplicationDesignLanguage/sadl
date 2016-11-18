import {
    File, FileContent
} from './types';

import {
    ResolveFileRequest, ResolveFileContentRequest
} from './protocol';

import {
    MessageConnection
} from 'vscode-jsonrpc'

export interface IWorkspace {
    readonly rootPath: string;
    resolveFile(uri: string, maxDepth?: number): PromiseLike<File | null>;
    resolveContent(uri: string): PromiseLike<FileContent | null>;
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

    resolveContent(uri: string): PromiseLike<FileContent | null> {
        const params = { uri };
        return this.props.connection.sendRequest(ResolveFileContentRequest.type, params);
    }

}

export namespace RemoteWorkspace {
    export interface Props {
        readonly rootPath: string;
        readonly connection: MessageConnection;
    }
}
