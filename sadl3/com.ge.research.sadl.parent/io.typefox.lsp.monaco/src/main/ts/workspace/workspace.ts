import {
    File
} from './types';

import {
    ResolveFileRequest
} from './protocol';

import {
    MessageConnection
} from 'vscode-jsonrpc'

export interface IWorkspace {
    readonly rootPath: string;
    resolveFile(uri: string, maxDepth?: number): Thenable<File | null>;
}

export class RemoteWorkspace implements IWorkspace {

    readonly rootPath: string;

    constructor(protected props: RemoteWorkspace.Props) {
        this.rootPath = props.rootPath;
    }

    resolveFile(uri: string, maxDepth?: number): Thenable<File | null> {
        const params = { uri, maxDepth };
        return this.props.connection.sendRequest(ResolveFileRequest.type, params);
    }

}

export namespace RemoteWorkspace {
    export interface Props {
        readonly rootPath: string;
        readonly connection: MessageConnection;
    }
}
