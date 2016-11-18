import './style.css';
import './resizer.css';

import {
    registerLanguages, LanguageClient, workspace, getRootPath
} from './client';

import {
    sadlLanguage
} from './sadl';

import {
    createWebSocketConnection
} from './jsonrpc-websocket';

import {
    URL, wsProtocol
} from './utils/network';

import {
    Explorer, ExplorerPart
} from './explorer';

import {
    Editor, EditorPart
} from './editor';

import * as React from 'react';
import { render } from 'react-dom';

import * as SplitPane from 'react-split-pane';

import {
    RemoteWorkspace
} from './workspace';

import {
    IWorkbench, Workbench
} from './workbench';

const port = 8080;
const basePath = 'sadlmonaco';

const rootPathProviderUrl = new URL({
    port, basePath,
    path: 'rest/rootPathProvider'
});

const languageServerUrl = new URL({
    protocol: wsProtocol,
    port, basePath,
    path: 'languageServer'
});

const supportedLanguages = [sadlLanguage];

// TODO this should happen after the static monaco load in index.html
// TODO remove window.onLoad()
window.onload = activate;

function activate(): void {
    registerLanguages(supportedLanguages);

    let workbench: IWorkbench | null = null;
    const explorerPart = new ExplorerPart();
    const editorPart = new EditorPart();

    const app = <SplitPane split='vertical' minSize={300}>
        <Explorer
            onDidMount={explorer => explorerPart.explorer = explorer}
            onOpen={file => workbench!.open(file.uri)}
            onExpand={file => workbench!.workspace.resolveFile(file.uri, 1)}
            />
        <Editor onEditorDidMount={e => editorPart.onEditorDidMount(e)}
            onEditorWillUnmount={e => editorPart.onEditorWillUnmount(e)}
            />
    </SplitPane>
    renderApp(app);

    getRootPath(rootPathProviderUrl).then(rootPath => {
        const { webSocket, connection } = createWebSocketConnection(languageServerUrl);
        webSocket.onopen = () => {
            const workspace = new RemoteWorkspace({ rootPath, connection });
            const languageClient = new LanguageClient(connection, supportedLanguages, rootPath);
            languageClient.start();

            workbench = new Workbench({ workspace, explorerPart, editorPart });
            workbench.openWorkspace();
        };
    });
}

function renderApp(app: JSX.Element, callback?: () => void) {
    const appContainer = document.createElement('app-container');
    document.body.appendChild(appContainer);
    render(app, appContainer, callback);
}
