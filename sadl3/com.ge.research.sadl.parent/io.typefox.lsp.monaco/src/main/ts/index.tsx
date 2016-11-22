import './resizer.css';
import './style.css';
import './contextmenu.css';

import {
    getRootPath, LanguageClient, registerLanguages
} from './client';

import {
    InferenceEditorService, InferenceResultProvider, sadlLanguage
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

import {
    DocumentManager
} from './documentManager';

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

const languages = [sadlLanguage];

// TODO this should happen after the static monaco load in index.html
// TODO remove window.onLoad()
window.onload = activate;

function activate(): void {
    registerLanguages(languages);

    const documentManager = new DocumentManager();

    let workbench: IWorkbench | null = null;
    const explorerPart = new ExplorerPart();
    const editorPart = new EditorPart({ documentManager });
    const inferenceEditorService = new InferenceEditorService();

    const app = <SplitPane split='vertical' minSize={300}>
        <Explorer
            onDidMount={explorer => explorerPart.explorer = explorer}
            onOpenFile={file => workbench!.open(file.uri)}
            onOpenFolder={file => workbench!.props.workspace.resolveFile(file.uri, 1)}
            onNewFile={(file, name) => workbench!.props.workspace.createFile(file.uri + name)}
            onNewFolder={(file, name) => workbench!.props.workspace.createDirectory(file.uri + name)}
            onDelete={file => workbench!.props.workspace.deleteFile(file.uri)}
            />
        <Editor onEditorDidMount={editor => {
            editorPart.onEditorDidMount(editor);
            inferenceEditorService.editor = editor;
        } }
            onEditorWillUnmount={e => editorPart.onEditorWillUnmount(e)}
            />
    </SplitPane>
    renderApp(app);

    getRootPath(rootPathProviderUrl).then(rootPath => {
        const { webSocket, connection } = createWebSocketConnection(languageServerUrl);
        const workspace = new RemoteWorkspace({ rootPath, connection });
        workbench = new Workbench({
            workspace, documentManager, explorerPart, editorPart
        });
        webSocket.onopen = () => {
            const languageClient = new LanguageClient({
                documentManager, connection, languages, rootPath
            });
            languageClient.start();

            inferenceEditorService.provider = new InferenceResultProvider(connection);

            workbench.openWorkspace();
        };
    });
}

function renderApp(app: JSX.Element, callback?: () => void) {
    const appContainer = document.createElement('app-container');
    document.body.appendChild(appContainer);
    render(app, appContainer, callback);
}
