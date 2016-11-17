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
window.onload = () => {
    registerLanguages(supportedLanguages);
    getRootPath(rootPathProviderUrl).then(rootPath => {
        createEditor(rootPath);

        const connection = createWebSocketConnection(languageServerUrl);
        connection.webSocket.onopen = () => {
            const languageClient = new LanguageClient(
                connection.connection, supportedLanguages, rootPath
            );
            languageClient.start();
        };
    });
}

function createEditor(rootPath: string) {
    const editor = monaco.editor.create(document.getElementById('monaco_editor_div'));
    const updateDocument = () => {
        const uri = editor.getModel().uri.toString();
        const languageId = editor.getModel().getModeId();
        workspace.update(uri, languageId, editor.getModel().getValue());
    }
    editor.onDidChangeModel(updateDocument)
    editor.onDidChangeModelContent(updateDocument);

    const model = monaco.editor.createModel(
        getEditorInitContent(),
        sadlLanguage.languageId,
        monaco.Uri.parse('file://' + rootPath + '/dummy.' + sadlLanguage.fileExtensions[0])
    );
    editor.setModel(model);
}

function getEditorInitContent(): string {
    return `uri "http://sadl.org/dummy.sadl".

Shape is a class described by area with values of type float.

Rectangle is a type of Shape,
	described by height with values of type float,
	described by width with values of type float.

Rule AreaOfRect: if x is a Rectangle then area of x is height of x * width of x.

MyRect is a Rectangle with height 2.5, with width 5.5.

Test: area of MyRect is 13.75.`;
}
