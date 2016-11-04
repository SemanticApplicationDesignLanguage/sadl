import openWebSocket from './languageServer/plugin';
import workspace from './languageServer/protocol/workspace';

// TODO this should happen after the static monaco load in index.html
// TODO remove window.onLoad()
window.onload = () => {

    const xhr = new XMLHttpRequest();
    const restEndpoint = location.href + 'rest/rootPathProvider';
    xhr.open('GET', restEndpoint);
    xhr.onload = function () {
        if (xhr.status === 200) {
            const rootPath = xhr.responseText;
            const client = openWebSocket(rootPath);

			const fileExtension = 'sadl';
            const languageId = 'com.ge.research.sadl.SADL';
            // Register the syntax coloring for the web-calc language
            const conf = require('./languageServer/languages/sadlLanguageSyntax').conf;
            monaco.languages.setLanguageConfiguration(languageId, conf);
            monaco.languages.setMonarchTokensProvider(languageId, conf);

            const editor = monaco.editor.create(document.getElementById('monaco_editor_div'));

            const updateDocument = () => {
                const uri = editor.getModel().uri.toString();
                const languageId = editor.getModel().getModeId();
                workspace.update(uri, languageId, editor.getModel().getValue());
            }
            editor.onDidChangeModel(updateDocument)
            editor.onDidChangeModelContent(updateDocument);

            const model = monaco.editor.createModel(getEditorInitContent(), languageId, monaco.Uri.parse('file://' + rootPath + '/dummy.' + fileExtension));
            editor.setModel(model);
        }
        else {
            throw Error('Error while getting root path for language server. Return status was: ' + xhr.status);
        }
    };
    xhr.send();
}

function getEditorInitContent(): string {
    return [
        ''
	].join('');
}
