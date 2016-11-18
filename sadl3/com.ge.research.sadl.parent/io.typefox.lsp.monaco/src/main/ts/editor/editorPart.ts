import {
    FileContent
} from '../workspace';

import {
    workspace
} from '../client/protocol/workspace';

import {
    findLanguageIdByURI
} from '../monaco/languages';

export interface IEditorPart {
    open(uri: string, fileContent: FileContent | null): void;
}

export class EditorPart implements IEditorPart {

    protected model: monaco.editor.IModel | null;
    protected editor: monaco.editor.IStandaloneCodeEditor | null;

    onEditorDidMount(editor: monaco.editor.IStandaloneCodeEditor) {
        this.editor = editor;
        editor.onDidChangeModel(e => {
            if (e.oldModelUrl) {
                workspace.close(e.oldModelUrl.toString());
            }
            if (this.editor) {
                this.updateDocument();
                this.editor.getModel().onDidChangeContent(() => this.updateDocument());
            }
        });
    }

    onEditorWillUnmount(editor: monaco.editor.IStandaloneCodeEditor) {
        this.editor = null;
        if (this.model) {
            this.model.dispose();
            this.model = null;
        }
    }

    protected updateDocument() {
        const editor = this.editor;
        if (editor) {
            const uri = editor.getModel().uri;
            if (uri) {
                const languageId = editor.getModel().getModeId();
                workspace.update(uri.toString(), languageId, editor.getModel().getValue());
            }
        }
    }

    open(uri: string, fileContent: FileContent | null) {
        if (this.editor) {
            if (fileContent) {
                const oldModel = this.editor.getModel();
                if (oldModel.uri.toString() === uri) {
                    oldModel.setValue(fileContent.value);
                } else {
                    const languageId = findLanguageIdByURI(uri)!;
                    this.updateModel(fileContent.value, languageId, uri);
                }
            } else {
                this.updateModel('');
            }
        }
    }

    protected updateModel(value: string, languageId?: string, uri?: string) {
        if (this.editor) {
            const oldModel = this.model;
            const modelUri = uri ? monaco.Uri.parse(uri) : undefined;
            this.model = monaco.editor.createModel(value, languageId, modelUri);
            this.editor.setModel(this.model);
            if (oldModel) {
                oldModel.dispose();
            }
        }
    }

}
