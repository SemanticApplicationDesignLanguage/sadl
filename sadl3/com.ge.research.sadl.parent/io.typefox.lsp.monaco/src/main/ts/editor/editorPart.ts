import {
    FileContent
} from '../workspace';

import {
    IDocumentManager
} from '../documentManager';

import {
    findLanguageIdByURI
} from '../monaco/languages';

export interface IEditorPart {
    open(uri: string, fileContent: FileContent | null): void;
}

export namespace EditorPart {
    export interface Props {
        readonly documentManager: IDocumentManager;
    }
}

export class EditorPart implements IEditorPart {

    protected model: monaco.editor.IModel | null;
    protected editor: monaco.editor.IStandaloneCodeEditor | null;

    constructor(protected props: EditorPart.Props) { }

    onEditorDidMount(editor: monaco.editor.IStandaloneCodeEditor) {
        this.editor = editor;
        editor.onDidChangeModel(e => {
            if (e.oldModelUrl) {
                this.props.documentManager.close(e.oldModelUrl.toString());
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
            const modelUri = editor.getModel().uri;
            if (modelUri) {
                const uri = modelUri.toString();
                const languageId = editor.getModel().getModeId();
                this.props.documentManager.update(uri, languageId, editor.getModel().getValue());
                this.props.documentManager.save(uri);
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
                    const languageId = findLanguageIdByURI(uri) !;
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
