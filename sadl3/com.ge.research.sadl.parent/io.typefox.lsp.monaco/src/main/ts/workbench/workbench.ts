import {
    IWorkspace
} from '../workspace';

import {
    IDocumentManager
} from '../documentManager';

import {
    IEditorPart
} from '../editor';

import {
    IExplorerPart
} from '../explorer';

export interface IWorkbench {
    readonly props: IWorkbench.Props
    openWorkspace(): void;
    open(uri: string): void;
}

export namespace IWorkbench {
    export interface Props {
        readonly workspace: IWorkspace;
        readonly documentManager: IDocumentManager;
        readonly explorerPart: IExplorerPart;
        readonly editorPart: IEditorPart;
    }
}


export class Workbench implements IWorkbench {

    constructor(readonly props: IWorkbench.Props) {
        this.updateFileContentOnDocumentSave();
    }

    protected updateFileContentOnDocumentSave() {
        this.props.documentManager.onDidSaveTextDocument(document => {
            this.props.workspace.updateFileContent(document.uri, {
                value: document.getText()
            });
        });
    }

    openWorkspace(): void {
        const uri = 'file://' + this.props.workspace.rootPath
        this.props.workspace.resolveFile(uri, 1).then(
            file => this.props.explorerPart.open(file)
        );
    }

    open(uri: string): void {
        this.props.workspace.resolveFileContent(uri).then(
            content => this.props.editorPart.open(uri, content)
        );
    }

}
