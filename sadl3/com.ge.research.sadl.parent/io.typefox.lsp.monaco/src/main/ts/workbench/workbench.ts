import {
    IWorkspace
} from '../workspace';

import {
    IEditorPart
} from '../editor';

import {
    IExplorerPart
} from '../explorer';

export interface IWorkbench {
    readonly workspace: IWorkspace
    openWorkspace(): void;
    open(uri: string): void;
}

export class Workbench implements IWorkbench {

    constructor(protected props: Workbench.Props) { }

    get workspace() {
        return this.props.workspace;
    }

    openWorkspace(): void {
        const uri = 'file://' + this.workspace.rootPath
        this.workspace.resolveFile(uri, 1).then(
            file => this.props.explorerPart.open(file)
        );
    }

    open(uri: string): void {
        this.workspace.resolveContent(uri).then(
            content => this.props.editorPart.open(uri, content)
        );
    }

}

export namespace Workbench {
    export interface Props {
        readonly workspace: IWorkspace;
        readonly explorerPart: IExplorerPart;
        readonly editorPart: IEditorPart;
    }
}
