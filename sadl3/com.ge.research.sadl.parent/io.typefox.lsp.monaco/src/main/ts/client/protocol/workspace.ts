import {
    TextDocument
} from 'vscode-languageserver-types';

import {
    Event, Emitter
} from 'vscode-jsonrpc/lib/events';

export class Workspace {

    isDisposed: boolean = false;

    private onDidOpenTextDocumentEmitter = new Emitter<TextDocument>();
    onDidOpenTextDocument = this.onDidOpenTextDocumentEmitter.event;

    private onDidChangeTextDocumentEmitter = new Emitter<TextDocument>();
    onDidChangeTextDocument = this.onDidChangeTextDocumentEmitter.event;

    private onDidSaveTextDocumentEmitter = new Emitter<TextDocument>();
    onDidSaveTextDocument = this.onDidSaveTextDocumentEmitter.event;

    private onDidCloseTextDocumentEmitter = new Emitter<TextDocument>();
    onDidCloseTextDocument = this.onDidCloseTextDocumentEmitter.event;

    private _documents: { [uri: string]: TextDocument } = {}

    dispose() {
        if (this.isDisposed) {
            return;
        }
        this.isDisposed = true;
        this.closeAll();
        this._documents = null;

        this.onDidOpenTextDocumentEmitter.dispose();
        this.onDidOpenTextDocumentEmitter = null;
        this.onDidOpenTextDocument = null;

        this.onDidChangeTextDocumentEmitter.dispose();
        this.onDidChangeTextDocumentEmitter = null;
        this.onDidChangeTextDocument = null;

        this.onDidSaveTextDocumentEmitter.dispose();
        this.onDidSaveTextDocumentEmitter = null;
        this.onDidSaveTextDocument = null;

        this.onDidCloseTextDocumentEmitter.dispose();
        this.onDidCloseTextDocumentEmitter = null;
        this.onDidCloseTextDocument = null;
    }

    isOpened(uri: string) {
        const document = this._documents[uri];
        return document !== null && document !== undefined;
    }

    open(uri: string, languageId: string, content: string) {
        if (this.createDocument(uri, languageId, content)) {
            this.fireOpen(uri);
        }
    }

    setContent(uri: string, content: string) {
        if (this.updateDocument(uri, content)) {
            this.fireChange(uri);
        }
    }

    update(uri: string, languageId: string, content: string) {
        if (this.isOpened(uri)) {
            this.setContent(uri, content);
        } else {
            this.open(uri, languageId, content);
        }
    }

    rename(oldUri: string, newUri: string, languageId: string) {
        if (this.isOpened(oldUri)) {
            const document = this._documents[oldUri];
            this.close(oldUri);
            this.open(newUri, languageId, document.getText());
        }
    }

    save(uri: string) {
        this.fireSave(uri);
    }

    close(uri: string) {
        this.fireClose(uri);
        delete this._documents[uri];
    }

    allOpened() {
        return this._documents;
    }

    openAll(): void {
        const allOpened = this.allOpened();
        for (const uri in allOpened) {
            if (allOpened.hasOwnProperty(uri)) {
                this.fireOpen(uri);
            }
        }
    }

    saveAll() {
        for (const uri in this._documents) {
            if (this._documents.hasOwnProperty(uri)) {
                this.save(uri);
            }
        }
    }

    closeAll() {
        for (const uri in this._documents) {
            if (this._documents.hasOwnProperty(uri)) {
                this.close(uri);
            }
        }
    }

    protected createDocument(uri: string, languageId: string, content: string): boolean {
        if (this.isOpened(uri)) {
            return false;
        }
        this._documents[uri] = TextDocument.create(uri, languageId, 1, content);
        return true;
    }

    protected updateDocument(uri: string, content: string): boolean {
        const document = this._documents[uri];
        if (!document) {
            return false;
        }
        if (document.getText() === content) {
            return false;
        }
        const version = document.version + 1;
        this._documents[uri] = TextDocument.create(document.uri, document.languageId, version, content);
        return true;
    }

    public fireOpen(uri: string) {
        this.fire(this.onDidOpenTextDocumentEmitter, uri);
    }

    protected fireSave(uri: string) {
        this.fire(this.onDidSaveTextDocumentEmitter, uri);
    }

    protected fireChange(uri: string) {
        this.fire(this.onDidChangeTextDocumentEmitter, uri);
    }

    protected fireClose(uri: string) {
        this.fire(this.onDidCloseTextDocumentEmitter, uri);
    }

    protected fire(emitter: Emitter<TextDocument>, uri: string) {
        const document = this._documents[uri];
        if (document) {
            try {
                emitter.fire(document);
            } catch (e) {
                console.error(e);
            }
        }
    }

}

// FIXME: move workspace out of protocol
export const workspace = new Workspace();
export default workspace;