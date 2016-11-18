import {
    TextDocument
} from 'vscode-languageserver-types';

import {
    Event, Emitter
} from 'vscode-jsonrpc/lib/events';

export interface IDocumentManager {
    onDidOpenTextDocument: Event<TextDocument>;
    onDidChangeTextDocument: Event<TextDocument>;
    onDidSaveTextDocument: Event<TextDocument>;
    onDidCloseTextDocument: Event<TextDocument>;

    isOpened(uri: string): boolean;
    getContent(uri: string): string | null;
    getLanguage(uri: string): string | null;

    open(uri: string, language: string, content: string): void;
    setContent(uri: string, content: string): void;
    update(uri: string, language: string, content: string): void;
    rename(oldUri: string, newUri: string, language: string): void;
    save(uri: string): void;
    close(uri: string): void;

    getAll(): string[];
    openAll(): void;
    saveAll(): void;
    closeAll(): void;
}

export class DocumentManager implements IDocumentManager {

    private onDidOpenTextDocumentEmitter = new Emitter<TextDocument>();
    readonly onDidOpenTextDocument = this.onDidOpenTextDocumentEmitter.event;

    private onDidChangeTextDocumentEmitter = new Emitter<TextDocument>();
    readonly onDidChangeTextDocument = this.onDidChangeTextDocumentEmitter.event;

    private onDidSaveTextDocumentEmitter = new Emitter<TextDocument>();
    readonly onDidSaveTextDocument = this.onDidSaveTextDocumentEmitter.event;

    private onDidCloseTextDocumentEmitter = new Emitter<TextDocument>();
    readonly onDidCloseTextDocument = this.onDidCloseTextDocumentEmitter.event;

    private _documents: { [uri: string]: TextDocument | null } = {};

    isOpened(uri: string) {
        const document = this._documents[uri];
        return document !== null && document !== undefined;
    }

    getContent(uri: string) {
        const document = this._documents[uri];
        return document ? document.getText() : null;
    }

    getLanguage(uri: string): string | null {
        const document = this._documents[uri];
        return document ? document.languageId : null;
    }

    open(uri: string, language: string, content: string) {
        if (this.createDocument(uri, language, content)) {
            this.fireOpen(uri);
        }
    }

    setContent(uri: string, content: string) {
        if (this.updateDocument(uri, content)) {
            this.fireChange(uri);
        }
    }

    update(uri: string, language: string, content: string) {
        if (this.isOpened(uri)) {
            this.setContent(uri, content);
        } else {
            this.open(uri, language, content);
        }
    }

    rename(oldUri: string, newUri: string, language: string) {
        const document = this._documents[oldUri];
        if (document) {
            this.close(oldUri);
            this.open(newUri, language, document!.getText());
        }
    }

    save(uri: string) {
        this.fireSave(uri);
    }

    close(uri: string) {
        this.fireClose(uri);
        delete this._documents[uri];
    }

    getAll() {
        return Object.keys(this._documents);
    }

    openAll(): void {
        for (const uri in this._documents) {
            if (this._documents.hasOwnProperty(uri)) {
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

    protected createDocument(uri: string, language: string, content: string): boolean {
        if (this.isOpened(uri)) {
            return false;
        }
        this._documents[uri] = TextDocument.create(uri, language, 1, content);
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