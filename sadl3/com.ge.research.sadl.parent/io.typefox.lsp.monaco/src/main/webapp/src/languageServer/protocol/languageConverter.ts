import * as is from 'vscode-jsonrpc/lib/is';
import * as lstypes from 'vscode-languageserver-types';
import * as protocol from 'vscode-languageclient/lib/protocol';
import { ProtocolCompletionItem } from './adapters';

export function asPosition(lineNumber: number, column: number): lstypes.Position {
    return lstypes.Position.create(lineNumber - 1, column - 1)
}

export function asRange(range: monaco.IRange): lstypes.Range {
    if (is.undefined(range)) {
        return undefined
    }

    if (is.nil(range)) {
        return null
    }

    return lstypes.Range.create(
        asPosition(range.startLineNumber, range.startColumn),
        asPosition(range.endLineNumber, range.endColumn)
    )
}

export function asTextEdit(textEdit: monaco.editor.ISingleEditOperation): lstypes.TextEdit {
    if (is.undefined(textEdit)) {
        return undefined
    }

    if (is.nil(textEdit)) {
        return null
    }

    return {
        range: asRange(textEdit.range),
        newText: textEdit.text
    }
}

export function asCompletionItem(completionItem: ProtocolCompletionItem): lstypes.CompletionItem {
    return {
        label: completionItem.label,
        detail: completionItem.detail,
        documentation: completionItem.documentation,
        filterText: completionItem.filterText,
        insertText: completionItem.insertText,
        kind: completionItem.kind + 1,
        sortText: completionItem.sortText,
        textEdit: asTextEdit(completionItem.textEdit),
        data: completionItem.data
    }
}

export type UriOwner = string | lstypes.TextDocument | monaco.editor.IReadOnlyModel;

export function asTextDocumentIdentifier(uriOwner:UriOwner): lstypes.TextDocumentIdentifier {
    if (is.string(uriOwner)) {
        return lstypes.TextDocumentIdentifier.create(uriOwner);
    }
    if (lstypes.TextDocument.is(uriOwner)) {
        return lstypes.TextDocumentIdentifier.create(uriOwner.uri);
    }
    const model = <monaco.editor.IReadOnlyModel>uriOwner;
    if (model && model.uri) {
        return lstypes.TextDocumentIdentifier.create(model.uri.toString());
    }
    throw Error('Unexpected: ' + uriOwner);
}

export function asTextDocumentPositionParams(uriOwner:UriOwner, position: monaco.IPosition): protocol.TextDocumentPositionParams {
    return {
        textDocument: asTextDocumentIdentifier(uriOwner),
        position: asPosition(position.lineNumber, position.column)
    };
}

export function asTextDocumentItem(textDocument: lstypes.TextDocument): lstypes.TextDocumentItem {
    return lstypes.TextDocumentItem.create(
        textDocument.uri,
        textDocument.languageId,
        textDocument.version,
        textDocument.getText()
    );
}

export function asVersionedTextDocumentIdentifier(textDocument: lstypes.TextDocument): lstypes.VersionedTextDocumentIdentifier {
    return lstypes.VersionedTextDocumentIdentifier.create(
        textDocument.uri,
        textDocument.version
    );
}

export function asDidOpenTextDocumentParams(textDocument: lstypes.TextDocument): protocol.DidOpenTextDocumentParams {
    return {
        textDocument: asTextDocumentItem(textDocument)
    };
}

export function asDidChangeTextDocumentParams(textDocument: lstypes.TextDocument): protocol.DidChangeTextDocumentParams {
    return {
        textDocument: asVersionedTextDocumentIdentifier(textDocument),
        contentChanges: [{
            text: textDocument.getText()
        }]
    };
}

export function asTextDocumentContentChangeEvent(change: lstypes.TextDocumentContentChangeEvent): protocol.TextDocumentContentChangeEvent {
    return {
        range: change.range,
        rangeLength: change.rangeLength,
        text: change.text
    };
}

export function asDidSaveTextDocumentParams(uriOwner:UriOwner): protocol.DidSaveTextDocumentParams {
    return {
        textDocument: asTextDocumentIdentifier(uriOwner)
    };
}

export function asDidCloseTextDocumentParams(uriOwner:UriOwner): protocol.DidCloseTextDocumentParams {
    return {
        textDocument: asTextDocumentIdentifier(uriOwner)
    };
}