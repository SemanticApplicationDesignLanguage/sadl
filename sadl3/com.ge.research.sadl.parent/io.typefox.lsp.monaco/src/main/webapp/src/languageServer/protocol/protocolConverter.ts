import * as is from 'vscode-jsonrpc/lib/is'
import * as lstypes from 'vscode-languageserver-types'
import { ProtocolCompletionItem } from './adapters'

export function asSeverity(severity: number): monaco.Severity {
    if (severity === 1) return monaco.Severity.Error
    if (severity === 2) return monaco.Severity.Warning
    if (severity === 3) return monaco.Severity.Info
    return monaco.Severity.Ignore
}

export function asMarker(diagnostic: lstypes.Diagnostic): monaco.editor.IMarkerData {
    return {
        code: diagnostic.code as string,
        severity: asSeverity(diagnostic.severity),
        message: diagnostic.message,
        source: diagnostic.source,
        startLineNumber: diagnostic.range.start.line + 1,
        startColumn: diagnostic.range.start.character + 1,
        endLineNumber: diagnostic.range.end.line + 1,
        endColumn: diagnostic.range.end.character + 1
    }
}

export function asRange(range: lstypes.Range): monaco.IRange {
    if (is.undefined(range)) {
		return undefined;
	} 
    if (is.nil(range)) {
		return null;
	}
    return {
        startLineNumber: range.start.line + 1,
        startColumn: range.start.character + 1,
        endLineNumber: range.end.line + 1,
        endColumn: range.end.character + 1
    }
}

export function asTextEdit(textEdit: lstypes.TextEdit): monaco.editor.ISingleEditOperation {
    if (is.undefined(textEdit))
        return undefined

    if (is.nil(textEdit))
        return null

    return {
        range: asRange(textEdit.range),
        text: textEdit.newText
    }
}

export function asCompletionItem(completionItem: lstypes.CompletionItem): ProtocolCompletionItem {
    return {
        label: completionItem.label,
        detail: completionItem.detail,
        documentation: completionItem.documentation,
        filterText: completionItem.filterText,
        insertText: completionItem.insertText,
        kind: completionItem.kind - 1,
        sortText: completionItem.sortText,
        textEdit: asTextEdit(completionItem.textEdit),
        data: completionItem.data
    }
}

export function asCompletionList(completionList: lstypes.CompletionList): monaco.languages.CompletionList {
    return {
        isIncomplete: completionList.isIncomplete,
        items: completionList.items.map(asCompletionItem)
    }
}

export function asHover(hover: lstypes.Hover): monaco.languages.Hover {
    if (is.undefined(hover)) {
        return undefined;
    }
    if (is.nil(hover)) {
        return null;
    }
    const contents = Array.isArray(hover.contents) ? <lstypes.MarkedString[]>hover.contents : [<lstypes.MarkedString>hover.contents];
    return {
        contents : contents,
        range : asRange(hover.range)
    };
}

export function asDocumentHighlight(highlights: lstypes.DocumentHighlight[]): monaco.languages.DocumentHighlight[] {
    if (is.undefined(highlights)) {
        return undefined;
    }
    if (is.nil(highlights)) {
        return null;
    }

    let converter = (highlight: lstypes.DocumentHighlight) => {
        return {
            range : asRange(highlight.range),
            kind: highlight.kind
        }
    }
    return highlights.map(converter);
}

export function asLocation(location: lstypes.Location|lstypes.Location[]) : monaco.languages.Location|monaco.languages.Location[] {
    if (is.undefined(location)) {
        return undefined;
    }
    if (is.nil(location)) {
        return null;
    }
    let converter = (location: lstypes.Location) => {
        return {
            uri : monaco.Uri.parse(location.uri),
            range : asRange(location.range)
        }
    }
    if (!is.array(location)) {
        return converter(location)
    } else {
        return location.map(converter)
    }
}