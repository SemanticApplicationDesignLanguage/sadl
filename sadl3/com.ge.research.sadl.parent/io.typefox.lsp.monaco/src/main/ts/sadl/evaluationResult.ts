import { EvaluationResultData } from './evaluationResultData'

export class EvaluationResult {

    protected viewZoneId: number;
    protected decorationId: string;
    protected element: HTMLElement;
    protected editor: monaco.editor.ICodeEditor;
    protected viewZone: monaco.editor.IViewZone;

    constructor(editor: monaco.editor.ICodeEditor, data: EvaluationResultData, viewChangeAccessor: monaco.editor.IViewZoneChangeAccessor) {
        this.editor = editor;

        const range = data.range;
        this.decorationId = editor.deltaDecorations([], [{ range, options: {} }])[0];

        this.element = this.createEvaluationResultElement();
        this.viewZone = { afterLineNumber: range.endLineNumber, domNode: this.element };
        this.viewZoneId = viewChangeAccessor.addZone(this.viewZone);
        this.setData(data);
    }

    isValid(): boolean {
        const range = this.getRange();
        if (range) {
            return !range.isEmpty();
        }
        return false;
    }

    dispose(viewZoneChangeAccessor: monaco.editor.IViewZoneChangeAccessor): void {
        this.editor.deltaDecorations([this.decorationId], []);
        viewZoneChangeAccessor.removeZone(this.viewZoneId);
    }

    match(data: EvaluationResultData): boolean {
        const range = this.getRange();
        if (range) {
            const intersection = range.intersectRanges(data.range);
            return !!intersection;
        }
        return false;
    }

    update(viewZoneChangeAccessor: monaco.editor.IViewZoneChangeAccessor): void {
        const range = this.getRange();
        if (range) {
            const newStartLine = this.updateStartLine(range);

            if (newStartLine !== range.startLineNumber) {
                const newRange = new monaco.Range(newStartLine, 0, range.endLineNumber, range.endColumn);
                this.decorationId = this.editor.deltaDecorations([this.decorationId], [{ range: newRange, options: {} }])[0];
            }

            this.viewZone.afterLineNumber = range.endLineNumber;
            viewZoneChangeAccessor.layoutZone(this.viewZoneId);
        }
    }

    protected getRange(): monaco.Range | null {
        return this.editor.getModel().getDecorationRange(this.decorationId);
    }

    protected isEmptyLine(lineNumber: number): boolean {
        return this.editor.getModel().getLineContent(lineNumber).trim().length === 0;
    }

    protected updateStartLine(range: monaco.IRange): number {
        let lineNumber = range.startLineNumber;
        while (lineNumber <= range.endLineNumber && this.isEmptyLine(lineNumber)) {
            lineNumber = lineNumber + 1;
        }
        return lineNumber;
    }

    protected setData(data: EvaluationResultData) {
        this.element.style.background = data.successful ? '#C7F2B1' : '#F2BBB1';
        if (data.successful) {
            this.element.textContent = 'Test passed.';
        } else {
            this.element.textContent = `Test failed. ${data.value}`;
        }
    }

    protected createEvaluationResultElement(): HTMLDivElement {
        const node = document.createElement('div');
        node.style.fontSize = '12px';
        node.style.fontFamily = 'Menlo, Monaco, \'Courier New\', monospace';
        node.style.lineHeight = '18px';
        node.style.fontStyle = 'italic';
        return node;
    }

}