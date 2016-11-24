import '../style.css';
import { EvaluationResultData } from './evaluationResultData'

export class EvaluationResult {

    protected viewZoneId: number;
    protected decorationId: string;
    protected element: ElementDetails;
    protected editor: monaco.editor.ICodeEditor;
    protected viewZone: monaco.editor.IViewZone;
    protected expanded: boolean = false;

    constructor(editor: monaco.editor.ICodeEditor, data: EvaluationResultData, viewChangeAccessor: monaco.editor.IViewZoneChangeAccessor) {
        this.editor = editor;

        this.editor.onMouseDown((e) => {
            const target = e!.target;
            if (target && target.type === monaco.editor.MouseTargetType.CONTENT_VIEW_ZONE && this.isValid()) {
                const targetRange = target.range;
                const currentRange = this.getRange();
                if (targetRange.endLineNumber === currentRange.endLineNumber
                    && targetRange.endColumn === (currentRange.endColumn + 1)) {

                    this.expanded = !this.expanded;
                    this.setData(data);
                    this.viewZone.heightInLines = this.expanded ? this.element.heightInLines : 1;
                    editor.changeViewZones((accessor) => {
                        this.update(accessor);
                    });

                }
            }
        });

        const range = data.range;
        this.decorationId = editor.deltaDecorations([], [{ range, options: {} }])[0];

        this.element = this.createEvaluationResultElement();
        this.setData(data);
        this.viewZone = {
            afterLineNumber: range.endLineNumber,
            domNode: this.element.node,
            suppressMouseDown: true,
            heightInLines: this.element.heightInLines
        };
        this.viewZoneId = viewChangeAccessor.addZone(this.viewZone);
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
                const decorationOptions: monaco.editor.IModelDecorationOptions = { };
                this.decorationId = this.editor.deltaDecorations([this.decorationId], [{ range: newRange, options: decorationOptions }])[0];
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
        const node = this.element.node;
        node.style.background = data.successful ? '#C7F2B1' : '#F2BBB1';
        this.element.heightInLines = 1;
        if (this.expanded) {
            this.element.heightInLines = (data.value.match(/\n/g) || []).length + 1;
            node.style.whiteSpace = 'pre-wrap';
        }
        const segments = data.value.split('\n') || [''];
        const multiline = segments.length > 1;
        const html: string[] = [];

        const height = this.editor.getConfiguration().fontInfo.fontSize;
        const svg = multiline ? `<img src="./media/${this.expanded ? 'collapse' : 'expand'}.svg" style="width:${height}px;height:${height}px;"/>` : undefined;
        if (svg) {
            html.push(svg);
        }
        html.push(this.expanded ? data.value : `${segments[0]} ${multiline ? '...' : ''}`);
        node.innerHTML = html.join('<span>&nbsp;</span>');
    }

    protected createEvaluationResultElement(): ElementDetails {
        const node = document.createElement('div');
        node.style.height = `${this.editor.getConfiguration().lineHeight}px`;
        node.style.fontSize = '12px';
        node.style.fontFamily = 'Menlo, Monaco, \'Courier New\', monospace';
        node.style.lineHeight = '18px';
        node.style.fontStyle = 'italic';
        return {
            node
        };
    }

}

export interface ElementDetails {
    node: HTMLElement,
    heightInLines?: number;
    heightInPx?: number;
}