import { EvaluationResult } from './evaluationResult'
import { EvaluationResultData } from './evaluationResultData'

export class EvaluationResults {

    results: EvaluationResult[] = [];

    protected editor: monaco.editor.ICodeEditor;

    constructor(editor: monaco.editor.ICodeEditor) {
        this.editor = editor;
    }

    clear() {
        this.disposeResults(this.results);
    }

    protected disposeResults(results: EvaluationResult[]) {
        if (results.length > 0) {
            this.editor.changeViewZones((viewZoneChangeAccessor) => {
                while (results.length !== 0) {
                    const result = results.pop();
                    if (result) {
                        result.dispose(viewZoneChangeAccessor);
                    }
                }
            });
        }
    }

    push(data: EvaluationResultData): void {
        const removedResults = this.removeMatched(data);
        this.disposeResults(removedResults);
        this.editor.changeViewZones((viewZoneChangeAccessor) => {
            this.results.push(new EvaluationResult(this.editor, data, viewZoneChangeAccessor));
        });
    }

    protected removeMatched(data: EvaluationResultData): EvaluationResult[] {
        const removedResults: EvaluationResult[] = [];
        let i = 0;
        while (i < this.results.length) {
            const result = this.results[i];
            if (result.match(data)) {
                removedResults.push(result);
                this.results.splice(i, 1);
            } else {
                i = i + 1;
            }
        }
        return removedResults;
    }

    update(): void {
        this.editor.changeViewZones((viewZoneChangeAccessor) => {
            const newResults: EvaluationResult[] = [];
            while (this.results.length !== 0) {
                const result = this.results.pop();
                if (result) {
                    if (result.isValid()) {
                        result.update(viewZoneChangeAccessor);
                        newResults.push(result);
                    } else {
                        result.dispose(viewZoneChangeAccessor);
                    }
                }
            }
            this.results = newResults;
        });
    }

}