import * as protocolConverter from '../client/protocolConverter'
import { InferenceResult, PASSED } from './protocol'

export const RESULT_OK = "inlineEvaluationOK";
export const RESULT_FAILURE = "inlineEvaluationError";

export interface EvaluationResultData {
    readonly value: string
    readonly successful: boolean
    readonly range: monaco.IRange
}

export function toEvaluationResultData(results: InferenceResult[]): EvaluationResultData[] {
    const evaluations: EvaluationResultData[] = [];
    if (results) {
        results.forEach((r) => {
            evaluations.push({
                value: r.value,
                range: protocolConverter.asRange(r.range),
                successful: r.status === PASSED
            });
        });
    }
    return evaluations;
}