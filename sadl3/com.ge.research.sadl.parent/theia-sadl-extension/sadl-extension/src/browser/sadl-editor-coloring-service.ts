/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/

import { inject, injectable } from 'inversify';
import URI from '@theia/core/lib/common/uri';
import { EditorManager } from '@theia/editor/lib/browser/editor-manager';
import { EditorDecoration } from '@theia/editor/lib/browser/decorations/editor-decoration';
import { SadlColoring } from './protocol/sadl-coloring';

@injectable()
export class SadlEditorColoringService {

    protected readonly uriToColoringDecorationIdsMap = new Map<string, string[]>();

    @inject(EditorManager)
    protected editorManager: EditorManager;

    async updateColoringInformation(params: SadlColoring.Params): Promise<void> {
        const { uri, infos } = params;
        if (uri.toString().endsWith('.sadl')) {
            const editor = await this.editorManager.getByUri(new URI(uri));
            if (editor) {
                const textEditor = editor.editor;
                const oldIds = this.uriToColoringDecorationIdsMap.get(uri);
                const oldDecorations: string[] = oldIds ? oldIds.slice() : [];
                if (oldIds) {
                    oldIds.length = 0;
                }
                const newDecorations: EditorDecoration[] = [];
                infos.forEach(info => {
                    info.styles.forEach(style => {
                        const inlineClassName = SadlEditorColors.CSS_MAP.get(style);
                        if (inlineClassName) {
                            newDecorations.push({
                                range: info.range,
                                options: {
                                    inlineClassName
                                }
                            });
                        }
                    });
                });
                this.uriToColoringDecorationIdsMap.set(uri, textEditor.deltaDecorations({
                    oldDecorations,
                    newDecorations
                }));
            }
        }
    }

}

namespace SadlEditorColors {
    export const CSS_MAP: Map<number, string> = new Map<number, string>([
        [3, 'constructor'],
        [5, 'tag'],
        [6, 'namespace'],
        [9, 'type'],
        [11, 'warn-token'],
        [12, 'predefined'],
        [20, 'constant'],
        [21, 'attribute']
    ]);
}
