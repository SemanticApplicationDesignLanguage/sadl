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

import { injectable, inject } from 'inversify';
import URI from '@theia/core/lib/common/uri';
import { ThemeService } from '@theia/core/lib/browser/theming';
import { MonacoEditor } from '@theia/monaco/lib/browser/monaco-editor';
import { MessageService } from '@theia/core/lib/common/message-service';
import { MonacoWorkspace } from '@theia/monaco/lib/browser/monaco-workspace';
import { EditorPreferences } from '@theia/editor/lib/browser/editor-preferences';
import { MonacoEditorModel } from '@theia/monaco/lib/browser/monaco-editor-model';
import { MonacoEditorService } from '@theia/monaco/lib/browser/monaco-editor-service';
import { DisposableCollection } from '@theia/core/lib/common/disposable';
import { MonacoEditorProvider } from '@theia/monaco/lib/browser/monaco-editor-provider';
import { MonacoQuickOpenService } from '@theia/monaco/lib/browser/monaco-quick-open-service';
import { MonacoTextModelService } from '@theia/monaco/lib/browser/monaco-text-model-service';
import { MonacoContextMenuService } from '@theia/monaco/lib/browser/monaco-context-menu';
import { MonacoDiffNavigatorFactory } from '@theia/monaco/lib/browser/monaco-diff-navigator-factory';
import { MonacoCommandServiceFactory } from '@theia/monaco/lib/browser/monaco-command-service';
import { MonacoToProtocolConverter, ProtocolToMonacoConverter } from 'monaco-languageclient';
import { FileSystemExt } from '../common/filesystem-ext';
import { ApplicationServer } from '@theia/core/lib/common/application-protocol';

@injectable()
export class SadlMonacoEditorProvider extends MonacoEditorProvider {

    constructor(
        @inject(MonacoEditorService) protected readonly codeEditorService: MonacoEditorService,
        @inject(MonacoTextModelService) protected readonly textModelService: MonacoTextModelService,
        @inject(MonacoContextMenuService) protected readonly contextMenuService: MonacoContextMenuService,
        @inject(MonacoToProtocolConverter) protected readonly m2p: MonacoToProtocolConverter,
        @inject(ProtocolToMonacoConverter) protected readonly p2m: ProtocolToMonacoConverter,
        @inject(MonacoWorkspace) protected readonly workspace: MonacoWorkspace,
        @inject(MonacoCommandServiceFactory) protected readonly commandServiceFactory: MonacoCommandServiceFactory,
        @inject(EditorPreferences) protected readonly editorPreferences: EditorPreferences,
        @inject(MonacoQuickOpenService) protected readonly quickOpenService: MonacoQuickOpenService,
        @inject(MonacoDiffNavigatorFactory) protected readonly diffNavigatorFactory: MonacoDiffNavigatorFactory,
        @inject(ApplicationServer) protected readonly applicationServer: ApplicationServer,
        @inject(monaco.contextKeyService.ContextKeyService) protected readonly contextKeyService: monaco.contextKeyService.ContextKeyService,
        @inject(FileSystemExt) protected readonly fileSystemExt: FileSystemExt,
        @inject(MessageService) protected readonly messageService: MessageService
    ) {
        super(codeEditorService, textModelService, contextMenuService, m2p, p2m, workspace, commandServiceFactory, editorPreferences, quickOpenService, diffNavigatorFactory, applicationServer, contextKeyService);
        const changeTheme = (editorTheme: string | undefined) => {
            const monacoTheme = this.extensionThemeFor(editorTheme || 'vs');
            monaco.editor.setTheme(monacoTheme);
            document.body.classList.add(monacoTheme);
        };
        changeTheme(ThemeService.get().getCurrentTheme().editorTheme);
        (ThemeService.get() as ThemeService).onThemeChange(event => changeTheme(event.newTheme.editorTheme));
    }

    protected async createEditor(uri: URI, override: monaco.editor.IEditorOverrideServices, toDispose: DisposableCollection): Promise<MonacoEditor> {
        if (await this.fileSystemExt.canWrite(uri.toString())) {
            return super.createEditor(uri, override, toDispose);
        } else {
            const editor = await super.createEditor(uri, override, toDispose);
            if (uri.scheme === 'file' && uri.toString().endsWith('.owl')) {
                editor.getControl().updateOptions({
                    readOnly: true
                });
                this.messageService.info('Editing is disabled for the generated and built-in models.');
            }
            return editor;
        }
    }

    protected createMonacoEditorOptions(model: MonacoEditorModel): MonacoEditor.IOptions {
        const editorOptions = super.createMonacoEditorOptions(model);
        if (model.uri.toString().endsWith('.sadl')) {
            return this.applyTheme(editorOptions);
        }
        return editorOptions;
    }

    protected applyTheme(editorOptions: MonacoEditor.IOptions): MonacoEditor.IOptions {
        const defaultTheme = ThemeService.get().getCurrentTheme().editorTheme;
        if (defaultTheme) {
            const theme = this.extensionThemeFor(defaultTheme);
            return {
                ...editorOptions,
                theme
            }
        }
        return editorOptions;
    }

    protected extensionThemeFor(theme: string): string {
        const candidate = SadlMonacoEditorTheme.THEMES.find(tuple => `${tuple[0]}` === `sadl-${theme}`);
        if (candidate) {
            return candidate[0];
        }
        return SadlMonacoEditorTheme.LIGHT[0];
    }

}

/**
 * Customized themes for SADL.
 */
export namespace SadlMonacoEditorTheme {

    export const LIGHT: [string, monaco.editor.IStandaloneThemeData] = ['sadl-light-plus', {
        base: 'vs',
        inherit: true,
        rules: [
            { token: 'keyword', foreground: '761b7c' },
            { token: 'boolean', foreground: '761b7c' },
            { token: 'string', foreground: '000000' },
            { token: 'number', foreground: '000000' }
        ],
        colors: {}
    }];

    export const DARK: [string, monaco.editor.IStandaloneThemeData] = ['sadl-dark-plus', {
        base: 'vs-dark',
        inherit: true,
        rules: [
            { token: 'keyword', foreground: '761b7c' },
            { token: 'boolean', foreground: '761b7c' },
            { token: 'string', foreground: 'f5f5f5' },
            { token: 'number', foreground: 'f5f5f5' }
        ],
        colors: {}
    }];

    export const THEMES = [LIGHT, DARK];

}
