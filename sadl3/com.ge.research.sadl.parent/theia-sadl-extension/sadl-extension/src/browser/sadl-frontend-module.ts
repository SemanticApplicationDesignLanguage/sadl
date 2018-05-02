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

import { ContainerModule, interfaces } from 'inversify';
import { ThemeService } from '@theia/core/lib/browser/theming';
import { WebSocketConnectionProvider } from '@theia/core/lib/browser/messaging/ws-connection-provider';
import { MonacoEditorProvider } from '@theia/monaco/lib/browser/monaco-editor-provider'
import { FrontendApplicationContribution } from '@theia/core/lib/browser/frontend-application';
import { LanguageClientContribution } from '@theia/languages/lib/browser/language-client-contribution';
import { CommandContribution, MenuContribution } from '@theia/core/lib/common';
import { FileSystemExt, FileSystemExtPath } from '../common/filesystem-ext';
import { bindSadlPreferences } from './sadl-preferences';
import { SadlMenuContribution } from './sadl-menu-contribution';
import { SadlCommandContribution } from './sadl-command-contribution';
import { SadlEditorColoringService } from './sadl-editor-coloring-service';
import { configuration, monarchLanguage } from './sadl-monaco-language';
import { SadlLanguageClientContribution } from './sadl-language-client-contribution';
import { SadlMonacoEditorProvider, SadlMonacoEditorTheme } from './sadl-monaco-editor-provider';
import { SadlHideHiddenFilesContribution } from './sadl-hide-hidden-files-contribution';

import '../../src/browser/style/index.css';

// Force the `light` theme to be the default one.
ThemeService.get().setCurrentTheme('light');

export default new ContainerModule((bind: interfaces.Bind, unbind: interfaces.Unbind, isBound: interfaces.IsBound, rebind: interfaces.Rebind) => {
    monaco.languages.register({
        id: 'sadl',
        aliases: ['SADL', 'sadl'],
        extensions: ['.sadl', '.owl'],
        mimetypes: ['text/sadl']
    });
    monaco.languages.onLanguage('sadl', () => {
        monaco.languages.setLanguageConfiguration('sadl', configuration)
        monaco.languages.setMonarchTokensProvider('sadl', monarchLanguage)
    });
    [SadlMonacoEditorTheme.LIGHT, SadlMonacoEditorTheme.DARK].forEach(theme => monaco.editor.defineTheme(theme[0], theme[1]));
    rebind(MonacoEditorProvider).to(SadlMonacoEditorProvider).inSingletonScope()

    bindSadlPreferences(bind);
    bind(SadlEditorColoringService).toSelf().inSingletonScope();
    bind(SadlLanguageClientContribution).toSelf().inSingletonScope();
    bind(LanguageClientContribution).toDynamicValue(ctx => ctx.container.get(SadlLanguageClientContribution));
    bind(CommandContribution).to(SadlCommandContribution).inSingletonScope();
    bind(MenuContribution).to(SadlMenuContribution).inSingletonScope();
    bind(FrontendApplicationContribution).to(SadlHideHiddenFilesContribution).inSingletonScope();

    bind(FileSystemExt).toDynamicValue(context => WebSocketConnectionProvider.createProxy(context.container, FileSystemExtPath)).inSingletonScope();
});