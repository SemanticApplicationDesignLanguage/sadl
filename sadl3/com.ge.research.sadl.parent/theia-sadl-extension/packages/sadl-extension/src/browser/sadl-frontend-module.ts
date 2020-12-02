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
import { MonacoThemingService } from '@theia/monaco/lib/browser/monaco-theming-service';
import { LanguageClientContribution } from '@theia/languages/lib/browser/language-client-contribution';
import { WebSocketConnectionProvider } from '@theia/core/lib/browser/messaging/ws-connection-provider';
import { FrontendApplicationContribution } from '@theia/core/lib/browser/frontend-application';
import { CommandContribution, MenuContribution } from '@theia/core/lib/common';
import { LanguageGrammarDefinitionContribution } from '@theia/monaco/lib/browser/textmate/textmate-contribution';
import { FileSystemExt, FileSystemExtPath } from '../common/filesystem-ext';
import { bindSadlPreferences } from './sadl-preferences';
import { SadlMenuContribution } from './sadl-menu-contribution';
import { SadlCommandContribution } from './sadl-command-contribution';
import { configuration, monarchLanguage } from './sadl-monaco-language';
import { SadlLanguageClientContribution } from './sadl-language-client-contribution';
import { SadlHideHiddenFilesContribution } from './sadl-hide-hidden-files-contribution';
import { SadlTextmateContribution } from './sadl-textmate-contribution';

MonacoThemingService.register({
    id: 'sadlTheme',
    label: 'Light (SADL)',
    uiTheme: 'vs',
    json: require('../../data/themes/sadl.color-theme.json')
});

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
    bind(LanguageGrammarDefinitionContribution).to(SadlTextmateContribution).inSingletonScope();

    bindSadlPreferences(bind);
    bind(SadlLanguageClientContribution).toSelf().inSingletonScope();
    bind(LanguageClientContribution).toDynamicValue(ctx => ctx.container.get(SadlLanguageClientContribution));
    bind(CommandContribution).to(SadlCommandContribution).inSingletonScope();
    bind(MenuContribution).to(SadlMenuContribution).inSingletonScope();
    bind(FrontendApplicationContribution).to(SadlHideHiddenFilesContribution).inSingletonScope();

    bind(FileSystemExt).toDynamicValue(context => WebSocketConnectionProvider.createProxy(context.container, FileSystemExtPath)).inSingletonScope();
});
