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

import { ContainerModule } from 'inversify';
import { LanguageServerContribution } from '@theia/languages/lib/node';
import { ConnectionHandler, JsonRpcConnectionHandler } from "@theia/core/lib/common";
import { FileSystemExt, FileSystemExtPath } from '../common/filesystem-ext';
import { SadlLanguageServerContribution } from './sadl-language-server-contribution';
import { NodeFileSystemExt } from './node-filesystem-ext';

export default new ContainerModule(bind => {
    bind(NodeFileSystemExt).toSelf().inSingletonScope();
    bind(FileSystemExt).toDynamicValue(context => context.container.get(NodeFileSystemExt));
    bind(ConnectionHandler).toDynamicValue(context => new JsonRpcConnectionHandler(FileSystemExtPath, () => context.container.get(FileSystemExt))).inSingletonScope();
    bind(LanguageServerContribution).to(SadlLanguageServerContribution).inSingletonScope();
});
