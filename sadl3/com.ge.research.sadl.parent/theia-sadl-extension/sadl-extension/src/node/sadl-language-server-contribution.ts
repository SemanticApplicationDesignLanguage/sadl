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

import { Socket } from 'net';
import { join, resolve } from 'path';
import { injectable } from 'inversify';
import { createSocketConnection } from 'vscode-ws-jsonrpc/lib/server';
import { isWindows } from '@theia/core/lib/common/os';
import { BaseLanguageServerContribution, IConnection } from '@theia/languages/lib/node';

const EXECUTABLE_NAME = isWindows ? 'sadl-language-server.bat' : 'sadl-language-server';
const EXECUTABLE_PATH = resolve(join(__dirname, '..', '..', 'bin', 'sadl-language-server', 'bin', EXECUTABLE_NAME));

@injectable()
export class SadlLanguageServerContribution extends BaseLanguageServerContribution {

    readonly id = 'sadl';
    readonly name = 'SADL';

    start(clientConnection: IConnection): void {
        let socketPort = this.port;
        if (socketPort) {
            const socket = new Socket();
            const serverConnection = createSocketConnection(socket, socket, () => socket.destroy());
            this.forward(clientConnection, serverConnection);
            socket.connect(socketPort);
        } else {
            const args: string[] = [];
            const serverConnection = this.createProcessStreamConnection(EXECUTABLE_PATH, args);
            this.forward(clientConnection, serverConnection);
        }
    }

    protected get port(): number | undefined {
        let arg = process.argv.filter(arg => arg.startsWith('--SADL_LSP='))[0];
        if (!arg) {
            return undefined;
        } else {
            return Number.parseInt(arg.substring('--SADL_LSP='.length), 10);
        }
    }

}
