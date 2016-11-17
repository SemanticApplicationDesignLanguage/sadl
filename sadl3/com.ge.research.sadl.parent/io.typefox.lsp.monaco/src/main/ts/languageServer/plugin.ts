import {
    createMessageConnection
} from 'vscode-jsonrpc';

import {
    workspace
} from './protocol/workspace'

import { WebSocketMessageWriter } from './protocol/webSocketMessageWriter'
import { WebSocketMessageReader } from './protocol/webSocketMessageReader'
import { ConsoleLogger } from './protocol/services'
import { LanguageClient } from './protocol/languageClient'
import { URLs } from '../utils/urls'
import { createWebSocket } from './utils/utils'

import {
    registerLanguage
} from '../monaco/languages';

import {
    supportedLanguages, toMonacoLanguage
} from './languages';

export function registerLanguages() {
    for (const language of supportedLanguages) {
        registerLanguage(toMonacoLanguage(language));
    }
}

export default function openWebSocket(rootPath: string): LanguageClient {
    registerLanguages();
    const wsUrl = new URLs(location).getWebSocketUrl('sadlmonaco/languageServer')
    const webSocket = createWebSocket(wsUrl)
    const messageWriter = new WebSocketMessageWriter(webSocket);
    const messageReader = new WebSocketMessageReader(webSocket);

    const logger = new ConsoleLogger();
    const connection = createMessageConnection(messageReader, messageWriter, logger);
    const languageClient = new LanguageClient(connection, supportedLanguages, rootPath);
    webSocket.addEventListener('open', () => {
        languageClient.start().then(() => {
            // send open notification for all open editors
            for (const uri in workspace.allOpened()) {
                if (workspace.allOpened().hasOwnProperty(uri)) {
                    workspace.fireOpen(uri);
                }
            }
        })
        webSocket.addEventListener('close', (event) => {
            connection.dispose();
            languageClient.dispose();
        });
    });
    return languageClient;
}
