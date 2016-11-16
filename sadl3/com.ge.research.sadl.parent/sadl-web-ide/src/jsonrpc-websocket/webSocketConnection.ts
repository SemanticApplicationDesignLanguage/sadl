import {
    MessageConnection,
    createMessageConnection
} from 'vscode-jsonrpc';

import {
    createWebSocket
} from './webSocket';

import {
    WebSocketMessageReader
} from './webSocketMessageReader';

import {
    WebSocketMessageWriter
} from './webSocketMessageWriter';

import {
    ConsoleLogger
} from './consoleLogger';

import {
    URL
} from '../utils/network';

export interface WebSocketMessageConnection {
    webSocket: WebSocket
    connection: MessageConnection
}

export function createWebSocketConnection(url: URL): WebSocketMessageConnection {
    const webSocket = createWebSocket(url.toString());
    const messageWriter = new WebSocketMessageWriter(webSocket);
    const messageReader = new WebSocketMessageReader(webSocket);

    const logger = new ConsoleLogger();
    const connection = createMessageConnection(messageReader, messageWriter, logger);
    webSocket.onopen = () => webSocket.onclose = () => connection.dispose();
    return { webSocket, connection };
}
