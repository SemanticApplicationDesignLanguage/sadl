import * as React from "react";
import { Explorer } from "./Explorer";
import { RemoteWorkspace } from "../workspace";

import {
    createWebSocketConnection
} from '../jsonrpc-websocket';

import {
    getRootPath
} from '../server';

import {
    URL, wsProtocol
} from '../utils/network';

const port = 8080;

const basePath = 'sadlmonaco';

const rootPathProviderUrl = new URL({
    port, basePath,
    path: 'rest/rootPathProvider'
});

const languageServerUrl = new URL({
    protocol: wsProtocol,
    port, basePath,
    path: 'languageServer'
});

export default () => {
    return new Promise<JSX.Element>(resolve => {
        const connection = createWebSocketConnection(languageServerUrl);
        connection.webSocket.onopen = () => {
            getRootPath(rootPathProviderUrl).then(rootPath => {
                const workspace = new RemoteWorkspace({
                    rootPath: '/Users/kosyakov/git/sadlos2/sadl3/com.ge.research.sadl.parent/sadl-web-ide',
                    connection: connection.connection
                });
                const explorer = <Explorer workspace={workspace} />;
                resolve(explorer);
            });
        };
        connection.connection.listen();
    });
}
