const WebSocket = require('reconnecting-websocket');

/**
 * Creates reconnectable WebSocket
 */
export function createWebSocket(url: string): WebSocket {
    const opts = {
        maxReconnectionDelay: 10000,
        minReconnectionDelay: 1000,
        reconnectionDelayGrowFactor: 1.3,
        connectionTimeout: 4000,
        maxRetries: Infinity,
        debug: false,
    };
    return new WebSocket(url, void 0, opts);
} 