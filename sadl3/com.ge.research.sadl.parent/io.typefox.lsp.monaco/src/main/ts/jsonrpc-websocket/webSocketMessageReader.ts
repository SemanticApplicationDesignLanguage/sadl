import { Readable } from 'stream'

import {
    MessageReader, AbstractMessageReader, DataCallback, StreamMessageReader
} from 'vscode-jsonrpc/lib/messageReader'

export class WebSocketMessageReader extends AbstractMessageReader implements MessageReader {

    private _socket: WebSocket

    constructor(socket: WebSocket) {
        super()
        this._socket = socket
    }

    listen(callback: DataCallback): void {
        this._socket.onmessage = (event) => {
            let readable = new DataReadable(event.data)
            let reader = new StreamMessageReader(readable)
            reader.onError(e => this.fireError(e))
            reader.onClose(() => this.fireClose())
            reader.listen(callback)
        }
        this._socket.onerror = (event) => {
            if (event instanceof ErrorEvent) {
                let errorEvent = event as ErrorEvent
                this.fireError(errorEvent.message)
            }
        }
        this._socket.onclose = (event) => {
            if (event.code !== 1000) {
                let error: Error = {
                    name: '' + event.code,
                    message: event.reason
                }
                this.fireError(error)
            }
            this.fireClose();
        }
    }

}

class DataReadable extends Readable {

    constructor(data: any) {
        super();
        this.push(data)
        this.push(null)
    }

    _read(size: number): void {
        // do nothing
    }

}