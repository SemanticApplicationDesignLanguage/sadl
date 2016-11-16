import { Writable } from 'stream'
import { Message } from 'vscode-jsonrpc/lib/messages'
import { MessageWriter, AbstractMessageWriter, StreamMessageWriter } from 'vscode-jsonrpc/lib/messageWriter'

export class WebSocketMessageWriter extends AbstractMessageWriter implements MessageWriter {

    private _socket: WebSocket

    constructor(socket: WebSocket) {
        super()
        this._socket = socket
    }

    write(msg: Message): void {
        let writable = new BufferWritable()
        let writer = new StreamMessageWriter(writable)
        writer.onError(e => this.fireError(e))
        writer.onClose(() => this.fireClose())
        writer.write(msg)
        writable.end()
        let content = writable.data.toString()
        this._socket.send(content)
    }

}

class BufferWritable extends Writable {

    data = new Buffer('')

    _write(data: any, encoding: string, callback: Function): void {
        const buffer = this.toBuffer(data, encoding);
        this.data = Buffer.concat([this.data, buffer])
        callback()
    }

    protected toBuffer(data: any, encoding: string): Buffer {
        if (Buffer.isBuffer(data)) {
            return data;
        }
        return new Buffer(data, encoding)
    }

}