import { Writable } from 'stream'
import { Message} from 'vscode-jsonrpc/lib/messages'
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
        let buffer: Buffer = null
        if (Buffer.isBuffer(data)) {
            buffer = data
        } else {
            buffer = new Buffer(data, encoding)
        }
        this.data = Buffer.concat([this.data, data])
        callback()
    }

}