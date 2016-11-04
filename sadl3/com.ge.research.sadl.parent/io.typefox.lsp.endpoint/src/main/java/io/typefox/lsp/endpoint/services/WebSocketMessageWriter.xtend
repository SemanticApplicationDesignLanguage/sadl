package io.typefox.lsp.endpoint.services

import io.typefox.lsapi.Message
import io.typefox.lsapi.services.json.MessageJsonHandler
import io.typefox.lsapi.services.json.StreamMessageWriter
import io.typefox.lsapi.services.transport.io.AbstractMessageWriter
import java.io.ByteArrayOutputStream
import javax.websocket.Session
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@FinalFieldsConstructor
class WebSocketMessageWriter extends AbstractMessageWriter {

    val Session session
    val MessageJsonHandler jsonHandler

    override write(Message message) {
        if (session.open) {
            val output = new ByteArrayOutputStream
            val writer = new StreamMessageWriter(output, jsonHandler)
            writer.onError = [fireError(it)]
            writer.onWrite = [fireWrite($0, $1)]
            writer.write(message)
            output.close

            val content = output.toString
            session.basicRemote.sendText(content)
        }
    }
    
    override close() {
        // do nothing
    }

}
