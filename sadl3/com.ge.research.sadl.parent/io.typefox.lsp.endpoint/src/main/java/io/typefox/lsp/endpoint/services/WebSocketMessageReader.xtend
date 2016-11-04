package io.typefox.lsp.endpoint.services

import io.typefox.lsapi.Message
import io.typefox.lsapi.services.json.MessageJsonHandler
import io.typefox.lsapi.services.json.StreamMessageReader
import io.typefox.lsapi.services.transport.io.AbstractMessageReader
import java.io.ByteArrayInputStream
import java.util.function.Consumer
import javax.websocket.CloseReason
import javax.websocket.Session
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor
import javax.websocket.MessageHandler.Partial

@FinalFieldsConstructor
class WebSocketMessageReader extends AbstractMessageReader {

    val Session session
    val MessageJsonHandler jsonHandler
    var buffer = new StringBuilder

    override listen(Consumer<Message> callback) {
        session.addMessageHandler(String, new Partial<String>() {

            override onMessage(String partialMessage, boolean last) {
                buffer.append(partialMessage);
                if (last) {
                    process(buffer.toString, callback)
                    buffer = new StringBuilder
                }
            }

        })
    }

    protected def void process(String content, Consumer<Message> callback) {
        val inputStream = new ByteArrayInputStream(content.bytes)
        val reader = new StreamMessageReader(inputStream, jsonHandler)
        reader.onError = [fireError(it)]
        reader.onRead = [fireRead($0, $1)]
        reader.listen(callback)
    }

    override close() {
        session.close(new CloseReason(CloseReason.CloseCodes.NORMAL_CLOSURE, null))
    }

}
